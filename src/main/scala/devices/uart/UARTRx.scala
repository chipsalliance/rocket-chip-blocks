package sifive.blocks.devices.uart

import Chisel.{defaultCompileOptions => _, _}
import freechips.rocketchip.util.CompileOptions.NotStrictInferReset

import freechips.rocketchip.util._

/** UARTRx module recivies serial input from Rx port and transmits them to Rx fifo in parallel
  *
  * ==Datapass==
  * Port(Rx) -> sample -> shifter -> Rx fifo -> TL bus
  *
  * ==Structure==
  *  - baud rate divisor counter:
  *   generate pulse, the enable signal for sample and data shift
  *  - sample counter:
  *   sample happens in middle
  *  - data counter
  *   control signals for data shift process
  *  - sample and data shift logic
  *
  * ==State Machine==
  * s_idle: detect start bit, init data_count and sample count, start pulse counter
  * s_data: data reciving
  *
  * @note Rx fifo transmits Rx data to TL bus
  */
class UARTRx(c: UARTParams) extends Module {
  val io = new Bundle {
    /** enable signal from top */
    val en = Bool(INPUT)
    /** input data from rx port */
    val in = Bits(INPUT, 1)
    /** output data to Rx fifo */
    val out = Valid(Bits(width = c.dataBits))
    /** divisor bits */
    val div = UInt(INPUT, c.divisorBits)
    /** parity enable */
    val enparity = c.includeParity.option(Bool(INPUT))
    /** parity select
      *
      * 0 -> even parity
      * 1 -> odd parity
      */
    val parity = c.includeParity.option(Bool(INPUT))
    /** parity error bit */
    val errorparity = c.includeParity.option(Bool(OUTPUT))
    /** databit select
      *
      * ture -> 8
      * false -> 9
      */
    val data8or9 = (c.dataBits == 9).option(Bool(INPUT))
  }

  if (c.includeParity)
    io.errorparity.get := false.B

  val debounce = Reg(init = UInt(0, 2))
  val debounce_max = (debounce === UInt(3))
  val debounce_min = (debounce === UInt(0))

  val prescaler = Reg(UInt(width = c.divisorBits - c.oversample + 1))
  val start = Wire(init = Bool(false))
  /** enable signal for sampling and data shifting */
  val pulse = (prescaler === UInt(0))

  private val dataCountBits = log2Floor(c.dataBits+c.includeParity.toInt) + 1
  /** init = data bits(8 or 9) + parity bit(0 or 1) + start bit(1) */
  val data_count = Reg(UInt(width = dataCountBits))
  val data_last = (data_count === UInt(0))
  val parity_bit = (data_count === UInt(1)) && io.enparity.getOrElse(false.B)

  val sample_count = Reg(UInt(width = c.oversample))
  val sample_mid = (sample_count === UInt((c.oversampleFactor - c.nSamples + 1) >> 1))
  // todo unused
  val sample_last = (sample_count === UInt(0))
  /** counter for data and sample
    *
    * {{{
    * |    data_count    |   sample_count  |
    * }}}
    */
  val countdown = Cat(data_count, sample_count) - UInt(1)

  // Compensate for the divisor not being a multiple of the oversampling period.
  // Let remainder k = (io.div % c.oversampleFactor).
  // For the last k samples, extend the sampling delay by 1 cycle.
  val remainder = io.div(c.oversample-1, 0)
  val extend = (sample_count < remainder) // Pad head: (sample_count > ~remainder)
  /** prescaler reset signal
    *
    * conditions:
    * {{{
    * start : transmisson starts
    * pulse : returns ture every pluse counter period
    * }}}
    */
  val restore = start || pulse
  val prescaler_in = Mux(restore, io.div >> c.oversample, prescaler)
  val prescaler_next = prescaler_in - Mux(restore && extend, UInt(0), UInt(1))
  /** buffer for sample results */
  val sample = Reg(Bits(width = c.nSamples))
  // take the majority bit of sample buffer
  val voter = Majority(sample.asBools.toSet)
  // data buffer
  val shifter = Reg(Bits(width = c.dataBits))

  val valid = Reg(init = Bool(false))
  valid := Bool(false)
  io.out.valid := valid
  io.out.bits := (if (c.dataBits == 8) shifter else Mux(io.data8or9.get, Cat(0.U, shifter(8,1)), shifter))

  val (s_idle :: s_data :: Nil) = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      // todo !(!io.in)?
      when (!(!io.in) && !debounce_min) {
        debounce := debounce - UInt(1)
      }
      when (!io.in) {
        debounce := debounce + UInt(1)
        when (debounce_max) {
          state := s_data
          start := Bool(true)
          prescaler := prescaler_next
          // init data_count
          data_count := UInt(c.dataBits+1) + (if (c.includeParity) io.enparity.get else 0.U) - io.data8or9.getOrElse(false.B).asUInt
          // init sample_count = 15
          sample_count := UInt(c.oversampleFactor - 1)
        }
      }
    }

    is (s_data) {
      prescaler := prescaler_next
      when (pulse) {
        // sample scan in
        sample := Cat(sample, io.in)
        data_count := countdown >> c.oversample
        sample_count := countdown(c.oversample-1, 0)

        when (sample_mid) {
          if (c.includeParity) {
            // act according to frame bit stage at its respective sampling point
            // check parity bit for error
            when (parity_bit) {
              io.errorparity.get := (shifter.asBools.reduce(_ ^ _) ^ voter ^ io.parity.get)
            }
            when (data_last) {
              state := s_idle
              valid := Bool(true)
            } .elsewhen (!parity_bit) {
              // do not add parity bit to final rx data
              shifter := Cat(voter, shifter >> 1)
            }
          } else {
            when (data_last) {
              state := s_idle
              valid := Bool(true)
            } .otherwise {
              shifter := Cat(voter, shifter >> 1)
            }
          }
        }
      }
    }
  }

  when (!io.en) {
    debounce := UInt(0)
  }
}

/*
   Copyright 2016 SiFive, Inc.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
