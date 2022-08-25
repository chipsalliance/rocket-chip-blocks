package sifive.blocks.devices.spi

import chisel3._ 
import chisel3.util._
import freechips.rocketchip.util._

class SPIMicroOp(c: SPIParamsBase) extends SPIBundle(c) {
  val fn = Bits(1.W)
  val stb = Bool()
  val cnt = UInt(c.countBits.W)
  val data = UInt(c.frameBits.W)
  val disableOE = c.oeDisableDummy.option(Bool()) // disable oe during dummy cycles in flash mode
}

object SPIMicroOp {
  def Transfer = 0.U(1.W)
  def Delay    = 1.U(1.W)
}

//Coarse delay is the number of system-clock cycles that can be added
//as a phase difference between sent and received SPI data
//Fine delay is the fine-grain delay that can be added as a phase 
//difference between send and received SPI data.
//Fine delay is typically achieved through foundry specific delay buffers
class SPIExtraDelay(c: SPIParamsBase) extends SPIBundle(c) {
  val coarse = UInt(c.divisorBits.W)
  val fine = UInt(c.fineDelayBits.W)
}
//Sample delay reflects minimum sequential delay that exists between 
//a slave and the SPI controller
class SPISampleDelay(c: SPIParamsBase) extends SPIBundle(c) {
  val sd = UInt(c.sampleDelayBits.W)
}

class SPIPhyControl(c: SPIParamsBase) extends SPIBundle(c) {
  val sck = new SPIClocking(c)
  val fmt = new SPIFormat(c)
  val extradel = new SPIExtraDelay (c)
  val sampledel = new SPISampleDelay (c)
}

/** Basic SPI component to deal with data transfer.
  *
  * It recives operations from SPI Fifo(Tx) and outputs them in SPI Port(out).
  * In the meantime, it recives feedback from slave device and uploads them to TL bus.
  *
  * ==Structure==
  *  - baud rate generator: counter to implement the target baud rate
  *  - scnt counter: counter of operation transfer
  *  - sample counter: delay counter for 'sample'
  *  - last counter: delay counter for 'last'
  *  - buffer control logic
  *   - rxd logic: transmit slave device response(dq.i) to buffer
  *   - txd logic: construct the data to be transmited on dq.o from buffer
  *
  * ==datapath==
  * {{{
  * dq.i -> rxd -> samples -> buffer -> txd -> dq.o
  * operation data(parellel in):
  * TL Interface -> buffer
  * }}}
  */
class SPIPhysical(c: SPIParamsBase) extends Module {
  val io = IO(new SPIBundle(c) {
    /** top-level SPI port */
    val port = new SPIPortIO(c)
    /** recives ctrol signals from SPI TopModule */
    val ctrl = Input(new SPIPhyControl(c))
    /** recives operation from TL bus through SPI Fifo(Tx) */
    val op = Flipped(Decoupled(new SPIMicroOp(c)))
    /** transmits slave device response to SPI Fifo(Rx) */
    val rx = Valid(Bits(c.frameBits.W))
  })

  private val op = io.op.bits
  val ctrl = Reg(new SPIPhyControl(c))
  val proto = SPIProtocol.decode(ctrl.fmt.proto)

  /** indicates operations have been all accepted */
  val accept = WireDefault(false.B)
  /** sample counter enable signal
    *
    * follows [[cref]]
    */
  val sample = WireDefault(false.B)
  /** setup txd
    *
    * follows [[cref]]
    * drives [[setup_d]]
    */
  val setup = WireDefault(false.B)
  /** enable for last delay counter */
  val last = WireDefault(false.B)

  val setup_d = RegNext(setup)

  /** counter of operation transfer
    *
    * init = op.cnt when op.data comes in
    * related ctrl signals: [[beat]]
    */
  val scnt = RegInit(0.U(c.countBits.W))
  /** Baud rate generator
    *
    * reset = io.ctrl.sck.div(3) when
    * related ctrl signals: [[stop]]
    */
  val tcnt = Reg(UInt(c.divisorBits.W))

  val stop = (scnt === 0.U)
  val beat = (tcnt === 0.U)

  // Making a delay counter for 'sample'
  val totalCoarseDel = (io.ctrl.extradel.coarse + io.ctrl.sampledel.sd)
  val sample_d = RegInit(false.B) 
  val del_cntr = RegInit(UInt(c.divisorBits.W), (c.defaultSampleDel).U)

  when (beat && sample) {
    when (totalCoarseDel > 1.U){
      del_cntr := totalCoarseDel - 1.U
    }
    .otherwise{
      del_cntr := 1.U  
      }
  }.otherwise {
   when (del_cntr =/= 0.U){
      del_cntr := del_cntr - 1.U
    }
   }

  when (del_cntr === 1.U) {
    sample_d := true.B
  }.otherwise {
    sample_d := false.B
  }
  // Making a delay counter for 'last'
  /** indicates the last set of data */
  val last_d = RegInit(false.B) 
  val del_cntr_last = RegInit(UInt(c.divisorBits.W), (c.defaultSampleDel).U)
  when (beat && last) {
    when (totalCoarseDel > 1.U){
      del_cntr_last := totalCoarseDel - 1.U
    }
    .otherwise{
      del_cntr_last := 1.U  
      }
  }.otherwise {
    when (del_cntr_last =/= 0.U){
      del_cntr_last := del_cntr_last - 1.U
    }
  }

  when (del_cntr_last === 1.U) {
    last_d := true.B
  }.otherwise {
    last_d := false.B
  }
  val decr = Mux(beat, scnt, tcnt) - 1.U
  val sched = WireDefault(beat)
  /** tcnt counter reset signal */
  tcnt := Mux(sched, ctrl.sck.div, decr)

  /** sck output */
  val sck = Reg(Bool())
  /** actual clock for
    *
    * flips in [[beat]]
    *
    * drives [[sample]] and [[setup]]
    */
  val cref = RegInit(true.B)
  /** spi mode */
  val cinv = ctrl.sck.pha ^ ctrl.sck.pol
  /** converts data to matched endian */
  private def convert(data: UInt, fmt: SPIFormat) =
    Mux(fmt.endian === SPIEndian.MSB, data, Cat(data.asBools))

  // recives reversed dq.i
  val rxd = Cat(io.port.dq.reverse.map(_.i))
  // rxd after dalay added
  val rxd_delayed = VecInit(Seq.fill(io.port.dq.size)(false.B))

  //Adding fine-granularity delay buffers on the received data
  if (c.fineDelayBits > 0){
    val fine_grain_delay = Seq.fill(io.port.dq.size) {Module(new BlackBoxDelayBuffer())}
    for (j <- 0 to (io.port.dq.size - 1)) { 
      fine_grain_delay(j).io.in := rxd(j)
      fine_grain_delay(j).io.sel := io.ctrl.extradel.fine
      rxd_delayed(j) := fine_grain_delay(j).io.mux_out
    }}
  else {
    rxd_delayed := rxd.asBools
  }

  // buffer logic
  val rxd_fin = rxd_delayed.asUInt
  val samples = Seq(rxd_fin(1), rxd_fin(1, 0), rxd_fin)

  // assuming quad
  val buffer = Reg(UInt(c.frameBits.W))
  val buffer_in = convert(io.op.bits.data, io.ctrl.fmt)
  val shift = Mux ((totalCoarseDel > 0.U), setup_d || (sample_d && stop), sample_d)
  buffer := Mux1H(proto, samples.zipWithIndex.map { case (data, i) =>
    val n = 1 << i
    val m = c.frameBits -1
    // shift: buffer[3:0], sample[3:0]
    // stay
    // buffer(3,0) or buffer(8,4)
    Cat(Mux(shift, buffer(m-n, 0), buffer(m, n)),
        // sample[3:0], or buffer (3:0)
        Mux(sample_d, data, buffer(n-1, 0)))
  })

  private def upper(x: UInt, n: Int) = x(c.frameBits-1, c.frameBits-n)

  /** the data to be transmited */
  val txd = RegInit(0.U(io.port.dq.size.W))
  val txd_in = Mux(accept, upper(buffer_in, 4), upper(buffer, 4))
  val txd_sel = SPIProtocol.decode(Mux(accept, io.ctrl.fmt.proto, ctrl.fmt.proto))
  val txd_shf = (0 until txd_sel.size).map(i => txd_in(3, 4-(1<<i)))
  when (setup) {
    txd := Mux1H(txd_sel, txd_shf)
  }
  // txd enable
  val tx = (ctrl.fmt.iodir === SPIDirection.Tx)
  val txen_in = (proto.head +: proto.tail.map(_ && tx)).scanRight(false.B)(_ || _).init
  val txen = txen_in :+ txen_in.last
  val rdisableOE = Reg(Bool())

  io.port.sck := sck
  io.port.cs := VecInit.fill(io.port.cs.size)(true.B) // dummy
  (io.port.dq zip (txd.asBools zip txen)).foreach {
    case (dq, (o, oe)) =>
      dq.o := o
      dq.oe := Mux(rdisableOE, false.B, oe)
      dq.ie := ~(dq.oe)
  }
  io.op.ready := false.B
  /** when true, transmits buffer to SPI fifo */
  val done = RegInit(true.B)
  done := done || last_d

  io.rx.valid := done
  io.rx.bits := convert(buffer, ctrl.fmt)
  /** indicates if the op requires data transfer */
  val xfr = Reg(Bool())

  when (stop) {
    sched := true.B 
    accept := true.B
  } .otherwise {
    when (beat) {
      cref := !cref
      when (xfr) {
        sck := cref ^ cinv
        sample := cref
        setup := !cref
      }
      when (!cref) {
        scnt := decr
      }
    }
  }

  when (scnt === 1.U) {
    last := beat && cref && xfr // Final sample
    when (beat && !cref) { // Final shift
      accept := true.B
      setup := false.B
      sck := ctrl.sck.pol
    }
  }
  // data and buffer transfer complete
  when (accept && done) {
    io.op.ready := true.B 
    // op data comes in
    when (io.op.valid) {
      scnt := op.cnt
      rdisableOE := io.op.bits.disableOE.getOrElse(false.B)
      when (op.stb) {
        ctrl.fmt := io.ctrl.fmt
      }

      xfr := false.B
      switch (op.fn) {
        is (SPIMicroOp.Transfer) {
          buffer := buffer_in
          sck := cinv
          setup := true.B 
          done := (op.cnt === 0.U)
          xfr := true.B
        }
        is (SPIMicroOp.Delay) {
          when (op.stb) {
            sck := io.ctrl.sck.pol
            ctrl.sck := io.ctrl.sck
          }
        }
      }
    }
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
