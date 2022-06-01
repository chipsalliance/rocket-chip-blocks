package sifive.blocks.devices.chiplink

import Chisel.{defaultCompileOptions => _, _}
import freechips.rocketchip.util.CompileOptions.NotStrictInferReset
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class ParitalExtractor[T <: TLDataChannel](gen: T) extends Module
{
  val io = new Bundle {
    val last = Bool(INPUT)
    val i = Decoupled(gen).flip
    val o = Decoupled(gen)
  }

  io.o <> io.i

  // Grab references to the fields we care about
  val (i_opcode, i_data) = io.i.bits match {
    case a: TLBundleA => (a.opcode, a.data)
    case b: TLBundleB => (b.opcode, b.data)
  }
  val (o_data, o_mask) = io.o.bits match {
    case a: TLBundleA => (a.data, a.mask)
    case b: TLBundleB => (b.data, b.mask)
  }

  val state  = RegInit(UInt(0, width=4)) // number of nibbles; [0,8]
  val shift  = Reg(UInt(width=32))
  val enable = i_opcode === TLMessages.PutPartialData
  val empty  = state === UInt(0)

  when (enable) {
    val wide = shift | (i_data << (state << 2))
    o_data := Vec.tabulate(4) { i => wide(9*(i+1)-1, 9*i+1) } .asUInt
    o_mask := Vec.tabulate(4) { i => wide(9*i) } .asUInt

    // Swallow beat if we have no nibbles
    when (empty) {
      io.i.ready := Bool(true)
      io.o.valid := Bool(false)
    }

    // Update the FSM
    when (io.i.fire()) {
      shift := Mux(empty, i_data, wide >> 36)
      state := state - UInt(1)
      when (empty)   { state := UInt(8) }
      when (io.last) { state := UInt(0) }
    }
  }
}

class PartialInjector[T <: TLDataChannel](gen: T) extends Module
{
  val io = new Bundle {
    val i_last = Bool(INPUT)
    val o_last = Bool(OUTPUT)
    val i = Decoupled(gen).flip
    val o = Decoupled(gen)
  }

  io.o <> io.i

  // Grab references to the fields we care about
  val (i_opcode, i_data, i_mask) = io.i.bits match {
    case a: TLBundleA => (a.opcode, a.data, a.mask)
    case b: TLBundleB => (b.opcode, b.data, b.mask)
  }
  val o_data = io.o.bits match {
    case a: TLBundleA => a.data
    case b: TLBundleB => b.data
  }

  val state = RegInit(UInt(0, width=4)) // number of nibbles; [0,8]
  val shift = RegInit(UInt(0, width=32))
  val full  = state(3)
  val partial = i_opcode === TLMessages.PutPartialData

  val last = RegInit(Bool(false))
  io.o_last := Mux(partial, last, io.i_last)

  when (partial) {
    val bytes = Seq.tabulate(4) { i => i_data(8*(i+1)-1, 8*i) }
    val bits  = i_mask.asBools
    val mixed = Cat(Seq(bits, bytes).transpose.flatten.reverse)
    val wide  = shift | (mixed << (state << 2))
    o_data := wide

    // Inject a beat
    when ((io.i_last || full) && !last) {
      io.i.ready := Bool(false)
    }

    // Update the FSM
    when (io.o.fire()) {
      shift := wide >> 32
      state := state + UInt(1)
      when (full || last) {
        state := UInt(0)
        shift := UInt(0)
      }
      last := io.i_last && !last
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
