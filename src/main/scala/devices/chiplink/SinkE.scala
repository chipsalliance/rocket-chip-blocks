package sifive.blocks.devices.chiplink

import Chisel.{defaultCompileOptions => _, _}
import freechips.rocketchip.util.CompileOptions.NotStrictInferReset
import freechips.rocketchip.tilelink._

class SinkE(info: ChipLinkInfo) extends Module
{
  val io = new Bundle {
    val e = Decoupled(new TLBundleE(info.edgeIn.bundle)).flip
    val q = Decoupled(new DataLayer(info.params))
    // Find the sink from D
    val d_tlSink = Valid(UInt(width = info.params.sinkBits))
    val d_clSink = UInt(INPUT, width = info.params.clSinkBits)
  }

  io.d_tlSink.valid := io.e.fire()
  io.d_tlSink.bits := io.e.bits.sink

  val header = info.encode(
    format = UInt(4),
    opcode = UInt(0),
    param  = UInt(0),
    size   = UInt(0),
    domain = UInt(0),
    source = io.d_clSink)

  io.e.ready := io.q.ready
  io.q.valid := io.e.valid
  io.q.bits.last  := Bool(true)
  io.q.bits.data  := header
  io.q.bits.beats := UInt(1)
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
