package sifive.blocks.util

import Chisel._
import chisel3.{BlackBox, RawModule, withClockAndReset}
import freechips.rocketchip.util.{AsyncResetRegVec, AsyncResetReg}

class AsyncDownCounter(clock: Clock, reset: Bool, value: Int)
    extends Module (_clock = clock, _reset = reset) {
  val io = new Bundle {
    val done = Bool(OUTPUT)
  }

  val count_next = Wire(UInt(width = log2Ceil(value)))
  val count = AsyncResetReg(
    updateData = count_next,
    resetData = value,
    name = "count_reg")
  val done_reg = AsyncResetReg(
    updateData = (count === UInt(0)),
    resetData = 0,
    name = "done_reg")

  when (count > UInt(0)) {
    count_next := count - UInt(1)
  } .otherwise {
    count_next := count
  }

  io.done := done_reg
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
