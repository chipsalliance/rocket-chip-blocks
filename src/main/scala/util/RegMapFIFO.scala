package sifive.blocks.util

import Chisel.{defaultCompileOptions => _, _}
import freechips.rocketchip.util.CompileOptions.NotStrictInferReset
import freechips.rocketchip.regmapper._

// MSB indicates full status
object NonBlockingEnqueue {
  def apply(enq: DecoupledIO[UInt], regWidth: Int = 32): Seq[RegField] = {
    val enqWidth = enq.bits.getWidth
    val quash = Wire(Bool())
    require(enqWidth > 0)
    require(regWidth > enqWidth)
    Seq(
      RegField(enqWidth,
        RegReadFn(UInt(0)),
        RegWriteFn((valid, data) => {
          enq.valid := valid && !quash
          enq.bits := data
          Bool(true)
        }), RegFieldDesc("data", "Transmit data", access=RegFieldAccessType.W)),
      RegField(regWidth - enqWidth - 1),
      RegField(1,
        !enq.ready,
        RegWriteFn((valid, data) =>  {
          quash := valid && data(0)
          Bool(true)
        }), RegFieldDesc("full", "Transmit FIFO full", access=RegFieldAccessType.R, volatile=true)))
  }
}

// MSB indicates empty status
object NonBlockingDequeue {
  def apply(deq: DecoupledIO[UInt], regWidth: Int = 32): Seq[RegField] = {
    val deqWidth = deq.bits.getWidth
    require(deqWidth > 0)
    require(regWidth > deqWidth)
    Seq(
      RegField.r(deqWidth,
        RegReadFn(ready => {
          deq.ready := ready
          (Bool(true), deq.bits)
        }), RegFieldDesc("data", "Receive data", volatile=true)),
      RegField(regWidth - deqWidth - 1),
      RegField.r(1, !deq.valid,
                 RegFieldDesc("empty", "Receive FIFO empty", volatile=true)))
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
