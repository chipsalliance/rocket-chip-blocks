package sifive.blocks.devices.spi

import Chisel.{defaultCompileOptions => _, _}
import freechips.rocketchip.util.CompileOptions.NotStrictInferReset

class SPIInnerIO(c: SPIParamsBase) extends SPILinkIO(c) {
  val lock = Bool(OUTPUT)
}

class SPIArbiter(c: SPIParamsBase, n: Int) extends Module {
  val io = new Bundle {
    val inner = Vec(n, new SPIInnerIO(c)).flip
    val outer = new SPILinkIO(c)
    val sel = UInt(INPUT, log2Up(n))
  }

  val sel = Reg(init = Vec(Bool(true) +: Seq.fill(n-1)(Bool(false))))

  io.outer.tx.valid := Mux1H(sel, io.inner.map(_.tx.valid))
  io.outer.tx.bits := Mux1H(sel, io.inner.map(_.tx.bits))
  io.outer.cnt := Mux1H(sel, io.inner.map(_.cnt))
  io.outer.fmt := Mux1H(sel, io.inner.map(_.fmt))
  // Workaround for overzealous combinational loop detection
  io.outer.cs := Mux(sel(0), io.inner(0).cs, io.inner(1).cs)
  io.outer.disableOE.foreach (_ := io.inner(0).disableOE.get)
  require(n == 2, "SPIArbiter currently only supports 2 clients")

  (io.inner zip sel).foreach { case (inner, s) =>
    inner.tx.ready := io.outer.tx.ready && s
    inner.rx.valid := io.outer.rx.valid && s
    inner.rx.bits := io.outer.rx.bits
    inner.active := io.outer.active && s
  }

  val nsel = Vec.tabulate(n)(io.sel === UInt(_))
  val lock = Mux1H(sel, io.inner.map(_.lock))
  when (!lock) {
    sel := nsel
    when (sel.asUInt =/= nsel.asUInt) {
      io.outer.cs.clear := Bool(true)
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
