package sifive.blocks.devices.spi

import Chisel._

object SPIProtocol {
  val width = 2
  def Single = UInt(0, width)
  def Dual   = UInt(1, width)
  def Quad   = UInt(2, width)

  def cases = Seq(Single, Dual, Quad)
  def decode(x: UInt): Seq[Bool] = cases.map(_ === x)
}

object SPIDirection {
  val width = 1
  def Rx = UInt(0, width)
  def Tx = UInt(1, width)
}

object SPIEndian {
  val width = 1
  def MSB = UInt(0, width)
  def LSB = UInt(1, width)
}

object SPICSMode {
  val width = 2
  def Auto = UInt(0, width)
  def Hold = UInt(2, width)
  def Off  = UInt(3, width)
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
