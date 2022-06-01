package sifive.blocks.devices.spi

import freechips.rocketchip.diplomaticobjectmodel.model.{OMDevice, OMInterrupt, OMMemoryRegion}

case class OMSPIXIP(
  rxDepth: Int,
  txDepth: Int,
  csWidthBits: Int,
  frameBits: Int,
  delayBits: Int,
  divisorBits: Int,
  coarseDelayBits: Int,
  fineDelayBits: Int,
  sampleDelayBits: Int,
  defaultSampleDelay: Int,
  instructionAddressBytes: Int,
  instructionPadLengthBits: Int,
  memMapAddressBase: BigInt,
  memMapAddressSizeBytes: BigInt,
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  _types: Seq[String] = Seq("OMSPIXIP", "OMDevice", "OMComponent"),
) extends baseSPI with OMDevice

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
