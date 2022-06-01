package sifive.blocks.devices.uart

import freechips.rocketchip.diplomaticobjectmodel.model.{OMDevice, OMInterrupt, OMMemoryRegion}

case class OMUART(
  divisorWidthBits: Int,
  divisorInit: Int,
  nRxEntries: Int,
  nTxEntries: Int,
  dataBits: Int,
  stopBits: Int,
  oversample: Int,
  nSamples: Int,
  includeFourWire: Boolean,
  includeParity: Boolean,
  includeIndependentParity: Boolean,
  memoryRegions: Seq[OMMemoryRegion],
  interrupts: Seq[OMInterrupt],
  _types: Seq[String] = Seq("OMUART", "OMDevice", "OMComponent"),
) extends OMDevice

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
