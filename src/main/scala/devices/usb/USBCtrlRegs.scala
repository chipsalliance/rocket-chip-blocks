package org.chipsalliance.rocketchip.blocks.devices.usb

object USBCtrlRegs {
  val addr      = 0x00
  val stall     = 0x01
  val pullup    = 0x02
  val frame     = 0x04
  val interrupt = 0x08

  val rxFifo     = 0x10
  val rxEmpty    = 0x11
  val rxSetup    = 0x12
  val rxPid      = 0x13
  val rxEndp     = 0x14
  val rxConsumed = 0x15

  val txFifo   = 0x20
  val txStatus = 0x21
  val txProduced = 0x22
  val txToggle = 0x23
  val txOffset = 0x4
}
