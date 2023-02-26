package org.chipsalliance.rocketchip.blocks.devices.usb

import chisel3._
import chisel3.util._

sealed trait USBIOType
case object USBRawIOType extends USBIOType
case object USBTransceiverSTUSB03EIOType extends USBIOType

abstract class USBIO extends Bundle

object USBIO {
  def apply(x: USBIOType): USBIO = {
    x match {
      case USBRawIOType => new USBRawIO
      case USBTransceiverSTUSB03EIOType => new USBTransceiverSTUSB03EIO
    }
  }
  def tieoff(port: USBIO) {
    port match {
      case raw: USBRawIO => {
        raw.rx := 0.U
      }
      case stusb03e: USBTransceiverSTUSB03EIO => {
        stusb03e.rcv := false.B
        stusb03e.vm := false.B
        stusb03e.vp := false.B
      }
    }
  }
  def connect(port: USBIO, impl: USBTop, pullup: Bool) {
    port match {
      case raw: USBRawIO => {
        impl.io.in := raw.rx
        raw.tx := impl.io.out
        raw.pullup := pullup
      }
      case stusb03e: USBTransceiverSTUSB03EIO => {
        stusb03e.spd := true.B // always full speed
        // do now handle stusb03e.rcv
        stusb03e.vmo := USBBusState.DN(impl.io.out)
        stusb03e.vpo := USBBusState.DP(impl.io.out)
        impl.io.in := USBBusState.Cat(stusb03e.vp, stusb03e.vm)
        stusb03e.con := pullup
        stusb03e.sus := false.B // never suspend for now
        stusb03e.rsel := true.B // always use internal pull-up resistor
        // when impl.io.out === SE1, we should not transmit
        // then oe# should be high
        stusb03e.oe_sharp := impl.io.out === USBBusState.SE1
      }
    }
  }
}

class USBRawIO extends USBIO {
  val rx = Input(UInt(2.W))
  val tx = Output(UInt(2.W))
  val pullup = Output(Bool())
}

class USBTransceiverSTUSB03EIO extends USBIO {
  val spd = Output(Bool())
  val rcv = Input(Bool())
  val vmo = Output(Bool()) // FIXME: should be inout
  val vpo = Output(Bool())
  val vm = Input(Bool())
  val vp = Input(Bool())
  val con = Output(Bool())
  val sus = Output(Bool())
  val rsel = Output(Bool())
  val oe_sharp = Output(Bool()) // active-low
}
