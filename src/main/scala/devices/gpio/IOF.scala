// See LICENSE for license details.
package sifive.blocks.devices.gpio

import Chisel.{defaultCompileOptions => _, _}
import freechips.rocketchip.util.CompileOptions.NotStrictInferReset
import sifive.blocks.devices.pinctrl.{PinCtrl, Pin, BasePin, EnhancedPin, EnhancedPinCtrl}

// This is the actual IOF interface.pa
// Add a valid bit to indicate whether
// there is something actually connected
// to this.
class IOFCtrl extends PinCtrl {
  val valid = Bool()
}

// By default,
object IOFCtrl {
  def apply(): IOFCtrl = {
    val iof = Wire(new IOFCtrl())
    iof.valid := Bool(false)
    iof.oval  := Bool(false)
    iof.oe    := Bool(false)
    iof.ie    := Bool(false)
    iof
  }
}

// Package up the inputs and outputs
// for the IOF
class IOFPin extends Pin {
  val o  = new IOFCtrl().asOutput

  def default(): Unit = {
    this.o.oval  := Bool(false)
    this.o.oe    := Bool(false)
    this.o.ie    := Bool(false)
    this.o.valid := Bool(false)
  }

  def inputPin(pue: Bool = Bool(false) /*ignored*/): Bool = {
    this.o.oval := Bool(false)
    this.o.oe   := Bool(false)
    this.o.ie   := Bool(true)
    this.i.ival
  }
  def outputPin(signal: Bool,
    pue: Bool = Bool(false), /*ignored*/
    ds: Bool = Bool(false), /*ignored*/
    ie: Bool = Bool(false)
  ): Unit = {
    this.o.oval := signal
    this.o.oe   := Bool(true)
    this.o.ie   := ie
  }
}

// Connect both the i and o side of the pin,
// and drive the valid signal for the IOF.
object BasePinToIOF {
  def apply(pin: BasePin, iof: IOFPin): Unit = {
    iof <> pin
    iof.o.valid := Bool(true)
  }
}

object InputPortToIOF {
  def apply(iof: IOFPin): Bool = {
    val pin = Wire(new BasePin())
    BasePinToIOF(pin, iof)
    pin.inputPin()
  }
}

object OutputPortToIOF {
  def apply(port: Bool, iof: IOFPin): Unit = {
    val pin = Wire(new BasePin())
    pin.outputPin(port)
    BasePinToIOF(pin, iof)
  }

  def apply(port: UInt, iofs: Vec[IOFPin], offset: Int) {
    require(offset >= 0, s"offset in OutputPortToIOF must be >= 0, not ${offset}")
    require((offset + port.getWidth) <= iofs.size, s"offset (${offset}) + port width(${port.getWidth}) must be <= IOF size (${iofs.size})")
    for (i <- 0 until port.getWidth) {
      apply(port(i), iofs(offset + i))
    }
  }
}
