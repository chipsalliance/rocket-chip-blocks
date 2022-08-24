package org.chipsalliance.rocketchip.blocks.devices.usb

import org.chipsalliance.cde.config.{Field, Config}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.BaseSubsystem

case object PeripheryUSBKey extends Field[Option[USBParams]](None)

trait CanHavePeripheryUSB { this: BaseSubsystem =>
  private val portName = "usb"

  val usb = p(PeripheryUSBKey) match {
    case Some(params) => {
      val module = if (params.useAXI4) {
        val usb = LazyModule(new USBAXI4(pbus.beatBytes, params)(p))
        pbus.coupleTo(portName) {
          usb.controlXing(NoCrossing) :*=
          AXI4Buffer () :*=
          TLToAXI4 () :*=
          // toVariableWidthSlave doesn't use holdFirstDeny, which TLToAXI4() needsx
          TLFragmenter(pbus.beatBytes, pbus.blockBytes, holdFirstDeny = true) :*=
          _
        }
        usb
      } else {
        val usb = LazyModule(new USBTL(pbus.beatBytes, params)(p))
        pbus.coupleTo(portName){ usb.controlXing(NoCrossing) :*= TLFragmenter(pbus) :*= _ }
        usb
      }

      // attach to interrupt bus
      ibus.fromSync := module.intXing(NoCrossing)

      Some(module.ioNode.makeSink())
    }
    case None => None
  }
}

trait CanHavePeripheryUSBModuleImp extends LazyModuleImp {
  val outer: CanHavePeripheryUSB
  val usb = outer.usb match {
    case Some(usb) => {
      Some(usb.makeIO()(ValName(s"usbport")))
    }
    case None => None
  }
}

class WithUSB(baseAddress: BigInt, txEpNum: Int, sampleRate: Int, useAXI4: Boolean = false) extends Config((site, here, up) => {
  case PeripheryUSBKey => Some(USBParams(baseAddress = baseAddress, txEpNum = txEpNum, sampleRate = sampleRate, useAXI4 = useAXI4))
})
