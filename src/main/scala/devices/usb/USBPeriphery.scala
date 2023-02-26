package org.chipsalliance.rocketchip.blocks.devices.usb

import org.chipsalliance.cde.config.{Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.BaseSubsystem

case object PeripheryUSBKey extends Field[Seq[USBParams]](Nil)

trait HasPeripheryUSB { this: BaseSubsystem =>
  val usbNodes = p(PeripheryUSBKey).map { ps =>
    USBAttachParams(ps).attachTo(this).ioNode.makeSink()
  }
}

trait HasPeripheryUSBBundle {
  val usb: Seq[USBPortIO]
}

trait HasPeripheryUSBModuleImp extends LazyModuleImp with HasPeripheryUSBBundle {
  val outer: HasPeripheryUSB
  val usb = outer.usbNodes.zipWithIndex.map { case(n,i) => n.makeIO()(ValName(s"usb_$i")) }
}

class WithUSB(baseAddress: BigInt, txEpNum: Int, initSampleRate: Int = 5) extends Config((site, here, up) => {
  case PeripheryUSBKey => Seq(USBParams(baseAddress = baseAddress, txEpNum = txEpNum, initSampleRate = initSampleRate))
})
