package org.chipsalliance.rocketchip.blocks.devices.usb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.ahb._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.interrupts._

case class USBParams(
  baseAddress: BigInt,
  // including EP0 IN
  txEpNum: Int = 2,
  sampleRate: Int,
  useAXI4: Boolean = false,
)

class USBPortIO extends Bundle {
  val rx = Input(UInt(2.W))
  val tx = Output(UInt(2.W))
  val pullup = Output(Bool())
}

class USBInterrupts(params: USBParams) extends Bundle {
  val reset = Bool()
  val rx = Bool()
  val tx = UInt(params.txEpNum.W)
}

abstract class USB(busWidthBytes: Int, val params: USBParams)
          (implicit p: Parameters)
    extends IORegisterRouter(
      RegisterRouterParams(
        name = "usb",
        compat = Seq("zenithal,usb"),
        base = params.baseAddress,
        beatBytes = busWidthBytes),
      new USBPortIO)
    with HasInterruptSources {

  def nInterrupts = 1

  lazy val module = new LazyModuleImp(this) {

  val impl = Module(new USBTop(params.txEpNum, params.sampleRate))
  // default value
  impl.io.stall := false.B

  impl.io.in := port.rx
  port.tx := impl.io.out

  val pullup = RegInit(false.B)
  port.pullup := pullup
  // write 1 to pullup, write 2 to clear pullup
  val pullupWire = WireDefault(0.U(2.W))
  when (pullupWire === 1.U) {
    pullup := true.B
  } .elsewhen (pullupWire === 2.U) {
    pullup := false.B
  }

  val ip = Wire(new USBInterrupts(params))
  interrupts := Seq(ip.asUInt.orR)

  val ipReset = RegInit(false.B)
  ip.reset := ipReset
  val ipRx = RegInit(false.B)
  ip.rx := ipRx
  val ipTx = RegInit(0.U(params.txEpNum.W))
  ip.tx := ipTx
  val txNakedAckedInit = VecInit(Seq.fill(params.txEpNum)(false.B))
  val txNaked = RegInit(txNakedAckedInit)
  val txAcked = RegInit(txNakedAckedInit)

  // FIXME: should do so but not possible now
  // generated verilog just does not contain the following assignment
  //when (impl.io.reset) {
  //  ipRx := false.B
  //  ipTx := 0.U
  //  txNaked := txNakedAckedInit
  //  txAcked := txNakedAckedInit
  //}

  val control = Seq(
    USBCtrlRegs.addr -> Seq(
      RegField.w(7, impl.io.addr)),
    USBCtrlRegs.stall -> Seq(
      RegField.w(1, impl.io.stall)),
    USBCtrlRegs.pullup -> Seq(
      RegField.w(2, pullupWire)),
    USBCtrlRegs.frame -> Seq(
      RegField.r(11, impl.io.frame)),
    )

  val interrupt = Seq(
    USBCtrlRegs.interrupt -> RegFieldGroup("ip", Some("USB interrupt pending"), Seq(
      RegField.w1ToClear(1, ipReset, impl.io.reset, Some(RegFieldDesc("reset","USB Bus reset", volatile=true, reset=Some(0)))),
      RegField.w1ToClear(1, ipRx, impl.io.ep.rxProduced, Some(RegFieldDesc("rx","USB RX Produced", volatile=true, reset=Some(0)))),
      RegField.w1ToClear(params.txEpNum, ipTx, txNaked.asUInt | txAcked.asUInt, Some(RegFieldDesc("tx","USB TX Complete or empty", volatile=true, reset=Some(0)))),
      ))
    )

  // give default value to rxConsumed
  val rxConsumed = WireDefault(false.B)
  impl.io.ep.rxConsumed := rxConsumed
  val rx = Seq(
    USBCtrlRegs.rxFifo -> Seq(
      RegField.r(8, impl.io.ep.rx)),
    // we do not know the length of the payload...
    // when produced but empty (actually always >= 2 bytes for CRC16), we know it is ZLP
    // NOTE: leveraged the timing of fulfilling io.ep.rx before setting rxProduced
    USBCtrlRegs.rxEmpty -> Seq(
      RegField.r(1, !impl.io.ep.rx.valid)),
    USBCtrlRegs.rxSetup -> Seq(
      RegField.r(1, impl.io.ep.rxSetup)),
    USBCtrlRegs.rxPid -> Seq(
      RegField.r(4, impl.io.ep.rxPid)),
    USBCtrlRegs.rxEndp -> Seq(
      RegField.r(4, impl.io.ep.rxEndp)),
    USBCtrlRegs.rxConsumed -> Seq(
      RegField.w(1, rxConsumed)),
    )

  require(params.txEpNum > 0)
  val tx = Seq.tabulate(params.txEpNum) { i =>
    val o = i * USBCtrlRegs.txOffset
    // give default value to txProduced
    val txProduced = WireDefault(false.B)
    impl.io.ep.txProduced(i) := txProduced
    Seq(
      (o + USBCtrlRegs.txFifo) -> Seq(
        RegField.w(8, impl.io.ep.tx(i))),
      (o + USBCtrlRegs.txStatus) -> RegFieldGroup("txStatus", Some(s"USB txStatus for $i"), Seq(
        RegField.w1ToClear(1, txNaked(i), impl.io.ep.txNaked(i), Some(RegFieldDesc("txNaked","tx FIFO empty", volatile=true, reset=Some(0)))),
        RegField.w1ToClear(1, txAcked(i), impl.io.ep.txAcked(i), Some(RegFieldDesc("txAcked","tx complete", volatile=true, reset=Some(0)))),
        // require txStatus width <= 8
      )),
      (o + USBCtrlRegs.txProduced) -> Seq(
        RegField.w(1, txProduced)),
      (o + USBCtrlRegs.txToggle) -> Seq(
        RegField.w(1, impl.io.ep.txToggle(i)))
      )
  } reduce(_ ++ _)

  regmap(control ++ interrupt ++ rx ++ tx:_*)
}
}

/** Specialize the generic USB to make it attachable to an TL interconnect. */
class USBTL(busWidthBytes: Int, params: USBParams)(implicit p: Parameters)
  extends USB(busWidthBytes, params) with HasTLControlRegMap

/** Specialize the generic USB to make it attachable to an AXI4 interconnect. */
class USBAXI4(busWidthBytes: Int, params: USBParams)(implicit p: Parameters)
  extends USB(busWidthBytes, params) with HasAXI4ControlRegMap

/** Specialize the generic USB to make it attachable to an AHB interconnect. */
class USBAHB(busWidthBytes: Int, params: USBParams)(implicit p: Parameters)
  extends USB(busWidthBytes, params) with HasAHBControlRegMap
