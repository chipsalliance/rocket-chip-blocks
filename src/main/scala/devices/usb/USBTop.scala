package org.chipsalliance.rocketchip.blocks.devices.usb

import chisel3._
import chisel3.util._

class USBTop(epNum: Int, sampleRate: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(2.W))
    val out = Output(UInt(2.W))
    val reset = Output(Bool())

    val addr = Flipped(DecoupledIO(UInt(7.W)))
    val frame = Output(UInt(11.W))
    val stall = Input(Bool())

    val ep = new EPIO(epNum)
  })

  val signalRx = Module(new USBSignalRx(sampleRate))
  val packetRx = Module(new USBPacketRx)
  val transaction = Module(new USBTransaction(epNum))
  val packetTx = Module(new USBPacketTx)
  val signalTx = Module(new USBSignalTx(sampleRate))

  packetRx.io.reset := signalRx.io.reset
  transaction.io.reset := signalRx.io.reset
  packetTx.io.reset := signalRx.io.reset
  signalTx.io.reset := signalRx.io.reset
  io.reset := signalRx.io.reset

  signalRx.io.in := io.in
  packetRx.io.in <> signalRx.io.out
  packetRx.io.eop := signalRx.io.eop
  signalRx.io.isAfterSync := packetRx.io.isAfterSync

  transaction.io.rx.pid <> packetRx.io.pid
  transaction.io.rx.data <> packetRx.io.data
  transaction.io.rx.last := packetRx.io.last

  transaction.io.addr <> io.addr
  transaction.io.stall := io.stall
  io.frame := transaction.io.frame

  transaction.io.ep <> io.ep

  packetTx.io.pid <> transaction.io.tx.pid
  packetTx.io.data <> transaction.io.tx.data
  packetTx.io.last := transaction.io.tx.last

  signalTx.io.in <> packetTx.io.out
  signalTx.io.eop <> packetTx.io.eop
  signalTx.io.signalRxEop := signalRx.io.eop

  io.out := signalTx.io.out
}
