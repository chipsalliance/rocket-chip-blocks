package org.chipsalliance.rocketchip.blocks.devices.usb

import chisel3._
import chisel3.util._

object EP {
  // plus 2 for CRC16
  val SIZE = 64 + 2
}

class EPIO(epNum: Int) extends Bundle {
  val rx = EnqIO(UInt(8.W))
  val rxConsumed = Input(Bool())
  val rxProduced = Output(Bool())
  val rxSetup = Output(Bool())
  val rxPid = Output(UInt(USBPID.SZ.W))
  val rxEndp = Output(UInt(4.W))
  val tx = Vec(epNum, DeqIO(UInt(8.W)))
  val txProduced = Input(Vec(epNum, Bool()))
  val txAcked = Output(Vec(epNum, Bool()))
  val txNaked = Output(Vec(epNum, Bool()))
  val txToggle = Vec(epNum, Flipped(Decoupled(UInt(1.W))))
}

class USBTransaction(epNum: Int) extends Module {
  val io = IO(new Bundle {
    // from usb bus
    val reset = Input(Bool())
    // from software
    val addr = Flipped(DecoupledIO(UInt(7.W)))
    val frame = Output(UInt(11.W))
    val stall = Input(Bool())
    val rx = new Bundle {
      val pid = Flipped(DecoupledIO(UInt(USBPID.SZ.W)))
      val data = Flipped(DecoupledIO(UInt(8.W)))
      val last = Flipped(Valid(Bool()))
    }
    val tx = new Bundle {
      val pid = DecoupledIO(UInt(USBPID.SZ.W))
      val data = DecoupledIO(UInt(8.W))
      val last = Output(Bool())
    }
    val ep = new EPIO(epNum)
  })

  // note that when in Rx**, when received rst, should go back to sIdle
  // in Tx**, it is normal to receive rst
  val sIdle :: sRxHandshake :: sRxAddr :: sRxEndp :: sRxToken :: sRxSOF1 :: sRxSOF2 :: sRxData :: sTxPid :: sTxData :: sTxEnd :: Nil = Enum(11)
  val state = RegInit(sIdle)
  when (io.reset) { state := sIdle }

  val pid = Reg(UInt(USBPID.SZ.W))

  val addr = RegInit(0.U(7.W)) // either empty or addressed
  val addrNext = RegInit(0.U(7.W))
  val addrNextValid = RegInit(false.B)
  io.addr.ready := true.B
  when (io.addr.valid) {
    addrNext := io.addr.bits
    addrNextValid := true.B
  }
  val addrMatch = RegInit(false.B)
  // reset addr
  when (io.reset) {
    addr := 0.U
    addrNext := 0.U
    addrNextValid := false.B
    addrMatch := false.B
  }

  val endp = Reg(UInt(4.W))

  val frame = Reg(UInt(11.W))
  io.frame := frame

  val rxEp = Module(new Queue(UInt(8.W), EP.SIZE, hasFlush = true))
  io.ep.rx <> rxEp.io.deq
  rxEp.io.flush.get := false.B
  when (io.reset) {
    rxEp.io.flush.get := true.B
  }
  val rxEpEnqThisTransaction = Reg(Bool())

  val rxEpConsumed = RegInit(true.B)
  when (io.ep.rxConsumed || io.reset) {
    rxEpConsumed := true.B
    // wont generate interrupt for produced data again
  }
  val rxEpProduced = !rxEpConsumed
  io.ep.rxProduced := rxEpProduced
  val pidProduced = Reg(UInt(USBPID.SZ.W))
  val endpProduced = Reg(UInt(4.W))
  io.ep.rxPid := pidProduced
  io.ep.rxEndp := endpProduced
  // no need reset xxxProduced as they are only valid when rxEpProduced

  require(epNum > 0 && epNum <= 16)
  val txEp = VecInit.fill(epNum) { Module(new Queue(UInt(8.W), EP.SIZE, hasFlush = true)).io }
  Seq.tabulate(epNum)(i => {
    txEp(i).enq <> io.ep.tx(i)
    // default value
    txEp(i).deq.ready := false.B
    txEp(i).flush.get := false.B
    // reset rxEp when received bus reset
    when (io.reset) {
      txEp(i).flush.get := true.B
    }
  })

  val txEpProduced = RegInit(VecInit(Seq.fill(epNum)(false.B)))
  Seq.tabulate(epNum)(i => {
    // note: io.ep.txProduced for one cycle, then txEpProduced for later cycles
    when (io.ep.txProduced(i)) {
      txEpProduced(i) := true.B
    }
    when (io.reset) {
      txEpProduced(i) := false.B
    }
  })
  // only signal for 1 cycle, interrupt logic would handle this
  val txEpAcked = WireDefault(VecInit(Seq.fill(epNum)(false.B)))
  val txEpNaked = WireDefault(VecInit(Seq.fill(epNum)(false.B)))
  io.ep.txAcked := txEpAcked
  io.ep.txNaked := txEpNaked
  val txEpConsumedThisTransaction = RegInit(false.B)

  // either data0 or data1
  val txEpToggle = RegInit(VecInit(Seq.fill(epNum)(0.U(1.W))))
  Seq.tabulate(epNum)(i => {
    io.ep.txToggle(i).ready := true.B
    when (io.ep.txToggle(i).valid) {
      txEpToggle(i) := io.ep.txToggle(i).bits
    }
    // no need to reset
  })

  // stall for EP0 IN/OUT
  // only protocol stall, no halt feature
  val stall = RegInit(false.B)
  when (io.reset) {
    stall := false.B
  }
  when (io.stall) {
    stall := true.B
  }
  val stallEffective = stall && endp === 0.U

  // flag for the next data packet
  val setup = RegInit(false.B)
  io.ep.rxSetup := setup
  when (io.reset) {
    setup := false.B
  }

  // default value
  io.rx.pid.ready := false.B
  io.rx.data.ready := false.B

  io.tx.pid.valid := false.B
  io.tx.pid.bits := 0.U
  io.tx.data.valid := false.B
  io.tx.data.bits := 0.U
  io.tx.last := false.B

  rxEp.io.enq.bits := 0.U
  rxEp.io.enq.valid := false.B

  // when rxEp has been consumed and address matches, we can enq
  // else discard the data
  val isRxEnq = rxEpConsumed && addrMatch && !(stallEffective)

  switch (state) {
    is (sIdle) {
      io.rx.pid.ready := true.B
      when (io.rx.pid.fire) {
        val rxPid = io.rx.pid.bits
        pid := rxPid
        switch (USBPID.pidType(rxPid)) {
          is (USBPID.Handshake) {
            state := sRxHandshake
          }
          is (USBPID.Data) {
            state := sRxData
            // record isRxEnq for this transaction
            // it may happen that isRxEnq would be changed by software (e.g. assert rxEpConsumed)
            // in the MIDDLE of the state sRxData and we could receive partial data
            //
            // we do it here instead of USBPID.Token
            // because addrMatch happens after sRxAddr
            rxEpEnqThisTransaction := isRxEnq
          }
          is (USBPID.Token) {
            // reset state for this transaction
            txEpConsumedThisTransaction := false.B

            when (rxPid === USBPID.SOF) {
              state := sRxSOF1
            } .otherwise {
              state := sRxAddr
            }
            when (rxPid === USBPID.SETUP) {
              // flush rxEp and set corresponding state
              // as if it has been consumed
              // so always send interrupt for setup packet
              rxEp.io.flush.get := true.B
              rxEpConsumed := true.B
              // reset stall (see protocol stall definition)
              stall := false.B
              // mark we have received setup packet
              // then the next data packet must be setup payload
              setup := true.B
            } .elsewhen (rxPid === USBPID.OUT && rxEpConsumed) {
              // clear setup flag for out token
              // when we are ready to receive the data
              setup := false.B
            }
          }
          // TODO: handle USBPID.Special
        }
      }
    }
    is (sRxHandshake) {
      // should only enter here for receiving ACK
      when (io.rx.last.valid) {
        state := sIdle
        when (io.rx.last.bits) {
          // if not receiving ack, data in Queue is now lost
          // handle it in software by detecting whether it is consumed
          // if not, re-enqueue data
          when (pid === USBPID.ACK && addrMatch && txEpConsumedThisTransaction) {
            when (addrNextValid) {
              addr := addrNext
              addrNextValid := false.B
            }
            // ready for next tx for this ep
            // signal that it is acked for one cycle
            // note: when software not receiving txEpAcked after txEpNaked, maybe retransmission...
            txEpAcked(endp) := true.B
          }
        }
      }
    }
    is (sRxAddr) {
      when (io.rx.last.valid && !io.rx.last.bits) {
        state := sIdle
      }

      io.rx.data.ready := true.B
      when (io.rx.data.fire) {
        val rxData = io.rx.data.bits
        val addressMatch = rxData(6,0) === addr
        addrMatch := addressMatch
        when (addressMatch) {
          // only proceed if address matches
          // NOTE: when addr = 0 (before addressed) it will also proceed for SetAddress
          endp := Cat(0.U(3.W), rxData(7))
        }
        state := sRxEndp
      }
    }
    is (sRxEndp) {
      when (io.rx.last.valid && !io.rx.last.bits) {
        state := sIdle
      }

      io.rx.data.ready := true.B
      when (io.rx.data.fire) {
        val rxData = io.rx.data.bits
        // rxData(7,3) is crc5, discard it
        when (addrMatch) {
          endp := Cat(rxData(2,0), endp(0))
        }
        state := sRxToken
      }
    }
    is (sRxSOF1) {
      when (io.rx.last.valid && !io.rx.last.bits) {
        state := sIdle
      }

      io.rx.data.ready := true.B
      when (io.rx.data.fire) {
        val rxData = io.rx.data.bits
        frame := Cat(0.U(3.W), rxData)
        state := sRxSOF2
      }
    }
    is (sRxSOF2) {
      when (io.rx.last.valid && !io.rx.last.bits) {
        state := sIdle
      }

      io.rx.data.ready := true.B
      when (io.rx.data.fire) {
        val rxData = io.rx.data.bits
        frame := Cat(rxData(2,0), frame(7,0))
        state := sRxToken
      }
    }
    is (sRxToken) {
      when (io.rx.last.valid) {
        when (io.rx.last.bits) {
          when (pid === USBPID.IN) {
            // we need to send data/handshake now!
            when (addrMatch) {
              state := sTxPid
            }
          } .otherwise {
            // otherwise
            //   wait for next Data packet for SETUP/OUT
            //   idle for SOF
            state := sIdle
          }
        } .otherwise {
          // TODO: discard invalid SOF (endp can be left untouched)
          state := sIdle
        }
      }
    }
    is (sRxData) {
      // we will also enq crc16 from io.rx.data
      when (io.rx.last.valid && !io.rx.last.bits) {
        state := sIdle
        rxEp.io.flush.get := true.B
      }

      when (rxEpEnqThisTransaction) {
        rxEp.io.enq <> io.rx.data
      } .otherwise {
        io.rx.data.ready := true.B
        // discard io.rx.data.bits
        rxEp.io.enq.bits := 0.U
        rxEp.io.enq.valid := false.B
      }

      when (io.rx.last.valid && io.rx.last.bits) {
        when (rxEpEnqThisTransaction) {
          // reset rxEpConsumed
          rxEpConsumed := false.B
          // store endp and pid for software
          endpProduced := endp
          pidProduced := pid
        }
        // we need to send handshake now!
        // FIXME: ISOC does not handshake
        state := sTxPid
      }
    }
    is (sTxPid) {
      switch (pid) {
        is (USBPID.IN) {
          io.tx.pid.valid := true.B
          // only check stall for ep0
          when (stallEffective) {
            io.tx.pid.bits := USBPID.STALL
          } .elsewhen (txEpProduced(endp)) {
            io.tx.pid.bits := Mux(txEpToggle(endp).asBool, USBPID.DATA1, USBPID.DATA0)
          } .otherwise {
            io.tx.pid.bits := USBPID.NAK
            // Nak for one cycle
            when (io.tx.pid.fire) {
              txEpNaked(endp) := true.B
            }
          }
          when (io.tx.pid.fire) {
            // for handshake, just go to idle
            // no need to send io.tx.last
            state := Mux(stallEffective, sIdle, Mux(txEpProduced(endp), sTxData, sIdle))
          }
        }
        is (USBPID.DATA0, USBPID.DATA1) {
          io.tx.pid.valid := true.B
          // only check stall for ep0
          when (stallEffective) {
            io.tx.pid.bits := USBPID.STALL
          } .elsewhen (rxEpEnqThisTransaction) {
            io.tx.pid.bits := USBPID.ACK
          } .otherwise {
            io.tx.pid.bits := USBPID.NAK
          }
          when (io.tx.pid.fire) {
            state := sIdle
          }
        }
      }
    }
    is (sTxData) {
      io.tx.data <> txEp(endp).deq
      // !txEp.io.deq.valid means it is empty now
      // see Queue source code for this API
      when (io.tx.data.ready && !txEp(endp).deq.valid) {
        // 1 cycle
        io.tx.last := true.B
        // no more data in txEp
        txEpProduced(endp) := false.B
        txEpConsumedThisTransaction := true.B
        state := sIdle
      }
    }
  }
}
