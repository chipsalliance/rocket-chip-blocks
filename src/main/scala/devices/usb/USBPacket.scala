package org.chipsalliance.rocketchip.blocks.devices.usb

import chisel3._
import chisel3.util._

object USBPID {
  val SZ    = 4
  // MSb order
  def OUT   = "b0001".U(SZ.W)
  def IN    = "b1001".U(SZ.W)
  def SOF   = "b0101".U(SZ.W)
  def SETUP = "b1101".U(SZ.W)

  def DATA0 = "b0011".U(SZ.W)
  def DATA1 = "b1011".U(SZ.W)

  def ACK   = "b0010".U(SZ.W)
  def NAK   = "b1010".U(SZ.W)
  def STALL = "b1110".U(SZ.W)

  def PRE   = "b1100".U(SZ.W)

  val TSZ   = 2
  def pidType(pid: UInt): UInt = pid(TSZ-1,0)
  def Token = "b01".U(TSZ.W)
  def Data = "b11".U(TSZ.W)
  def Handshake = "b10".U(TSZ.W)
  def Special = "b00".U(TSZ.W)

  val NSZ   = 2
  def name(pid: UInt): UInt = pid(NSZ+TSZ-1,TSZ)
}

class USBCRC(len: Int) extends Module {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val in = Input(UInt(1.W))
    val dump = Input(Bool())
    val valid = Input(Bool())
    val out = Output(UInt(1.W))
    val equal = Output(Bool())
  })

  val reg = Reg(UInt(len.W))

  val top = reg(len-1)

  require(len == 5 || len == 16)
  val generator = if (len == 5) "b00101".U(5.W) else "b1000000000000101".U(16.W)
  val residual  = if (len == 5) "b01100".U(5.W) else "b1000000000001101".U(16.W)

  val nextReg = Mux(io.reset,
    ~0.U(len.W),
    Mux(io.valid,
      Mux((io.in ^ top).asBool && !io.dump,
        (reg << 1) ^ generator,
        reg << 1),
      reg))(len-1,0)
  reg := nextReg

  // the CRC in the generator is inverted and sent to the checker MSb first
  io.out := !top
  io.equal := nextReg === residual
}

class USBPacketTx extends Module {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val pid = Flipped(DecoupledIO(UInt(USBPID.SZ.W)))
    val data = Flipped(DecoupledIO(UInt(8.W)))
    // last bit of data
    // TODO: use Decoupled
    val last = Input(Bool())
    val out = DecoupledIO(UInt(1.W))
    val eop = DecoupledIO(Bool())
  })

  val sIdle :: sSync :: sPID :: sData :: sCRC1 :: sCRC2 :: sEOP :: Nil = Enum(7)
  val state = RegInit(sIdle)
  when (io.reset) { state := sIdle }

  val data = Reg(UInt(8.W))

  val pid = Reg(UInt(USBPID.SZ.W))
  // SYNC contains 8 bits, no log2Ceil ...
  // also used by PID, 8 bits
  // also used by CRC1 and CRC2, 8 + 8 bits
  val counterOn = WireDefault(false.B)
  val counterReset = WireDefault(false.B)
  val counter = Reg(UInt(3.W))
  counter := Mux(counterReset || io.reset,
    0.U,
    Mux(counterOn,
      counter + 1.U,
      counter))
  val counterLast = counter === 7.U

  val crc = Module(new USBCRC(16))
  crc.io.reset := io.reset // see USBPID.Data below
  crc.io.dump := false.B
  crc.io.valid := false.B
  crc.io.in := USBData.IDLE

  // default value
  io.pid.ready := false.B
  io.data.ready := false.B

  io.eop.bits := true.B
  io.eop.valid := false.B

  io.out.valid := false.B
  io.out.bits := USBData.IDLE

  switch (state) {
    is (sIdle) {
      io.pid.ready := true.B
      when (io.pid.valid) {
        pid := io.pid.bits
        counterReset := true.B
        state := sSync
      }
    }
    is (sSync) {
      io.out.valid := true.B
      io.out.bits := USBData.SYNC(counter)
      when (io.out.ready) {
        counterOn := true.B
        // last bit of SYNC
        // counter overflowed back to 0 in next cycle
        when (counterLast) {
          state := sPID
        }
      }
    }
    is (sPID) {
      io.out.valid := true.B
      val hi = counter(2)
      val lo = counter(1,0)
      // 0, 1, 2, 3, ^0, ^1, ^2, ^3
      io.out.bits := hi ^ pid(lo)
      when (io.out.ready) {
        counterOn := true.B
        // last bit of PID
        // counter overflowed back to 0 in next cycle
        when (counterLast) {
          state := sEOP
          when (USBPID.pidType(pid) === USBPID.Data) {
            state := sData
            crc.io.reset := true.B
          }
        }
      }
    }
    is (sData) {
      val dataReady = counter === 0.U && io.out.ready
      io.data.ready := dataReady
      when (io.data.fire) {
        data := io.data.bits
        counterOn := true.B
        io.out.valid := true.B
        io.out.bits := io.data.bits(0)
        crc.io.valid := true.B
        crc.io.in := io.data.bits(0)
      }

      when (counter =/= 0.U) {
        io.out.valid := true.B
        io.out.bits := data(counter)
        when (io.out.ready) {
          counterOn := true.B
          crc.io.valid := true.B
          crc.io.in := data(counter)
        }
      }

      // need to signal ready for last by setting io.data.ready
      when (io.last && dataReady) {
        state := sCRC1
      }
    }
    is (sCRC1, sCRC2) {
      io.out.valid := true.B
      // TODO: move it out of state machine
      io.out.bits := crc.io.out
      crc.io.dump := true.B
      when (io.out.ready) {
        crc.io.valid := true.B
        counterOn := true.B
        // first 8 bits of CRC16
        when (counterLast) {
          switch (state) {
            is (sCRC1) {
              state := sCRC2
            }
            is (sCRC2) {
              state := sEOP
            }
          }
        }
      }
    }
    is (sEOP) {
      io.eop.valid := true.B
      when (io.eop.ready) {
        state := sIdle
      }
    }
  }
}

class USBPacketRx extends Module {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val in = Flipped(DecoupledIO(UInt(1.W)))
    val eop = Flipped(Valid(Bool()))
    // for SignalRx
    val isAfterSync = Output(Bool())
    // for Transaction
    val pid = DecoupledIO(UInt(USBPID.SZ.W))
    val data = DecoupledIO(UInt(8.W))
    // last indicate it is a valid packet
    val last = Valid(Bool())
  })

  val sIdle :: sPID :: sData5 :: sData16 :: sEOP :: Nil = Enum(5)
  val state = RegInit(sIdle)
  when (io.reset) { state := sIdle }

  // when not idle, we have recognized a SYNC pattern
  // this will continue until we received EOP
  io.isAfterSync := state =/= sIdle

  io.in.ready := false.B
  io.last.bits := false.B
  io.last.valid := false.B

  io.pid.valid := false.B
  io.data.valid := false.B

  // init with all 1 to simulate idle
  val syncInit = "b11111111".U(8.W)
  val sync = RegInit(syncInit)
  val syncHi7 = sync(sync.getWidth-1,1)
  // right shift by 1 and prepend input
  val nextSyncShift = Cat(io.in.bits, syncHi7)
  val isNext = WireDefault(false.B)
  val nextSync = Mux(io.reset, syncInit, Mux(isNext, nextSyncShift, sync))
  sync := nextSync
  val nextSyncLo4 = nextSync(sync.getWidth-4-1,0)
  val nextSyncHi4 = nextSync(sync.getWidth-1,sync.getWidth-4)

  io.data.bits := nextSync

  val pid = nextSyncLo4
  val pidComplement = ~nextSyncHi4
  val pidValid = pid === pidComplement
  io.pid.bits := pid

  // SYNC contains 8 bits
  // also used by PID, 8 bits
  // also used by ADDR+ENDP, 7 + 4 + 5 bits
  // also used by CRC1 and CRC2, 8 + 8 bits
  // also used by DATA, 8 + 8 bits
  val counterOn = WireDefault(false.B)
  val counterReset = WireDefault(false.B)
  val counter = Reg(UInt(3.W))
  counter := Mux(counterReset || io.reset,
    0.U,
    Mux(counterOn,
      counter + 1.U,
      counter))
  val counterLast = counter === 7.U

  val crc16 = Module(new USBCRC(16))
  crc16.io.reset := io.reset
  crc16.io.dump := false.B
  crc16.io.valid := false.B
  crc16.io.in := io.in.bits

  val crc5 = Module(new USBCRC(5))
  crc5.io.reset := io.reset
  crc5.io.dump := false.B
  crc5.io.valid := false.B
  crc5.io.in := io.in.bits

  switch (state) {
    is (sIdle) {
      io.last.bits := false.B
      io.last.valid := true.B
      io.in.ready := true.B
      when (io.in.fire) {
        isNext := true.B
        when (nextSync === USBData.SYNC.asUInt) {
          counterReset := true.B
          state := sPID
        }
      }
    }
    is (sPID) {
      // when last bit, we need to transfer a pid
      io.in.ready := !counterLast || (counterLast && io.pid.ready)
      when (io.in.fire) {
        // consume one bit
        isNext := true.B
        counterOn := true.B
        when (counterLast) {
          // counter back to 0 in the next state!
          when (pidValid) {
            // ensured io.pid.ready
            io.pid.valid := true.B
            when (USBPID.pidType(pid) === USBPID.Data) {
              state := sData16
              crc16.io.reset := true.B
            } .elsewhen (USBPID.pidType(pid) === USBPID.Token) {
              state := sData5
              crc5.io.reset := true.B
            } .elsewhen (USBPID.pidType(pid) === USBPID.Handshake) {
              state := sEOP
            }
          } .otherwise {
            // error detected
            state := sIdle
          }
        }
      }
    }
    is (sData5, sData16) {
      // when last bit, we need to transfer a byte
      io.in.ready := !counterLast || (counterLast && io.data.ready)
      when (io.in.fire) {
        // consume one bit
        isNext := true.B
        counterOn := true.B

        crc16.io.valid := true.B
        crc5.io.valid := true.B

        when (counterLast) {
          // counter back to 0 in the next state of counterOn
          // otherwise keep data
          io.data.valid := true.B
        }
      }

      // CRC5/16 and EOP for them is handled here!
      // NOTE: it is assumed when io.eop, io.in.valid === 0
      // thus crc16.io.equal comes from nextReg from reg directly
      when (io.eop.valid) {
        state := sIdle
        switch (state) {
          is (sData5) {
            when (crc5.io.equal && io.eop.bits) {
              io.last.bits := true.B
              io.last.valid := true.B
            }
          }
          is (sData16) {
            when (crc16.io.equal && io.eop.bits) {
              io.last.bits := true.B
              io.last.valid := true.B
            }
          }
        }
      }
    }
    is (sEOP) {
      // only for Handshake packet
      when (io.eop.valid) {
        io.last.bits := io.eop.bits
        io.last.valid := true.B
        state := sIdle
      }
    }
  }
}
