package org.chipsalliance.rocketchip.blocks.devices.usb

import chisel3._
import chisel3.util._

object USBBusState {
  def ONE  = "b10".U(2.W)
  def ZERO = "b01".U(2.W)
  def SE0   = "b00".U(2.W)

  // full speed for now
  def J     = ONE
  def K     = ZERO

  def DP(x: UInt): UInt = x(1)
  def DN(x: UInt): UInt = x(0)
}

// 1 for J and 0 for K. Ref to Figure 7-21
object USBData {
  def T = 1.U(1.W)
  def F = 0.U(1.W)
  def IDLE = T
  def SYNC = VecInit(Seq(F,F,F,F,F,F,F,T))

  // full speed for now
  // RESET: SE0 for at least 2.5us (or 30 bit time)
  // EOP: SE0 for at least 1 bit time
  val RESET = 30
}

// Bus <- bus <- NRZIRx <- tx <- ... controller
class USBNRZITx extends Module {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val in = Flipped(Decoupled(UInt(1.W)))
    val out = Decoupled(UInt(1.W))
  })

  val lastOut = RegInit(USBData.IDLE)

  io.in.ready := io.out.ready

  // if 0, flip; if 1, keep original state
  val out = Mux(io.in.bits === USBData.T, lastOut, !lastOut)
  io.out.bits := out
  io.out.valid := io.in.valid

  // state transition when fire
  lastOut := Mux(io.reset, USBData.IDLE, Mux(io.in.fire, out, lastOut))
}

// Bus -> bus -> NRZIRx -> rx -> ... controller
class USBNRZIRx extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(1.W)))
    val out = Decoupled(UInt(1.W))
  })

  val lastIn = Reg(UInt(1.W))

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid

  when (io.in.fire) {
    lastIn := io.in.bits
  }

  // if equal, Data.T; if not equal, Data.F
  io.out.bits := lastIn === io.in.bits
}

class USBBitStuffingTx extends Module {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val in = Flipped(Decoupled(UInt(1.W)))
    val out = Decoupled(UInt(1.W))
  })

  // from 0 to 6 (6 included), 7 state in total
  // when sent 6 ones, stuff a zero
  val counter = Reg(UInt(3.W))
  when (io.reset) { counter := 0.U }
  val isStuff = counter === 6.U

  when (isStuff) {
    when (io.out.ready) {
      counter := 0.U
    } .otherwise {
      counter := counter
    }

    // stop receiving data
    io.in.ready := false.B

    // stuff one 0
    io.out.bits := USBData.F
    io.out.valid := true.B
  } .otherwise {
    val isZero = io.in.fire && io.in.bits === USBData.F
    val isOne = io.in.fire && io.in.bits === USBData.T
    // when received a zero, set back to 0
    counter := Mux1H(Seq(
      isZero -> 0.U,
      isOne -> (counter + 1.U),
      (!isZero && !isOne) -> counter))

    io.out <> io.in
  }
}

class USBBitStuffingRx extends Module {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val in = Input(UInt(1.W))
    val fire = Input(Bool())
    val isAfterSync = Input(Bool())
    val isStuff = Output(Bool())
  })

  // from 0 to 6 (6 included), 7 state in total
  // when received 6 ones, de-stuff a zero
  val counter = Reg(UInt(3.W))
  when (io.reset) { counter := 0.U }

  // we only counter 1's in packet payload
  val isInputOne = io.fire && io.in.asBool
  val isInputZero = io.fire && !io.in.asBool
  val isStuff = counter === 6.U

  // when received a zero, set back to 0
  // note that the last 0 in SYNC will reset the counter
  counter := Mux(isInputZero,
      0.U,
      Mux(isInputOne && io.isAfterSync,
        counter + 1.U,
        counter))

  io.isStuff := isStuff
}

// output 1 signal every sampleRate (e.g. 4) clocks
// when downstream can not consume this much data, buffer it
// TODO: verify that sampleRatio the enough length for Queue
class USBSamplerRx(sampleRate: Int) extends Module {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val in = Input(UInt(2.W))
    val out = DecoupledIO(UInt(2.W))
  })

  require(sampleRate > 0)

  // 2-FF synchronizer
  val inSample1 = Reg(UInt(2.W))
  val inSample2 = Reg(UInt(2.W))
  inSample1 := io.in
  inSample2 := inSample1

  val inSampleOld = Reg(UInt(2.W))
  inSampleOld := inSample2
  val inMajorityCounter = RegInit(0.U(log2Ceil(sampleRate + 1).W))
  // increase counter when sampled same signal
  val inMajorityCounterNext = Mux(inMajorityCounter === sampleRate.U, 1.U,
      Mux(inSampleOld === inSample2, inMajorityCounter + 1.U, 1.U))
  inMajorityCounter := inMajorityCounterNext
  val inMajorityValue = if (sampleRate % 2 == 0) {
    sampleRate / 2
  }  else {
    (sampleRate + 1) / 2
  }
  val inMajoritySampleOn = inMajorityCounterNext === inMajorityValue.U

  // use sampleRate as buffer length
  // TODO: actually a length of 1 may be enough
  // pipe/flow is for sampleRate === 1
  val buf = Module(new Queue(UInt(2.W), sampleRate, pipe=true, flow=true, hasFlush=true))
  buf.io.enq.bits := inSample2
  buf.io.enq.valid := inMajoritySampleOn
  buf.io.flush.get := io.reset
  // regard less of buf.ready...must enq when valid

  io.out <> buf.io.deq
}

class USBSamplerTx(sampleRate: Int) extends Module {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val in = Flipped(DecoupledIO(UInt(2.W)))
    val signalRxEop = Flipped(Valid(Bool()))
    val out = Output(UInt(2.W))
  })

  val eopCounter = RegInit(0.U(2.W))
  val eopCounterNonZero = eopCounter =/= 0.U
  when (io.signalRxEop.valid && io.signalRxEop.bits) {
    // stuff 3 idle bit time before tx real data after rx some data
    eopCounter := 3.U
  }
  when (io.reset) {
    eopCounter := 0.U
  }

  require(sampleRate > 0)
  val counter = RegInit(0.U(log2Ceil(sampleRate + 1).W))
  val sampleOn = counter === (sampleRate - 1).U
  counter := Mux(sampleOn, 0.U, counter + 1.U)
  when (io.reset) {
    counter := 0.U
  }

  when (sampleOn && eopCounterNonZero) {
    eopCounter := eopCounter - 1.U
  }

  val dataOn = sampleOn && !eopCounterNonZero

  // TODO: always fulfill buf when transmitting packet
  // pipe/flow is for sampleRate === 1
  val buf = Module(new Queue(UInt(2.W), sampleRate, pipe=true, flow=true, hasFlush=true))
  buf.io.enq <> io.in
  buf.io.deq.ready := dataOn
  buf.io.flush.get := io.reset

  // when dataOn, deq one bit from buf
  // if not bit, means high impedance
  // encoded as SE1
  val out = Reg(UInt(2.W))
  out := Mux(dataOn,
    Mux(buf.io.deq.valid,
      buf.io.deq.bits,
      "b11".U(2.W)),
    out)

  io.out := out
}

class USBSignalRx(sampleRate: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(2.W))
    val isAfterSync = Input(Bool())
    val out = DecoupledIO(UInt(1.W))
    // not decoupled eop/reset
    // packetRx/other components should be lucky to receive them
    // or, must accept them immediately
    // NOTE: when using decoupled IO, the state machine could be stuck
    val eop = Valid(Bool())
    val reset = Output(Bool())
  })

  val resetWire = WireDefault(false.B)

  // default to false
  io.eop.bits := false.B
  io.eop.valid := false.B
  io.reset := resetWire

  val sampler = Module(new USBSamplerRx(sampleRate))
  sampler.io.in := io.in
  sampler.io.out.ready := false.B
  sampler.io.reset := resetWire

  val j = sampler.io.out.bits === USBBusState.J
  val se0 = sampler.io.out.bits === USBBusState.SE0

  val dataQueue = Module(new Queue(UInt(1.W), 1, pipe=true, flow=true))
  val samplerCanFire = sampler.io.out.valid && dataQueue.io.enq.ready
  // default value
  dataQueue.io.enq.bits := 0.U
  dataQueue.io.enq.valid := false.B

  // RESET: SE0 for at least 2.5us (or 30 bit time)
  // EOP: SE0 for at least 1 bit time
  // will issue corresponding signal when received non-SE0
  val se0CounterCap = USBData.RESET
  val se0Counter = RegInit(0.U(log2Ceil(se0CounterCap).W))
  val se0CounterMax = se0Counter === (se0CounterCap - 1).U
  val se0CounterNext = se0Counter + 1.U
  val se0CounterOn = WireDefault(false.B)
  val se0CounterReset = WireDefault(false.B)
  val se0CounterNonZero = se0Counter =/= 0.U
  se0Counter := Mux(se0CounterReset,
    0.U,
    Mux(se0CounterOn && !se0CounterMax,
      se0CounterNext,
      se0Counter
      ))

  when (samplerCanFire) {
    // consume bus state from sampler
    sampler.io.out.ready := true.B
    // but dispatch it to different consumer based on state/value
    when (se0) {
      // consumer: se0 counter
      se0CounterOn := true.B
    } .otherwise {
      when (se0CounterNonZero) {
        // received se0 before
        // reset counter
        se0CounterReset := true.B
        when (se0CounterMax) {
          // consumer: reset
          resetWire := true.B
        } .otherwise {
          // consumer: eop
          // maybe we will receive SE0, SE0, K...
          io.eop.bits := j
          io.eop.valid := true.B
        }
      } .otherwise {
        // consumer: dataQueue
        // samplerCanFire ensured io.enq.ready
        dataQueue.io.enq.bits := j
        dataQueue.io.enq.valid := true.B
      }
    }
  }

  val nrzi = Module(new USBNRZIRx)
  nrzi.io.in <> dataQueue.io.deq

  val bitstuffing = Module(new USBBitStuffingRx)
  bitstuffing.io.in := nrzi.io.out.bits
  bitstuffing.io.fire := nrzi.io.out.fire
  bitstuffing.io.isAfterSync := io.isAfterSync
  bitstuffing.io.reset := resetWire

  nrzi.io.out.ready := io.out.ready
  io.out.bits := nrzi.io.out.bits
  io.out.valid := nrzi.io.out.valid && !bitstuffing.io.isStuff
}

class USBSignalTx(sampleRate: Int) extends Module {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val in = Flipped(DecoupledIO(UInt(1.W)))
    val eop = Flipped(DecoupledIO(Bool()))
    val signalRxEop = Flipped(Valid(Bool()))
    val out = Output(UInt(2.W))
  })

  val sIdle :: sEOP1 :: sEOP2 :: Nil = Enum(3)
  val state = RegInit(sIdle)
  when (io.reset) { state := sIdle }

  val bitstuffing = Module(new USBBitStuffingTx)
  bitstuffing.io.in <> io.in
  bitstuffing.io.reset := io.reset

  val nrzi = Module(new USBNRZITx)
  nrzi.io.in <> bitstuffing.io.out
  nrzi.io.reset := io.reset

  val sampler = Module(new USBSamplerTx(sampleRate))
  sampler.io.signalRxEop := io.signalRxEop
  sampler.io.reset := io.reset
  io.out := sampler.io.out

  val data = Mux(nrzi.io.out.bits === USBData.T, USBBusState.J, USBBusState.K)
  val dataValid = nrzi.io.out.valid
  val dataReady = sampler.io.in.ready && (state === sIdle)
  val dataFire = dataValid && dataReady

  nrzi.io.out.ready := dataReady
  // when no data pending, we can receive eop
  // after crc16, we may stuff one bit before we receive eop
  io.eop.ready := dataReady && !dataValid

  // default value
  sampler.io.in.bits := USBBusState.J
  sampler.io.in.valid := false.B

  switch (state) {
    is (sIdle) {
      sampler.io.in.bits := data
      sampler.io.in.valid := dataValid
      // send data normally; when no data and eop, send SE0 instead and goto EOP1
      when (io.eop.fire) {
        sampler.io.in.bits := USBBusState.SE0
        sampler.io.in.valid := true.B
        state := sEOP1
      }
    }
    is (sEOP1) {
      sampler.io.in.bits := USBBusState.SE0
      sampler.io.in.valid := true.B
      when (sampler.io.in.ready) {
        state := sEOP2
      }
    }
    is (sEOP2) {
      // reset bitstuffing for next packet
      bitstuffing.io.reset := true.B
      // reset nrzi to IDLE
      nrzi.io.reset := true.B

      sampler.io.in.bits := USBBusState.J
      sampler.io.in.valid := true.B
      when (sampler.io.in.ready) {
        state := sIdle
      }
    }
  }
}
