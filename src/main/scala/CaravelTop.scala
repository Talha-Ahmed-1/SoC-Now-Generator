package Caravel

import chisel3._
import chisel3.util.Enum
//import buraq_mini.core.Core
import nucleusrv.components.Core
import caravan.bus.common.{AddressMap, BusDecoder, Switch1toN, Peripherals}

import caravan.bus.tilelink.{TLRequest, TLResponse, TilelinkConfig, TilelinkDevice, TilelinkErr, TilelinkHost, TilelinkMaster, TilelinkSlave}
import caravan.bus.wishbone.{WBRequest, WBResponse, WishboneConfig, WishboneDevice, WishboneHost, WishboneMaster, WishboneSlave}
import caravan.bus.wishbone.{WishboneErr}
import chisel3.experimental.Analog
import chisel3.stage.ChiselStage
import jigsaw.fpga.boards.artyA7._
import jigsaw.rams.fpga.BlockRam
import jigsaw.peripherals.gpio._
import jigsaw.peripherals.spiflash._
import jigsaw.peripherals.spi._
import jigsaw.peripherals.UART._
import jigsaw.peripherals.timer._
import jigsaw.peripherals.i2c._
import jigsaw.peripherals.programmable_uart._
import jigsaw.rams.sram._

class CaravelTop(programFile: Option[String], val n:Int = 32, GPIO:Boolean = true, UART:Boolean = false, SPI:Boolean = false, TIMER:Boolean = false, I2C:Boolean = false, M:Boolean = false) extends Module {
  val io = IO(new Bundle {
    val gpio_o = Output(UInt(n.W))
    val gpio_en_o = Output(UInt(n.W))
    val gpio_i = Input(UInt(n.W))

    val rx_i = Input(UInt(1.W))
    val CLK_PER_BIT = Input(UInt(16.W))
  })

  val soc = Module(new SoCNow(programFile=None, n=n, GPIO=GPIO, UART=UART, SPI=SPI, TIMER=TIMER, I2C=I2C, M=M))
  val pll = Module(new PLL_8MHz())

  // pll.io.clk_in1 := clock
  // soc.clock := pll.io.clk_out1

  io.gpio_o := soc.io.gpio_o
  io.gpio_en_o := soc.io.gpio_en_o
  soc.io.gpio_i := io.gpio_i

  soc.io.rx_i := io.rx_i
  soc.io.CLK_PER_BIT := io.CLK_PER_BIT

}

class SoCNow(programFile: Option[String], val n:Int = 32, GPIO:Boolean = true, UART:Boolean = true, SPI:Boolean = true, TIMER:Boolean = true, I2C:Boolean = true, M:Boolean = false) extends Module {
  val io = IO(new Bundle {
    val gpio_o = Output(UInt(n.W))
    val gpio_en_o = Output(UInt(n.W))
    val gpio_i = Input(UInt(n.W))

    val rx_i = Input(UInt(1.W))
    val CLK_PER_BIT = Input(UInt(16.W))
  })

  // implicit val config: TilelinkConfig = TilelinkConfig()
  implicit val config:WishboneConfig = WishboneConfig(32,32)

  val gen_imem_host = Module(new WishboneHost())
  val gen_imem_slave = Module(new WishboneDevice())
  val gen_dmem_host = Module(new WishboneHost())
  val gen_dmem_slave = Module(new WishboneDevice())


// GPIO
  val gpio = Module(new Gpio(new WBRequest(), new WBResponse()))
  val gen_gpio_slave = Module(new WishboneDevice())

  gen_gpio_slave.io.reqOut <> gpio.io.req
  gen_gpio_slave.io.rspIn <> gpio.io.rsp

  io.gpio_o := gpio.io.cio_gpio_o(n-1,0)
  io.gpio_en_o := gpio.io.cio_gpio_en_o(n-1,0)
  gpio.io.cio_gpio_i := io.gpio_i

//   val gpioInputWires = Wire(Vec(n, Bool()))
//   val gpioOutputWires = Wire(Vec(n, Bool()))
//   val gpioEnableWires = Wire(Vec(n, Bool()))

//   val gpioPads = TriStateBuffer(quantity=n)
//   val triStateBufferWires = for {
//     ((((a,b),c),d),e) <- gpioPads zip gpioInputWires zip gpioOutputWires zip gpioEnableWires zip io.gpio_io
//   } yield (a,b,c,d,e)

//   triStateBufferWires map { case(buf: IOBUF, in: Bool, out: Bool, en: Bool, io: Analog) => {
//     buf.io.connect(in, out, io, en)
//   }}

//   gpio.io.cio_gpio_i := gpioInputWires.asUInt()
//   gpioOutputWires := gpio.io.cio_gpio_o(n-1,0).asBools()
//   gpioEnableWires := gpio.io.cio_gpio_en_o(n-1,0).asBools()
//

  var slaves = Seq(gen_dmem_slave, gen_gpio_slave)

  // val imem = Module(BlockRam.createNonMaskableRAM(None, bus=config, rows=1024))
  // val imem = Module(BlockRam.createMaskableRAM(bus=config, rows=1024))
  // val dmem = Module(BlockRam.createMaskableRAM(bus=config, rows=1024))
  val imem = Module(new SRAM1kb(new WBRequest, new WBResponse)(programFile = None))
  val dmem = Module(new SRAM1kb(new WBRequest, new WBResponse)(programFile = None))
  
  val wbErr = Module(new WishboneErr())
  val core = Module(new Core(new WBRequest, new WBResponse))


  val addresses = Seq("h40001000".U(32.W), "h40002000".U(32.W)) //, "h40003000".U(32.W), "h40004000".U(32.W) , "h40005000".U(32.W), "h40006000".U(32.W))
  val addressMap = new AddressMap

  for (i <- Peripherals.all.indices){
    addressMap.addDevice(Peripherals.all(i), addresses(i), "h00000FFF".U(32.W), slaves(i))
  }

  val devices = addressMap.getDevices

  val switch = Module(new Switch1toN(new WishboneMaster(), new WishboneSlave(), devices.size))

  // PUART
  val puart = Module(new PUart)
  puart.io.rxd := io.rx_i
  puart.io.CLK_PER_BIT := io.CLK_PER_BIT

  core.io.stall :=  false.B
  puart.io.isStalled   :=  false.B

  val idle :: read_uart :: write_iccm :: prog_finish :: Nil = Enum(4)
  val state = RegInit(idle)
  val reset_reg = RegInit(true.B)
  reset_reg := reset.asBool()
  val rx_data_reg                   =       RegInit(0.U(32.W))
  val rx_addr_reg                   =       RegInit(0.U(32.W))
  // val  state_check = RegInit(0.B)

  when(~puart.io.done){
    gen_imem_host.io.reqIn.bits.addrRequest := 0.U
    gen_imem_host.io.reqIn.bits.dataRequest := 0.U
    gen_imem_host.io.reqIn.bits.activeByteLane := 0xffff.U
    gen_imem_host.io.reqIn.bits.isWrite := 0.B
    gen_imem_host.io.reqIn.valid := 0.B
    gen_imem_host.io.rspOut.ready := true.B
    when(state === idle){
      // checking to see if the reset button was pressed previously and now it falls back to 0 for starting the read uart condition
        when(reset_reg === true.B && reset.asBool() === false.B) {
            state :=  read_uart
        }.otherwise {
            state := idle
        }

        // setting all we_i to be false, since nothing to be written
        // instr_we.foreach(w => w := false.B)
        gen_imem_host.io.reqIn.valid := false.B
        //instr_we                       :=       false.B  // active high
        // instr_addr                     :=       iccm_wb_device.io.addr_o
        // instr_wdata                    :=       DontCare
        core.io.stall :=  true.B
        puart.io.isStalled   :=  false.B
    }
    .elsewhen(state === read_uart){
        // state_check := 0.B
        // when valid 32 bits available the next state would be writing into the ICCM.
        when(puart.io.valid) {
            state                    :=       write_iccm

        }.elsewhen(puart.io.done) {
            // if getting done signal it means the read_uart state got a special ending instruction which means the
            // program is finish and no need to write to the iccm so the next state would be prog_finish

            state                  :=       prog_finish

        }.otherwise {
            // if not getting valid or done it means the 32 bits have not yet been read by the UART.
            // so the next state would still be read_uart

            state                  :=       read_uart
        }

        // setting all we_i to be false, since nothing to be written
        // instr_we.foreach(w => w := false.B)
        gen_imem_host.io.reqIn.valid := false.B
        core.io.stall           :=       true.B
        puart.io.isStalled              :=       true.B

        // store data and addr in registers if uart_ctrl.valid is high to save it since going to next state i.e write_iccm
        // will take one more cycle which may make the received data and addr invalid since by then another data and addr
        // could be written inside it.

        rx_data_reg                    :=       Mux(puart.io.valid, puart.io.rx_data_o, 0.U)
        //    rx_addr_reg                    :=       Mux(puart.io.valid, puart.io.addr_o << 2, 0.U)    // left shifting address by 2 since uart ctrl sends address in 0,1,2... format but we need it in word aligned so 1 translated to 4, 2 translates to 8 (dffram requirement)
        rx_addr_reg                    :=       Mux(puart.io.valid, puart.io.addr_o, 0.U)
    }
    .elsewhen(state === write_iccm){
      // when writing to the iccm state checking if the uart received the ending instruction. If it does then
      // the next state would be prog_finish and if it doesn't then we move to the read_uart state again to
      // read the next instruction
        when(puart.io.done) {

            state                   :=       prog_finish

        }.otherwise {

            state                   :=       read_uart

        }
        // setting all we_i to be true, since instruction (32 bit) needs to be written
        // instr_we.foreach(w => w := true.B)
        gen_imem_host.io.reqIn.valid := true.B
        gen_imem_host.io.reqIn.bits.addrRequest := rx_addr_reg
        gen_imem_host.io.reqIn.bits.dataRequest := rx_data_reg
        gen_imem_host.io.reqIn.bits.activeByteLane := 0xffff.U
        gen_imem_host.io.reqIn.bits.isWrite := 1.B
        // keep stalling the core
        core.io.stall           :=       true.B
        puart.io.isStalled         :=       true.B
    }
    .elsewhen(state === prog_finish){
        // setting all we_i to be false, since nothing to be written
        // instr_we.foreach(w => w := false.B)
        gen_imem_host.io.reqIn.valid := false.B
        //instr_we                       :=       true.B   // active low
        // instr_wdata                    :=       DontCare
        // instr_addr                     :=       iccm_wb_device.io.addr_o
        core.io.stall       :=       false.B
        puart.io.isStalled         :=       false.B
        state                      :=       idle
        // state_check := 1.B
    }

    core.io.imemRsp.bits.dataResponse := 0.U
    core.io.imemRsp.bits.error := 0.B
    // core.io.imemRsp.bits.ackWrite := 0.B
    core.io.imemRsp.valid := 0.B
    core.io.imemReq.ready := true.B

  }
  .otherwise{
        gen_imem_host.io.reqIn <> core.io.imemReq
        core.io.imemRsp <> gen_imem_host.io.rspOut
  }
////////////////////////////////////////////////////////////////////////////////////

  // tl <-> Core (fetch)
//   gen_imem_host.io.reqIn <> core.io.imemReq
//   core.io.imemRsp <> gen_imem_host.io.rspOut
  gen_imem_slave.io.reqOut <> imem.io.req
  gen_imem_slave.io.rspIn <> imem.io.rsp

  // wb <-> wb (fetch)
  gen_imem_host.io.wbMasterTransmitter <> gen_imem_slave.io.wbMasterReceiver
  gen_imem_slave.io.wbSlaveTransmitter <> gen_imem_host.io.wbSlaveReceiver

  // wb <-> Core (memory)
  gen_dmem_host.io.reqIn <> core.io.dmemReq
  core.io.dmemRsp <> gen_dmem_host.io.rspOut
  gen_dmem_slave.io.reqOut <> dmem.io.req
  gen_dmem_slave.io.rspIn <> dmem.io.rsp


  // Switch connection
  switch.io.hostIn <> gen_dmem_host.io.wbMasterTransmitter
  switch.io.hostOut <> gen_dmem_host.io.wbSlaveReceiver
  for (i <- 0 until devices.size) {
    switch.io.devIn(devices(i)._2.litValue().toInt) <> devices(i)._1.asInstanceOf[WishboneDevice].io.wbSlaveTransmitter
    switch.io.devOut(devices(i)._2.litValue().toInt) <> devices(i)._1.asInstanceOf[WishboneDevice].io.wbMasterReceiver
  }
  switch.io.devIn(devices.size) <> wbErr.io.wbSlaveTransmitter
  switch.io.devOut(devices.size) <> wbErr.io.wbMasterReceiver
  // switch.io.devSel := BusDecoder.decode(gen_dmem_host.io.wbMasterTransmitter.bits.a_address, addressMap)
  switch.io.devSel := BusDecoder.decode(gen_dmem_host.io.wbMasterTransmitter.bits.adr, addressMap)
}


import spray.json._
import DefaultJsonProtocol._

object CaravelDriver extends App {

  val file = scala.io.Source.fromFile((os.pwd.toString)+"//src//main//scala//config.json").mkString

  val fileToJson = file.parseJson.convertTo[Map[String, JsValue]]
  val config = fileToJson.map({case (a,b) => a -> {if (b == JsNumber(1)) true else false}})
  println("--------------config---------" , fileToJson)

  (new ChiselStage).emitVerilog(new CaravelTop(programFile=None, n = 32, GPIO = config("gpio"), UART = config("uart"), SPI = config("spi_flash"), TIMER = config("timer"), I2C = config("i2c"), M = config("m")))
}
