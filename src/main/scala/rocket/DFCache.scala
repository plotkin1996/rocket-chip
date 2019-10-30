package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import chisel3.experimental._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile._
import freechips.rocketchip.util._

//  >> rowOffBits


class DFCache(n: Int)(implicit p: Parameters) extends CoreModule()(p) with HasL1HellaCacheParameters {
    def rotate1Right(x: Bits): Bits = Cat(x(0),x(x.getWidth-1,1))
    
    val io = new Bundle {
        val req = Bool(INPUT)
        val cmd = Bits(INPUT,M_SZ)
        val addr = Bits(INPUT,xLen)
        val data_in = Bits(INPUT,wordBits)
        
        val data_out = Bits(OUTPUT,wordBits)
        val data_out_valid = Bool(OUTPUT)
      }
    
    val addrs = Reg(Vec(n,Bits((tlBundleParams.addressBits-blockOffBits).W)))
    val valid = RegInit(0.U(n.W))
    val valid_words = Reg(Vec(n,Bits((cacheBlockBytes/wordBytes).W)))
    val data  = Reg(Vec(n,Vec(cacheBlockBytes/wordBytes,Bits(wordBits.W))))
    val l1way = Reg(Vec(n,Bits(wayBits.W)))
    
    val inp_addr_line = io.addr(tlBundleParams.addressBits,blockOffBits)
    val inp_addr_word = io.addr(blockOffBits,wordOffBits)
    val input_line_match = Vec((0 until n).map(i => valid(i) && addrs(i) === inp_addr_line)).asUInt
    val line_out = Mux1H(input_line_match.asBools zip data)
    
    val way2replace = RegInit(1.U(n.W))
    val req_line_selector =  Mux(input_line_match.orR,input_line_match,way2replace)
    
    val need_new_way2replace = (io.req && (
        io.cmd === M_XWR && ~input_line_match.orR ||
        io.cmd == M_XRD && input_line_match === way2replace
      ))
    
    val new_way2replace = Mux((valid|way2replace).andR, 
        rotate1Right(req_line_selector),
        PriorityMux(~(valid|way2replace).asBools  zip (0 until n).map(1.U<<_.U)))
    
    def info() = {
        for(i <-0 until n) {
            printf("[DFC] %b %x [",valid(i),addrs(i))
              for(j <- 0 until  cacheBlockBytes/wordBytes) {
                    printf("%b:%x ",valid_words(i)(j),data(i)(j))
                }
            printf("]\n")
          }
       }
    
    when(need_new_way2replace) {
        way2replace := new_way2replace
        //printf("[DFC] next way %b %b %b %b\n",way2replace,new_way2replace,valid,req_line_selector)
      }
    
    when(io.req && io.cmd === M_XRD) {
        printf("[DFC] DFC READ %x %x %x\n",io.addr,io.data_out_valid,io.data_out)
        //info()
      }
      
    io.data_out_valid := io.req && io.cmd === M_XRD && input_line_match.orR && Mux1H(input_line_match,valid_words)(inp_addr_word)
    io.data_out:= Mux1H(input_line_match,data)(inp_addr_word)
    
    when(io.req && io.cmd === M_XWR) {
        valid := valid | req_line_selector
        
        for(i <- 0 until n) {
            when(req_line_selector(i)) {
                valid_words(i) := Mux(input_line_match.orR,valid_words(i),0.U)|(1.U<<inp_addr_word)
                data(i)(inp_addr_word) := io.data_in
                addrs(i) := inp_addr_line
              }
          }
          
        printf("[VPL] DFC WRITE %x %b %x\n",io.addr,req_line_selector,io.data_in)
        //info()
      }
  }
