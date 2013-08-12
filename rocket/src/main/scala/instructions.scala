package rocket

import Chisel._
import Node._

object Instructions
{
 // automatically generated by parse-opcodes
  def J          = Bits("b?????????????????????????_1100111",32);
  def JAL        = Bits("b?????????????????????????_1101111",32);
  def JALR_C     = Bits("b?????_?????_????????????_000_1101011",32);
  def JALR_R     = Bits("b?????_?????_????????????_001_1101011",32);
  def JALR_J     = Bits("b?????_?????_????????????_010_1101011",32);
  def RDNPC      = Bits("b?????_00000_000000000000_100_1101011",32);
  def BEQ        = Bits("b?????_?????_?????_???????_000_1100011",32);
  def BNE        = Bits("b?????_?????_?????_???????_001_1100011",32);
  def BLT        = Bits("b?????_?????_?????_???????_100_1100011",32);
  def BGE        = Bits("b?????_?????_?????_???????_101_1100011",32);
  def BLTU       = Bits("b?????_?????_?????_???????_110_1100011",32);
  def BGEU       = Bits("b?????_?????_?????_???????_111_1100011",32);
  def LUI        = Bits("b?????_????????????????????_0110111",32);
  def AUIPC      = Bits("b?????_????????????????????_0010111",32);
  def ADDI       = Bits("b?????_?????_????????????_000_0010011",32);
  def SLLI       = Bits("b?????_?????_000000_??????_001_0010011",32);
  def SLTI       = Bits("b?????_?????_????????????_010_0010011",32);
  def SLTIU      = Bits("b?????_?????_????????????_011_0010011",32);
  def XORI       = Bits("b?????_?????_????????????_100_0010011",32);
  def SRLI       = Bits("b?????_?????_000000_??????_101_0010011",32);
  def SRAI       = Bits("b?????_?????_000001_??????_101_0010011",32);
  def ORI        = Bits("b?????_?????_????????????_110_0010011",32);
  def ANDI       = Bits("b?????_?????_????????????_111_0010011",32);
  def ADD        = Bits("b?????_?????_?????_0000000000_0110011",32);
  def SUB        = Bits("b?????_?????_?????_1000000000_0110011",32);
  def SLL        = Bits("b?????_?????_?????_0000000001_0110011",32);
  def SLT        = Bits("b?????_?????_?????_0000000010_0110011",32);
  def SLTU       = Bits("b?????_?????_?????_0000000011_0110011",32);
  def riscvXOR   = Bits("b?????_?????_?????_0000000100_0110011",32);
  def SRL        = Bits("b?????_?????_?????_0000000101_0110011",32);
  def SRA        = Bits("b?????_?????_?????_1000000101_0110011",32);
  def riscvOR    = Bits("b?????_?????_?????_0000000110_0110011",32);
  def riscvAND   = Bits("b?????_?????_?????_0000000111_0110011",32);
  def MUL        = Bits("b?????_?????_?????_0000001000_0110011",32);
  def MULH       = Bits("b?????_?????_?????_0000001001_0110011",32);
  def MULHSU     = Bits("b?????_?????_?????_0000001010_0110011",32);
  def MULHU      = Bits("b?????_?????_?????_0000001011_0110011",32);
  def DIV        = Bits("b?????_?????_?????_0000001100_0110011",32);
  def DIVU       = Bits("b?????_?????_?????_0000001101_0110011",32);
  def REM        = Bits("b?????_?????_?????_0000001110_0110011",32);
  def REMU       = Bits("b?????_?????_?????_0000001111_0110011",32);
  def ADDIW      = Bits("b?????_?????_????????????_000_0011011",32);
  def SLLIW      = Bits("b?????_?????_000000_0_?????_001_0011011",32);
  def SRLIW      = Bits("b?????_?????_000000_0_?????_101_0011011",32);
  def SRAIW      = Bits("b?????_?????_000001_0_?????_101_0011011",32);
  def ADDW       = Bits("b?????_?????_?????_0000000000_0111011",32);
  def SUBW       = Bits("b?????_?????_?????_1000000000_0111011",32);
  def SLLW       = Bits("b?????_?????_?????_0000000001_0111011",32);
  def SRLW       = Bits("b?????_?????_?????_0000000101_0111011",32);
  def SRAW       = Bits("b?????_?????_?????_1000000101_0111011",32);
  def MULW       = Bits("b?????_?????_?????_0000001000_0111011",32);
  def DIVW       = Bits("b?????_?????_?????_0000001100_0111011",32);
  def DIVUW      = Bits("b?????_?????_?????_0000001101_0111011",32);
  def REMW       = Bits("b?????_?????_?????_0000001110_0111011",32);
  def REMUW      = Bits("b?????_?????_?????_0000001111_0111011",32);
  def LB         = Bits("b?????_?????_????????????_000_0000011",32);
  def LH         = Bits("b?????_?????_????????????_001_0000011",32);
  def LW         = Bits("b?????_?????_????????????_010_0000011",32);
  def LD         = Bits("b?????_?????_????????????_011_0000011",32);
  def LBU        = Bits("b?????_?????_????????????_100_0000011",32);
  def LHU        = Bits("b?????_?????_????????????_101_0000011",32);
  def LWU        = Bits("b?????_?????_????????????_110_0000011",32);
  def SB         = Bits("b?????_?????_?????_???????_000_0100011",32);
  def SH         = Bits("b?????_?????_?????_???????_001_0100011",32);
  def SW         = Bits("b?????_?????_?????_???????_010_0100011",32);
  def SD         = Bits("b?????_?????_?????_???????_011_0100011",32);
  def AMOADD_W   = Bits("b?????_?????_?????_0000000010_0101011",32);
  def AMOSWAP_W  = Bits("b?????_?????_?????_0000001010_0101011",32);
  def AMOAND_W   = Bits("b?????_?????_?????_0000010010_0101011",32);
  def AMOOR_W    = Bits("b?????_?????_?????_0000011010_0101011",32);
  def AMOMIN_W   = Bits("b?????_?????_?????_0000100010_0101011",32);
  def AMOMAX_W   = Bits("b?????_?????_?????_0000101010_0101011",32);
  def AMOMINU_W  = Bits("b?????_?????_?????_0000110010_0101011",32);
  def AMOMAXU_W  = Bits("b?????_?????_?????_0000111010_0101011",32);
  def AMOADD_D   = Bits("b?????_?????_?????_0000000011_0101011",32);
  def AMOSWAP_D  = Bits("b?????_?????_?????_0000001011_0101011",32);
  def AMOAND_D   = Bits("b?????_?????_?????_0000010011_0101011",32);
  def AMOOR_D    = Bits("b?????_?????_?????_0000011011_0101011",32);
  def AMOMIN_D   = Bits("b?????_?????_?????_0000100011_0101011",32);
  def AMOMAX_D   = Bits("b?????_?????_?????_0000101011_0101011",32);
  def AMOMINU_D  = Bits("b?????_?????_?????_0000110011_0101011",32);
  def AMOMAXU_D  = Bits("b?????_?????_?????_0000111011_0101011",32);
  def LR_W       = Bits("b?????_?????_00000_1000000010_0101011",32);
  def LR_D       = Bits("b?????_?????_00000_1000000011_0101011",32);
  def SC_W       = Bits("b?????_?????_?????_1000001010_0101011",32);
  def SC_D       = Bits("b?????_?????_?????_1000001011_0101011",32);
  def FENCE_I    = Bits("b?????_?????_????????????_001_0101111",32);
  def FENCE      = Bits("b?????_?????_????????????_010_0101111",32);
  def SYSCALL    = Bits("b00000_00000_00000_0000000000_1110111",32);
  def BREAK      = Bits("b00000_00000_00000_0000000001_1110111",32);
  def RDCYCLE    = Bits("b?????_00000_00000_0000000100_1110111",32);
  def RDTIME     = Bits("b?????_00000_00000_0000001100_1110111",32);
  def RDINSTRET  = Bits("b?????_00000_00000_0000010100_1110111",32);
  def CLEARPCR   = Bits("b?????_?????_????????????_000_1111011",32);
  def SETPCR     = Bits("b?????_?????_????????????_001_1111011",32);
  def MFPCR      = Bits("b?????_?????_00000_0000000010_1111011",32);
  def MTPCR      = Bits("b?????_?????_?????_0000000011_1111011",32);
  def ERET       = Bits("b00000_00000_00000_0000000100_1111011",32);
  def CFLUSH     = Bits("b00000_00000_00000_0000000101_1111011",32);
  // floating point instructions
  def FMOVZ      = Bits("b?????_?????_?????_0000010101_1110111",32);
  def FMOVN      = Bits("b?????_?????_?????_0000011101_1110111",32);
  def FADD_S     = Bits("b?????_?????_?????_00000_???_00_1010011",32);
  def FSUB_S     = Bits("b?????_?????_?????_00001_???_00_1010011",32);
  def FMUL_S     = Bits("b?????_?????_?????_00010_???_00_1010011",32);
  def FDIV_S     = Bits("b?????_?????_?????_00011_???_00_1010011",32);
  def FSQRT_S    = Bits("b?????_?????_00000_00100_???_00_1010011",32);
  def FSGNJ_S    = Bits("b?????_?????_?????_00101_000_00_1010011",32);
  def FSGNJN_S   = Bits("b?????_?????_?????_00110_000_00_1010011",32);
  def FSGNJX_S   = Bits("b?????_?????_?????_00111_000_00_1010011",32);
  def FADD_D     = Bits("b?????_?????_?????_00000_???_01_1010011",32);
  def FSUB_D     = Bits("b?????_?????_?????_00001_???_01_1010011",32);
  def FMUL_D     = Bits("b?????_?????_?????_00010_???_01_1010011",32);
  def FDIV_D     = Bits("b?????_?????_?????_00011_???_01_1010011",32);
  def FSQRT_D    = Bits("b?????_?????_00000_00100_???_01_1010011",32);
  def FSGNJ_D    = Bits("b?????_?????_?????_00101_000_01_1010011",32);
  def FSGNJN_D   = Bits("b?????_?????_?????_00110_000_01_1010011",32);
  def FSGNJX_D   = Bits("b?????_?????_?????_00111_000_01_1010011",32);
  def FCVT_L_S   = Bits("b?????_?????_00000_01000_???_00_1010011",32);
  def FCVT_LU_S  = Bits("b?????_?????_00000_01001_???_00_1010011",32);
  def FCVT_W_S   = Bits("b?????_?????_00000_01010_???_00_1010011",32);
  def FCVT_WU_S  = Bits("b?????_?????_00000_01011_???_00_1010011",32);
  def FCVT_L_D   = Bits("b?????_?????_00000_01000_???_01_1010011",32);
  def FCVT_LU_D  = Bits("b?????_?????_00000_01001_???_01_1010011",32);
  def FCVT_W_D   = Bits("b?????_?????_00000_01010_???_01_1010011",32);
  def FCVT_WU_D  = Bits("b?????_?????_00000_01011_???_01_1010011",32);
  def FCVT_S_L   = Bits("b?????_?????_00000_01100_???_00_1010011",32);
  def FCVT_S_LU  = Bits("b?????_?????_00000_01101_???_00_1010011",32);
  def FCVT_S_W   = Bits("b?????_?????_00000_01110_???_00_1010011",32);
  def FCVT_S_WU  = Bits("b?????_?????_00000_01111_???_00_1010011",32);
  def FCVT_D_L   = Bits("b?????_?????_00000_01100_???_01_1010011",32);
  def FCVT_D_LU  = Bits("b?????_?????_00000_01101_???_01_1010011",32);
  def FCVT_D_W   = Bits("b?????_?????_00000_01110_???_01_1010011",32);
  def FCVT_D_WU  = Bits("b?????_?????_00000_01111_???_01_1010011",32);
  def FCVT_S_D   = Bits("b?????_?????_00000_10001_???_00_1010011",32);
  def FCVT_D_S   = Bits("b?????_?????_00000_10000_???_01_1010011",32);
  def FEQ_S      = Bits("b?????_?????_?????_10101_000_00_1010011",32);
  def FLT_S      = Bits("b?????_?????_?????_10110_000_00_1010011",32);
  def FLE_S      = Bits("b?????_?????_?????_10111_000_00_1010011",32);
  def FEQ_D      = Bits("b?????_?????_?????_10101_000_01_1010011",32);
  def FLT_D      = Bits("b?????_?????_?????_10110_000_01_1010011",32);
  def FLE_D      = Bits("b?????_?????_?????_10111_000_01_1010011",32);
  def FMIN_S     = Bits("b?????_?????_?????_11000_000_00_1010011",32);
  def FMAX_S     = Bits("b?????_?????_?????_11001_000_00_1010011",32);
  def FMIN_D     = Bits("b?????_?????_?????_11000_000_01_1010011",32);
  def FMAX_D     = Bits("b?????_?????_?????_11001_000_01_1010011",32);
  def MFTX_S     = Bits("b?????_?????_00000_11100_000_00_1010011",32);
  def MFTX_D     = Bits("b?????_?????_00000_11100_000_01_1010011",32);
  def MFFSR      = Bits("b?????_00000_00000_11101_000_00_1010011",32);
  def MXTF_S     = Bits("b?????_?????_00000_11110_000_00_1010011",32);
  def MXTF_D     = Bits("b?????_?????_00000_11110_000_01_1010011",32);
  def MTFSR      = Bits("b?????_?????_00000_11111_000_00_1010011",32);
  def FLW        = Bits("b?????_?????_????????????_010_0000111",32);
  def FLD        = Bits("b?????_?????_????????????_011_0000111",32);
  def FSW        = Bits("b?????_?????_?????_???????_010_0100111",32);
  def FSD        = Bits("b?????_?????_?????_???????_011_0100111",32);
  def FMADD_S    = Bits("b?????_?????_?????_?????_???_00_1000011",32);
  def FMSUB_S    = Bits("b?????_?????_?????_?????_???_00_1000111",32);
  def FNMSUB_S   = Bits("b?????_?????_?????_?????_???_00_1001011",32);
  def FNMADD_S   = Bits("b?????_?????_?????_?????_???_00_1001111",32);
  def FMADD_D    = Bits("b?????_?????_?????_?????_???_01_1000011",32);
  def FMSUB_D    = Bits("b?????_?????_?????_?????_???_01_1000111",32);
  def FNMSUB_D   = Bits("b?????_?????_?????_?????_???_01_1001011",32);
  def FNMADD_D   = Bits("b?????_?????_?????_?????_???_01_1001111",32);
  // vector instructions
  def FENCE_V_L  = Bits("b?????_?????_????????????_100_0101111",32);
  def FENCE_V_G  = Bits("b?????_?????_????????????_101_0101111",32);
  def MOVZ       = Bits("b?????_?????_?????_0000000101_1110111",32);
  def MOVN       = Bits("b?????_?????_?????_0000001101_1110111",32);
  def STOP       = Bits("b00000_00000_00000_0000000010_1110111",32);
  def UTIDX      = Bits("b?????_00000_00000_0000000011_1110111",32);
  def VLD        = Bits("b?????_?????_00000_0000000011_0001011",32);
  def VLW        = Bits("b?????_?????_00000_0000000010_0001011",32);
  def VLWU       = Bits("b?????_?????_00000_0000000110_0001011",32);
  def VLH        = Bits("b?????_?????_00000_0000000001_0001011",32);
  def VLHU       = Bits("b?????_?????_00000_0000000101_0001011",32);
  def VLB        = Bits("b?????_?????_00000_0000000000_0001011",32);
  def VLBU       = Bits("b?????_?????_00000_0000000100_0001011",32);
  def VFLD       = Bits("b?????_?????_00000_0000001011_0001011",32);
  def VFLW       = Bits("b?????_?????_00000_0000001010_0001011",32);
  def VLSTD      = Bits("b?????_?????_?????_0000100011_0001011",32);
  def VLSTW      = Bits("b?????_?????_?????_0000100010_0001011",32);
  def VLSTWU     = Bits("b?????_?????_?????_0000100110_0001011",32);
  def VLSTH      = Bits("b?????_?????_?????_0000100001_0001011",32);
  def VLSTHU     = Bits("b?????_?????_?????_0000100101_0001011",32);
  def VLSTB      = Bits("b?????_?????_?????_0000100000_0001011",32);
  def VLSTBU     = Bits("b?????_?????_?????_0000100100_0001011",32);
  def VFLSTD     = Bits("b?????_?????_?????_0000101011_0001011",32);
  def VFLSTW     = Bits("b?????_?????_?????_0000101010_0001011",32);
  def VLSEGD     = Bits("b?????_?????_?????_0001000011_0001011",32);
  def VLSEGW     = Bits("b?????_?????_?????_0001000010_0001011",32);
  def VLSEGWU    = Bits("b?????_?????_?????_0001000110_0001011",32);
  def VLSEGH     = Bits("b?????_?????_?????_0001000001_0001011",32);
  def VLSEGHU    = Bits("b?????_?????_?????_0001000101_0001011",32);
  def VLSEGB     = Bits("b?????_?????_?????_0001000000_0001011",32);
  def VLSEGBU    = Bits("b?????_?????_?????_0001000100_0001011",32);
  def VFLSEGD    = Bits("b?????_?????_?????_0001001011_0001011",32);
  def VFLSEGW    = Bits("b?????_?????_?????_0001001010_0001011",32);
  def VLSEGSTD   = Bits("b?????_?????_?????_?????_100_11_0001011",32);
  def VLSEGSTW   = Bits("b?????_?????_?????_?????_100_10_0001011",32);
  def VLSEGSTWU  = Bits("b?????_?????_?????_?????_101_10_0001011",32);
  def VLSEGSTH   = Bits("b?????_?????_?????_?????_100_01_0001011",32);
  def VLSEGSTHU  = Bits("b?????_?????_?????_?????_101_01_0001011",32);
  def VLSEGSTB   = Bits("b?????_?????_?????_?????_100_00_0001011",32);
  def VLSEGSTBU  = Bits("b?????_?????_?????_?????_101_00_0001011",32);
  def VFLSEGSTD  = Bits("b?????_?????_?????_?????_110_11_0001011",32);
  def VFLSEGSTW  = Bits("b?????_?????_?????_?????_110_10_0001011",32);
  def VSD        = Bits("b?????_?????_00000_0000000011_0001111",32);
  def VSW        = Bits("b?????_?????_00000_0000000010_0001111",32);
  def VSH        = Bits("b?????_?????_00000_0000000001_0001111",32);
  def VSB        = Bits("b?????_?????_00000_0000000000_0001111",32);
  def VFSD       = Bits("b?????_?????_00000_0000001011_0001111",32);
  def VFSW       = Bits("b?????_?????_00000_0000001010_0001111",32);
  def VSSTD      = Bits("b?????_?????_?????_0000100011_0001111",32);
  def VSSTW      = Bits("b?????_?????_?????_0000100010_0001111",32);
  def VSSTH      = Bits("b?????_?????_?????_0000100001_0001111",32);
  def VSSTB      = Bits("b?????_?????_?????_0000100000_0001111",32);
  def VFSSTD     = Bits("b?????_?????_?????_0000101011_0001111",32);
  def VFSSTW     = Bits("b?????_?????_?????_0000101010_0001111",32);
  def VSSEGD     = Bits("b?????_?????_?????_0001000011_0001111",32);
  def VSSEGW     = Bits("b?????_?????_?????_0001000010_0001111",32);
  def VSSEGH     = Bits("b?????_?????_?????_0001000001_0001111",32);
  def VSSEGB     = Bits("b?????_?????_?????_0001000000_0001111",32);
  def VFSSEGD    = Bits("b?????_?????_?????_0001001011_0001111",32);
  def VFSSEGW    = Bits("b?????_?????_?????_0001001010_0001111",32);
  def VSSEGSTD   = Bits("b?????_?????_?????_?????_100_11_0001111",32);
  def VSSEGSTW   = Bits("b?????_?????_?????_?????_100_10_0001111",32);
  def VSSEGSTH   = Bits("b?????_?????_?????_?????_100_01_0001111",32);
  def VSSEGSTB   = Bits("b?????_?????_?????_?????_100_00_0001111",32);
  def VFSSEGSTD  = Bits("b?????_?????_?????_?????_110_11_0001111",32);
  def VFSSEGSTW  = Bits("b?????_?????_?????_?????_110_10_0001111",32);
  def VMVV       = Bits("b?????_?????_00000_0000000000_1110011",32);
  def VMSV       = Bits("b?????_?????_00000_0000010000_1110011",32);
  def VMST       = Bits("b?????_?????_?????_0000100000_1110011",32);
  def VMTS       = Bits("b?????_?????_?????_0000110000_1110011",32);
  def VFMVV      = Bits("b?????_?????_00000_0000000010_1110011",32);
  def VFMSV      = Bits("b?????_?????_00000_0000010010_1110011",32);
  def VFMST      = Bits("b?????_?????_?????_0000100010_1110011",32);
  def VFMTS      = Bits("b?????_?????_?????_0000110010_1110011",32);
  def VVCFGIVL   = Bits("b?????_?????_????????????_001_1110011",32);
  def VTCFGIVL   = Bits("b?????_?????_????????????_011_1110011",32);
  def VVCFG      = Bits("b00000_?????_?????_0000001000_1110011",32);
  def VTCFG      = Bits("b00000_?????_?????_0000011000_1110011",32);
  def VSETVL     = Bits("b?????_?????_000000000000_101_1110011",32);
  def VF         = Bits("b00000_?????_????????????_111_1110011",32);
  // vector supervisor instructions
  def VENQCMD    = Bits("b00000_?????_?????_0001010110_1111011",32)
  def VENQIMM1   = Bits("b00000_?????_?????_0001011110_1111011",32)
  def VENQIMM2   = Bits("b00000_?????_?????_0001100110_1111011",32)
  def VENQCNT    = Bits("b00000_?????_?????_0001101110_1111011",32)
  def VXCPTKILL  = Bits("b00000_00000_00000_0000010110_1111011",32)
  def VXCPTEVAC  = Bits("b00000_?????_00000_0001000110_1111011",32)
  def VXCPTHOLD  = Bits("b00000_00000_00000_0001001110_1111011",32)

  def NOP        = Bits("b00000_00000_000000000000_000_0010011",32);
}

object Disassemble
{
  def apply(insn: UInt) = {
    val name :: fmt :: Nil = ListLookup(insn, default, table)
    sprintf("%s %s", name, operands(insn, fmt))
  }

  private def operands(insn: Bits, fmt: Bits): Bits = {
    val x = AVec(Str(" x0"), Str(" ra"), Str(" s0"), Str(" s1"),
                 Str(" s2"), Str(" s3"), Str(" s4"), Str(" s5"),
                 Str(" s6"), Str(" s7"), Str(" s8"), Str(" s9"),
                 Str("s10"), Str("s11"), Str(" sp"), Str(" tp"),
                 Str(" v0"), Str(" v1"), Str(" a0"), Str(" a1"),
                 Str(" a2"), Str(" a3"), Str(" a4"), Str(" a5"),
                 Str(" a6"), Str(" a7"), Str(" a8"), Str(" a9"),
                 Str("a10"), Str("a11"), Str("a12"), Str("a13"))
    val f = AVec(Str(" fs0"), Str(" fs1"), Str(" fs2"), Str(" fs3"),
                 Str(" fs4"), Str(" fs5"), Str(" fs6"), Str(" fs7"),
                 Str(" fs8"), Str(" fs9"), Str("fs10"), Str("fs11"),
                 Str("fs12"), Str("fs13"), Str("fs14"), Str("fs15"),
                 Str(" fv0"), Str(" fv1"), Str(" fa0"), Str(" fa1"),
                 Str(" fa2"), Str(" fa3"), Str(" fa4"), Str(" fa5"),
                 Str(" fa6"), Str(" fa7"), Str(" fa8"), Str(" fa9"),
                 Str("fa10"), Str("fa11"), Str("fa12"), Str("fa13"))

    def hex(x: SInt, plus: Char = ' ') =
      Cat(Mux(x < SInt(0), Str("-0x"), Str(plus + "0x")), Str(x.abs, 16))

    val comma = Str(',')
    val lparen = Str('(')
    val rparen = Str(')')

    val rd = insn(31,27)
    val rs1 = insn(26,22)
    val rs2 = insn(21,17)
    val rs3 = insn(16,12)
    val immv = insn(21,10).toSInt
    val bmmv = Cat(insn(31,27), insn(16,10)).toSInt
    val jmmv = insn(31,7).toSInt

    val imm = hex(immv)
    val bmm = hex(bmmv << UInt(1))
    val jmm = hex(jmmv << UInt(1))
    val lmm = Cat(Str("0x"), Str(insn(26,7).toUInt, 16))

    val laddr = Cat(Str(immv), lparen, x(rs1), rparen)
    val saddr = Cat(Str(bmmv), lparen, x(rs1), rparen)

    val r0 = x(rd)
    val r1 = Cat(r0, comma, x(rs1))
    val r2 = Cat(r1, comma, x(rs2))
    val f1 = Cat(f(rd), comma, f(rs1))
    val f2 = Cat(f1, comma, f(rs2))
    val f3 = Cat(f2, comma, f(rs3))
    val fx = Cat(f(rd), comma, x(rs1))
    val xf1 = Cat(x(rd), comma, f(rs1))
    val xf2 = Cat(xf1, comma, f(rs2))
    val z = Str(' ')
    val i = Cat(r1, comma, imm)
    val b = Cat(x(rs1), comma, x(rs2), comma, bmm)
    val j = jmm
    val l = Cat(x(rd), comma, lmm)
    val ld = Cat(x(rd), comma, laddr)
    val st = Cat(x(rs2), comma, saddr)
    val fld = Cat(f(rd), comma, laddr)
    val fst = Cat(f(rs2), comma, saddr)
    val amo = r2

    val opts = Seq(r0, r1, r2, f1, f2, f3, fx, xf1, xf2, z, i, b, j, l,  ld, st,
                   fld, fst, amo)
    val maxLen = opts.map(_.getWidth).reduce(_ max _)
    val padded = opts.map(x => x.toUInt << UInt(maxLen - x.getWidth))
    AVec(padded)(fmt.toUInt)
  }

  private def FMT_R0  = Bits(0, 5)
  private def FMT_R1  = Bits(1, 5)
  private def FMT_R2  = Bits(2, 5)
  private def FMT_F1  = Bits(3, 5)
  private def FMT_F2  = Bits(4, 5)
  private def FMT_F3  = Bits(5, 5)
  private def FMT_FX  = Bits(6, 5)
  private def FMT_XF1 = Bits(7, 5)
  private def FMT_XF2 = Bits(8, 5)
  private def FMT_0   = Bits(9, 5)
  private def FMT_I   = Bits(10, 5)
  private def FMT_B   = Bits(11, 5)
  private def FMT_J   = Bits(12, 5)
  private def FMT_L   = Bits(13, 5)
  private def FMT_LD  = Bits(14, 5)
  private def FMT_ST  = Bits(15, 5)
  private def FMT_FLD = Bits(16, 5)
  private def FMT_FST = Bits(17, 5)
  private def FMT_AMO = Bits(18, 5)

  private def default = List(Str("unknown   "), FMT_0)

  import Instructions._
  private def table = Array(
    BNE->       List(Str("bne       "), FMT_B),
    BEQ->       List(Str("beq       "), FMT_B),
    BLT->       List(Str("blt       "), FMT_B),
    BLTU->      List(Str("bltu      "), FMT_B),
    BGE->       List(Str("bge       "), FMT_B),
    BGEU->      List(Str("bgeu      "), FMT_B),

    J->         List(Str("j         "), FMT_J),
    JAL->       List(Str("jal       "), FMT_J),
    JALR_C->    List(Str("jalr.c    "), FMT_LD),
    JALR_J->    List(Str("jalr.j    "), FMT_LD),
    JALR_R->    List(Str("jalr.r    "), FMT_LD),
    AUIPC->     List(Str("auipc     "), FMT_L),

    LB->        List(Str("lb        "), FMT_LD),
    LH->        List(Str("lh        "), FMT_LD),
    LW->        List(Str("lw        "), FMT_LD),
    LD->        List(Str("ld        "), FMT_LD),
    LBU->       List(Str("lbu       "), FMT_LD),
    LHU->       List(Str("lhu       "), FMT_LD),
    LWU->       List(Str("lwu       "), FMT_LD),
    SB->        List(Str("sb        "), FMT_ST),
    SH->        List(Str("sh        "), FMT_ST),
    SW->        List(Str("sw        "), FMT_ST),
    SD->        List(Str("sd        "), FMT_ST),

    AMOADD_W->  List(Str("amoadd.w  "), FMT_AMO),
    AMOSWAP_W-> List(Str("amoswap.w "), FMT_AMO),
    AMOAND_W->  List(Str("amoand.w  "), FMT_AMO),
    AMOOR_W->   List(Str("amoor.w   "), FMT_AMO),
    AMOMIN_W->  List(Str("amomin.w  "), FMT_AMO),
    AMOMINU_W-> List(Str("amominu.w "), FMT_AMO),
    AMOMAX_W->  List(Str("amomax.w  "), FMT_AMO),
    AMOMAXU_W-> List(Str("amomaxu.w "), FMT_AMO),
    AMOADD_D->  List(Str("amoadd.d  "), FMT_AMO),
    AMOSWAP_D-> List(Str("amoswap.d "), FMT_AMO),
    AMOAND_D->  List(Str("amoand.d  "), FMT_AMO),
    AMOOR_D->   List(Str("amoor.d   "), FMT_AMO),
    AMOMIN_D->  List(Str("amomin.d  "), FMT_AMO),
    AMOMINU_D-> List(Str("amominu.d "), FMT_AMO),
    AMOMAX_D->  List(Str("amomax.d  "), FMT_AMO),
    AMOMAXU_D-> List(Str("amomaxu.d "), FMT_AMO),

    LR_W->      List(Str("lr.w      "), FMT_AMO),
    LR_D->      List(Str("lr.d      "), FMT_AMO),
    SC_W->      List(Str("sc.w      "), FMT_AMO),
    SC_D->      List(Str("sc.d      "), FMT_AMO),

    LUI->       List(Str("lui       "), FMT_L),
    ADDI->      List(Str("addi      "), FMT_I),
    SLTI ->     List(Str("slti      "), FMT_I),
    SLTIU->     List(Str("sltiu     "), FMT_I),
    ANDI->      List(Str("andi      "), FMT_I),
    ORI->       List(Str("ori       "), FMT_I),
    XORI->      List(Str("xori      "), FMT_I),
    SLLI->      List(Str("slli      "), FMT_I),
    SRLI->      List(Str("srli      "), FMT_I),
    SRAI->      List(Str("srai      "), FMT_I),
    ADD->       List(Str("add       "), FMT_R2),
    SUB->       List(Str("sub       "), FMT_R2),
    SLT->       List(Str("slt       "), FMT_R2),
    SLTU->      List(Str("sltu      "), FMT_R2),
    riscvAND->  List(Str("and       "), FMT_R2),
    riscvOR->   List(Str("or        "), FMT_R2),
    riscvXOR->  List(Str("xor       "), FMT_R2),
    SLL->       List(Str("sll       "), FMT_R2),
    SRL->       List(Str("srl       "), FMT_R2),
    SRA->       List(Str("sra       "), FMT_R2),

    ADDIW->     List(Str("addiw     "), FMT_I),
    SLLIW->     List(Str("slliw     "), FMT_I),
    SRLIW->     List(Str("srliw     "), FMT_I),
    SRAIW->     List(Str("sraiw     "), FMT_I),
    ADDW->      List(Str("addw      "), FMT_R2),
    SUBW->      List(Str("subw      "), FMT_R2),
    SLLW->      List(Str("sllw      "), FMT_R2),
    SRLW->      List(Str("srlw      "), FMT_R2),
    SRAW->      List(Str("sraw      "), FMT_R2),

    MUL->       List(Str("mul       "), FMT_R2),
    MULH->      List(Str("mulh      "), FMT_R2),
    MULHU->     List(Str("mulhu     "), FMT_R2),
    MULHSU->    List(Str("mulhsu    "), FMT_R2),
    MULW->      List(Str("mulw      "), FMT_R2),

    DIV->       List(Str("div       "), FMT_R2),
    DIVU->      List(Str("divu      "), FMT_R2),
    REM->       List(Str("rem       "), FMT_R2),
    REMU->      List(Str("remu      "), FMT_R2),
    DIVW->      List(Str("divw      "), FMT_R2),
    DIVUW->     List(Str("divuw     "), FMT_R2),
    REMW->      List(Str("remw      "), FMT_R2),
    REMUW->     List(Str("remuw     "), FMT_R2),

    SYSCALL->   List(Str("syscall   "), FMT_0),
    SETPCR->    List(Str("setpcr    "), FMT_I),
    CLEARPCR->  List(Str("clearpcr  "), FMT_I),
    ERET->      List(Str("eret      "), FMT_0),
    FENCE->     List(Str("fence     "), FMT_0),
    FENCE_I->   List(Str("fence.i   "), FMT_0),
    MFPCR->     List(Str("mfpcr     "), FMT_R2),
    MTPCR->     List(Str("mtpcr     "), FMT_R2),
    RDTIME->    List(Str("rdtime    "), FMT_R0),
    RDCYCLE->   List(Str("rdcycle   "), FMT_R0),
    RDINSTRET-> List(Str("rdinstret "), FMT_R0),

    FCVT_S_D->  List(Str("fcvt.s.d  "), FMT_F1),
    FCVT_D_S->  List(Str("fcvt.d.s  "), FMT_F1),
    FSGNJ_S->   List(Str("fsgnj.s   "), FMT_F2),
    FSGNJ_D->   List(Str("fsgnj.d   "), FMT_F2),
    FSGNJX_S->  List(Str("fsgnx.s   "), FMT_F2),
    FSGNJX_D->  List(Str("fsgnx.d   "), FMT_F2),
    FSGNJN_S->  List(Str("fsgnjn.s  "), FMT_F2),
    FSGNJN_D->  List(Str("fsgnjn.d  "), FMT_F2),
    FMIN_S->    List(Str("fmin.s    "), FMT_F2),
    FMIN_D->    List(Str("fmin.d    "), FMT_F2),
    FMAX_S->    List(Str("fmax.s    "), FMT_F2),
    FMAX_D->    List(Str("fmax.d    "), FMT_F2),
    FADD_S->    List(Str("fadd.s    "), FMT_F2),
    FADD_D->    List(Str("fadd.d    "), FMT_F2),
    FSUB_S->    List(Str("fsub.s    "), FMT_F2),
    FSUB_D->    List(Str("fsub.d    "), FMT_F2),
    FMUL_S->    List(Str("fmul.s    "), FMT_F2),
    FMUL_D->    List(Str("fmul.d    "), FMT_F2),
    FMADD_S->   List(Str("fmadd.s   "), FMT_F3),
    FMADD_D->   List(Str("fmadd.d   "), FMT_F3),
    FMSUB_S->   List(Str("fmsub.s   "), FMT_F3),
    FMSUB_D->   List(Str("fmsub.d   "), FMT_F3),
    FNMADD_S->  List(Str("fnmadd.s  "), FMT_F3),
    FNMADD_D->  List(Str("fnmadd.d  "), FMT_F3),
    FNMSUB_S->  List(Str("fnmsub.s  "), FMT_F3),
    FNMSUB_D->  List(Str("fnmsub.d  "), FMT_F3),
    MFTX_S->    List(Str("mftx.s    "), FMT_XF1),
    MFTX_D->    List(Str("mftx.d    "), FMT_XF1),
    FCVT_W_S->  List(Str("fcvt.w.s  "), FMT_XF1),
    FCVT_W_D->  List(Str("fcvt.w.d  "), FMT_XF1),
    FCVT_WU_S-> List(Str("fcvt.wu.s "), FMT_XF1),
    FCVT_WU_D-> List(Str("fcvt.wu.d "), FMT_XF1),
    FCVT_L_S->  List(Str("fcvt.l.s  "), FMT_XF1),
    FCVT_L_D->  List(Str("fcvt.l.d  "), FMT_XF1),
    FCVT_LU_S-> List(Str("fcvt.lu.s "), FMT_XF1),
    FCVT_LU_D-> List(Str("fcvt.lu.d "), FMT_XF1),
    FEQ_S->     List(Str("feq.s     "), FMT_XF2),
    FEQ_D->     List(Str("feq.d     "), FMT_XF2),
    FLT_S->     List(Str("flt.s     "), FMT_XF2),
    FLT_D->     List(Str("flt.d     "), FMT_XF2),
    FLE_S->     List(Str("fle.s     "), FMT_XF2),
    FLE_D->     List(Str("fle.d     "), FMT_XF2),
    MXTF_S->    List(Str("mxtf.s    "), FMT_FX),
    MXTF_D->    List(Str("mxtf.d    "), FMT_FX),
    FCVT_S_W->  List(Str("fcvt.s.w  "), FMT_FX),
    FCVT_D_W->  List(Str("fcvt.d.w  "), FMT_FX),
    FCVT_S_WU-> List(Str("fcvt.s.wu "), FMT_FX),
    FCVT_D_WU-> List(Str("fcvt.d.wu "), FMT_FX),
    FCVT_S_L->  List(Str("fcvt.s.l  "), FMT_FX),
    FCVT_D_L->  List(Str("fcvt.d.l  "), FMT_FX),
    FCVT_S_LU-> List(Str("fcvt.s.lu "), FMT_FX),
    FCVT_D_LU-> List(Str("fcvt.d.lu "), FMT_FX),
    MFFSR->     List(Str("mffsr     "), FMT_R0),
    MTFSR->     List(Str("mtfsr     "), FMT_R1),
    FLW->       List(Str("flw       "), FMT_FLD),
    FLD->       List(Str("fld       "), FMT_FLD),
    FSW->       List(Str("fsw       "), FMT_FST),
    FSD->       List(Str("fsd       "), FMT_FST),

    VVCFGIVL->  List(Str("vecInst   "), FMT_0),
    VVCFG->     List(Str("vecInst   "), FMT_0),
    VSETVL->    List(Str("vecInst   "), FMT_0),
    VF->        List(Str("vecInst   "), FMT_0),
    VMVV->      List(Str("vecInst   "), FMT_0),
    VMSV->      List(Str("vecInst   "), FMT_0),
    VFMVV->     List(Str("vecInst   "), FMT_0),
    FENCE_V_L-> List(Str("vecInst   "), FMT_0),
    FENCE_V_G-> List(Str("vecInst   "), FMT_0),
    VLD->       List(Str("vecInst   "), FMT_0),
    VLW->       List(Str("vecInst   "), FMT_0),
    VLWU->      List(Str("vecInst   "), FMT_0),
    VLH->       List(Str("vecInst   "), FMT_0),
    VLHU->      List(Str("vecInst   "), FMT_0),
    VLB->       List(Str("vecInst   "), FMT_0),
    VLBU->      List(Str("vecInst   "), FMT_0),
    VSD->       List(Str("vecInst   "), FMT_0),
    VSW->       List(Str("vecInst   "), FMT_0),
    VSH->       List(Str("vecInst   "), FMT_0),
    VSB->       List(Str("vecInst   "), FMT_0),
    VFLD->      List(Str("vecInst   "), FMT_0),
    VFLW->      List(Str("vecInst   "), FMT_0),
    VFSD->      List(Str("vecInst   "), FMT_0),
    VFSW->      List(Str("vecInst   "), FMT_0),
    VLSTD->     List(Str("vecInst   "), FMT_0),
    VLSTW->     List(Str("vecInst   "), FMT_0),
    VLSTWU->    List(Str("vecInst   "), FMT_0),
    VLSTH->     List(Str("vecInst   "), FMT_0),
    VLSTHU->    List(Str("vecInst   "), FMT_0),
    VLSTB->     List(Str("vecInst   "), FMT_0),
    VLSTBU->    List(Str("vecInst   "), FMT_0),
    VSSTD->     List(Str("vecInst   "), FMT_0),
    VSSTW->     List(Str("vecInst   "), FMT_0),
    VSSTH->     List(Str("vecInst   "), FMT_0),
    VSSTB->     List(Str("vecInst   "), FMT_0),
    VFLSTD->    List(Str("vecInst   "), FMT_0),
    VFLSTW->    List(Str("vecInst   "), FMT_0),
    VFSSTD->    List(Str("vecInst   "), FMT_0),
    VFSSTW->    List(Str("vecInst   "), FMT_0),

    VENQCMD->   List(Str("vecInst   "), FMT_0),
    VENQIMM1->  List(Str("vecInst   "), FMT_0),
    VENQIMM2->  List(Str("vecInst   "), FMT_0),
    VENQCNT->   List(Str("vecInst   "), FMT_0),
    VXCPTEVAC-> List(Str("vecInst   "), FMT_0),
    VXCPTKILL-> List(Str("vecInst   "), FMT_0),
    VXCPTHOLD-> List(Str("vecInst   "), FMT_0)
  )
}
