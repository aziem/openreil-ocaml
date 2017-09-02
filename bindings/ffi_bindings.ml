open Ctypes
open Foreign
open Cstubs.Types


module Types (S : Cstubs.Types.TYPE) =
  struct
    open S

type reil_op_t =
  | NONE
  | UNK
  | JCC
  | STR
  | STM
  | LDM
  | ADD
  | SUB
  | NEG
  | MUL
  | DIV
  | MOD
  | SMUL
  | SDIV
  | SMOD
  | SHL
  | SHR
  | AND
  | OR
  | XOR
  | NOT
  | EQ
  | LT

type reil_type_t =
  | A_NONE
  | A_REG
  | A_TEMP
  | A_CONST

type reil_size_t =
  | U1
  | U8
  | U16
  | U32
  | U64

type reil_arch_t =
  | ARCH_X86
  | ARCH_ARM

let arch_x86 = constant "ARCH_X86" int64_t
let arch_arm = constant "ARCH_ARM" int64_t
let reil_arch_t = enum "_reil_arch_t" [
                         ARCH_X86, arch_x86;
                         ARCH_ARM, arch_arm;
                       ]

  type reil_arg_t
  type reil_raw_t
  type reil_inst_t


  let none = constant "I_NONE" int64_t
  let unk  = constant "I_UNK" int64_t
  let jcc  = constant "I_JCC" int64_t
  let str  = constant "I_STR" int64_t
  let stm  = constant "I_STM" int64_t
  let ldm  = constant "I_LDM" int64_t
  let add  = constant "I_ADD" int64_t
  let sub  = constant "I_SUB" int64_t
  let neg  = constant "I_NEG" int64_t
  let mul  = constant "I_MUL" int64_t
  let div  = constant "I_DIV" int64_t
  let mod_  = constant "I_MOD" int64_t
  let smul = constant "I_SMUL" int64_t
  let sdiv = constant "I_SDIV" int64_t
  let smod = constant "I_SMOD" int64_t
  let shl  = constant "I_SHL" int64_t
  let shr  = constant "I_SHR" int64_t
  let and_  = constant "I_AND" int64_t
  let or_   = constant "I_OR" int64_t
  let xor  = constant "I_XOR" int64_t
  let not_  = constant "I_NOT" int64_t
  let eq   = constant "I_EQ" int64_t
  let lt   = constant "I_LT" int64_t

  let reil_op_t = enum "_reil_op_t" [
                         NONE, none;
                         UNK , unk;
                         JCC , jcc;
                         STR , str;
                         STM , stm;
                         LDM , ldm;
                         ADD , add;
                         SUB , sub;
                         NEG , neg;
                         MUL , mul;
                         DIV , div;
                         MOD , mod_;
                         SMUL, smul;
                         SDIV, sdiv;
                         SMOD, smod;
                         SHL , shl;
                         SHR , shr;
                         AND , and_;
                         OR  , or_;
                         XOR , xor;
                         NOT , not_;
                         EQ  , eq;
                         LT  , lt;
                       ]

  let a_none = constant "A_NONE" int64_t
  let a_reg = constant "A_REG" int64_t
  let a_temp = constant "A_TEMP" int64_t
  let a_const = constant "A_CONST" int64_t

  let reil_type_t = enum "_reil_type_t" [
                           A_NONE, a_none;
                           A_REG, a_reg;
                           A_TEMP, a_temp;
                           A_CONST, a_const;
                         ]

  let u1 = constant "U1" int64_t
  let u8 = constant "U8" int64_t
  let u16 = constant "U16" int64_t
  let u32 = constant "U32" int64_t
  let u64 = constant "U64" int64_t

  let reil_size_t = enum "_reil_size_t" [
                           U1,u1;
                           U8,u8;
                           U16,u16;
                           U32,u32;
                           U64,u64;
                         ]

  let reil_arg_t : reil_arg_t structure typ = structure "_reil_arg_t"
  let type_ = field reil_arg_t "type" reil_type_t
  let size = field reil_arg_t "size" reil_size_t
  let val_ = field reil_arg_t "val" ullong
  let name = field reil_arg_t "name" (array 15 char)
  let () = seal reil_arg_t


  let reil_raw_t : reil_raw_t structure typ = structure "_reil_raw_t"
  let addr = field reil_raw_t "addr" ullong
  let size = field reil_raw_t "size" int
  let data = field reil_raw_t "data" (ptr string)
  let str_mnem = field reil_raw_t "str_mnem" (ptr string)
  let str_op = field reil_raw_t "str_op" (ptr string)
  let () = seal reil_raw_t


  let reil_inst_t : reil_inst_t structure typ = structure "_reil_inst_t"
  let raw_info = field reil_inst_t "raw_info" reil_raw_t
  let inum = field reil_inst_t "inum" ushort
  let op = field reil_inst_t "op" reil_op_t
  let a = field reil_inst_t "a" reil_arg_t
  let b = field reil_inst_t "b" reil_arg_t
  let c = field reil_inst_t "c" reil_arg_t
  let flags = field reil_inst_t "flags" ullong
  let () = seal reil_inst_t

  end

module Bindings (F: Cstubs.FOREIGN) = struct
  (* let reil_translate = foreign "reil_translate" (reil_t @-> ullong @-> string @-> int @-> returning int) *)
  (* let reil_translate_insn = foreign "reil_translate_insn" (reil_t @-> ullong @-> string @-> int @-> returning int) *)
      
  (* let reil_inst_print = foreign "reil_inst_print" (ptr T.reil_inst_t @-> returning void)  *)
  (* let reil_init = foreign "reil_init" (T.reil_arch_t @-> funptr (ptr T.reil_inst_t @-> ptr void @-> returning int) @-> (ptr void) @-> returning reil_t) *)
      
  (* let reil_close = foreign "reil_close" (reil_t @-> returning void) *)
end


