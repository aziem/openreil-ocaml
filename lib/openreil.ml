open Ctypes
open Foreign

module T = Ffi_bindings.Types(Ffi_generated_types)

type reil_t = unit ptr
let reil_t : reil_t typ = ptr void

let reil_translate = foreign "reil_translate" (reil_t @-> ullong @-> string @-> int @-> returning int) 
let reil_inst_print = foreign "reil_inst_print" (ptr T.reil_inst_t @-> returning void) 
let reil_init = foreign "reil_init" (T.reil_arch_t @-> funptr (ptr T.reil_inst_t @-> ptr void @-> returning int) @-> (ptr void) @-> returning (ptr void) ) 

let reil_close = foreign "reil_close" (reil_t @-> returning void)



type reil_instr_type = 
  | Nop
  | Unk
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

type reil_type = A_NONE | REG | TEMP | CONST


