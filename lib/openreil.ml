open Ctypes
open Foreign

module T = Ffi_bindings.Types(Ffi_generated_types)

type reil_t = unit ptr
let reil_t : reil_t typ = ptr void

type reil_inst = T.reil_inst_t Ctypes.structure ptr

type context = unit ptr
let context : context typ = ptr void 

type reil_inst_handler = reil_inst -> context -> int

let create_context () = (to_voidp (allocate int 0))

let reil_translate = foreign "reil_translate" (reil_t @-> ullong @-> string @-> int @-> returning int)
let reil_translate_insn = foreign "reil_translate_insn" (reil_t @-> ullong @-> string @-> int @-> returning int)

let reil_inst_print = foreign "reil_inst_print" (ptr T.reil_inst_t @-> returning void) 
let reil_init = foreign "reil_init" (T.reil_arch_t @-> funptr (ptr T.reil_inst_t @-> ptr void @-> returning int) @-> (ptr void) @-> returning reil_t)

let reil_close = foreign "reil_close" (reil_t @-> returning void)

type reil_instr_type = T.reil_op_t = 
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

type reil_type = T.reil_type_t = 
  | A_NONE
  | A_REG
  | A_TEMP
  | A_CONST

type reil_size = T.reil_size_t =
  | U1
  | U8
  | U16
  | U32
  | U64

type reil_arch = T.reil_arch_t = 
  | ARCH_X86
  | ARCH_ARM


