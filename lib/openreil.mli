type reil_t

type reil_inst

type context

type reil_instr_type =
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

type reil_type = 
  | A_NONE
  | A_REG
  | A_TEMP
  | A_CONST

type reil_arch = 
  | ARCH_X86
  | ARCH_ARM

type reil_inst_handler = reil_inst -> context -> int

val create_context : unit -> context 
val reil_translate : reil_t -> Unsigned.ullong -> string -> int -> int
val reil_translate_insn : reil_t -> Unsigned.ullong -> string -> int -> int
val reil_inst_print : reil_inst -> unit
val reil_init : reil_arch -> reil_inst_handler -> context -> reil_t
val reil_close : reil_t -> unit
