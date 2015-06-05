open Openreil
open Ctypes
open Unsigned

let _ =
  let testdata = "\x33\xC0" in
  let inst_handler i j = reil_inst_print i;0 in
  let i : int ptr = allocate int 0 in
  let r' = reil_init T.ARCH_X86 inst_handler (to_voidp i) in
  let t = reil_translate r' (ULLong.of_int 0) testdata 2 in
  reil_close r'
