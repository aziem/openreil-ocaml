open Openreil
open Ctypes
open Unsigned

let _ =
  let testdata = "\x33\xC0" in
  let inst_handler i j = reil_inst_print i;0 in
  let i = create_context () in
  let r' = reil_init ARCH_X86 inst_handler i in
  ignore (reil_translate r' (ULLong.of_int 0) testdata 2);
  reil_close r'
