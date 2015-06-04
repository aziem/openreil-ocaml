open Openreil
open Ctypes

let _ =
  Printf.printf "hello world";
  let testdata = "\x33\xC0" in
  let inst_handler i j = 1 in
  let r = allocate reil_t in 
  let r' = reil_init T.ARCH_X86 inst_handler r in
  ()
