open Ctypes
open Foreign

module T = Ffi_bindings.Types(Ffi_generated_types)

type reil_t = unit ptr
let reil_t : reil_t typ = ptr void 

let libopenreil = Dl.(dlopen ~filename:"/usr/local/lib/libopenreil.so" ~flags:[RTLD_NOW])
       

let reil_translate = foreign "reil_translate" (reil_t @-> ullong @-> string @-> int @-> returning int) ~from:libopenreil
let reil_inst_print = foreign "reil_inst_print" (ptr T.reil_inst_t @-> returning void) ~from:libopenreil
let reil_init = foreign "reil_init" (T.reil_arch_t @-> funptr (ptr T.reil_inst_t @-> ptr void @-> returning int) @-> (ptr void) @-> returning (ptr void) ) ~from:libopenreil

let reil_close = foreign "reil_close" (reil_t @-> returning void) ~from:libopenreil
