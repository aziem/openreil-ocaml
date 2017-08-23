let () =
  print_endline "#include <stdint.h>";
  print_endline "#include \"/usr/local/include/libopenreil.h\"";
  print_endline "#include \"/usr/local/include/reil_ir.h\"";
  Cstubs.Types.write_c Format.std_formatter (module Ffi_bindings.Types)
