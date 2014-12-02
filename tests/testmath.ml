
let _ = Printf.printf "The cube root of 27 is %g\n" (Math.cbrt 27.0) in
let _ = print_endline "Double checking..." in
let _ = Printf.printf "The cube root of 27 is %g\n" (Math.root 27.0 3) in
let x = { Complex.re = 0.34; Complex.im = 0.11 } in
let y = Complexer.sin x in
  Genops.ComplexOps.print stdout y;
  print_newline ()
