(* This should result in output that's identical to mt1997ar.out *)
let _ =
  let state = Mersenne.make (`Array [| 0x123; 0x234; 0x345; 0x456 |]) in
    print_string "1000 outputs of genrand_int32()\n";
    for i = 0 to 999 do
      let r = Mersenne.uint32 state in 
	Printf.printf "%10lu " r; 
	if i mod 5 = 4 then
          print_newline ();        
    done;
    print_string "\n1000 outputs of genrand_real2()\n";
    for i = 0 to 999 do
      let r = Mersenne.real2 state in
      Printf.printf "%10.8f " r;
	if i mod 5 = 4 then
          print_newline ();
    done
