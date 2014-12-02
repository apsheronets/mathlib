(* Compare functions in Complexer against results from glibc's C99
_Complex double tests.

Compile with:
   ocamlfind ocamlc -o comptest comptest.ml -package mathlib -linkpkg

*)

open Complex

let compare_float a b =
  match (classify_float a, classify_float b) with
    | FP_zero, FP_zero -> Float.signbit a = Float.signbit b
    | FP_nan, FP_nan -> true
    | FP_infinite, FP_infinite -> a = b
    | FP_normal, FP_normal -> Float.fcmp ~epsilon:0.0000001 a b = 0
    | _, _ -> a = b

let compare_comp a b = compare_float a.re b.re && compare_float a.im b.im
					       
let pc out num =
    Printf.fprintf out "%f %+f i" num.re num.im

let tests = ref 0
and failures = ref 0
and xfailures = ref 0
and usuccess = ref 0
and verbose = ref false

let start_test func =
  Printf.printf "Running tests on %s()\n" func;
  tests := 0;
  failures := 0;
  usuccess := 0;
  xfailures := 0
    
let end_test func =
  Printf.printf "Ran %d tests on %s(): %d passed, %d failed, %d unexpected passes, %d expected failures\n"
    !tests func (!tests - !failures - !usuccess) !failures !usuccess !xfailures
  
let test_fun1 f func arg ?(flags=[])res =
  let r = f arg in
    incr tests;
    if compare_comp res r then begin
      if List.mem `XFAIL flags then begin
	Printf.printf "Expected %s(%a) = %a\nGot %s(%a) = %a: unexpected ok\n"
	  func pc arg pc res func pc arg pc r;
	incr usuccess
      end else begin
	if !verbose then
	  Printf.printf "Expected %s(%a) = %a\nGot %s(%a) = %a: ok\n"
	    func pc arg pc res func pc arg pc r
      end
    end else if List.mem `XFAIL flags then begin
      if !verbose then
	Printf.printf "Expected %s(%a) = %a\nGot %s(%a) = %a: expected not ok\n"
	  func pc arg pc res func pc arg pc r;
      incr xfailures
    end else if (List.mem `IZEROSIGN flags && r.im = 0.0) 
      || (List.mem `IINFSIGN flags && (r.im = infinity || r.im = neg_infinity))
    then begin
      if !verbose then
	Printf.printf "Expected %s(%a) = %a\nGot %s(%a) = %a: ok, 0/inf imaginary sign ignored\n"
	  func pc arg pc res func pc arg pc r
    end else if (List.mem `RZEROSIGN flags && r.re = 0.0)
      || (List.mem `RINFSIGN flags && (r.re = infinity || r.re = neg_infinity))
    then begin
      if !verbose then
	Printf.printf "Expected %s(%a) = %a\nGot %s(%a) = %a: ok, 0/inf real sign ignored\n"
	  func pc arg pc res func pc arg pc r
    end else begin
      Printf.printf "Expected %s(%a) = %a\nGot %s(%a) = %a: not ok\n"
	func pc arg pc res func pc arg pc r;
      incr failures
    end

let zero_zero = { re = 0.0; im = 0.0 }
let negzero_zero = { re = -0.0; im = 0.0 }
let zero_negzero = { re = 0.0; im = -0.0 }
let negzero_negzero = { re = -0.0; im = -0.0 }
let inf_inf = { re = infinity; im = infinity }
let neginf_inf = { re = neg_infinity; im = infinity }
let inf_neginf = { re = infinity; im = neg_infinity }
let neginf_neginf = { re = neg_infinity; im = neg_infinity }
let inf_pi2 = { re = infinity; im = Constant.pi2 }
let inf_negpi2 = { re = infinity; im = ~-. Constant.pi2 }
let neginf_pi2 = { re = neg_infinity; im = Constant.pi2 }
let neginf_negpi2 = { re = neg_infinity; im = ~-. Constant.pi2 }
let inf_pi4 = { re = infinity; im = Constant.pi4 }
let inf_negpi4 = { re = infinity; im = ~-. Constant.pi4 }
let neginf_pi4 = { re = neg_infinity; im = Constant.pi4 }
let neginf_negpi4 = { re = neg_infinity; im = ~-. Constant.pi4 }
let pi2_inf = { re = Constant.pi2; im = infinity }
let pi2_neginf = { re = Constant.pi2; im = neg_infinity }
let negpi2_inf = { re = ~-. Constant.pi2; im = infinity }
let negpi2_neginf = { re = ~-. Constant.pi2; im = neg_infinity }



let test_sin () =
  let test = test_fun1 Complexer.sin "sin" in
    start_test "sin";
    test Complex.zero Complex.zero;
    test negzero_zero negzero_zero;
    test zero_negzero zero_negzero;
    test negzero_negzero negzero_negzero;
    test { re=0.0; im=infinity } { re=0.0; im=infinity };
    test { re = -0.0; im=infinity } { re = -0.0; im = infinity };
    test { re = 0.0; im = neg_infinity } { re = 0.0; im = neg_infinity };
    test { re = -0.0; im = neg_infinity} { re = -0.0; im = neg_infinity };
    test { re = infinity; im = 0.0} { re = nan; im = 0.0 };
    test { re = neg_infinity; im = 0.0} { re = nan; im = 0.0 };
    test { re = infinity; im = -0.0} { re = nan; im = 0.0 };
    test { re = neg_infinity; im = -0.0} { re = nan; im = 0.0 };
    test inf_inf { re = nan; im = infinity };
    test neginf_inf { re = nan; im = infinity };
    test neginf_neginf { re = nan; im = infinity };
    test { re = infinity; im = 6.75 } Complexer.nanc;
    test {re = infinity; im = -6.75 } Complexer.nanc;
    test { re = neg_infinity; im = 6.75 } Complexer.nanc;
    test { re = neg_infinity; im = -6.75 } Complexer.nanc;
    test { re = 4.625; im = infinity; } neginf_neginf;
    test { re = 4.625; im = neg_infinity } neginf_inf;
    test { re = -4.625; im = infinity} inf_neginf;
    test { re = -4.625; im = neg_infinity } inf_inf;
    test { re = nan; im = 0.0 } { re = nan; im = 0.0 };
    test { re = nan; im = -0.0 } { re = nan; im = 0.0 };
    test { re = nan; im = infinity } { re = nan; im = infinity };
    test { re = nan; im = neg_infinity } { re = nan; im =infinity };
    test { re = nan; im = 9.0 } Complexer.nanc;
    test { re = nan; im = -9.0 } Complexer.nanc;
    test { re = 0.0; im = nan } { re = 0.0; im = nan };
    test { re = -0.0; im = nan } { re = -0.0; im = nan };
    test { re = 10.0; im = nan } Complexer.nanc;
    test { re = nan; im = -10.0 } Complexer.nanc;
    test { re = 0.75; im = 1.25 } { re = 1.28722291002649180669e+00; im = 1.17210635989270262058e+00 };
    test { re = -2.0; im = -3.0 } { re = -9.15449914691143007417e+00; im = 4.16890695996656468481e+00 };
    end_test "sin"

let test_cos () =
  let test = test_fun1 Complexer.cos "cos" in
    start_test "cos";
    test Complex.zero { re = 1.0; im = -0.0 };
    test negzero_zero { re = 1.0; im = 0.0 };
    test zero_negzero { re = 1.0; im = 0.0 };
    test negzero_negzero { re = 1.0; im = -0.0 };
    test { re = infinity; im = 0.0 } { re = nan; im = 0.0 };
    test { re = infinity; im = -0.0 } { re = nan; im = 0.0 };
    test { re = neg_infinity; im = 0.0 } { re = nan; im = 0.0 };
    test { re = neg_infinity; im = -0.0 } { re = nan; im = 0.0 };
    test { re = 0.0; im = infinity } { re = infinity; im = -0.0 };
    test { re  = 0.0; im = neg_infinity } { re = infinity; im = 0.0 };
    test { re = -0.0; im = infinity } { re = infinity; im = 0.0 };
    test { re = -0.0; im = neg_infinity } { re = infinity; im = -0.0 };
    test { re = infinity; im = infinity } { re = infinity; im = nan };
    test { re = neg_infinity; im = infinity } { re = infinity; im = nan };
    test { re = infinity; im = neg_infinity } { re = infinity; im = nan };
    test neginf_neginf { re = infinity; im = nan };
    test { re = 4.625; im = infinity } { re = neg_infinity; im = infinity };
    test { re = 4.625; im = neg_infinity } neginf_neginf;
    test { re = -4.625; im = infinity } neginf_neginf;
    test { re = -4.625; im = neg_infinity } { re = neg_infinity; im = infinity };
    test { re = infinity; im = 6.75 } Complexer.nanc;
    test { re = infinity; im = -6.75 } Complexer.nanc;
    test { re = neg_infinity; im = 6.75 } Complexer.nanc;
    test { re = neg_infinity; im = -6.75 } Complexer.nanc;
    test { re = nan; im = 0.0 } { re = nan; im = 0.0 };
    test { re = nan; im = -0.0 } { re = nan; im = 0.0 };
    test { re = nan; im = infinity } { re = infinity; im = nan };
    test { re = nan; im = neg_infinity } { re = infinity; im = nan };
    test { re = nan; im = 9.0 } Complexer.nanc;
    test { re = nan; im = -9.0 } Complexer.nanc;
    test { re = 0.0; im = nan } { re = nan; im = 0.0 };
    test { re = -0.0; im = nan } { re = nan; im = 0.0 };
    test { re = 10.0; im = nan } Complexer.nanc;
    test { re = -10.0; im = nan } Complexer.nanc;
    test { re = infinity; im = nan } Complexer.nanc;
    test { re = neg_infinity; im = nan } Complexer.nanc;
    test Complexer.nanc Complexer.nanc;
    test { re = 0.75; im = 1.25 } { re = 1.38173873063425900476e+00; im = -1.09193013555397455683e+00 };
    test { re = -2.0; im = -3.0} { re = -4.18962569096880699249e+00; im = -9.10922789375533703549e+00 };    
    end_test "cos"

let test_tan () =
  let test = test_fun1 Complexer.tan "tan" in
    start_test "tan";
    test Complex.zero Complex.zero;
    test zero_negzero zero_negzero;
    test negzero_zero negzero_zero;
    test negzero_negzero negzero_negzero;
    test { re = 0.0; im = infinity } { re = 0.0; im = 1.0 };
    test { re = 1.0; im = infinity } { re = 0.0; im = 1.0 };
    test { re = -0.0; im = infinity } { re = -0.0; im = 1.0 };
    test { re = -1.0; im = infinity } { re = -0.0; im = 1.0 };
    test { re = 0.0; im = neg_infinity } { re = 0.0; im = -1.0 };
    test { re = 1.0; im = neg_infinity } { re = 0.0; im = -1.0 };
    test { re = -0.0; im = neg_infinity } { re = -0.0; im = -1.0 };
    test { re = -1.0; im = neg_infinity } { re = -0.0; im = -1.0 };
    test { re = infinity; im = 0.0 } Complexer.nanc;
    test { re = infinity; im = 2.0 } Complexer.nanc;
    test { re = neg_infinity; im = 0.0 } Complexer.nanc;
    test { re = neg_infinity; im = 2.0 } Complexer.nanc;
    test { re = infinity; im = -0.0 } Complexer.nanc;
    test { re = infinity; im = -2.0 } Complexer.nanc;
    test { re = neg_infinity; im = -0.0 } Complexer.nanc;
    test { re = neg_infinity; im = -2.0 } Complexer.nanc;
    test { re = nan; im = infinity } Complex.i;
    test { re = nan; im = neg_infinity } { re = 0.0; im = -1.0 };
    test { re = 0.0; im = nan } { re = 0.0; im = nan };
    test { re = -0.0; im = nan } { re = -0.0; im = nan };
    test { re = 0.5; im = nan } Complexer.nanc;
    test { re = -4.5; im = nan } Complexer.nanc;
    test { re = nan; im = 0.0 } Complexer.nanc;
    test { re = nan; im = 5.0 } Complexer.nanc;
    test { re = nan; im = -0.0 } Complexer.nanc;
    test { re = nan; im = -0.25 } Complexer.nanc;
    test Complexer.nanc Complexer.nanc;
    test { re = 0.75; im = 1.25 } { re = 1.60807785916206424925e-01; im = 9.75363285031235593969e-01 };
    test { re = -2.0; im = -3.0 } { re = 3.76402564150424799594e-03; im = -1.00323862735360980203e+00 };        
    end_test "tan"

let test_asin () =
  let test = test_fun1 Complexer.asin "asin" in
    start_test "asin";
    test Complex.zero Complex.zero;
    test negzero_zero negzero_zero;
    test zero_negzero zero_negzero;
    test negzero_negzero negzero_negzero;
    test inf_inf { re = Constant.pi4; im = infinity };
    test inf_neginf { re = Constant.pi4; im = neg_infinity };
    test neginf_inf { re = ~-. Constant.pi4; im = infinity };
    test neginf_neginf { re = ~-. Constant.pi4; im = neg_infinity };
    test { re = -10.0; im = infinity } { re = -0.0; im = infinity };
    test { re = -10.0; im = neg_infinity } { re = -0.0; im = neg_infinity };
    test { re = 0.0; im = infinity } { re = 0.0; im = infinity };
    test { re = 0.0; im = neg_infinity } { re = 0.0; im = neg_infinity };
    test { re = -0.0; im = infinity } { re = -0.0; im = infinity };
    test { re = -0.0; im = neg_infinity } { re = -0.0; im = neg_infinity };
    test { re = 0.1; im = infinity } { re = 0.0; im = infinity };
    test { re = 0.1; im = neg_infinity } { re = 0.0; im = neg_infinity };
    test { re = neg_infinity; im = 0.0 } negpi2_inf;
    test { re = neg_infinity; im = -0.0 } negpi2_neginf;
    test { re = neg_infinity; im = 100.0 } negpi2_inf;
    test { re = neg_infinity; im = -100.0 } negpi2_neginf;
    test { re = infinity; im = 0.0 } pi2_inf;
    test { re = infinity; im = -0.0 } pi2_neginf;
    test { re = infinity; im = 0.5 } pi2_inf;
    test { re = infinity; im = -0.5 } pi2_neginf;
    test { re = nan; im = infinity } { re = nan; im = infinity };
    test { re = nan; im = neg_infinity  } { re = nan; im = neg_infinity };
    test { re = 0.0; im = nan } { re = 0.0; im = nan };
    test { re = -0.0; im = nan } { re = -0.0; im = nan };
    test { re = infinity; im = nan } { re = nan; im = infinity };
    test { re = neg_infinity; im = nan } { re = nan; im = infinity };
    test { re = nan; im = 10.5 } Complexer.nanc;
    test { re = nan; im = -10.5 } Complexer.nanc;
    test { re = 0.75; im = nan } Complexer.nanc;
    test { re = -0.75; im = nan } Complexer.nanc;
    test Complexer.nanc Complexer.nanc;
    test { re = 0.75; im = 1.25 } { re = 4.53276177638793908997e-01; im = 1.13239363160530825958e+00 };
    test { re = -2.0; im = -3.0 } { re = -5.70652784321099448839e-01; im = -1.98338702991653548224e+00 };
    end_test "asin"

let test_acos () =
  let test = test_fun1 Complexer.acos "acos" in 
    start_test "acos";
    test Complex.zero { re = Constant.pi2; im = -0.0 };
    test negzero_zero { re = Constant.pi2; im = -0.0 };
    test negzero_negzero  { re = Constant.pi2; im = 0.0 };
    test zero_negzero  { re = Constant.pi2; im = 0.0 };
    test neginf_inf { re = 2.35619449019234483700e+00; im = neg_infinity };
    test neginf_neginf { re = 2.35619449019234483700e+00; im = infinity };
    test inf_inf { re = Constant.pi4; im = neg_infinity };
    test inf_neginf { re = Constant.pi4; im = infinity };
    test { re = -10.0; im = infinity } pi2_neginf;
    test { re = -10.0; im = neg_infinity } pi2_inf;
    test { re = 0.0; im = infinity } pi2_neginf;
    test { re = 0.0; im = neg_infinity } pi2_inf;
    test { re = 0.1; im = infinity } pi2_neginf;
    test { re = -0.1; im = neg_infinity } pi2_inf;
    test { re = neg_infinity; im = 0.0 } { re = Constant.pi; im = neg_infinity };
    test { re = neg_infinity; im = -0.0 } { re = Constant.pi; im = infinity };
    test { re = neg_infinity; im = 100.0 } { re = Constant.pi; im = neg_infinity };
    test { re = neg_infinity; im = -100.0 } { re = Constant.pi; im = infinity };
    test { re = infinity; im = 0.0 } { re = 0.0; im = neg_infinity };
    test { re = infinity; im = -0.0 } { re = 0.0; im = infinity };
    test { re = infinity; im = 0.5 } { re = 0.0; im = neg_infinity };
    test { re = infinity; im = -0.5 } { re = 0.0; im = infinity };
    test ~flags:[`IINFSIGN] { re = infinity; im = nan } { re = nan; im = infinity };
    test ~flags:[`IINFSIGN] { re = neg_infinity; im = nan } { re = nan; im = infinity };
    test { re = 0.0; im = nan } { re = Constant.pi2; im = nan };
    test { re = -0.0; im = nan } { re = Constant.pi2; im = nan };
    test { re = nan; im = infinity } { re = nan; im = neg_infinity };
    test { re = nan; im = neg_infinity } { re = nan; im = infinity };
    test { re = 10.5; im = nan } Complexer.nanc;
    test { re = -10.5; im = nan } Complexer.nanc;
    test { re = nan; im = 0.75 } Complexer.nanc;
    test { re = nan; im = -0.75 } Complexer.nanc;
    test Complexer.nanc Complexer.nanc;
    test { re = 0.75; im = 1.25 } { re = 1.11752014915610264900e+00; im = -1.13239363160530825958e+00 };
    test { re = -2.0; im = -3.0 } { re = 2.14144911111599611786e+00; im = 1.98338702991653548224e+00 };
    end_test "acos"

let test_atan () =
  let test = test_fun1 Complexer.atan "atan" in
    start_test "atan";
    test Complex.zero Complex.zero;
    test negzero_zero negzero_zero;
    test zero_negzero zero_negzero;
    test negzero_negzero negzero_negzero;
    test inf_inf { re = Constant.pi2; im = 0.0 };
    test inf_neginf { re = Constant.pi2; im = -0.0 };
    test neginf_inf { re = ~-. Constant.pi2; im = 0.0 };
    test neginf_neginf { re = ~-. Constant.pi2; im = -0.0 };
    test { re = infinity; im = -10.0 } { re = Constant.pi2; im = -0.0 };
    test { re = neg_infinity; im = -10.0 } { re = ~-. Constant.pi2; im = -0.0 };
    test { re = infinity; im = -0.0 } { re = Constant.pi2; im = -0.0 };
    test { re = neg_infinity; im = -0.0 } { re = ~-. Constant.pi2; im = -0.0 };
    test { re = infinity; im = 0.0 } { re = Constant.pi2; im = 0.0 };
    test { re = neg_infinity; im = 0.0 } { re = ~-. Constant.pi2; im = 0.0 };
    test { re = infinity; im = 0.1 } { re = Constant.pi2; im = 0.0 };
    test { re = neg_infinity; im = 0.1 } { re = ~-. Constant.pi2; im = 0.0 };
    test { re = 0.0; im = neg_infinity } { re = Constant.pi2; im = -0.0 };
    test { re = -0.0; im = neg_infinity } { re = ~-. Constant.pi2; im = -0.0 };
    test { re = 100.0; im = neg_infinity } { re = Constant.pi2; im = -0.0 };
    test { re = -100.0; im = neg_infinity } { re = ~-. Constant.pi2; im = -0.0 };
    test { re = 0.0; im = infinity } { re = Constant.pi2; im = 0.0 };
    test { re = -0.0; im = infinity } { re = ~-. Constant.pi2; im = 0.0 };

    test { re = 0.5; im = infinity } { re = Constant.pi2; im = 0.0 };
    test { re = -0.5; im = infinity } { re = ~-. Constant.pi2; im = 0.0 };
    test { re = nan; im = 0.0 } { re = nan; im = 0.0 };
    test { re = nan; im = -0.0 } { re = nan; im = -0.0 };
    test { re = nan; im = infinity } { re = nan; im = 0.0 };
    test { re = nan; im = neg_infinity } { re = nan; im = -0.0 };
    test { re = 0.0; im = nan } Complexer.nanc;
    test { re = -0.0; im = nan } Complexer.nanc;
    test { re = infinity; im = nan } { re = Constant.pi2; im = 0.0 };
    test { re = neg_infinity; im = nan } { re = ~-. Constant.pi2; im = 0.0 };
    test { re = nan; im = 10.5 } Complexer.nanc;
    test { re = nan; im = -10.5 } Complexer.nanc;
    test { re = 0.75; im = nan } Complexer.nanc;
    test { re = -0.75; im = nan } Complexer.nanc;
    test Complexer.nanc Complexer.nanc;
    test { re = 0.75; im = 1.25 } { re = 1.10714871779409040897e+00; im = 5.49306144334054891054e-01 };
    test { re = -2.0; im = -3.0 } { re = -1.40992104959657549301e+00; im = -2.29072682968538776649e-01 };
    end_test "atan"
      
let test_asinh () =
  let test = test_fun1 Complexer.asinh "asinh" in
    start_test "asinh";
    test zero_zero zero_zero;
    test negzero_zero negzero_zero;
    test zero_negzero zero_negzero;
    test negzero_negzero negzero_negzero;
    test inf_inf { re = infinity; im = Constant.pi4 };
    test inf_neginf { re = infinity; im = ~-. Constant.pi4 };
    test neginf_inf { re = neg_infinity; im = Constant.pi4 };
    test neginf_neginf { re = neg_infinity; im = ~-. Constant.pi4 };
    test { re = -10.0; im = infinity } neginf_pi2;
    test { re = -10.0; im = neg_infinity } { re = neg_infinity; im = ~-. Constant.pi2 };
    test { re = 0.0; im = neg_infinity } inf_negpi2;
    test { re = 0.0; im = infinity } inf_pi2;
    test { re = -0.0; im = infinity } neginf_pi2;
    test { re = -0.0; im = neg_infinity } neginf_negpi2;
    test { re = 0.1; im = infinity } inf_pi2;
    test { re = 0.1; im = neg_infinity } inf_negpi2;
    test { re = neg_infinity; im = 0.0 } { re = neg_infinity; im = 0.0 };
    test { re = neg_infinity; im = -0.0 } { re = neg_infinity; im = -0.0 };
    test { re = neg_infinity; im = 100.0 } { re = neg_infinity; im = 0.0 };
    test { re = neg_infinity; im = -100.0 } { re = neg_infinity; im = -0.0 };
    test { re = infinity; im = 0.0 } { re = infinity; im = 0.0 };
    test { re = infinity; im = -0.0 } { re = infinity; im = -0.0 };
    test { re = infinity; im = 0.5 } { re = infinity; im = 0.0 };
    test { re = infinity; im = -0.5 } { re = infinity; im = -0.0 };
    test { re = infinity; im = nan } { re = infinity; im = nan };
    test { re = neg_infinity; im = nan } { re = neg_infinity; im = nan };
    test { re = nan; im = 0.0 } { re = nan; im = 0.0 };
    test { re = nan; im = -0.0 } { re = nan; im = -0.0 };
    test { re = nan; im = infinity } { re = infinity; im = nan };
    test { re = nan; im = neg_infinity } { re = infinity; im = nan };
    test { re = 10.5; im = nan } Complexer.nanc;
    test { re = -10.5; im = nan } Complexer.nanc;
    test { re = nan; im = 0.75 } Complexer.nanc;
    test { re = -0.75; im = nan } Complexer.nanc;
    test Complexer.nanc Complexer.nanc;
    test { re = 0.75; im = 1.25 } { re = 1.03171853444778016673e+00; im = 9.11738290968487685717e-01 };
    test { re = -2.0; im = -3.0 } { re = -1.96863792579309637709e+00; im = -9.64658504407602812591e-01 };
    end_test "asinh"

let test_acosh () =
  let test = test_fun1 Complexer.acosh "acosh" in
    start_test "acosh";
    test zero_zero { re = 0.0; im = Constant.pi2 };
    test negzero_zero { re = 0.0; im = Constant.pi2 };
    test zero_negzero { re = 0.0; im = ~-. Constant.pi2 };
    test negzero_negzero { re = 0.0; im = ~-. Constant.pi2 };
    test neginf_inf { re = infinity; im = 2.35619449019234483700e+00 };
    test neginf_neginf { re = infinity; im = -2.35619449019234483700e+00 };
    test inf_inf { re = infinity; im = Constant.pi4 };
    test inf_neginf { re = infinity; im = ~-. Constant.pi4 };
    test { re = -10.0; im = infinity } { re = infinity; im = Constant.pi2 };
    test { re = -10.0; im = neg_infinity } { re = infinity; im = ~-. Constant.pi2 };
    test { re = 0.0; im = infinity } { re = infinity; im = Constant.pi2 };
    test { re = 0.0; im = neg_infinity } { re = infinity; im = ~-. Constant.pi2 };
    test { re = 0.1; im = infinity } { re = infinity; im = Constant.pi2 };
    test { re = 0.1; im = neg_infinity } { re = infinity; im = ~-. Constant.pi2 };
    test { re = neg_infinity; im = 0.0 } { re = infinity; im = Constant.pi };
    test { re = neg_infinity; im = -0.0 } { re = infinity; im = ~-. Constant.pi };
    test { re = neg_infinity; im = 100.0 } { re = infinity; im = Constant.pi };
    test { re = neg_infinity; im = -100.0 } { re = infinity; im = ~-. Constant.pi };
    test { re = infinity; im = 0.0 } { re = infinity; im = 0.0 };
    test { re = infinity; im = -0.0 } { re = infinity; im = -0.0 };
    test { re = infinity; im = 0.5 } { re = infinity; im = 0.0 };
    test { re = infinity; im = -0.5 } { re = infinity; im = -0.0 };
    test { re = infinity; im = nan } { re = infinity; im = nan };
    test { re = neg_infinity; im = nan } { re = infinity; im = nan };
    test { re = 0.0; im = nan } Complexer.nanc;
    test { re = -0.0; im = nan } Complexer.nanc;
    test { re = nan; im = infinity } { re = infinity; im = nan };
    test { re = nan; im = neg_infinity } { re = infinity; im = nan };
    test { re = 10.5; im = nan } Complexer.nanc;
    test { re = -10.5; im = nan } Complexer.nanc;
    test { re = nan; im = 0.75 } Complexer.nanc;
    test { re = nan; im = -0.75 } Complexer.nanc;
    test Complexer.nanc Complexer.nanc;
    test { re = 0.75; im = 1.25 } { re = 1.13239363160530825958e+00; im = 1.11752014915610264900e+00 };
    test { re = -2.0; im = -3.0 } { re = -1.98338702991653548224e+00; im = 2.14144911111599611786e+00 };
    end_test "acosh"

let test_atanh () =
  let test = test_fun1 Complexer.atanh "atanh" in
    start_test "atanh";
    test zero_zero zero_zero;
    test negzero_zero negzero_zero;
    test zero_negzero zero_negzero;
    test negzero_negzero negzero_negzero;
    test inf_inf { re = 0.0; im = Constant.pi2 };
    test inf_neginf { re = 0.0; im = ~-. Constant.pi2 };
    test neginf_inf { re = -0.0; im = Constant.pi2 };
    test neginf_neginf { re = -0.0; im = ~-. Constant.pi2 };
    test { re = -10.0; im = infinity } { re = -0.0; im = Constant.pi2 };
    test { re = -10.0; im = neg_infinity } { re = -0.0; im = ~-. Constant.pi2 };
    test { re = -0.0; im = infinity } { re = -0.0; im = Constant.pi2 };
    test { re = -0.0; im = neg_infinity } { re = -0.0; im = ~-. Constant.pi2 };
    test { re = 0.0; im = infinity } { re = 0.0; im = Constant.pi2 };
    test { re = 0.0; im = neg_infinity } { re = 0.0; im = ~-. Constant.pi2 };
    test { re = 0.1; im = infinity } { re = 0.0; im = Constant.pi2 };
    test { re = 0.1; im = neg_infinity } { re = 0.0; im = ~-. Constant.pi2 };
    test { re = neg_infinity; im = 0.0 } { re = -0.0; im = Constant.pi2 };
    test { re = neg_infinity; im = -0.0 } { re = -0.0; im = ~-. Constant.pi2 };
    test { re = neg_infinity; im = 100.0 } { re = -0.0; im = Constant.pi2 };
    test { re = neg_infinity; im = -100.0 } { re = -0.0; im = ~-. Constant.pi2 };
    test { re = infinity; im = 0.0 } { re = 0.0; im = Constant.pi2 };
    test { re = infinity; im = -0.0 } { re = 0.0; im = ~-. Constant.pi2 };
    test { re = infinity; im = 0.5 } { re = 0.0; im = Constant.pi2 };
    test { re = infinity; im = -0.5 } { re = 0.0; im = ~-. Constant.pi2 };
    test { re = 0.0; im = nan } { re = 0.0; im = nan };
    test { re = -0.0; im = nan } { re = -0.0; im = nan };
    test { re = infinity; im = nan } { re = 0.0; im = nan };
    test { re = neg_infinity; im = nan } { re = -0.0; im = nan };
    test { re = nan; im = 0.0 } Complexer.nanc;
    test { re = nan; im = -0.0 } Complexer.nanc;
    test { re = nan; im = infinity } { re = 0.0; im = Constant.pi2 };
    test { re = nan; im = neg_infinity } { re = 0.0; im = ~-. Constant.pi2 };
    test { re = 10.5; im = nan } Complexer.nanc;
    test { re = -10.5; im = nan } Complexer.nanc;
    test { re = nan; im = 0.75 } Complexer.nanc;
    test { re = nan; im = -0.75 } Complexer.nanc;
    test Complexer.nanc Complexer.nanc;
    test { re = 0.75; im = 1.25 } { re = 2.61492138795671902329e-01; im = 9.96825126463918631536e-01 };
    test { re = -2.0; im = -3.0 } { re = -1.46946666225529765093e-01; im = -1.33897252229449348349e+00 };
    end_test "atanh"

let test_sinh () =
  let test = test_fun1 Complexer.sinh "sinh" in
    start_test "sinh";
    test zero_zero zero_zero;
    test negzero_zero negzero_zero;
    test zero_negzero zero_negzero;
    test negzero_negzero negzero_negzero;
    test { re = 0.0; im = infinity } { re = 0.0; im = nan };
    test ~flags:[`RZEROSIGN] { re = -0.0; im = infinity } { re = 0.0; im = nan };
    test { re = 0.0; im = neg_infinity } { re = 0.0; im = nan };
    test ~flags:[`RZEROSIGN] { re = -0.0; im = neg_infinity } { re = 0.0; im = nan };
    test { re = infinity; im = 0.0 } { re = infinity; im = 0.0 };
    test { re = neg_infinity; im = 0.0 } { re = neg_infinity; im = 0.0 };
    test { re = infinity; im = -0.0 } { re = infinity; im = -0.0 };
    test { re = neg_infinity; im = -0.0 } { re = neg_infinity; im = -0.0 };
    test ~flags:[`RINFSIGN] inf_inf { re = infinity; im = nan };
    test ~flags:[`RINFSIGN] neginf_inf { re = infinity; im = nan };
    test ~flags:[`RINFSIGN] inf_neginf { re = infinity; im = nan };
    test ~flags:[`RINFSIGN] neginf_neginf { re = infinity; im = nan };
    test { re = infinity; im = 4.625 } neginf_neginf;
    test { re = neg_infinity; im = 4.625 } inf_neginf;
    test { re = infinity; im = -4.625 } neginf_inf;
    test { re = neg_infinity; im = -4.625 } inf_inf;
    test { re = 6.75; im = infinity } Complexer.nanc;
    test { re = -6.75; im = infinity } Complexer.nanc;
    test { re = 6.75; im = neg_infinity } Complexer.nanc;
    test { re = -6.75; im = neg_infinity } Complexer.nanc;
    test ~flags:[`RZEROSIGN] { re = 0.0; im = nan } { re = 0.0; im = nan };
    test ~flags:[`RZEROSIGN] { re = -0.0; im = nan } { re = 0.0; im = nan };
    test ~flags:[`RINFSIGN] { re = infinity; im = nan } { re = infinity; im = nan };
    test ~flags:[`RINFOSIGN] { re = neg_infinity; im = nan } { re = infinity; im = nan };
    test { re = 9.0; im = nan } Complexer.nanc;
    test { re = -9.0; im = nan } Complexer.nanc;
    test { re = nan; im = 0.0 } { re = nan; im = 0.0 };
    test { re = nan; im = -0.0 } { re = nan; im = -0.0 };
    test { re = nan; im = 10.0 } Complexer.nanc;
    test { re = nan; im = -10.0 } Complexer.nanc;
    test { re = nan; im = infinity } Complexer.nanc;
    test { re = nan; im = neg_infinity } Complexer.nanc;
    test Complexer.nanc Complexer.nanc;
    test { re = 0.75; im = 1.25 } { re = 2.59294854551162801126e-01; im = 1.22863452409509554819e+00 };
    test { re = -2.0; im = -3.0 } { re = 3.59056458998577987529e+00; im = -5.30921086248519857875e-01 };
    end_test "sinh"

let test_cosh () =
  let test = test_fun1 Complexer.cosh "cosh" in
    start_test "cosh";
    test zero_zero { re = 1.0; im = 0.0 };
    test negzero_zero { re = 1.0; im = -0.0 };
    test zero_negzero { re = 1.0; im = -0.0 };
    test negzero_negzero { re = 1.0; im = 0.0 };
    test ~flags:[`IZEROSIGN] { re = 0.0; im = infinity } { re = nan; im = 0.0 };
    test ~flags:[`IZEROSIGN] { re = -0.0; im = infinity } { re = nan; im = 0.0 };
    test ~flags:[`IZEROSIGN] { re = 0.0; im = neg_infinity } { re = nan; im = 0.0 };
    test ~flags:[`IZEROSIGN] { re = -0.0; im = neg_infinity } { re = nan; im = 0.0 };
    test { re = infinity; im = 0.0 } { re = infinity; im = 0.0 };
    test { re = neg_infinity; im = 0.0 } { re = infinity; im = -0.0 };
    test { re = infinity; im = -0.0 } { re = infinity; im = -0.0 };
    test { re = neg_infinity; im = -0.0 } { re = infinity; im = 0.0 };
    test inf_inf { re = infinity; im = nan };
    test neginf_inf { re = infinity; im = nan };
    test inf_neginf { re = infinity; im = nan };
    test neginf_neginf { re = infinity; im = nan };
    test { re = infinity; im = 4.625 } neginf_neginf;
    test { re = neg_infinity; im = 4.625 } neginf_inf;
    test { re = infinity; im = -4.625 } neginf_inf;
    test { re = neg_infinity; im = -4.625 } neginf_neginf;
    test { re = 6.75; im = infinity } Complexer.nanc;
    test { re = -6.75; im = infinity } Complexer.nanc;
    test { re = 6.75; im = neg_infinity } Complexer.nanc;
    test { re = -6.75; im = neg_infinity } Complexer.nanc;
    test ~flags:[`IZEROSIGN] { re = 0.0; im = nan } { re = nan; im = 0.0 };
    test ~flags:[`IZEROSIGN] { re = -0.0; im = nan } { re = nan; im = 0.0 };
    test { re = infinity; im = nan } { re = infinity; im = nan };
    test { re = neg_infinity; im = nan } { re = infinity; im = nan };
    test { re = 9.0; im = nan } Complexer.nanc;
    test { re = -9.0; im = nan } Complexer.nanc;
    test ~flags:[`IZEROSIGN] { re = nan; im = 0.0 } { re = nan; im = 0.0 };
    test ~flags:[`IZEROSIGN] { re = nan; im = -0.0 } { re = nan; im = 0.0 };
    test { re = nan; im = 10.0 } Complexer.nanc;
    test { re = nan; im = -10.0 } Complexer.nanc;
    test { re = nan; im = infinity } Complexer.nanc;
    test { re = nan; im = neg_infinity } Complexer.nanc;
    test Complexer.nanc Complexer.nanc;
    test { re = 0.75; im = 1.25 } { re = 4.08242591877968796776e-01; im = 7.80365930845853261744e-01 };
    test { re = -2.0; im = -3.0} { re = -3.72454550491532243583e+00; im = 5.11822569987384623502e-01 };
    end_test "cosh"

let test_tanh () =
  let test = test_fun1 Complexer.tanh "tanh" in
    start_test "tanh";
    test zero_zero zero_zero;
    test zero_negzero zero_negzero;
    test negzero_zero negzero_zero;
    test negzero_negzero negzero_negzero;
    test { re = infinity; im = 0.0 } { re = 1.0; im = 0.0 };
    test { re = infinity; im = 1.0 } { re = 1.0; im = 0.0 };
    test { re = infinity; im = -0.0 } { re = 1.0; im = -0.0 };
    test { re = infinity; im = -1.0 } { re = 1.0; im = -0.0 };
    test { re = neg_infinity; im = 0.0 } { re = -1.0; im = 0.0 };
    test { re = neg_infinity; im = 1.0 } { re = -1.0; im = 0.0 };
    test { re = neg_infinity; im = -0.0 } { re = -1.0; im = -0.0 };
    test { re = neg_infinity; im = -1.0 } { re = -1.0; im = -0.0 };
    test { re = 0.0; im = infinity } Complexer.nanc;
    test { re = 2.0; im = infinity } Complexer.nanc;
    test { re = 0.0; im = neg_infinity } Complexer.nanc;
    test { re = 2.0; im = neg_infinity } Complexer.nanc;
    test { re = -0.0; im = infinity } Complexer.nanc;
    test { re = -2.0; im = infinity } Complexer.nanc;
    test { re = -0.0; im = neg_infinity } Complexer.nanc;
    test { re = -2.0; im = neg_infinity } Complexer.nanc;
    test ~flags:[`IZEROSIGN] { re = infinity; im = nan } { re = 1.0; im = 0.0 };
    test ~flags:[`IZEROSIGN] { re = neg_infinity; im = nan } { re = -1.0; im = 0.0 };
    test { re = nan; im = 0.0 } { re = nan; im = 0.0 };
    test { re = nan; im = -0.0 } { re = nan; im = -0.0 };
    test { re = nan; im = 0.5 } Complexer.nanc;
    test { re = nan; im = -4.5 } Complexer.nanc;
    test { re = 0.0; im = nan } Complexer.nanc;
    test { re = 5.0; im = nan } Complexer.nanc;
    test { re = -0.0; im = nan } Complexer.nanc;
    test { re = -0.25; im = nan } Complexer.nanc;
    test Complexer.nanc Complexer.nanc;
    test { re = 0.0; im = Constant.pi4 } { re = 0.0; im = 1.0 };
    test { re = 0.75; im = 1.25 } { re = 1.37260757053378323889e+00; im = 3.85795952609750636420e-01 };
    test { re = -2.0; im = -3.0 } { re = -9.65385879022133130967e-01; im =  9.88437503832249354796e-03 };
    end_test "tanh"

let test_log10 () =
  let test = test_fun1 Complexer.log10 "log10" in
    start_test "log10";
    test negzero_zero { re = neg_infinity; im = Constant.pi };
    test negzero_negzero { re = neg_infinity; im = ~-. Constant.pi };
    test zero_zero { re = neg_infinity; im = 0.0 };
    test zero_negzero { re = neg_infinity; im = -0.0 };
    test neginf_inf { re = infinity; im = 1.02328226538138111756e+00 };
    test inf_inf { re = infinity; im = 3.41094088460460354018e-01 };
    test inf_neginf { re = infinity; im = -3.41094088460460354018e-01 };
    test { re = 0.0; im = infinity } { re = infinity; im = 6.82188176920920708035e-01 };
    test { re = 3.0; im = infinity } { re = infinity; im = 6.82188176920920708035e-01 };
    test { re = -0.0; im = infinity } { re = infinity; im = 6.82188176920920708035e-01 };
    test { re = -3.0; im = infinity } { re = infinity; im = 6.82188176920920708035e-01 };
    test { re = 0.0; im = neg_infinity } { re = infinity; im = -6.82188176920920708035e-01 };
    test { re = 3.0; im = neg_infinity } { re = infinity; im = -6.82188176920920708035e-01 };
    test { re = -0.0; im = neg_infinity } { re = infinity; im = -6.82188176920920708035e-01 };
    test { re = -3.0; im = neg_infinity } { re = infinity; im = -6.82188176920920708035e-01 };
    test { re = neg_infinity; im = 0.0 } { re = infinity; im = 1.36437635384184141607e+00 };
    test { re = neg_infinity; im = 1.0 } { re = infinity; im = 1.36437635384184141607e+00 };
    test { re = neg_infinity; im = -0.0 } { re = infinity; im = -1.36437635384184141607e+00 };
    test { re = neg_infinity; im = -1.0 } { re = infinity; im = -1.36437635384184141607e+00 };
    test { re = infinity; im = 0.0 } { re = infinity; im = 0.0 };
    test { re = infinity; im = 1.0 } { re = infinity; im = 0.0 };
    test { re = infinity; im = -0.0 } { re = infinity; im = -0.0 };
    test { re = infinity; im = -1.0 } { re = infinity; im = -0.0 };
    test { re = infinity; im = nan } { re = infinity; im = nan };
    test { re = neg_infinity; im = nan } { re = infinity; im = nan };
    test { re = nan; im = infinity } { re = infinity; im = nan };
    test { re = nan; im = neg_infinity } { re = infinity; im = nan };
    test { re = 0.0; im = nan } Complexer.nanc;
    test { re = -0.0; im = nan } Complexer.nanc;
    test { re = -3.0; im = nan } Complexer.nanc;
    test { re = nan; im = 0.0 } Complexer.nanc;
    test { re = nan; im = -0.0 } Complexer.nanc;
    test { re = nan; im = -5.0 } Complexer.nanc;
    test Complexer.nanc Complexer.nanc;
    test { re = 0.75; im = 1.25 } { re = 1.63679467193165173455e-01; im = 4.47486970040493092782e-01 };
    test { re = -2.0; im = -3.0 } { re =  5.56971676153418360222e-01; im = -9.37554462986374681499e-01 }; 
    
    



    end_test "log10"


let main () =
  Arg.parse [ "-v", Arg.Set verbose, "Enable verbose test reporting" ] (fun _ -> prerr_endline "Unknown argument"; exit 1) "Run tests in the Mathlib Complexer module";
  test_sin ();
  test_cos ();
  test_tan ();

  test_asin ();
  test_acos (); 
  test_atan ();

  test_sinh ();
  test_cosh ();
  test_tanh ();

  test_asinh ();
  test_acosh ();
  test_atanh ();

  test_log10 ();

  ()

let _ = Printexc.print main ()
