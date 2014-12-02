(* Modular math ops
   Copyright (C) 2002 Shawn Wagner <raevnos@pennmush.org>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.
	 
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
	 
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

let pred_float r = r -. 1.0
let succ_float r = r +. 1.0

let root m n =  
  if n < 0 then
    raise (Failure "Negative root")
  else if m < 0. && n land 1 <> 1 then
    raise (Failure "Imaginary result")
  else if m < 0. then
    -. exp (log (abs_float m) /. (float_of_int n))
  else 
    exp (log m /. (float_of_int n))

let swap a b =
  let c = !a in
    a := !b;
    b := c

open Numref.IntRefOps

(*
module IntRefOps = Numref.GenRefOps(Genops.IntOps)
open IntRefOps
*)
let fraction ?(error=1.0e-10) v =
  if v < 0.0 or error < 0.0 then raise (Invalid_argument "Math.fraction");
  let d = ref 1
  and n = ref (int_of_float v) in
  let d2 = ref 1
  and n2 = ref (!n + 1)
  and r = ref 0.0	    
  and finished = ref false
  and first = ref true in
    while !first or !r <> 0.0 do
      if not !first then begin
	if !r <= 1.0 then
	  r := 1.0 /. !r;
	let ri = int_of_float !r in
	  n2 += (!n * ri);
	  d2 += (!d * ri);
	  n += !n2;
	  d += !d2
      end else
	first := false;
      r := 0.0;
      let df = float_of_int !d
      and nf = float_of_int !n in
	if v *. df <> nf then begin
	  r := ((float_of_int !n2) -. v *. (float_of_int !d2))
	  /. (v *. df -. nf);
	  if !r <= 1.0 then begin
	    swap n n2;
	    swap d d2
	  end
	end;
	if abs_float (1.0 -. (float_of_int !n)
		      /. (v *. (float_of_int !d))) <= error then
	  r := 0.0
    done;
    (!n, !d)

	
	  
    

external fma: float -> float -> float -> float = "ml_fma" "fma" "float"
external fdim: float -> float -> float = "ml_fdim" "fdim" "float"
external nextafter: float -> float -> float = "ml_nextafter" "nextafter" "float"
external remainder: float -> float -> float = "ml_remainder" "remainder" "float"
external trunc: float -> float = "ml_trunc" "trunc" "float"
external round: float -> float = "ml_round" "round" "float"
external nearbyint: float -> float = "ml_nearbyint" "nearbyint" "float"
external tgamma: float -> float = "ml_tgamma" "tgamma" "float"
external lgamma: float -> float = "ml_lgamma" "lgamma" "float"
external erfc: float -> float = "ml_erfc" "erfc" "float"
external erf: float -> float = "ml_erf" "erf" "float"
external hypot: float -> float -> float = "ml_hypot" "hypot" "float"
external cbrt: float -> float = "ml_cbrt" "cbrt" "float"
external scalbn: float -> int -> float = "ml_scalbn" 
external logb: float -> float = "ml_logb" "logb" "float"
external log2: float -> float = "ml_log2" "log2" "float"
external log1p: float -> float = "ml_log1p" "log1p" "float"
external ilogb: float -> int = "ml_ilogb" 
external expm1: float -> float = "ml_expm1" "expm1" "float"
external exp2: float -> float = "ml_exp2" "exp2" "float"
external atanh: float -> float = "ml_atanh" "atanh" "float"
external asinh: float -> float = "ml_asinh" "asinh" "float"
external acosh: float -> float = "ml_acosh" "acosh" "float"
external j0: float -> float = "ml_j0" "j0" "float"
external j1: float -> float = "ml_j1" "j1" "float"
external y0: float -> float = "ml_y0" "y0" "float"
external y1: float -> float = "ml_y1" "y1" "float"
