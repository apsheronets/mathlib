(* float type manipulation/information routines for ocaml
   Copyright (C) 2003 Shawn Wagner <raevnos@pennmush.org>

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


let compare x y = Pervasives.compare x y

let fcmp ~epsilon a b =
  let (_, a_exp) = frexp a 
  and (_, b_exp) = frexp b in
  let a_pow = 2.0 ** ((float_of_int a_exp) -. 1.0)
  and b_pow = 2.0 ** ((float_of_int b_exp) -. 1.0) in
  let delta = epsilon *. (max a_pow b_pow)
  and s = a -. b in
    if abs_float s <= delta then 0 else if s > delta then 1 else -1
    

let isfinite x =
  match classify_float x with
    | FP_normal | FP_subnormal | FP_zero -> true
    | _ -> false

let isinf x = (classify_float x) = FP_infinite
let isnan x = (classify_float x) = FP_nan
let isnormal x = (classify_float x) = FP_normal
let iszero x = (classify_float x) = FP_zero

let signbit x = Int64.shift_right_logical (Int64.bits_of_float x) 63 = Int64.one

let copysign x s =
  if signbit s then 
    -. (abs_float x)
  else
    abs_float x
