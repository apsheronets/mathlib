(* Extra complex number routines for ocaml
   Copyright (C) 2004 Shawn Wagner <raevnos@pennmush.org>

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

open Complex


(* let signbit x =
  x < 0.0 or (x = 0.0 && 1.0 /. x < 0.0)
(** Return true if sign of x is negative *)  
*)

let isfinite2 = function 
  | FP_zero | FP_subnormal | FP_normal -> true
  | FP_infinite | FP_nan -> false
let notfinite = function | FP_nan | FP_infinite -> true | _ -> false

let nanc = { re = nan; im = nan; }

let sinh x =
  let negate = Float.signbit x.re
  and rcls = classify_float x.re
  and icls = classify_float x.im 
  and x = { x with re = abs_float x.re } in
    match rcls with
      | FP_zero
      | FP_subnormal
      | FP_normal ->
	  (* Real part is finite *)
	  if isfinite2 icls then (* Imaginary part is finite *)
	    let sinh_val = Pervasives.sinh x.re
	    and cosh_val = Pervasives.cosh x.re
	    and sinix = Pervasives.sin x.im
	    and cosix = Pervasives.cos x.im in
	    let ret = { re = sinh_val *. cosix; im = cosh_val *. sinix } in
	      if negate then
		{ ret with re = -. (ret.re) }
	      else
		ret
	  else if rcls = FP_zero then (* Real part is 0.0 *)
	    { re = Float.copysign 0.0 (if negate then -1.0 else 1.0); im = nan }
	  else 
	    nanc
      | FP_infinite -> (* Real part is infinite *)
	  if icls = FP_zero then (* Imaginary part is 0.0 *)
	    { re = Float.copysign infinity (if negate then -1.0 else 1.0); im = x.im }
	  else if isfinite2 icls then (* Imaginary part is finite *)
	    let sinix = Pervasives.sin x.im
	    and cosix = Pervasives.cos x.im in
	    let ret = { re = Float.copysign infinity cosix;
			im = Float.copysign infinity sinix } in
	      if negate then
		{ ret with re = -. (ret.re) }
	      else
		ret
	  else 
	    { re = infinity; im = nan }
      | _ ->
	  { re = nan; im = if icls = FP_zero then x.im else nan }
(** Returns the complex hyperbole sine of x *)

let sin x =
  let negate = Float.signbit x.re
  and rcls = classify_float x.re
  and icls = classify_float x.im
  and r = abs_float x.re in
    match icls with
      | FP_zero
      | FP_subnormal
      | FP_normal ->
	  (* Imaginary part is finite *)
	  if Float.isfinite r then begin (* Real part is finite *)
	    let sinh_val = Pervasives.sinh x.im
	    and cosh_val = Pervasives.cosh x.im
	    and sinix = Pervasives.sin r
	    and cosix = Pervasives.cos r in
	    let retval = { re = cosh_val *. sinix;
			   im = sinh_val *. cosix } in
	      if negate then
		{ retval with re = -.retval.re }
	      else
		retval
	  end else if icls = FP_zero then
	    { re = nan; im = 0.0 }
	  else
	    nanc
      | FP_infinite -> (* Imaginary part is infinite *)
	  begin match rcls with
	    | FP_zero -> (* Real part is 0.0 *)
		{ x with re = if negate then -0.0 else 0.0 }
	    | FP_subnormal
	    | FP_normal ->
		(* Real part is finite *)
		let sinix = Pervasives.sin r
		and cosix = Pervasives.cos r in
		let rv = Float.copysign infinity sinix
		and iv = Float.copysign infinity cosix in
		let rv = if negate then ~-. rv else rv
		and iv = if Float.signbit x.im then ~-. iv else iv in
		  { re = rv; im = iv }
	    | _ -> (* The addition raises the invalid exception *)
		{ re = nan; im = infinity }
	  end
      | _ -> (* Imaginary part is something else *)
	  if rcls = FP_zero then
	    { re = Float.copysign 0.0 (if negate then -1.0 else 1.0); im = nan }
	  else
	    nanc
(** Returns the complex sine of x *)

let asinh x =
  let rcls = classify_float x.re
  and icls = classify_float x.im in
    match (rcls, icls) with
      | (FP_nan, FP_infinite) ->
	  { re = Float.copysign infinity x.re; im = nan }
      | (FP_zero, FP_infinite)
      | (FP_subnormal, FP_infinite)
      | (FP_normal, FP_infinite) ->
	  { re = Float.copysign infinity x.re;
	    im = Float.copysign Constant.pi2 x.im }
      | (FP_infinite, FP_infinite) ->
	  { re = Float.copysign infinity x.re;
	    im = Float.copysign Constant.pi4 x.im }	  
      | (FP_infinite, FP_zero)
      | (FP_infinite, FP_subnormal)
      | (FP_infinite, FP_normal) ->
	  { re = x.re; im = Float.copysign 0.0 x.im }
      | (FP_infinite, _)
      | (FP_nan, FP_zero) ->
	  { re = x.re; im = x.im }
      | (FP_nan, _) ->
	  { re = x.re; im = nan }
      | (_, FP_nan) ->
	  nanc
      | (FP_zero, FP_zero) ->
	  x
      | (_, _) ->
	  let y = Complex.sqrt { re = (x.re -. x.im) *. (x.re +. x.im) +. 1.0;
				 im = 2.0 *. x.re *. x.im } in
	    Complex.log { re = y.re +. x.re; im = y.im +. x.im }
(** Complex arc hyperbole sine of x*)

let asin x = 
  let rcls = classify_float x.re
  and icls = classify_float x.im in
    match (rcls, icls) with
      | (FP_zero, FP_nan) ->
	  x
      | (FP_infinite, FP_nan) 
      | (FP_nan, FP_infinite) ->
	  { re = nan; im = Float.copysign infinity x.im }
      | (FP_nan, _)
      | (_, FP_nan) ->
	  nanc
      | (_, _) ->
	  let y = asinh { re = x.im; im = x.re } in
	    { re = y.im; im = y.re }
(** Complex arc sine of x *)
	
let cosh x =
  let icls = classify_float x.im in
    match classify_float x.re with
      | FP_zero | FP_subnormal | FP_normal -> (* Real part is finite *)
	  if isfinite2 icls then (* Imaginary part is finite *)
	    let sinh_val = Pervasives.sinh x.re
	    and cosh_val = Pervasives.cosh x.re
	    and sinix = Pervasives.sin x.im
	    and cosix = Pervasives.cos x.im in
	      { re = cosh_val *. cosix; im = sinh_val *. sinix }
	  else
	    { re = nan; im = if x.re = 0.0 then 0.0 else nan }
      | FP_infinite -> (* Real part is infinite *)
	  if icls = FP_zero then
	    { re = infinity; im = x.im *. (Float.copysign 1.0 x.re) }
	  else if isfinite2 icls then
	    let sinix = Pervasives.sin x.im
	    and cosix = Pervasives.cos x.im in
	      { re = Float.copysign infinity cosix;
		im = (Float.copysign infinity sinix) *. (Float.copysign 1.0 x.re) }
	  else
	    { re = infinity; im = nan }
      | _ -> 
	  { re = nan; im = if x.im = 0.0 then 0.0 else nan }
(** Returns the complex hyperbole cosine of x *)

let acosh x =
  let rcls = classify_float x.re
  and icls = classify_float x.im in
    if notfinite rcls or notfinite icls then begin
      if icls = FP_infinite then
	{ re = infinity;
	  im = if rcls = FP_nan then
	    nan
	  else
	    Float.copysign (if rcls = FP_infinite then 
			if x.re < 0.0 then 
			  Constant.pi -. Constant.pi4
			else
			  Constant.pi4
		      else
			Constant.pi2) x.im 
	}
      else if rcls = FP_infinite then
	{ re = infinity;
	  im = if isfinite2 icls then
	    Float.copysign (if Float.signbit x.re then Constant.pi else 0.0) x.im
	  else
	    nan
	}
      else
	nanc
    end else if rcls = FP_zero && icls = FP_zero then
      { re = 0.0; im = Float.copysign Constant.pi2 x.im }
    else
      let y = Complex.sqrt { re = (x.re -. x.im) *. (x.re +. x.im) -. 1.0;
		im = 2.0 *. x.re *. x.im } in
	Complex.log { re = y.re +. x.re; im = y.im +. x.im }
(** The complex arc hyperbole cosine of x *)

let cos x =
  let rcls = classify_float x.re
  and icls = classify_float x.im in
  match (rcls, icls) with
    | (FP_nan, _)
    | (FP_infinite, _)
    | (_, FP_nan) -> 
	begin match (rcls, icls) with
	  | (FP_zero, _)
	  | (_, FP_zero) ->
	      { re = nan; im = 0.0 }
	  | (_, FP_infinite) ->
	      { re = infinity; im = nan }
	  | (_, _) ->
	      nanc
	end 
    | _ ->
	cosh { re = ~-. (x.im); im = x.re }
(** Returns the complex cosine of x *)

let acos x =
  let y = asin x in
    { re = Constant.pi2 -. y.re; im = ~-. (y.im) }
(** The complex arc cosine of x *)

let tanh x =
  if not (Float.isfinite x.re) or not (Float.isfinite x.im) then begin
    if Float.isinf x.re then
      { re = Float.copysign 1.0 x.re; im = Float.copysign 0.0 x.im }
    else if x.im = 0.0 then
      x
    else 
      nanc
  end else
    let rx = 2.0 *. x.re
    and ix = 2.0 *. x.im in
    let sin2ix = Pervasives.sin ix
    and cos2ix = Pervasives.cos ix in
    let den = (Pervasives.cosh rx) +. cos2ix in
      { re = (Pervasives.sinh rx) /. den; im = sin2ix /. den }
(** Returns the complex hyperbole tangent of x *)
		   
let tan x =
  if not (Float.isfinite x.re) or not (Float.isfinite x.im) then begin
    if Float.isinf x.im then
      { re = Float.copysign 0.0 x.re; im = Float.copysign 1.0 x.im }
    else if x.re = 0.0 then
      x
    else
      nanc
  end else 
    let rx = 2.0 *. x.re 
    and ix = 2.0 *. x.im in
    let sin2rx = Pervasives.sin rx
    and cos2rx = Pervasives.cos rx in
    let den = cos2rx +. (Pervasives.cosh ix) in
      { re = sin2rx /. den; im = (Pervasives.sinh ix) /. den }
(** Returns the complex tangent of x *)

let atan x = 
  let rcls = classify_float x.re
  and icls = classify_float x.im in
    if notfinite rcls or notfinite icls then begin
      if rcls = FP_infinite then
	{ re = Float.copysign Constant.pi2 x.re; im = Float.copysign 0.0 x.im }
      else if icls = FP_infinite then
	{ re = if isfinite2 rcls then Float.copysign Constant.pi2 x.re else nan;
	  im = Float.copysign 0.0 x.im }
      else if icls = FP_zero or icls = FP_infinite then
	{ re = nan; im = Float.copysign 0.0 x.im }
      else
	nanc
    end else if rcls = FP_zero && icls = FP_zero then
      x
    else
      let r2 = x.re *. x.re in
      let den = 1.0 -. r2 -. x.im *. x.im in
      let rp = 0.5 *. (Pervasives.atan2 (2.0 *. x.re) den) in
      let num = x.im +. 1.0 in
      let num = r2 +. num *. num in
      let den = x.im -. 1.0 in
      let den = r2 +. den *. den in
	{ re = rp; im = 0.25 *. (Pervasives.log (num /. den)) }
(** The complex arc tangent of x *)

let atanh x =
  let rcls = classify_float x.re
  and icls = classify_float x.im in
    if notfinite rcls or notfinite icls then begin
      if icls = FP_infinite then
	{ re = Float.copysign 0.0 x.re; im = Float.copysign Constant.pi2 x.im }
      else if rcls = FP_infinite or rcls = FP_zero then
	{ re = Float.copysign 0.0 x.re;
	  im = if isfinite2 icls then Float.copysign Constant.pi2 x.im else nan }
      else
	nanc
    end else if rcls = FP_zero && icls = FP_zero then
      x
    else begin
      let i2 = x.im *. x.im 
      and num' = 1.0 +. x.re 
      and den' = 1.0 -. x.re in
      let num = i2 +. num' *. num' 
      and den = i2 +. den' *. den' 
      and den2 = 1.0 -. x.re *. x.re -. i2 in
	{ re = 0.25 *. (Pervasives.log num -. Pervasives.log den);
	  im = 0.5 *. (atan2 (2.0 *. x.im) den2)
	}
    end  
      (** The complex arc hyperbole tanget of x *)


let log10 x = 
  let rcls = classify_float x.re
  and icls = classify_float x.im in
    if rcls = FP_zero && icls = FP_zero then 
      { re = -1.0 /. abs_float x.re;
	im = Float.copysign (if Float.signbit x.re then Constant.pi else 0.0) x.im
      }
    else if rcls <> FP_nan && icls <> FP_nan then
      { re = Pervasives.log10 (Math.hypot x.re x.im);
	im = Constant.log10e *. (Pervasives.atan2 x.im x.re)
      }
    else if rcls = FP_infinite || icls = FP_infinite then
      { re = infinity; im = nan }
    else 
      nanc
