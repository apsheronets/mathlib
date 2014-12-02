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

(** Useful mathmatical functions *)

(** There's a lot of math functions that aren't provided by ocaml's
standard library. Some of them are C functions in the C99 standard,
others are common in unix C libraries, and others are just plain handy
at times. *)


(** {1 Useful functions} *)

(** @return [n+1.0] *)
val succ_float: float -> float

(**  @return [n-1.0] *)
val pred_float: float -> float

(** [root x n] calculates the nth root of x.  *)
val root: float -> int -> float

(** [fraction x] returns the numerator and denominator of a fraction approximating [x] *)
val fraction: ?error:float -> float -> int * int
(** @param error The accuracy to which the fraction should represent the original number. *)

(** {1 C99 functions} *)

(** If these functions aren't provided by a system's libc, they raise Failure. *)

(** The C99 function fma() *)
val fma: float -> float -> float -> float 
						   
(** @return The positive difference between its arguments. *)
val fdim: float -> float -> float 

(** @return The next representable value after x in the direction of y. *)
val nextafter: float -> float -> float 

(** @return x REM y. See IEC 60559. *)
val remainder: float -> float -> float 

(** @return The integer value nearest to but no larger in magnitude than x. *)
val trunc: float -> float 

(**
  @return X rounded to the nearest integer value, rounding halfway cases away from zero.
*)
val round: float -> float 

(**
  @return x, rounded to the nearest integer value, using the current rounding direction.
*)
val nearbyint: float -> float 

(** @return The gamma function of x. *)
val tgamma: float -> float 

(** @return The natural logarithm of the absolute value of gamma of x. *)
val lgamma: float -> float 

(** @return The complementary error function of x. *)
val erfc: float -> float 

(** @return The error function of x. *)
val erf: float -> float 

(** @return The square root of the sum of the squares of x and y. *)
val hypot: float -> float -> float 

(** @return the cube root of its argument *)
val cbrt: float -> float

(** @return x * (FLT_RADIX ** n) *)
val scalbn: float -> int -> float 

(** @return the exponent of x, as a signed integer value in float format. *)
val logb: float -> float 

(** @return The log base 2 of its argument. *)
val log2: float -> float 

(** @return The base-e logarithm of 1 plus x. *)
val log1p: float -> float 

(** @return The exponent of x. *)
val ilogb: float -> int 

(** @return The base-e exponential of x, minus 1. *)
val expm1: float -> float 

(** @return The base-2 exponential of x. *)
val exp2: float -> float 

(** @return The hyperbolic arc cotangent of x. *)
val atanh: float -> float 

(** @return The hyperbolic arc sine of x. *)
val asinh: float -> float 

(** @return The hyperbolic arc cosine of x. *)
val acosh: float -> float 

(** {1 Other C functions } *)

(** Once again, if these functions don't exist in the C library, they raise Failure. *)

(** @return The Bessel function of the first kind of order 0 *)
val j0: float -> float

(** @return The Bessel function of the first kind of order 0 *)
val j1: float -> float

(** @return The Bessel function of the second kind of order 0 *)
val y0: float -> float

(** @return The Bessel function of the second kind of order 1 *)
val y1: float -> float

