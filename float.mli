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

(** Floating-point manipulation and information routines *)

(** {1 Comparison} *)

(** Just [Pervasives.compare] constrained to float *)
val compare: float -> float -> int

(** Like [compare], but takes an extra epsilon value to use in figuring
out if the floats are 'close enough' to be considered equal. See The
Art of Computer Programming, 4.2.2. As an example,
[fcmp ~epsilon:0.00001 5.000005 5.000006] returns 0, meaning
5.000005 ~~ 5.000006. *)
val fcmp: epsilon:float -> float -> float -> int

(** {1 Informational} *)

val isfinite: float -> bool
(**
  @return True if x is a finite value
*)

val isinf: float -> bool
(**
  @return True if x is infinite
*)

val isnan: float -> bool
(**
  @return True if x is nan 
*)

val isnormal: float -> bool
(**
  @return True if x is a normal number
*)

val iszero: float -> bool
(**
  @return True if x is 0.0 or -0.0
*)

val signbit: float -> bool
(**
  @return True if the sign bit of x is set
*)

(** {1 Other stuff} *)

val copysign: float -> float -> float
(**
  [copysign x y] returns a copy of x with the same sign as y.
*)
