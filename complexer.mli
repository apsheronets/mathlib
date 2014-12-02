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

(** More functions for complex numbers *)

(** {1 Constants} *)

(** A complex number with real and imaginary parts both nans *)
val nanc: Complex.t

(** {1 Logarithm functions} *)

val log10: Complex.t -> Complex.t
(** Complex log base 10 of x *)

(** {1 Trig functions} *)

val sin: Complex.t -> Complex.t
(** Complex sine of x *)

val cos: Complex.t -> Complex.t
(** Complex cosine of x *)

val tan: Complex.t -> Complex.t
(** Complex tanget of x *)

val asin: Complex.t -> Complex.t
(** Complex arc sine of x *)

val acos: Complex.t -> Complex.t
(** Complex arc cosine of x *)

val atan: Complex.t -> Complex.t
(** Complex arc tanget of x *)

val sinh: Complex.t -> Complex.t
(** Complex hyperbole sine of x *)

val cosh: Complex.t -> Complex.t
(** Complex hyperbole cosine of x *)

val tanh: Complex.t -> Complex.t
(** Complex hyperbole tanget of x *)

val asinh: Complex.t -> Complex.t
(** Complex arc hyperbole sine of x *)

val acosh: Complex.t -> Complex.t
(** Complex arc hyperbole cosine of x *)

val atanh: Complex.t -> Complex.t
(** Complex arc hyperbole tanget of x *)
