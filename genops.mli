(* Generic numeric math routines for ocaml
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


(** Modules for writing numeric algorithms that work on a variety of numeric types *)

(** Many times it's desirable to write some code that works with
numbers, and have versions that work on the different types of numbers
ocaml provides. With the modules here, it's possible to write the
algorithm once and then use the right functor to get it to work on the
desired type, instead of having different copies of the same routine
for each type. *)


(** {1 Types} *)

(** Contrains the type to integer ones *)
type integer

(** Constrains the type to float *)
type real

(** The type-generic operations that can be performed *)
module type Ops = sig
  type t (** The actual numeric type that's being used. *)
  type num_type (** Either [integer] or [real] *)
  val add: t -> t -> t
  val sub: t -> t -> t
  val succ: t -> t
  val pred: t -> t
  val mul: t -> t -> t
  val div: t -> t -> t
  val rem: t -> t -> t
  val abs: t -> t
  val zero: t
  val one: t
  val min: t
  val max: t
  val print: out_channel -> t -> unit
  val to_float: t -> float
  val of_float: float -> t
end

(** {1 Specializations for all the built-in integer and real types} *)

(** int *)
module IntOps : (Ops with type num_type = integer and type t = int)

(** int32 *)
module Int32Ops : (Ops with type num_type = integer and type t = int32)

(** int64 *)
module Int64Ops : (Ops with type num_type = integer and type t = int64)

(** nativeint *)
module NativeOps : (Ops with type num_type = integer and type t = nativeint)

(** float *)
module FloatOps : (Ops with type num_type = real and type t = float)

(** Complex.t *)
module ComplexOps : (Ops with type num_type = real and type t = Complex.t)

