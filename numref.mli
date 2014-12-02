(* Convenience functions for int refs
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

(** C-style update-left-hand-side math operations *)

(** Opening the appropriate submodule lets you use C-style [+=] etc
  with reference types. *)

(** The operations supported. *)
module type REFOPS = sig

  (** The numeric type of the operations *)
  type t

  (** [a += b] adds b to the value in a. *)
  val (+=): t ref -> t -> unit

  (** [a -= b] subtracts b from the value in a. *)
  val (-=): t ref -> t -> unit

  (** [a *= b] multiplies a with b and updates a with the result. *)
  val ( *=): t ref -> t -> unit

    (** [a /= b] divides a with b and updates a with the result. *)
  val (/=): t ref -> t -> unit
end    

(** Functor to create modules with the appropriate operations *)
module GenRefOps : functor (Ops : Genops.Ops) -> REFOPS with type t = Ops.t 

(** Pre-defined modules for the builtin types are provided. *)
module IntRefOps : (REFOPS with type t = int)
module FloatRefOps : (REFOPS with type t = float)
module Int32RefOps : (REFOPS with type t = int32)
module Int64RefOps : (REFOPS with type t = int64)
module NativeRefOps : (REFOPS with type t = nativeint)

