(* Simple vector routines
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


(** Basic vector rountines. *)


(** Vectors of different sizes are distinct types, built by a functor
  fed a module containing the length of the vector. Internally, they're
  just a [float array], but this removes the need to check arguments for
  the same dimensionality. 

  If you want to do serious linear algebra, you're probably better off
  with the lapack interface. This is just for lightweight use.

*)

(** Thrown whe ncreating a vector from an array of the wrong
  length. The first int is the expected length, the second is the
  actual. *)
exception Different_dimensions of int * int

(** Describing the dimension of a vector *)
module type Dimension = sig
  val length: int (* The length of a vector type *)
end

(** Vector operations *)
module type V = sig
  type t

  val dimension: int

  (** Return a given element of a vector *)
  val get: t -> int -> float

  (** Sets a given element of a vector *)
  val set: t -> int -> float -> unit

  (** [make val] returns a new vector, all elements of which are
    filled with val *)
  val make: float -> t

  (** Return a new vector filled by the given array. *)
  val from_array: float array -> t

  (** Return an array representation of the vector. *)
  val to_array: t -> float array

  (** Vector addition *)
  val add: t -> t -> t

  (** Vector subtraction *)
  val sub: t -> t -> t

  (** Vector multiplication *)
  val mul: t -> t -> t

  (** Multiplication of a scalar *)
  val muls: t -> float -> t

  (** Calculate the dot product of two vectors *)
  val dot_product: t-> t -> float

  (** Calculate the magnitude of a vector. *)
  val magnitude: t -> float

  (** Return the unit vector that points in the same direction as the 
    given vector. *)
  val unit_vector: t -> t

end

module Make(Dim: Dimension) : V

(** Dimension for three-element vectors *)
module DimThree: Dimension 

(** Three-element vector *)
module Three : V

(** Cross-product of two 3d vectors *)
val cross_product: Three.t -> Three.t -> Three.t
