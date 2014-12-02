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

exception Different_dimensions of int * int

module type Dimension = sig
  val length: int (* The length of a vector type *)
end

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

module Make(Dim: Dimension) = struct
  type t = float array

  let dimension = Dim.length

  let get v p = Array.get v p

  let set v p n = Array.set v p n

  let make x = Array.make Dim.length x

  let from_array ar =
    if Array.length ar <> Dim.length then
      raise (Different_dimensions (Dim.length, Array.length ar));
    Array.copy ar

  let to_array v = Array.copy v
      
  let add a b =
    Array.init Dim.length (function i -> a.(i) +. b.(i))

  let sub a b =
    Array.init Dim.length (function i -> a.(i) -. b.(i))

  let mul a b =
    Array.init Dim.length (function i -> a.(i) *. b.(i))

  let muls a s =
    Array.init Dim.length (function i -> a.(i) *. s)

  let dot_product a b =
    Array.fold_left (fun x dp -> dp +. x) 0.0 (mul a b)
    
  let magnitude v =
    sqrt (dot_product v v)

  let unit_vector v =
    let mag = magnitude v in
      if mag = 0.0 then
	make 0.0
      else
	Array.init Dim.length (function i -> v.(i) /. mag)	
end

module DimThree = struct
  let length = 3
end

module Three = Make(DimThree)

let cross_product a b =
  [| a.(1) *. b.(2) -. b.(1) *. a.(2);
     a.(2) *. b.(0) -. b.(2) *. a.(0);
     a.(0) *. b.(1) -. b.(0) *. a.(1) |]
