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

type integer
type real

module type Ops = sig
  type t
  type num_type
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

module IntOps = struct
  type t = int
  type num_type = integer
  let add x y = x + y
  let sub x y = x - y
  let pred = pred
  let succ = succ
  let mul x y = x * y
  let div x y = x / y
  let rem x y = x mod y
  let abs = abs
  let zero = 0
  let one = 1
  let min = min_int
  let max = max_int
  let print out x = Printf.fprintf out "%d" x
  let to_float = float_of_int
  let of_float = int_of_float
end

module Int32Ops = struct
  type t = int32
  type num_type = integer
  let add = Int32.add
  let sub = Int32.sub
  let pred = Int32.pred
  let succ = Int32.succ
  let mul = Int32.mul
  let div = Int32.div
  let rem = Int32.rem
  let abs = Int32.abs
  let zero = Int32.zero
  let one = Int32.one
  let min = Int32.min_int
  let max = Int32.max_int
  let print out x = Printf.fprintf out "%ld" x
  let to_float = Int32.to_float
  let of_float = Int32.of_float
end

module Int64Ops = struct
  type t = int64
  type num_type = integer
  let add = Int64.add
  let sub = Int64.sub
  let pred = Int64.pred
  let succ = Int64.pred
  let mul = Int64.mul
  let div = Int64.div
  let rem = Int64.rem
  let abs = Int64.abs
  let zero = Int64.zero
  let one = Int64.one
  let min = Int64.min_int
  let max = Int64.max_int
  let print out x = Printf.fprintf out "%Ld" x
  let to_float = Int64.to_float
  let of_float = Int64.of_float
end

module NativeOps = struct
  type t = nativeint
  type num_type = integer
  let add = Nativeint.add
  let sub = Nativeint.sub
  let pred = Nativeint.pred
  let succ = Nativeint.succ
  let mul = Nativeint.mul
  let div = Nativeint.div
  let rem = Nativeint.rem
  let abs = Nativeint.abs
  let zero = Nativeint.zero
  let one = Nativeint.one
  let min = Nativeint.min_int
  let max = Nativeint.max_int
  let print out x = Printf.fprintf out "%nd" x
  let to_float = Nativeint.to_float
  let of_float = Nativeint.of_float
end

module FloatOps = struct
  type t = float
  type num_type = real
  let add x y = x +. y
  let sub x y = x -. y
  let pred x = x -. 1.0
  let succ x = x +. 1.0
  let mul x y = x *. y
  let div x y = x /. y
  let rem = mod_float
  let abs = abs_float
  let zero = 0.0
  let one = 1.0
  let min = min_float
  let max = max_float
  let print out x = Printf.fprintf out "%g" x
  let to_float x = x
  let of_float x = x
end

module ComplexOps = struct
  type t = Complex.t
  type num_type = real
  let add = Complex.add
  let sub = Complex.sub
  let pred x = Complex.sub x Complex.one
  let succ x = Complex.add x Complex.one
  let mul = Complex.mul
  let div = Complex.div
  let rem = Complex.div
  let abs x = { Complex.re = abs_float x.Complex.re; Complex.im = abs_float x.Complex.im }
  let zero = Complex.zero
  let one = Complex.one
  let min = { Complex.re = min_float; Complex.im = 0. }
  let max = { Complex.re = max_float; Complex.im = 0. }
  let print out x = Printf.fprintf out "%g%+gi" x.Complex.re x.Complex.im
  let to_float x = x.Complex.re
  let of_float r = { Complex.re = r; Complex.im = 0. }
end

