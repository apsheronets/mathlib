(* Random Number Distributions
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


(* Source should generate in the range [min, max] *)
module type RNGSource = sig
  type t (* The type of numbers produced *)
  type state
  val genrand: state -> t (* Return a random number *)
  val min: t (* The smallest number genrand will return *)
  val max: t (* The largest number genrand will return *)
end

(* Filter that takes a Source where min < 0 and makes it a 0-max 
   range. Many of the *Dists need min == 0 *)
module S2USSource (Ops : Genops.Ops) (Source : RNGSource with type t = Ops.t) =
struct
  type t = Source.t
  type state = Source.state
  let genrand s = Ops.abs (Source.genrand s)
  let min = Ops.zero
  let max = max (Ops.abs Source.min) (Ops.abs Source.max)
end

(* Source interface to Random.int *)
module SysSource = struct
  type t = int
  type state = unit
  let min = 0
  let max = max_int - 1
  let genrand _ = Random.int max_int
end

module SysFloatSource = struct
  type t = float
  type state = unit
  let min = 0.0
  let max = 1.0
  let genrand _ = Random.float 1.0
end

module type IDIST =
  functor (Ops : Genops.Ops with type num_type = Genops.integer) ->
    functor (Source : RNGSource with type t = Ops.t) ->
      sig
        type t = Source.t
	type state = Source.state
        val min : t
        val max : t
        class rng :
	  state ->
          t ->
          t ->
          object
            method genrand : t
            method max : t
            method min : t
          end
      end

module type IFDIST = 
  functor (Ops: Genops.Ops with type num_type = Genops.integer) ->
    functor (Source: RNGSource with type t = float) ->
sig
  type t = Ops.t
  type state = Source.state
  val min: t
  val max: t
  class rng: state -> t -> object
    method min: t
    method max: t
    method genrand: t
  end
end


module type FDIST =
  functor (Source: RNGSource with type t = float) ->
    sig 
      type t = float
      class rng: Source.state -> object
	method genrand: t
	method min: t
	method max: t
      end
      val min: t
      val max: t
    end

(* Uniformly in the range [low, high] in theory. It needs testing. *)
module UniformDist (Ops : Genops.Ops with type num_type = Genops.integer)
  (Source: RNGSource with type t = Ops.t) = struct
    type t = Source.t
    type state = Source.state
    let min = Source.min
    let max = Source.max
    class rng state low high = object 
      method min = low
      method max = high
      val range = Ops.succ (Ops.sub high low)
      val limit = Ops.sub Ops.max
		    (Ops.rem Ops.max (Ops.add (Ops.sub high low) Ops.one))
      method genrand =
        if low > high then
          Ops.zero
	else if low = high then
	  low
        else
          let rec f () =
	    let n = Source.genrand state in
	      if n >= limit then f () else n in
	    Ops.add low (Ops.rem (f ()) range)
    end
  end

(* Uniformly in the range [low, high) in theory. It needs testing. *)
module UniformDist2 (Ops : Genops.Ops with type num_type = Genops.integer)
  (Source: RNGSource with type t = Ops.t) = struct
    type t = Source.t
    type state = Source.state
    let min = Source.min
    let max = Source.max
    class rng state low high = object 
      method min = low
      method max = Ops.pred high 
      val range = Ops.succ (Ops.sub (Ops.pred high) low) 
      val limit = Ops.sub Ops.max
		    (Ops.rem Ops.max (Ops.succ (Ops.sub high low)))
      method genrand =
        if low > high then
          Ops.zero
	else if low = high then
	  low
        else
          let rec f () =
	    let n = Source.genrand state in
	      if n >= limit then f () else n in
	    Ops.add low (Ops.rem (f ()) range)
    end
  end

(* Normal distribution, using the ratio method. Algorithm 3.4.1R from
The Art of Computer Programming *)

module NormalDist (Source: RNGSource with type t = float) = struct
  type t = Source.t
  type state = Source.state
  (* These min and maxes are just guesses. I need to sit down and work
  out the actual range sometime *)
  let min = -10.0
  let max = 10.0

  class rng state =  object (self)
    method min = 0.
    method max = 1.
    method private gen_nonzero = 
      let x = Source.genrand state in
	if x > 0.0 then x else self#gen_nonzero

    method private gen_normal =
      let u = self#gen_nonzero
      and v = Source.genrand state in
      let x = (2.2981988973 (* sqrt (8.0 -. Genops.e) *) *. (v -. 0.5)) /. u in
      let x' = x ** 2.0 in 
	if x' <= 5.0 -. (5.13610166675 (* 4.0 *. (Genops.e ** 0.25) *) *. u) then
	  x
	else if x' >= (1.0369610425 (* 4.0 *. (Genops.e ** -1.35) *)  /. u)
	  +. 1.4 then
	  self#gen_normal
	else if x' <= -4.0 *. log u then
	  x
	else
	  self#gen_normal

    method genrand = self#gen_normal
  end
end

(* The logarithm method from TAOCP. Someday I'll make it use algorithm
3.4.1S *)

module ExponentialDist (Source: RNGSource with type t = float) = struct
  type t = Source.t
  type state = Source.state
  let min = 0.0
  let max = max_float
	      
  class rng state mean = object
    method min = 0.0
    method max = max_float
    val neg_mean = -.mean
    method genrand = neg_mean *. log (Source.genrand state)
  end
end      

(* Geometric distribution *)
module GeometricDist (Ops: Genops.Ops with type num_type = Genops.integer)
  (Source: RNGSource with type t = float) = struct
    type t = Ops.t
    type state = Source.state
    let min = Ops.zero
    let max = Ops.max
    class rng state mean = object
      method min = Ops.zero
      method max = Ops.max
      method genrand =
	let pm1 = Ops.to_float (Ops.sub Ops.one mean) in
	  Ops.of_float (ceil (log (Source.genrand state)) /. log pm1)
    end
  end

(* Poisson distribution *)
module PoissonDist (Ops: Genops.Ops with type num_type = Genops.integer)
  (Source: RNGSource with type t = float) = struct
    type t = Ops.t
    type state = Source.state
    let min = Ops.zero
    let max = Ops.max
    module Dist = ExponentialDist(Source)
    class rng state mean = object
      val erng = new Dist.rng state (1.0 /. (Ops.to_float mean))
      method min = Ops.zero
      method max = Ops.max
      method genrand = 
	let accum = ref 0.0 
	and m = ref Ops.zero in
	  while !accum < 1.0 do
	    m := Ops.succ !m;
	    accum := !accum +. erng#genrand
	  done;
	  Ops.pred !m
    end
  end

