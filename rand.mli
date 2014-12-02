(* Random Number Distributions
   Copyright (C) 2002-2003 Shawn Wagner <raevnos@pennmush.org>

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

(** Random number distributions *)

(**

  The idea here is to provide a variety of different random number
  distributions, for any math type you'd ever want to use, using any
  source of random numbers you'd ever need.

  So, the *Dist modules are two-argument functors for the most
  part. The first parameter is one of the ones in the {b Math}
  module (Or a user-provided one that has a compatible signature),
  that tells the code how to do basic math on a numeric type. The
  second parameter is a source of random numbers.  Numbers generated
  by it are fiddled with by the Dist module to get the proper
  distribution.

  There are several Source modules in the {b MtRand} and
  {b FileRand} packages, and {b SysSource} in this file is a
  Source for the standard Random generator.

  Each fully functorized module has two functions ([min] and [max],
  aliases for [Source.min] and [Source.max]), and one class,
  [rng]. [rng]'s constructor arguments vary depending on the
  distribution. The class has three methods: min and max, for the
  lowest and highest numbers an object will return, and genrand, which
  returns one random number according to the proper distribution.

  Note that these distributions {b do not} do any seeding of the
  underlying Source generator.

  They also haven't been tested exceedingly well. Most of the
  implementations are based on algorithms and descriptions from
  Knuth's The Art of Computer Programming, but I might have made
  errors in the translation. One of these days I'll have to look into
  doing some real testing to make sure the distributions are accurate.

*)

(**
   Example:

  Set up a new module that works with ints using the Mersenne Twister:

   [module MyRNG = Rand.UniformDist2(Math.IntOps)(MtRand.IntSource)]

   Create a uniform RNG that returns values in the range $0\leq x<100$
   and print one number:

   [let myrand = new MyRNG.rng 0 100 in print_int myrand#genrand;
   print_newline ()]
   \bigskip
*)


(** All sources must provide this signature: *)
module type RNGSource =
  sig type t type state val genrand : state -> t val min : t val max : t end

(** A source that takes a source that returns numbers less than 0 and
forces them to be positive *)
module S2USSource :
  functor (Ops : Genops.Ops) ->
    functor
      (Source : RNGSource with type t = Ops.t) ->
	(RNGSource with type t = Ops.t)

(** Source for the standard [Random.int] generator. *)
module SysSource : (RNGSource with type t = int and type state = unit)
(** Source for the standard [Random.float] generator. *)
module SysFloatSource: (RNGSource with type t = float and type state = unit)

(** Some of the {b Distribution} modules that work on integer
ranges will implement this signature. *)
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

(** [new UniformDist.rng low high] returns an object that will generate
random numbers in the range low < x < high. I need to test more. *)
module UniformDist : IDIST


(** [new UniformDist2.rng low high] returns an object that will
generate random numbers in the range low <= x < high. See above
about testing, especially since these uniform algorithms didn't come
from Knuth. *)
module UniformDist2 : IDIST

(** Module type for distributions that need a uniform float source but
return integer types and whose rng class constructor takes one argument. *)
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

(** [new GeometricDist.rng mean] returns an object that will generate
random numbers in the geometric distribution using the given mean. *)
module GeometricDist: IFDIST

(** [new PoissonDist.rng mean] returns an object that will generate
random numbers in the Poisson distribution using the given mean. *)
module PoissonDist: IFDIST

(** Signature used by many distributions that work entirely with
floats. These expect a generator that works in the range $0 <= x <= 1 unless otherwise noted. *)
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
	  
(** [new NormalDist.rng] returns an object that will generate random
floats in the normal distribution (Mean 0, standard deviation 1). It
looks like this is also known as the gaussian distribution. *)
module NormalDist: FDIST

(** [new ExponentialDist.rng mean] returns an object that will generate
random floats in an exponential distribution with the given mean.*)
module ExponentialDist:
  functor (Source: RNGSource with type t = float) ->
sig
  type t = float
  val min: t
  val max: t
  class rng: Source.state -> t -> object
    method min: t
    method max: t
    method genrand: t
  end
end

(** Future possibilities for distributions include

- GammaDist (float)
- BinomialDist (int)


*)

