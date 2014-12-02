(* Mersenne Twister PRNG routines for ocaml
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

(** Mersenne Twister PRNG *)

(** This is an ocaml version of the Mersenne Twister random number
generator.

The interface of this pretty much follows the C reference version,
except instead of using a single global seed, you can have multiple
concurrent seeds that are passed to the generator functions. Thus, it
can be used in multi-threaded programs as long as two threads don't
use the same seed without explicit locking. It produces the same
output for a given seed as the reference version, of course.

@see <http://www.math.keio.ac.jp/matumoto/emt.html> for more
information.

*)

(** {1 Initialization} *)

(** The type of the seed pool. *)
type t

(** You can use a variety of different values for the seed when creating a new MT t, but thanks to the magic of polymorphic variants, they're all handled by the same function. *)
val make: [< `Seed of int | `Seed32 of int32 | `Array of int array | `Array32 of int32 array | `CurrentTime ] -> t

(** {1 Functions returning random numbers} *)

(** All of these but the int64 ones produce the same output
as the reference C code. *)

(** Unsigned int32. Range: 0 <= x <= 0xffffffff *)
val uint32: t -> int32

(** Signed int32. Range: 0 <= x <= 0x7fffffff *)
val int32: t -> int32

(* Unsigned int64. Range: 0 <= x <= 0xffffffffffffffff *)
val uint64: t -> int64

(* Signed int64. Range: 0 <= x <= 0x7fffffffffffffff *)
val int64: t -> int64

(** Unsigned nativeint. *)
val unativeint: t -> nativeint

(** Signed nativeint *)
val nativeint: t -> nativeint

(** Unsigned int. Range: 0 <= x <= 0x7fffffff or 0 <= x <= 0x7fffffffffffffff *)
val uint: t -> int

(** Signed int. Range: 0 <= x <= 0x3fffffff or 0 <= x <= 0x3fffffffffffffff *)
val int: t -> int

(** Range: 0 <= x <= 1 *)
val real1: t -> float

(** Range: 0 <= x < 1 *)
val real2: t -> float

(** Range: 0 < x < 1 *)
val real3: t -> float

(** Range: 0 <= x < 1 with 53-bit resolution *)
val res53: t -> float

(** {1 Sources for use with the [Rand] distributions}  *)
module IntSource : (Rand.RNGSource with type t = int and type state = t)
module Int32Source : (Rand.RNGSource with type t = int32 and type state = t)
module FloatSource : (Rand.RNGSource with type t = float and type state = t)
