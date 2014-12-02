(* Generic numeric math routines for ocaml
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

module type REFOPS = sig
  type t
  val (+=): t ref -> t -> unit
  val (-=): t ref -> t -> unit
  val ( *=): t ref -> t -> unit
  val (/=): t ref -> t -> unit
end

module GenRefOps (Base : Genops.Ops) = struct
  type t = Base.t
  let (+=) a b = a := Base.add !a b
  let (-=) a b = a := Base.sub !a b
  let ( *=) a b = a := Base.mul !a b
  let (/=) a b = a := Base.div !a b
end
  
module IntRefOps = GenRefOps(Genops.IntOps)
module FloatRefOps = GenRefOps(Genops.FloatOps)
module Int32RefOps = GenRefOps(Genops.Int32Ops)
module Int64RefOps = GenRefOps(Genops.Int64Ops)
module NativeRefOps = GenRefOps(Genops.NativeOps)


