(* Mersenne Twister PRNG for ocaml
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

(* Ocaml version of the Mersenne Twister random number generator based
   on mt19937ar.c.

   http://www.math.keio.ac.jp/matumoto/emt.html

*)

(* Period parameters *)
let n = 624
let m = 397

type t = {
  mt: int32 array;
  mutable mti: int;
}


(* Tempering parameters *)
(* Ocaml really needs a way to write literal int32's and int64s! *)
let mask_b = 0x9d2c5680l
and mask_c = 0xefc60000l
and upper_mask = 0x80000000l (* most significant w-r bits *)
and lower_mask = 0x7fffffffl (* least significant r bits *)
and mag01 = [| 0l; 0x9908b0dfl |] 
and mask_high = 0xffff0000l

let new_state () =
  {
    mt = Array.make n 0l;
    mti = n + 1;
  }

let init32 seed = 
  let t = new_state () in
  let m1 = 1812433253l
  and mfill = 0xffffffffl in
    Array.unsafe_set t.mt 0 (Int32.logand seed mfill);
    for i = 1 to n - 1 do
      let mtprev = Array.unsafe_get t.mt (i - 1) in
        Array.unsafe_set t.mt i (Int32.add
                           (Int32.mul m1
                              (Int32.logxor mtprev
                                 (Int32.shift_right_logical mtprev 30)))
                              (Int32.of_int i))
    done;
    t

let init_array32 seed =
  let t = init32 19650218l in
  let k = ref (max n (Array.length seed))
  and i = ref 1 
  and j = ref 0 
  and mval1 = 1664525l
  and mval2 = 1566083941l
  in
    while !k > 0 do
      let mtprev = Array.unsafe_get t.mt (!i - 1) in
        Array.unsafe_set t.mt !i
          (Int32.add
             (Int32.add
                (Int32.logxor (Array.unsafe_get t.mt !i)
                   (Int32.mul
                      (Int32.logxor mtprev
                         (Int32.shift_right_logical mtprev 30))
                      mval1))
                seed.(!j))
             (Int32.of_int !j));
        incr i;
        incr j;
        if !i >= n then begin
          Array.unsafe_set t.mt 0 (Array.unsafe_get t.mt (n - 1));
          i := 1
        end;
        if !j >= Array.length seed then j := 0;
        decr k
    done;

    k := n - 1;

    while !k > 0 do
      let mtprev = Array.unsafe_get t.mt (!i - 1) in
        Array.unsafe_set t.mt !i
          (Int32.sub
           (Int32.logxor
              (Array.unsafe_get t.mt !i)
              (Int32.mul
                 (Int32.logxor
                    mtprev
                    (Int32.shift_right_logical mtprev 30))
                    mval2))
              (Int32.of_int !i));
        incr i;
        if !i >= n then begin
          Array.unsafe_set t.mt 0 (Array.unsafe_get t.mt (n - 1));
          i := 1
        end;
        decr k
    done;
    Array.unsafe_set t.mt 0 0x80000000l;
    t

let make = function
  | `Seed32 x -> init32 x
  | `Seed x -> init32 (Int32.of_int x)
  | `Array32 x -> init_array32 x
  | `Array x -> init_array32 (Array.map Int32.of_int x)
  | `CurrentTime -> init32 (Int32.of_float (Sys.time ()))

      
let fill_mt t =
  let y = ref 0l
  and kk = ref 0 in 
  let fiddle i ip ig =
    y := Int32.logor
      (Int32.logand (Array.unsafe_get t.mt i) upper_mask)
      (Int32.logand (Array.unsafe_get t.mt ip) lower_mask);
    Array.unsafe_set t.mt i
      (Int32.logxor
         (Int32.logxor
            (Array.unsafe_get t.mt ig)
            (Int32.shift_right_logical !y 1))
         (Array.unsafe_get mag01 (Int32.to_int (Int32.logand !y Int32.one))))
  in
    while !kk < n - m do
      fiddle !kk (!kk + 1) (!kk + m);
      incr kk
    done;
    while !kk < n - 1 do
      fiddle !kk (!kk + 1) (!kk + m - n);
      incr kk
    done;
    fiddle (n - 1) 0 (m - 1);
    t.mti <- 0

let uint32 t =
  if t.mti >= n then fill_mt t;
  let y = Array.unsafe_get t.mt t.mti in
  let y' = Int32.logxor y (Int32.shift_right_logical y 11) in
  let y = Int32.logxor y' (Int32.logand (Int32.shift_left y' 7)
                             mask_b) in
  let y' = Int32.logxor y (Int32.logand (Int32.shift_left y 15)
                             mask_c) in
  let r = Int32.logxor y' (Int32.shift_right_logical y' 18) in
    t.mti <- t.mti + 1;
    r
     
let int32 t =
  let r = uint32 t in
    Int32.shift_right_logical r 1
 
let uint64 t =
  let high = Int64.of_int32 (uint32 t)
  and low = Int64.of_int32 (uint32 t) in
    Int64.logor low (Int64.shift_left high 32) 

let int64 t =
  let r = uint64 t in
    Int64.shift_right_logical r 1

let unativeint t =
  let v = Nativeint.of_int32 (uint32 t) in
    if Sys.word_size = 32 then v
  else Nativeint.logor v
    (Nativeint.shift_left (Nativeint.of_int32 (uint32 t)) 32)
  
let nativeint t =
  let r = unativeint t in
    Nativeint.shift_right_logical r 1

let uint t =
  let v = unativeint t in
    Nativeint.to_int (Nativeint.shift_right_logical v 1)

let int t =
  let r = unativeint t in
    Nativeint.to_int (Nativeint.shift_right_logical r 2)

let uint32_to_float ui32 =
  if ui32 >= Int32.zero then
    Int32.to_float ui32
  else (* ACK! *) 
    float_of_string (Printf.sprintf "%lu" ui32)

let real0 t =
  let i32 = uint32 t in
    uint32_to_float i32

let real1 t =
  let r = real0 t in
    r *. (1.0 /. 4294967295.0)

let real2 t =
  let r= real0 t in
    r *. (1.0 /. 4294967296.0)

let real3 t =
  let r = real0 t in
    (r +. 0.5) *. (1.0 /. 4294967296.0)

let res53 t =
  let a = Int32.shift_right_logical (uint32 t) 5 in
  let b = Int32.shift_right_logical (uint32 t) 6 in
  let a' = uint32_to_float a
  and b' = uint32_to_float b in
    (a' *. 67108864.0 +. b') *. (1.0 /. 9007199254740992.0)

module IntSource = struct
  type state = t
  type t = int
  let genrand s = int s
  let min = 0
  let max = max_int
end

module Int32Source = struct
  type state = t
  type t = int32
  let genrand s = int32 s
  let min = Int32.zero
  let max = Int32.max_int
end

module FloatSource = struct
  type state = t
  type t = float
  let genrand s = real1 s
  let min = 0.0
  let max = 1.0
end


