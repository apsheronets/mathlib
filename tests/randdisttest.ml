
#use "findlib";;
#require "math";;

let seed = Mersenne.make `CurrentTime;;
module UniDistOf = Rand.UniformDist2(Genops.IntOps);;
module RNG=UniDistOf(Mersenne.IntSource);;
module RNG2 = Rand.ExponentialDist(Mersenne.FloatSource);;
module RNG3 = Rand.PoissonDist(Genops.IntOps)(Mersenne.FloatSource);;

let rand = new RNG.rng seed 0 10;;
let rand2 = new RNG2.rng seed 0.75;;
let rand3 = new RNG3.rng seed 10;;

let testintrand r = 
  Printf.printf "Max result: %5d and Min result: %5d\n" r#max r#min;
  for n = 0 to 49 do
    Printf.printf "%5d " r#genrand;
    if n mod 5 = 4 then print_newline ()
  done;;

let testfloatrand r =
  Printf.printf "Max result: %f and Min result: %f\n" r#max r#min;
  for n = 0 to 49 do
    Printf.printf "%8.6g " r#genrand;
    if n mod 5 = 4 then print_newline ()
  done;;


testintrand rand;;
testfloatrand rand2;;
testintrand rand3;;
