open Format ;;
open List ;;

let rec unfold p g b = if p b then [] else
    (match g b with (a, bprime) -> a :: unfold p g bprime) ;;

let range0 i = unfold (fun x -> x > i) (fun x -> (x, x +. 1.)) 0. ;;

let odd i = i*.2. +. 1. ;;

let sum = fold_left (+.) 0. ;;

let madhava i = -1.**i *. sqrt 12. /. (odd i *. 3.**i) ;;

let series f i = i |> range0 |> map f |> sum ;;

let pi i = series madhava i ;;

let _ = printf "%f" (pi 10.) ;;
