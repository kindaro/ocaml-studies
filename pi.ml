open Format ;;
open List ;;

let rec unfold p g b = if p b then [] else
    (match g b with (a, bprime) -> a :: unfold p g bprime) ;;

let range0 i = unfold (fun x -> x > i) (fun x -> (x, x +. 1.)) 0. ;;

let odd i = i*.2. +. 1. ;;

let sum = fold_left (+.) 0. ;;

let average x y = (x +. y) /. 2. ;;

let uncurry f (x, y) = f x y ;;

let binary_approach_step f y left_bound right_bound =
    let middle_value = average left_bound right_bound
    in if y < f middle_value
        then (left_bound, middle_value)
        else (middle_value, right_bound) ;;

let binary_approach f y e left_bound right_bound =
    unfold
        (fun (left_bound, right_bound) -> right_bound -. left_bound < e)
        (fun bounds -> (bounds, uncurry (binary_approach_step f y) bounds))
        (left_bound, right_bound) ;;

let madhava i =
    let sqrt12 =
        let bounds_list = binary_approach (fun x -> x*.x) 12. (10. ** (-10.)) 1. 100.
        in bounds_list |> rev |> hd |> fst
    in -1.**i *. sqrt12 /. (odd i *. 3.**i) ;;

let series f i = i |> range0 |> map f |> sum ;;

let pi i = series madhava i ;;

let _ = printf "%f" (pi 10.) ;;
