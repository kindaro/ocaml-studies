open Format ;;
open List ;;

let rec unfold p g b = if p b then [] else
    (match g b with (a, bprime) -> a :: unfold p g bprime) ;;

let range0 i = unfold (fun x -> x > i) (fun x -> (x, x +. 1.)) 0. ;;

let odd i = i*.2. +. 1. ;;

let sum = fold_left (+.) 0. ;;

let average x y = (x +. y) /. 2. ;;

let uncurry f (x, y) = f x y ;;

let last xs = xs |> rev |> hd ;;

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

let derivative f x =
    let dx = 10. ** (-10.)
    in (f (x +. dx) -. f x) /. dx ;;

let tangent_approach_step f x = x -. (f x /. derivative f x) ;;

let tangent_approach f y e =
    let next y = tangent_approach_step f y
    in unfold
        (fun y -> (next y -. y) ** 2. < e ** 2.)
        (fun y -> (y, next y))
        y ;;

let madhava i =
    let sqrt12 = last (tangent_approach (fun x -> x*.x -. 12.) 1. (10. ** (-10.)))
    in -1.**i *. sqrt12 /. (odd i *. 3.**i) ;;

let series f i = i |> range0 |> map f |> sum ;;

let pi i = series madhava i ;;

let _ = printf "%f" (pi 10.) ;;
