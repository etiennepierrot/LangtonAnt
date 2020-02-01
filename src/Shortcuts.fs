module Shortcuts

let inc a = a + 1
let dec a = a - 1

let rec remove n lst =
    match lst with
    | h :: tl when h = n -> tl
    | h :: tl -> h :: (remove n tl)
    | [] -> []