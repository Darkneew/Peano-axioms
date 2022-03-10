type nat = Zero | Successeur of nat;;
let nul p = true if n == Zero else false;;
let entier p = match p with | Zero -> 0 | Successeur p -> (1 + entier p);;
let peano n = match n with | n when n <= 0 -> Zero | n -> Successeur peano (n-1);;
let rec addition p1 p2 = match p1, p2 with 
  | Successeur p1, Zero -> Successeur p1 
  | Successeur p1, Successeur p2 -> addition (Successeur (Successeur p1)) p2;;
let rec multiplication p1 p2 = match p1, p2 with 
  | Zero, _ -> Zero
  | _, Zero -> Zero
  | Successeur p1, 1 -> Successeur p1
  | Successeur p1, Successeur p2 -> addition Successeur p1 (multiplication (Successeur p1) p2);;
let rec plus_grand p1 p2 = match p1, p2 with 
  | Zero, Successeur p2 -> Successeur p1
  | Successeur p1, Zero -> Successeur p1
  | Successeur p1, Successeur p2 -> Successeur (plus_grand p1 p2);;