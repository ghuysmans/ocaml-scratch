let rec sequence = function
  | [] -> Ok []
  | h :: t -> Result.(bind h (fun h -> map (fun t -> h :: t) (sequence t)))
