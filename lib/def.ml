type 'a t = 'a option

let of_yojson f j =
  Result.map (fun x -> Some x) (f j)

let to_yojson f = function
  | None -> failwith "Def.to_yojson: please specify [@default None]"
  | Some x -> f x

