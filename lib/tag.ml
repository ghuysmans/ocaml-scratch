type 'a t = 'a

let of_yojson f j = f (`List [j])

let to_yojson f x =
  match f x with
  | `List (h :: _) -> h
  | _ -> failwith "Tag.to_yojson"
