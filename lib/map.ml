type 'a t = (Id.t * 'a) list

let of_yojson f = function
  | `Assoc l ->
    Util.sequence (List.map (fun (k, v) -> Result.map (fun v -> k, v) (f v)) l)
  | _ -> Error "Map.of_yojson"

let to_yojson f l =
  `Assoc (List.map (fun (k, v) -> k, f v) l)
