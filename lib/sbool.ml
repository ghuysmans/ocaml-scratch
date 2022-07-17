type t = bool

let of_yojson = function
  | `String "true" -> Ok true
  | `String "false" -> Ok false
  | _ -> Error "Sbool.of_yojson"

let to_yojson b = `String (if b then "true" else "false")
