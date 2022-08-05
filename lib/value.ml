module R = Ppx_deriving_yojson_runtime

type t =
  | Float of float
  | Int of int
  | String of string
  | List of t list

let rec of_yojson = function
  | `Float f -> Ok (Float f)
  | `Int i -> Ok (Int i)
  | `String s -> Ok (String s)
  | `List l -> Result.map (fun l -> List l) (R.map_bind of_yojson [] l)
  | _ -> Error "Value.of_yojson"

let rec to_yojson = function
  | Float f -> `Float f
  | Int i -> `Int i
  | String s -> `String s
  | List l -> `List (List.map to_yojson l)
