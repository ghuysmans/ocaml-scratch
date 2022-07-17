type t = {
  name: string;
  value: Value.t;
  cloud: bool;
}

let of_yojson = function
  | `List [`String name; v] ->
    Result.map (fun value -> {name; value; cloud = false}) (Value.of_yojson v)
  | `List [`String name; v; `Bool true] ->
    Result.map (fun value -> {name; value; cloud = true}) (Value.of_yojson v)
  | _ -> Error "Variable.of_yojson"

let to_yojson {name; value; cloud} =
  `List (
    `String name ::
    Value.to_yojson value ::
    if cloud then [`Bool true] else []
  )

