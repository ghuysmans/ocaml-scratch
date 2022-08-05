module R = Ppx_deriving_yojson_runtime

type t = {
  name: string;
  value: Value.t list;
}

let of_yojson = function
  | `List [`String name; `List l] ->
    Result.map (fun value -> {name; value}) (R.map_bind Value.of_yojson [] l)
  | _ -> Error "Lst.of_yojson"

let to_yojson {name; value} =
  `List [`String name; `List (List.map Value.to_yojson value)]

