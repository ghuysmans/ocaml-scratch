type t = {
  name: string;
  value: Value.t list;
}

let of_yojson = function
  | `List [`String name; `List l] ->
    List.map Value.of_yojson l |>
    Util.sequence |>
    Result.map (fun value -> {name; value})
  | _ -> Error "Lst.of_yojson"

let to_yojson {name; value} =
  `List [`String name; `List (List.map Value.to_yojson value)]

