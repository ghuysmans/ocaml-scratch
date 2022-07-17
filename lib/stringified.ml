type 'a t = 'a

let of_yojson f = function
  | `String s ->
    begin try
      f (Yojson.Safe.from_string s)
    with Yojson.Json_error e ->
      Error ("Stringified.of_yojson: " ^ e)
    end
  | _ -> Error "Stringified.of_yojson"

let to_yojson f x =
  `String (Yojson.Safe.to_string (f x))
