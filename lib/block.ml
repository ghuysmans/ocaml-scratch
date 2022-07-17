type field = Yojson.Safe.t [@@deriving yojson] (* FIXME *)

type bot = |
let bot_of_yojson _ =
  Error "non-empty list"
let bot_to_yojson : bot -> Yojson.Safe.t = function
  | _ -> .

type mutation = {
  tag_name: string [@key "tagName"];
  children: bot list; (** "Seems to always be an empty array" *)
  proc_code: string [@default ""] [@key "proccode"];
  argument_ids: Id.t list Stringified.t [@default []] [@key "argumentids"];
  argument_names: string list Stringified.t [@default []] [@key "argumentnames"];
  argument_defaults: Value.t list Stringified.t [@default []] [@key "argumentdefaults"];
  no_refresh: Sbool.t [@default false] [@key "warp"];
  has_next: Sbool.t Def.t [@default None] [@key "hasnext"];
} [@@deriving yojson]

type opcode =
  | X
  [@@deriving yojson]

type pos = {
  x: int;
  y: int;
}

type drawn = {
  name: string;
  id: Id.t;
  pos: pos option;
}

let drawn_of_yojson = function
  | `List [`Int _; `String name; `String id] ->
    Ok {name; id; pos = None}
  | `List [`Int _; `String name; `String id; `Int x; `Int y] ->
    Ok {name; id; pos = Some {x; y}}
  | _ -> Error "Block.drawn_of_yojson"

let drawn_to_yojson n {name; id; pos} =
  match pos with
  | None -> `List [`Int n; `String name; `String id]
  | Some {x; y} -> `List [`Int n; `String name; `String id; `Int x; `Int y]

type simple =
  | Number of float
  | Positive_number of float
  | Positive_integer of int
  | Integer of int
  | Angle of float
  | Color of int
  | String of string
  | Broadcast of {name: string; id: Id.t}
  | Variable of drawn
  | List of drawn

let simple_of_yojson = function
  | `List [`Int 4; `String ""] -> Ok (Number 0.)
  | `List [`Int 4; `String s] -> Ok (Number (float_of_string s))
  | `List [`Int 5; `String ""] -> Ok (Positive_number 0.)
  | `List [`Int 5; `String s] -> Ok (Positive_number (float_of_string s))
  | `List [`Int 6; `String ""] -> Ok (Positive_integer 0)
  | `List [`Int 6; `String s] -> Ok (Positive_integer (int_of_string s))
  | `List [`Int 7; `String ""] -> Ok (Integer 0)
  | `List [`Int 7; `String s] -> Ok (Integer (int_of_string s))
  | `List [`Int 8; `String ""] -> Ok (Angle 0.)
  | `List [`Int 8; `String s] -> Ok (Angle (float_of_string s))
  | `List [`Int 9; `String h] -> Ok (Scanf.sscanf h "#%x" (fun x -> Color x))
  | `List [`Int 10; `String s] -> Ok (String s)
  | `List [`Int 11; `String name; `String id] -> Ok (Broadcast {name; id})
  | `List (`Int 12 :: _) as j ->
    Result.map (fun x -> Variable x) (drawn_of_yojson j)
  | `List (`Int 13 :: _) as j ->
    Result.map (fun x -> List x) (drawn_of_yojson j)
  | _ -> Error "Block.of_yojson"

let float_to_json_string f =
  (* FIXME *)
  `String (Yojson.Safe.to_string (`Float f))

let simple_to_yojson = function
  | Number f -> `List [`Int 4; float_to_json_string f]
  | Positive_number f -> `List [`Int 5; float_to_json_string f]
  | Positive_integer i -> `List [`Int 6; `String (string_of_int i)]
  | Integer i -> `List [`Int 7; `String (string_of_int i)]
  | Angle f -> `List [`Int 8; float_to_json_string f]
  | Color c -> `List [`Int 9; `String (Printf.sprintf "#%06x" c)]
  | String s -> `List [`Int 10; `String s]
  | Broadcast {name; id} -> `List [`Int 11; `String name; `String id]
  | Variable d -> drawn_to_yojson 12 d
  | List d -> drawn_to_yojson 13 d

type id_or_simple = [
  | `Id of Id.t
  | `Simple of simple
]

let id_or_simple_of_yojson = function
  | `String s -> Ok (`Id s)
  | j -> Result.map (fun x -> `Simple x) (simple_of_yojson j)

let id_or_simple_to_yojson = function
  | `Id s -> `String s
  | `Simple x -> simple_to_yojson x

type input =
  | Shadow of {front: id_or_simple option; back: id_or_simple}
  | No_shadow of id_or_simple

let active = function
  | Shadow {front = Some x; _} -> x
  | Shadow {front = None; back} -> back
  | No_shadow x -> x

let input_of_yojson = function
  | `List [`Int 1; j] ->
    Result.map (fun back -> Shadow {front = None; back}) (id_or_simple_of_yojson j)
  | `List [`Int 2; j] ->
    Result.map (fun x -> No_shadow x) (id_or_simple_of_yojson j)
  | `List [`Int 3; f; b] ->
    Result.bind (id_or_simple_of_yojson f) (fun f ->
      Result.map (fun back -> Shadow {front = Some f; back}) (id_or_simple_of_yojson b)
    )
  | _ -> Error "Block.input_of_yojson"

let input_to_yojson = function
  | Shadow {front = None; back} ->
    `List [`Int 1; id_or_simple_to_yojson back]
  | No_shadow x ->
    `List [`Int 2; id_or_simple_to_yojson x]
  | Shadow {front = Some f; back} ->
    `List [`Int 3; id_or_simple_to_yojson f; id_or_simple_to_yojson back]

type full = {
  opcode: string; (* FIXME opcode tag *)
  next: Id.t option;
  parent: Id.t option;
  inputs: input Assoc.t;
  fields: field Assoc.t;
  shadow: bool;
  top_level: bool [@key "topLevel"];
  comment: Id.t Def.t [@default None];
  mutation: mutation Def.t [@default None];
  x: int Def.t [@default None]; (** when [top_level] is true *)
  y: int Def.t [@default None]; (** when [top_level] is true *)
} [@@deriving yojson]

type t = [
  | `Full of full
  | `Simple of simple
]

let of_yojson = function
  | `Assoc _ as j -> Result.map (fun x -> `Full x) (full_of_yojson j)
  | j -> Result.map (fun x -> `Simple x) (simple_of_yojson j)

let to_yojson = function
  | `Full x -> full_to_yojson x
  | `Simple x -> simple_to_yojson x
