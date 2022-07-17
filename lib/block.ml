type input = Yojson.Safe.t [@@deriving yojson] (* FIXME *)
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

type t =
  | Full of full
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

let of_yojson = function
  | `Assoc _ as j -> Result.map (fun f -> Full f) (full_of_yojson j)
  | `List [`Int 4; `Int i] -> Ok (Number (float_of_int i))
  | `List [`Int 4; `Float f] -> Ok (Number f)
  | `List [`Int 5; `Int i] -> Ok (Positive_number (float_of_int i))
  | `List [`Int 6; `Int i] -> Ok (Positive_integer i)
  | `List [`Int 7; `Int i] -> Ok (Integer i)
  | `List [`Int 8; `Int i] -> Ok (Angle (float_of_int i))
  | `List [`Int 8; `Float f] -> Ok (Angle f)
  | `List [`Int 9; `String h] -> Ok (Scanf.sscanf h "#%x" (fun x -> Color x))
  | `List [`Int 10; `String s] -> Ok (String s)
  | `List [`Int 11; `String name; `String id] -> Ok (Broadcast {name; id})
  | `List (`Int 12 :: _) as j ->
    Result.map (fun x -> Variable x) (drawn_of_yojson j)
  | `List (`Int 13 :: _) as j ->
    Result.map (fun x -> List x) (drawn_of_yojson j)
  | _ -> Error "Block.of_yojson"

let to_yojson = function
  | Full f -> full_to_yojson f
  | Number f -> `List [`Int 4; `Float f]
  | Positive_number f -> `List [`Int 5; `Float f]
  | Positive_integer i -> `List [`Int 6; `Int i]
  | Integer i -> `List [`Int 7; `Int i]
  | Angle f -> `List [`Int 8; `Float f]
  | Color c -> `List [`Int 9; `String (Printf.sprintf "#%06x" c)]
  | String s -> `List [`Int 10; `String s]
  | Broadcast {name; id} -> `List [`Int 11; `String name; `String id]
  | Variable d -> drawn_to_yojson 12 d
  | List d -> drawn_to_yojson 13 d
