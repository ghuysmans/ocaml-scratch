type 'a tag = 'a
let tag_of_yojson f j = f (`List [j])
let tag_to_yojson f x =
  match f x with
  | `List (h :: _) -> h
  | _ -> failwith "tag_to_yojson"

type 'a def = 'a option
let def_of_yojson f j =
  Result.map (fun x -> Some x) (f j)
let def_to_yojson f = function
  | None -> failwith "def_to_yojson: please specify [@default None]"
  | Some x -> f x

let rec sequence = function
  | [] -> Ok []
  | h :: t -> Result.(bind h (fun h -> map (fun t -> h :: t) (sequence t)))

type id = string [@@deriving yojson]

type 'a map = (id * 'a) list
let map_of_yojson f = function
  | `Assoc l ->
    sequence (List.map (fun (k, v) -> Result.map (fun v -> k, v) (f v)) l)
  | _ -> Error "map_of_yojson"
let map_to_yojson f l =
  `Assoc (List.map (fun (k, v) -> k, f v) l)

type 'a assoc = 'a map (* FIXME *)
let assoc_of_yojson = map_of_yojson
let assoc_to_yojson = map_to_yojson

type value =
  | Float of float
  | Int of int
  | String of string
  | List of value list

let rec value_of_yojson = function
  | `Float f -> Ok (Float f)
  | `Int i -> Ok (Int i)
  | `String s -> Ok (String s)
  | `List l -> Result.map (fun l -> List l) (sequence (List.map value_of_yojson l))
  | _ -> Error "value_of_yojson"

let rec value_to_yojson = function
  | Float f -> `Float f
  | Int i -> `Int i
  | String s -> `String s
  | List l -> `List (List.map value_to_yojson l)

type input = Yojson.Safe.t [@@deriving yojson] (* FIXME *)
type field = Yojson.Safe.t [@@deriving yojson] (* FIXME *)

type sbool = bool
let sbool_of_yojson = function
  | `String "true" -> Ok true
  | `String "false" -> Ok false
  | _ -> Error "sbool_of_yojson"
let sbool_to_yojson b = `String (if b then "true" else "false")

type 'a stringified = 'a
let stringified_of_yojson f = function
  | `String s ->
    begin try
      f (Yojson.Safe.from_string s)
    with Yojson.Json_error e ->
      Error ("stringified_of_yojson: " ^ e)
    end
  | _ -> Error "stringified_of_yojson"
let stringified_to_yojson f x =
  `String (Yojson.Safe.to_string (f x))

type bot = |
let bot_of_yojson _ =
  Error "non-empty list"
let bot_to_yojson : bot -> Yojson.Safe.t = function
  | _ -> .

type mutation = {
  tag_name: string [@key "tagName"];
  children: bot list; (** "Seems to always be an empty array" *)
  proc_code: string [@default ""] [@key "proccode"];
  argument_ids: id list stringified [@default []] [@key "argumentids"];
  argument_names: string list stringified [@default []] [@key "argumentnames"];
  argument_defaults: value list stringified [@default []] [@key "argumentdefaults"];
  no_refresh: sbool [@default false] [@key "warp"];
  has_next: sbool def [@default None] [@key "hasnext"];
} [@@deriving yojson]

type opcode =
  | X
  [@@deriving yojson]

type full_block = {
  opcode: string; (* FIXME opcode tag *)
  next: id option;
  parent: id option;
  inputs: input assoc;
  fields: field assoc;
  shadow: bool;
  top_level: bool [@key "topLevel"];
  comment: id def [@default None];
  mutation: mutation def [@default None];
  x: int def [@default None]; (** when [top_level] is true *)
  y: int def [@default None]; (** when [top_level] is true *)
} [@@deriving yojson]

type drawn_block = {
  name: string;
  id: id;
  x: int;
  y: int;
}

type block =
  | Full of full_block
  | Number of float
  | Positive_number of float
  | Positive_integer of int
  | Integer of int
  | Angle of float
  | Color of int
  | String of string
  | Broadcast of {name: string; id: id}
  | Variable of drawn_block
  | List of drawn_block

let block_of_yojson = function
  | `Assoc _ as j -> Result.map (fun f -> Full f) (full_block_of_yojson j)
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
  | `List [`Int 12; `String name; `String id; `Int x; `Int y] -> Ok (Variable {name; id; x; y})
  | `List [`Int 13; `String name; `String id; `Int x; `Int y] -> Ok (List {name; id; x; y})
  | _ -> Error "block_of_yojson"

let block_to_yojson = function
  | Full f -> full_block_to_yojson f
  | Number f -> `List [`Int 4; `Float f]
  | Positive_number f -> `List [`Int 5; `Float f]
  | Positive_integer i -> `List [`Int 6; `Int i]
  | Integer i -> `List [`Int 7; `Int i]
  | Angle f -> `List [`Int 8; `Float f]
  | Color c -> `List [`Int 9; `String (Printf.sprintf "#%06x" c)]
  | String s -> `List [`Int 10; `String s]
  | Broadcast {name; id} -> `List [`Int 11; `String name; `String id]
  | Variable {name; id; x; y} -> `List [`Int 12; `String name; `String id; `Int x; `Int y]
  | List {name; id; x; y} -> `List [`Int 13; `String name; `String id; `Int x; `Int y]

type variable = {
  name: string;
  value: value;
  cloud: bool;
}

let variable_of_yojson = function
  | `List [`String name; v] ->
    Result.map (fun value -> {name; value; cloud = false}) (value_of_yojson v)
  | `List [`String name; v; `Bool true] ->
    Result.map (fun value -> {name; value; cloud = true}) (value_of_yojson v)
  | _ -> Error "variable_of_yojson"

let variable_to_yojson {name; value; cloud} =
  `List (
    `String name ::
    value_to_yojson value :: 
    if cloud then [`Bool true] else []
  )

type lst = {
  name: string;
  value: value list;
}

let lst_of_yojson = function
  | `List [`String name; `List l] ->
    List.map value_of_yojson l |>
    sequence |>
    Result.map (fun value -> {name; value}) 
  | _ -> Error "lst_of_yojson"

let lst_to_yojson {name; value} =
  `List [`String name; `List (List.map value_to_yojson value)]

type comment = {
  block_id: id [@key "blockId"];
  x: int;
  y: int;
  width: int;
  height: int;
  minimized: bool;
  text: string;
} [@@deriving yojson]

module MD5 = struct
  type t = Digest.t
  let of_yojson j = Result.map Digest.from_hex ([%of_yojson: string] j)
  let to_yojson x = Digest.to_hex x |> [%to_yojson: string]
end

type asset = {
  asset_id: MD5.t [@key "assetId"];
  name: string;
  filename: string [@key "md5ext"];
  data_format: string [@key "dataFormat"];
} [@@deriving yojson {meta=true}]

module Costume = struct
  type s = {
    resolution: int [@default 2] [@key "bitmapResolution"];
    cx: float [@key "rotationCenterX"];
    cy: float [@key "rotationCenterY"];
  } [@@deriving yojson]

  type t = asset * s

  let of_yojson = function
    | `Assoc l ->
      let a, s = List.(partition (fun (x, _) -> mem x Yojson_meta_asset.keys) l) in
      Result.bind (asset_of_yojson (`Assoc a)) (fun a ->
        Result.map (fun s -> a, s) (s_of_yojson (`Assoc s))
      )
    | _ -> Error "Costume.of_yojson"

  let to_yojson (a, s) =
    Yojson.Safe.Util.combine (asset_to_yojson a) (s_to_yojson s)
end

module Sound = struct
  type s = {
    rate: int; (** in Hz *)
    sample_count: int [@key "sampleCount"];
    format: string [@default ""];
  } [@@deriving yojson]

  type t = asset * s

  let of_yojson = function
    | `Assoc l ->
      let a, s = List.(partition (fun (x, _) -> mem x Yojson_meta_asset.keys) l) in
      Result.bind (asset_of_yojson (`Assoc a)) (fun a ->
        Result.map (fun s -> a, s) (s_of_yojson (`Assoc s))
      )
    | _ -> Error "Sound.of_yojson"

  let to_yojson (a, s) =
    Yojson.Safe.Util.combine (asset_to_yojson a) (s_to_yojson s)
end

type target = {
  is_stage: bool [@key "isStage"];
  name: string;
  variables: variable map;
  lists: lst map;
  broadcasts: string map;
  blocks: block map;
  comments: comment map;
  current_costume: int [@key "currentCostume"];
  costumes: Costume.t list;
  sounds: Sound.t list;
  layer_order: int [@key "layerOrder"];
  volume: int; (** between 0 and 100 *)
} [@@deriving yojson {strict=false (* FIXME? *)}]

type video_state =
  | On [@name "on"]
  | Off [@name "off"]
  | On_flipped [@name "on-flipped"]
  [@@deriving yojson]

type stage = {
  tempo: int; (** in BPM *)
  video_state: video_state tag [@key "videoState"];
  video_transparency: int [@key "videoTransparency"];
  tts_language: string [@key "textToSpeechLanguage"];
} [@@deriving yojson {strict=false (* FIXME target *)}]

type rotation_style =
  | All_around [@name "all around"]
  | Left_right [@name "left-right"]
  | Don't_rotate [@name "don't rotate"]
  [@@deriving yojson]

type sprite = {
  visible: bool;
  x: int;
  y: int;
  size: int; (** in percent *)
  direction: float;
  draggable: bool;
  rotation_style: rotation_style tag [@key "rotationStyle"];
} [@@deriving yojson {strict=false (* FIXME target *)}]

type mode =
  | Default [@name "default"]
  | Large [@name "large"]
  | Slider [@name "slider"]
  | List [@name "list"]
  [@@deriving yojson]

type monitor = {
  id: id;
  mode: mode tag;
  opcode: string; (* FIXME opcode tag *)
  params: value assoc;
  sprite_name: string option [@key "spriteName"];
  value: value;
  width: int;
  height: int;
  x: int;
  y: int;
  visible: bool;
  slider_min: int def [@default None] [@key "sliderMin"];
  slider_max: int def [@default None] [@key "sliderMax"];
  is_discrete: bool def [@default None] [@key "isDiscrete"];
} [@@deriving yojson]

type extension =
  | Pen [@name "pen"]
  | Wedo2 [@name "wedo2"]
  | Music [@name "music"]
  | Microbit [@name "microbit"]
  | Text2speech [@name "text2speech"]
  | Translate [@name "translate"]
  | Video_sensing [@name "videoSensing"]
  | EV3 [@name "ev3"]
  | Makey_makey [@name "makeymakey"]
  | Boost [@name "boost"]
  | Gdx_for [@name "gdxfor"]
  [@@deriving yojson]

type meta = {
  semver: string;
  creator_vm: string [@key "vm"];
  last_user_agent: string [@key "agent"];
} [@@deriving yojson]

type t = {
  targets: target list;
  monitors: monitor list;
  extensions: extension tag list;
  meta: meta;
} [@@deriving yojson]
