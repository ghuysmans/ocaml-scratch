type target = {
  is_stage: bool [@key "isStage"];
  name: string;
  variables: Variable.t Map.t;
  lists: Lst.t Map.t;
  broadcasts: string Map.t;
  blocks: Block.t Map.t;
  comments: Comment.t Map.t;
  current_costume: int [@key "currentCostume"];
  costumes: Asset.Costume.t list;
  sounds: Asset.Sound.t list;
  layer_order: int [@key "layerOrder"];
  volume: int; (** between 0 and 100 *)
} [@@deriving yojson {meta=true}]

type video_state =
  | On [@name "on"]
  | Off [@name "off"]
  | On_flipped [@name "on-flipped"]
  [@@deriving yojson]

type stage = {
  tempo: int; (** in BPM *)
  video_state: video_state Tag.t [@key "videoState"];
  video_transparency: int [@key "videoTransparency"];
  tts_language: string option [@key "textToSpeechLanguage"];
} [@@deriving yojson]

type rotation_style =
  | All_around [@name "all around"]
  | Left_right [@name "left-right"]
  | Don't_rotate [@name "don't rotate"]
  [@@deriving yojson]

type sprite = {
  visible: bool;
  x: float;
  y: float;
  size: float; (** in percent *)
  direction: float;
  draggable: bool;
  rotation_style: rotation_style Tag.t [@key "rotationStyle"];
} [@@deriving yojson]

type t =
  | Sprite of target * sprite
  | Stage of target * stage

let of_yojson = function
  | `Assoc l ->
    let t, s = List.(partition (fun (x, _) -> mem x Yojson_meta_target.keys) l) in
    Result.bind (target_of_yojson (`Assoc t)) (fun t ->
      if t.is_stage then
        Result.map (fun s -> Stage (t, s)) (stage_of_yojson (`Assoc s))
      else
        Result.map (fun s -> Sprite (t, s)) (sprite_of_yojson (`Assoc s))
    )
  | _ -> Error "Target.of_yojson"

let to_yojson x =
  let combine = Yojson.Safe.Util.combine in
  match x with
  | Sprite (a, s) -> combine (target_to_yojson a) (sprite_to_yojson s)
  | Stage (a, s) -> combine (target_to_yojson a) (stage_to_yojson s)
