type mode =
  | Default [@name "default"]
  | Large [@name "large"]
  | Slider [@name "slider"]
  | List [@name "list"]
  [@@deriving yojson]

type t = {
  id: Id.t;
  mode: mode Tag.t;
  opcode: string; (* FIXME opcode tag *)
  params: Value.t Assoc.t;
  sprite_name: string option [@key "spriteName"];
  value: Value.t;
  width: int;
  height: int;
  x: int;
  y: int;
  visible: bool;
  slider_min: int Def.t [@default None] [@key "sliderMin"];
  slider_max: int Def.t [@default None] [@key "sliderMax"];
  is_discrete: bool Def.t [@default None] [@key "isDiscrete"];
} [@@deriving yojson]
