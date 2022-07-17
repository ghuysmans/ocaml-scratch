
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
  targets: Target.t list;
  monitors: Monitor.t list;
  extensions: extension Tag.t list;
  meta: meta;
} [@@deriving yojson]
