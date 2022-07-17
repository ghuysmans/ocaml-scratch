type t = {
  block_id: Id.t [@key "blockId"];
  x: int;
  y: int;
  width: int;
  height: int;
  minimized: bool;
  text: string;
} [@@deriving yojson]
