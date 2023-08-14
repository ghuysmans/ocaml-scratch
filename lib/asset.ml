type asset = {
  asset_id: Md5.t [@key "assetId"];
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

  let make ?(cx=0.) ?(cy=0.) ~name ~data_format asset_id =
    let filename = Digest.to_hex asset_id ^ "." ^ data_format in
    {asset_id; name; filename; data_format}, {resolution = 2; cx; cy}

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
