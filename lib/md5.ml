type t = Digest.t
let of_yojson j = Result.map Digest.from_hex ([%of_yojson: string] j)
let to_yojson x = Digest.to_hex x |> [%to_yojson: string]
