type t = string [@@deriving yojson]

let gen ~prefix =
  let n = ref 0 in
  fun () ->
    let ret = prefix ^ string_of_int !n in
    incr n;
    ret
