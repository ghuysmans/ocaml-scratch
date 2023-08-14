open Scratch
open Js_of_ocaml
open Dom_html

let _ =
  Dom_events.listen document Event.domContentLoaded (fun _ _ ->
    let module C = CoerceTo in
    let ta name = getElementById_coerce name C.textarea in
    match getElementById_coerce "f" C.form, ta "json", ta "msg", ta "out" with
    | None, _, _, _ | _, None, _, _ | _, _, None, _ | _, _, _, None ->
      failwith "missing f, json, or msg"
    | Some f, Some json, Some msg, Some out ->
      f##.onsubmit := Dom.handler (fun _ ->
        begin match
          Js.to_string json##.value |>
          Yojson.Safe.from_string |>
          Project.of_yojson
        with
        | Error e -> prerr_endline e
        | Ok project ->
          let blocks =
            Js.to_string msg##.value |>
            String.split_on_char '\n' |>
            List.map (fun x -> Block.say ~secs:1. (`Simple (String x))) |>
            Block.chain (Id.gen ~prefix:"b") |>
            Block.top_level ~x:0 ~y:0
          in
          let f = function
            | Target.Stage (t, s) ->
              Target.Stage ({t with blocks = blocks @ t.blocks}, s)
            | x -> x
          in
          out##.value :=
            {project with targets = List.map f project.targets} |>
            Project.to_yojson |>
            Yojson.Safe.to_string |>
            Js.string
        end;
        Js._false
      );
      false
  )
