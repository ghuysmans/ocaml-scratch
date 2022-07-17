let () =
  match Yojson.Safe.from_channel stdin |> Scratch.Project.of_yojson with
  | Error e -> prerr_endline e
  | Ok _ -> print_endline "ok"
