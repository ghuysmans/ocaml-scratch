open Scratch

let () =
  match Yojson.Safe.from_channel stdin |> Project.of_yojson with
  | Error e -> prerr_endline e
  | Ok {targets; meta; _} ->
    Printf.printf "Version: %s\nUser-Agent: %s\n"
      meta.creator_vm meta.last_user_agent;
    List.iter (fun t ->
      let basic =
        match t with
        | Target.Sprite (t, s) ->
          Printf.printf "Sprite: %s%s @ (%f,%f)\n"
            t.name (if s.visible then "" else "*")
            s.x s.y;
          t
        | Stage (t, _) ->
          Printf.printf "Stage: %s\n" t.name;
          t
      in
      List.iter (fun (_, x) ->
        match x with
        | Block.Full f -> Printf.printf "* %s\n" f.opcode
        | _ -> ()
      ) basic.blocks
    ) targets
