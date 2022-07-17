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
      List.iter (fun (id, fs) ->
        match fs with
        | `Full f ->
          Printf.printf "%s %s" id f.Block.opcode;
          begin match f.inputs with
            | [] -> Printf.printf "\n"
            | l ->
              List.map (fun (k, v) -> k ^ "=" ^ Block.(active v |> to_string)) l |>
              String.concat ", " |>
              Printf.printf " (%s)\n"
          end
        | `Simple s ->
          Printf.printf "* %s\n" (Block.simple_to_string s)
      ) basic.blocks
    ) targets
