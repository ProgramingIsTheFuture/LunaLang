open Cmdliner

let outf =
  let doc = "Input the file name" in
  Arg.(value & opt string "a.s" & info [ "o" ] ~docv:"files" ~doc)

let file =
  let doc = "Input the file name" in
  Arg.(value & pos 0 string "main.moon" & info [] ~docv:"files" ~doc)

let check_cmd =
  let doc = "Just use the typechecker on the program" in
  Arg.(value & flag & info [ "check" ] ~docv:"bool" ~doc)

let luna check file outf =
  if check then Parse.parse ~fname:file () |> Typecheck.check_types |> ignore
  else
    let code =
      Parse.parse ~fname:file () |> Typecheck.check_types |> Gen_llvm.compile
    in
    let out = Format.formatter_of_out_channel (open_out outf) in
    Format.fprintf out "%s\n" code

let luna_t = Term.(const luna $ check_cmd $ file $ outf)

let cmd =
  let info = Cmd.info "Luna Lang" in
  Cmd.v info luna_t

let () = exit (Cmd.eval cmd)
