open Cmdliner

let version = "1.0.0"

let type_it s =
  try
    Parser.of_file s |> Parser.parse
    |> Type_checker.type_ast ~env:(Type_checker.Env.empty Runtime.default)
    |> Compiler.compile ~runtime:Runtime.default
    |> print_string |> print_newline
  with
  | Parser.Errors.LexingError s ->
      Format.printf "Lexing Error.\n%s\n" s;
      exit 1
  | Parser.Errors.ParsingError s ->
      Format.printf "Parsing Error.\n%s\n" s;
      exit 1
  | Type_checker.Errors.TypeError s ->
      Format.printf "Type Error.\n%s\n" s;
      exit 1
  | Type_checker.Errors.UnboundVariableError s ->
      Format.printf "Unbound Variable Error.\n%s\n" s;
      exit 1

let filename =
  let doc = "Luna file name" in
  Arg.(
    value & pos 1 string "LunaLang" & info ~docs:doc ~doc ~docv:"filename" [])

let luna_type_checker =
  Cmd.info ~doc:"Type check the program (verify if it is correct)" ~version
    ~docs:"Type check the program (verify if it is correct)" "typecheck"

let input = Term.(const type_it $ filename)
let cmd = Cmd.v luna_type_checker input
let () = exit (Cmd.eval cmd)
