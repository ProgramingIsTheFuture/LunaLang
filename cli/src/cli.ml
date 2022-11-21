let () =
  try Lex.parse ~fname:"code.dyri" () |> Typing.check_types |> ignore with
  | Lex.Error.InvalidKwd s -> Format.eprintf "%s\n" s
  | Lex.Error.InvalidSyntax s -> Format.eprintf "%s\n" s
  | Lex.Error.InvalidFname s -> Format.eprintf "%s\n" s
  | Typing.Error.InvalidType (_pos, s) -> Format.eprintf "%s\n" s
