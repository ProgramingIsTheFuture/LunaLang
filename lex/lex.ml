
let parse f =
  let buf = Lexing.from_channel f in
  Parser.code Lexer.token buf;;
