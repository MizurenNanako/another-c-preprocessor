module PPToken = struct
  type pp_token =
    | Whitespace of int (* len *)
    | Newline
    | Identifier of string
    | PPnum of string
    | PPstr of char * string (* delim, ctx *)
    | LPAREN (* ( *)
    | RPAREN (* ) *)
    | COMMA (* , *)
    | Punctuator of string
    | Eof
    | Other of string

  let token_ctx tok =
    match tok with
    | Whitespace n -> Printf.sprintf "<ws:%i>" n
    | Newline -> "<br>"
    | PPstr (c, x) -> Printf.sprintf "<str(%c):\"%s\">" c x
    | LPAREN -> "<L>"
    | RPAREN -> "<R>"
    | COMMA -> "<comma>"
    | Identifier x -> Printf.sprintf "<id:\"%s\">" x
    | PPnum x -> Printf.sprintf "<num:\"%s\">" x
    | Punctuator x -> Printf.sprintf "<op:\"%s\">" x
    | Other x -> Printf.sprintf "<other:\"%s\">" x
    | Eof -> "<eof>"

  let token_repr tok =
    match tok with
    | Whitespace n -> String.make n ' '
    | Newline -> "\n"
    | PPstr (c, x) -> Printf.sprintf "%c%s%c" c x c
    | LPAREN -> "("
    | RPAREN -> ")"
    | COMMA -> ","
    | Identifier x | PPnum x | Punctuator x | Other x -> x
    | Eof -> "<eof>"
end
