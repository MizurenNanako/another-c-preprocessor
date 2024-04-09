module PPType = struct
  type pp_token =
    | Whitespace
    | Newline
    | Identifier of string
    | PPnum of string
    | PPstr of char * string (* delim, ctx *)
    | LPAREN (* ( *)
    | RPAREN (* ) *)
    | STRINGFY (* # *)
    | CONCAT (* ## *)
    | COMMA (* , *)
    | Punctuator of string
    | Eof
    | Other of string

  let token_ctx tok =
    match tok with
    | Whitespace -> "<ws>"
    | Newline -> "<br>"
    | PPstr (c, x) -> Printf.sprintf "%c%s%c" c x c
    | LPAREN -> "("
    | RPAREN -> ")"
    | STRINGFY -> "<stringfier>"
    | CONCAT -> "<concat>"
    | COMMA -> ","
    | Identifier x -> Printf.sprintf "<id:\"%s\">" x
    | PPnum x -> Printf.sprintf "<num:\"%s\">" x
    | Punctuator x -> Printf.sprintf "<op:\"%s\">" x
    | Other x -> x
    | Eof -> "<eof>"
end
