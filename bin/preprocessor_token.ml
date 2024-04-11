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
    | Cmd_Line1 of int
    | Cmd_Line2 of int * string
    | Cmd_Include of bool * string
    | Cmd_Define1 of string (* name *)
    | Cmd_Define2 of string * pp_token list (* name, ctx *)
    | Cmd_Define3 of
        string * pp_token list * pp_token list (* name, param, ctx *)

  let token_repr tok =
    match tok with
    | Whitespace n -> String.make n ' '
    | PPstr (c, x) -> Printf.sprintf "%c%s%c" c x c
    | LPAREN -> "("
    | RPAREN -> ")"
    | COMMA -> ","
    | Identifier x | PPnum x | Punctuator x | Other x -> x
    | Eof -> "<eof>"
    | _ -> "\n" (* All instructions and newline *)
end
