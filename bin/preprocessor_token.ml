module PPToken = struct
  exception ConcatenationError of string

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

  let token_concate t1 t2 =
    match (t1, t2) with
    | Identifier a, Identifier b -> Some (Identifier (a ^ b))
    | Identifier a, PPnum b -> Some (Identifier (a ^ b))
    | PPnum a, Identifier b -> Some (PPnum (a ^ b))
    | PPnum a, PPnum b -> Some (PPnum (a ^ b))
    | Punctuator a, Punctuator b -> Some (Punctuator (a ^ b))
    | Punctuator _, _ -> None
    | a, b -> raise (ConcatenationError (token_repr a ^ token_repr b))
end
