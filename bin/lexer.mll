{
    open Lexing
    open Preprocessor_token.PPToken
    exception SyntaxError of string
}

(* 
    pp_token will parse from a processed buffer that
    has replaced comments with same length of whitespaces
    and replaced backslash-newline with 1 newline after next line.

    So it will not handle backslash-newline and comments.
*)

let ws    = [' ' '\t']
let digit = ['0'-'9']
let hex   = digit | ['A' - 'F' 'a' - 'f']
let alpha =   (['A' - 'Z' 'a' - 'z' '$'])
            | "\\u" ((hex hex hex hex))
            | "\\U" ((hex hex hex hex hex hex hex hex))
let alnum = alpha | digit
let exponents = ['e' 'E' 'p' 'P'] ['+' '-']
let ppnumbody = ['_' '.'] | alnum | exponents
let ppnumber = '.'? digit ppnumbody*

let identifier = (alpha | '_') (alnum | '_')*
let punctuator = ['!' '#' '%' '&' '*'
                  '+' '-' '.' '/' ':'
                  ';' '<' '=' '>' '?'
                  '[' '\\' ']' '^' '{' 
                  '|' '}' '~']+

let _filename = [^ '!' '#' '%' '&' '*' '+' ':' ';' '<' '\''
                   '=' '>' '?' '[' ']' '^' '{' '|' '}' '\"']
let filename = _filename+
let hash = ws* '#' ws*

rule pp_token = parse
| hash "include" ws* { cmd_include lexbuf }
| hash "define" ws+  { cmd_define lexbuf }
| hash "line" ws+    { cmd_line lexbuf }
| identifier         { Identifier (lexeme lexbuf) }
| ppnumber           { PPnum (lexeme lexbuf) }
| ('\"' | '\'') as d { pp_string d (Buffer.create 17) lexbuf }
| '('                { LPAREN }
| ')'                { RPAREN }
| ','                { COMMA }
| punctuator         { Punctuator (lexeme lexbuf) }
| [' ' '\t']+        { Whitespace (lexbuf.lex_curr_pos - lexbuf.lex_start_pos) }
| '\n'               { new_line lexbuf; Newline }
| '\n'? eof          { Eof }
(* | _                  { Other (lexeme lexbuf) } *)
| _                  { raise (SyntaxError (lexeme lexbuf)) }

and cmd_include = parse
| '<' (filename as a) '>' '\n' { new_line lexbuf; Cmd_Include (true, a) }
| '\"' (filename as a) '\"' '\n' { new_line lexbuf; Cmd_Include (false, a) }
| _ { raise (SyntaxError (lexeme lexbuf)) }

and cmd_define = parse
| (identifier as id) ws+ '\n'
    { 
        new_line lexbuf;
        Cmd_Define1 id
    }
| (identifier as id) ws+ ([^ '\n']+ as ctx) '\n'
    {
        new_line lexbuf;
        Cmd_Define2 (id, ctx)
    }
| (identifier as id) '('
    {
        let param, ctx = cmd_define_param [] lexbuf in
        Cmd_Define3 (id, param, ctx)
    }
| _ { raise (SyntaxError (lexeme lexbuf)) }

and cmd_define_param lst = parse
| identifier as e            { cmd_define_param (e :: lst) lexbuf }
| ws? ',' ws?                { cmd_define_param lst lexbuf }
| ")" ws* ([^ '\n']+ as ctx) { new_line lexbuf; (List.rev lst, ctx) }
| _ { raise (SyntaxError ("Invaild macro param: " ^ Lexing.lexeme lexbuf)) }

and cmd_line = parse
| ((['1' - '9'] digit*) as a) ws+ '\"' ( filename as b) '\"' ws* '\n'
         {
            let linenum = int_of_string a in
            let lcp = lexbuf.lex_curr_p in
            lexbuf.lex_curr_p <-
                {
                    lcp with
                    pos_lnum = linenum;
                    pos_bol = lcp.pos_cnum;
                    pos_fname = b;
                };
            Cmd_Line2 (linenum, b)
         }
| ((['1' - '9'] digit*) as a) ws* '\n'
         {
            let linenum = int_of_string a in
            let lcp = lexbuf.lex_curr_p in
            lexbuf.lex_curr_p <-
                {
                    lcp with
                    pos_lnum = linenum;
                    pos_bol = lcp.pos_cnum;
                };
            Cmd_Line1 linenum
         }
| _      { raise (SyntaxError (lexeme lexbuf)) }

and pp_string d buf = parse
| ('\"' | '\'') as dd
         { 
            if dd = d then (PPstr (dd, Buffer.contents buf))
            else (Buffer.add_char buf dd; pp_string d buf lexbuf)
         }
| eof    { raise (SyntaxError "unterminated string") }
| _ as x { Buffer.add_char buf x; pp_string d buf lexbuf }
