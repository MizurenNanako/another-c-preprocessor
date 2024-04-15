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
let newline = '\n' | ("//" [^ '\n']* '\n')

rule pp_token = parse
| hash "include" ws* { cmd_include lexbuf }
| hash "define" ws+  { cmd_define lexbuf }
| hash "line" ws+    { cmd_line lexbuf }
| hash "undef" ws+ (identifier as id) ws* (newline | eof)
                     { new_line lexbuf; Cmd_Undef id }
| hash "ifdef" ws+ (identifier as id) ws* (newline | eof)
                     { new_line lexbuf; Cmd_Ifdef id }
| hash "ifndef" ws+ (identifier as id) ws* (newline | eof)
                     { new_line lexbuf; Cmd_Ifndef id }
| hash "else" ws* (newline | eof)
                     { new_line lexbuf; Cmd_Else }
| hash "endif" ws* (newline | eof)
                     { new_line lexbuf; Cmd_Endif }
| identifier         { Identifier (lexeme lexbuf) }
| ppnumber           { PPnum (lexeme lexbuf) }
| ('\"' | '\'') as d { pp_string d (Buffer.create 17) lexbuf }
| '('                { LPAREN }
| ')'                { RPAREN }
| ','                { COMMA }
| punctuator         { Punctuator (lexeme lexbuf) }
| [' ' '\t']+        { Whitespace (lexbuf.lex_curr_pos - lexbuf.lex_start_pos) }
| newline            { new_line lexbuf; Newline }
| newline? eof       { Eof }
(* | _                  { Other (lexeme lexbuf) } *)
| hash               { _report_unsupport_command lexbuf }
| _                  { raise (SyntaxError (lexeme lexbuf)) }

and _report_unsupport_command = parse
| [^ ' ' '\n']*
    { raise (SyntaxError ("Unsupport: " ^ lexeme lexbuf)) }
| _ { raise (SyntaxError ("Unsupport: " ^ lexeme lexbuf)) }

and cmd_include = parse
| '<' (filename as a) '>' (newline | eof) { new_line lexbuf; Cmd_Include (true, a) }
| '\"' (filename as a) '\"' (newline | eof) { new_line lexbuf; Cmd_Include (false, a) }
| _ { raise (SyntaxError (lexeme lexbuf)) }

and cmd_define = parse
| (identifier as id) ws* (newline | eof)
    { 
        new_line lexbuf;
        Cmd_Define1 id
    }
| (identifier as id) ws+
    {
        new_line lexbuf;
        Cmd_Define2 (id, cmd_define_ctx [] lexbuf)
    }
| (identifier as id) '('
    {
        let param, ctx, variadic = cmd_define_param [] lexbuf in
        Cmd_Define3 (id, param, ctx, variadic)
    }
| _ { raise (SyntaxError (lexeme lexbuf)) }

and cmd_define_param lst = parse
| identifier as e   { cmd_define_param (e :: lst) lexbuf }
| ws? ',' ws?       { cmd_define_param lst lexbuf }
| "..." ws* ")" ws* { (List.rev lst, cmd_define_ctx [] lexbuf, true) }
| ")" ws*           { (List.rev lst, cmd_define_ctx [] lexbuf, false) }
| _ { raise (SyntaxError ("Invaild macro param: " ^ Lexing.lexeme lexbuf)) }

and cmd_define_ctx lst = parse
| identifier         { cmd_define_ctx ((Identifier (lexeme lexbuf))::lst) lexbuf }
| '#' ws* (identifier as e)
                     { cmd_define_ctx ((SIdentifier e)::lst) lexbuf }
| ppnumber           { cmd_define_ctx ((PPnum (lexeme lexbuf))::lst) lexbuf }
| ('\"' | '\'') as d { cmd_define_ctx ((pp_string d (Buffer.create 17) lexbuf)::lst) lexbuf }
| '('                { cmd_define_ctx (LPAREN::lst) lexbuf }
| ')'                { cmd_define_ctx (RPAREN::lst) lexbuf }
| ','                { cmd_define_ctx (COMMA::lst) lexbuf }
| punctuator         { cmd_define_ctx ((Punctuator (lexeme lexbuf))::lst) lexbuf }
| [' ' '\t']+        { cmd_define_ctx lst lexbuf }
(* | [' ' '\t']+        { cmd_define_ctx ((Whitespace (lexbuf.lex_curr_pos - lexbuf.lex_start_pos))::lst) lexbuf } *)
| newline? eof | newline
                     { new_line lexbuf; List.rev lst }
| _                  { raise (SyntaxError (lexeme lexbuf)) }

and cmd_line = parse
| ((['1' - '9'] digit*) as a) ws+ '\"' ( filename as b) '\"' ws* newline
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
| ((['1' - '9'] digit*) as a) ws* newline
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
