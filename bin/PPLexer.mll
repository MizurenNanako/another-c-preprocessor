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
| hash "line" ws+    { cmd_line lexbuf }
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
