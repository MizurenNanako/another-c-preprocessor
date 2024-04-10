{
    open Lexing
    open Preprocessor_token.PPToken
    exception SyntaxError of string
}

(* This file will contain a few lexers *)
(* 1. The Phase 1 and Phase 2 lexer *)

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

rule pp_token = parse
| '#'                { macro_exec lexbuf; Newline }
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

and macro_exec = parse
(* | ([^ '\n']* as cmd) '\n' { print_endline cmd } *)
| "include"          { cmd_include  lexbuf }
| "define"           { cmd_define   lexbuf }
| "line"             { cmd_line     lexbuf }

and cmd_include = parse
| "\n"               {  }
| _                  { cmd_include lexbuf }

and cmd_define = parse
| "\n"               {  }
| _                  { cmd_define lexbuf }

and cmd_line = parse
| "\n"               {  }
| _                  { cmd_line lexbuf }

and pp_string d buf = parse
| ('\"' | '\'') as dd
         { 
            if dd = d then (PPstr (dd, Buffer.contents buf))
            else (Buffer.add_char buf dd; pp_string d buf lexbuf)
         }
(* | "\\t"  { Buffer.add_char buf '\t'; pp_string d buf lexbuf } *)
(* | "\\\\" { Buffer.add_char buf '\\'; pp_string d buf lexbuf } *)
(* | "\\n"  { Buffer.add_char buf '\n'; pp_string d buf lexbuf } *)
(* | "\\\'" { Buffer.add_char buf '\''; pp_string d buf lexbuf } *)
(* | "\\\"" { Buffer.add_char buf '\"'; pp_string d buf lexbuf } *)
(* | "\\b"  { Buffer.add_char buf '\b'; pp_string d buf lexbuf } *)
| eof    { raise (SyntaxError "unterminated string") }
| _ as x { Buffer.add_char buf x; pp_string d buf lexbuf }
