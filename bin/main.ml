open Preprocessor_token
open Preprocessor_context

let fetch_filename is_sys name =
  ignore is_sys;
  name (* todo: add resource dict *)

let _print_lnum (lcp : Lexing.position) out_ch =
  Printf.fprintf out_ch "%-12s:%4i:" lcp.pos_fname lcp.pos_lnum

(* let _print_lnum (lcp : Lexing.position) out_ch =
   ignore lcp;
   ignore out_ch *)

let mark_line (lcp : Lexing.position) out_ch =
  Printf.fprintf out_ch "#line %i \"%s\"\n" lcp.pos_lnum lcp.pos_fname

let run (context : PPCtx.t) =
  let rec _run fname out_ch =
    let in_ch = In_channel.open_text fname in
    let lexbuf = Lexing.from_channel in_ch in
    Lexing.set_filename lexbuf fname;
    mark_line lexbuf.Lexing.lex_curr_p out_ch;
    _print_lnum lexbuf.Lexing.lex_curr_p out_ch;

    let rec loop (t : PPToken.pp_token) =
      let _common c =
        output_char out_ch '\n';
        c ();
        _print_lnum lexbuf.Lexing.lex_curr_p out_ch;
        loop (Lexer.pp_token lexbuf)
      in
      match t with
      | Eof -> output_string out_ch "\n" (* keep newline at end *)
      | Newline -> _common (fun () -> ())
      | Cmd_Line1 n -> _common (fun () -> Printf.fprintf out_ch "#line %i\n" n)
      | Cmd_Line2 (n, m) ->
          _common (fun () -> Printf.fprintf out_ch "#line %i \"%s\"\n" n m)
      | Cmd_Include (is_sys, file) ->
          _common (fun () ->
              _run (fetch_filename is_sys file) out_ch;
              mark_line lexbuf.Lexing.lex_curr_p out_ch)
      | Cmd_Define1 tok ->
          _common (fun () -> PPCtx.add_symbol context (Identifier tok))
      | _ as x ->
          Printf.fprintf out_ch "%s" (PPToken.token_repr x);
          loop (Lexer.pp_token lexbuf)
    in
    loop (Lexer.pp_token lexbuf)
  in
  _run

let () =
  let out_ch = Out_channel.open_text "output.out" in
  let context = PPCtx.make () in
  try run context Sys.argv.(1) out_ch
  with _ as e ->
    Out_channel.flush out_ch;
    raise e
