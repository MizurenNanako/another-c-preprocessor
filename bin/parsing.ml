module Parser = struct
  open Preprocessor_context
  open Preprocessor_token
  open Resource_manager

  let _print_lnum (lcp : Lexing.position) out_ch =
    (* Printf.fprintf out_ch "%-12s:%4i:" lcp.pos_fname lcp.pos_lnum *)
    ignore (lcp, out_ch)

  let _mark_line (lcp : Lexing.position) out_ch =
    Printf.fprintf out_ch "#line %i \"%s\"\n" lcp.pos_lnum lcp.pos_fname

  let _get_params lexbuf =
    let rec _f (level : int) (ptoks : PPToken.pp_token list list) (cnt : int) =
      match Lexer.pp_token lexbuf with
      | LPAREN -> (
          match level with
          | 0 -> (*Start*) _f 1 ptoks 1
          | n ->
              _f
                (n + 1) (*increased paren level*)
                (match ptoks with
                | [] -> (LPAREN :: []) :: []
                | hd :: tl -> (LPAREN :: hd) :: tl)
                cnt)
      | RPAREN -> (
          match level with
          | 1 -> (*End*) (List.rev ptoks, cnt)
          | n ->
              _f
                (n - 1) (*decreased paren level*)
                (match ptoks with
                | [] -> (RPAREN :: []) :: []
                | hd :: tl -> (RPAREN :: hd) :: tl)
                cnt)
      | COMMA -> (
          match level with
          | 1 ->
              (*Sep*)
              _f 1
                (match ptoks with
                | [] -> []
                | [] :: tl -> [] :: [] :: tl
                | hd :: tl -> [] :: List.rev hd :: tl)
                (cnt + 1)
          | n ->
              _f n (*unchanged paren level*)
                (match ptoks with
                | [] -> (COMMA :: []) :: []
                | hd :: tl -> (COMMA :: hd) :: tl)
                cnt)
      | Whitespace _ ->
          _f level
            (match ptoks with [] -> [] :: [] | hd :: tl -> hd :: tl)
            cnt
      | _ as tk ->
          _f level
            (match ptoks with
            | [] -> (tk :: []) :: []
            | hd :: tl -> (tk :: hd) :: tl)
            cnt
    in
    _f 0 [] 0

  let _expand_macro (macro : PPCtx.Macro.t) (out_ch : out_channel)
      (lexbuf : Lexing.lexbuf) =
    match macro.m_param_cnt with
    | 0 -> (*Just a symbol*) output_string out_ch (PPCtx.Macro.repr macro)
    | _ ->
        (* Macro Function, Parse to token lists *)
        let params, count = _get_params lexbuf in
        if count = macro.m_param_cnt then
          let expanded = PPCtx.Macro.expand macro params in
          List.iter
            (fun tk -> output_string out_ch (PPToken.token_repr tk))
            expanded
        else
          raise
            (Lexer.SyntaxError
               (Printf.sprintf "Macro Parameter expected %i but got %i"
                  macro.m_param_cnt count))

  (** Parse with [context] from [filename] to [out_channel] *)
  let run context resource =
    let rec _run filename out_channel =
      let lexbuf = Resource.lexbuf_from_filename resource filename in
      _mark_line lexbuf.Lexing.lex_curr_p out_channel;
      _print_lnum lexbuf.Lexing.lex_curr_p out_channel;

      let rec loop (t : PPToken.pp_token) =
        let _act c =
          output_char out_channel '\n';
          c ();
          _print_lnum lexbuf.Lexing.lex_curr_p out_channel;
          loop (Lexer.pp_token lexbuf)
        in
        let cp = lexbuf.lex_curr_p in
        let mcp = { cp with pos_lnum = cp.pos_lnum - 1 } in
        match t with
        | Eof -> output_string out_channel "\n" (* keep newline at end *)
        | Newline -> _act (fun () -> ())
        | Cmd_Else -> _act (fun () -> PPCtx.state_invert context)
        | Cmd_Endif -> _act (fun () -> PPCtx.state_enable context)
        | _ -> (
            match context.PPCtx.active_state with
            | true -> (
                match t with
                | Cmd_Line1 n ->
                    _act (fun () -> Printf.fprintf out_channel "#line %i\n" n)
                | Cmd_Line2 (n, m) ->
                    _act (fun () ->
                        Printf.fprintf out_channel "#line %i \"%s\"\n" n m)
                | Cmd_Include (is_sys, file) ->
                    _act (fun () ->
                        _run
                          (Resource.fetch_filename resource is_sys file)
                          out_channel;
                        _mark_line cp out_channel)
                | Cmd_Define1 tok ->
                    _act (fun () -> PPCtx.add_symbol context tok mcp)
                | Cmd_Define2 (n, tks) ->
                    _act (fun () -> PPCtx.add_macro_parsed context n tks mcp)
                | Cmd_Define3 (n, p, k) ->
                    _act (fun () -> PPCtx.add_macro context n p k mcp)
                | Cmd_Undef n -> _act (fun () -> PPCtx.remove_symbol context n)
                | Cmd_Ifdef n -> _act (fun () -> PPCtx.state_ifdef context n)
                | Cmd_Ifndef n -> _act (fun () -> PPCtx.state_ifndef context n)
                | Identifier id -> (
                    match
                      PPCtx.SymbolTable.find_opt
                        { PPCtx.Macro.dummy with m_trigger = id }
                        context.sym_table
                    with
                    | None ->
                        output_string out_channel id;
                        loop (Lexer.pp_token lexbuf)
                    | Some macro ->
                        output_string out_channel "\n";
                        _mark_line macro.pos out_channel;
                        output_string out_channel
                          (String.make
                             (lexbuf.lex_start_p.pos_cnum
                            - lexbuf.lex_start_p.pos_bol)
                             ' ');
                        _expand_macro macro out_channel lexbuf;
                        output_string out_channel "\n";
                        (* _mark_line with pos updated in macro parsing. *)
                        _mark_line lexbuf.lex_curr_p out_channel;
                        output_string out_channel
                          (String.make
                             (lexbuf.lex_curr_p.pos_cnum
                            - lexbuf.lex_curr_p.pos_bol)
                             ' ');
                        loop (Lexer.pp_token lexbuf))
                | _ as x ->
                    output_string out_channel (PPToken.token_repr x);
                    loop (Lexer.pp_token lexbuf))
            | false -> (
                match t with
                | Cmd_Line1 _ | Cmd_Line2 _ | Cmd_Include _ | Cmd_Define1 _
                | Cmd_Define2 _ | Cmd_Define3 _ | Cmd_Ifdef _ | Cmd_Ifndef _ ->
                    _act (fun () -> ())
                | _ -> loop (Lexer.pp_token lexbuf)))
      in
      try loop (Lexer.pp_token lexbuf)
      with e ->
        Printf.eprintf "%s:%i:%i-%i\n" lexbuf.lex_curr_p.pos_fname
          lexbuf.lex_start_p.pos_lnum
          (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol + 1)
          (match lexbuf.lex_curr_p.pos_lnum = lexbuf.lex_start_p.pos_lnum with
          | true -> lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol + 1
          | false -> lexbuf.lex_curr_p.pos_bol - lexbuf.lex_start_p.pos_bol);
        raise e
    in
    _run
end
