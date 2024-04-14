module PPCtx = struct
  open Preprocessor_token

  module Macro = struct
    exception ExpansionError of string

    type t = {
      m_trigger : string;
      m_body : body_token list;
      m_param_cnt : int;
      m_variadic : bool;
      pos : Lexing.position;
    }

    and body_token =
      | PLACEHOLDER of int (* Will be replaced when expand *)
      | SPLACEHOLDER of int (* Will be replaced when expand *)
      | TOKEN of PPToken.pp_token (* Will remain *)
      | CONCATE
      | VA_ARGS

    let dummy =
      {
        m_trigger = "";
        m_body = [];
        m_param_cnt = 0;
        pos = Lexing.dummy_pos;
        m_variadic = false;
      }

    let compare m1 m2 = Stdlib.compare m1.m_trigger m2.m_trigger

    (** Replace parameter placeholders 
        in token list [replacement],
        respecting to token parameter list [parameters],
        returning macro body *)
    let parse_from_tokens parameters replacement =
      let rec _replace param (nth : int) (token : PPToken.pp_token) =
        match param with
        | [] -> (
            match token with
            | Punctuator "##" -> CONCATE
            | Identifier "__VA_ARGS__" -> VA_ARGS
            | _ -> TOKEN token)
        | hd :: tl -> (
            match (hd, token) with
            | PPToken.Identifier a, Identifier b when a = b -> PLACEHOLDER nth
            | PPToken.Identifier a, SIdentifier b when a = b -> SPLACEHOLDER nth
            | _ -> _replace tl (nth + 1) token)
      in
      let f = _replace parameters 1 in
      List.map f replacement

    let _body_repr b =
      match b with
      | PLACEHOLDER n -> Printf.sprintf "_%d" n
      | SPLACEHOLDER n -> Printf.sprintf "#%d" n
      | TOKEN k -> Printf.sprintf "\"%s\"" (PPToken.token_repr k)
      | CONCATE -> "_CON_"
      | VA_ARGS -> "_VA_ARGS_"

    let rec _body_list_repr body =
      match body with
      | [] -> ""
      | [ lst ] -> Printf.sprintf "%s" (_body_repr lst)
      | hd :: tl -> Printf.sprintf "%s, %s" (_body_repr hd) (_body_list_repr tl)

    let dump t =
      Printf.sprintf "<sym:\"%s\", body:[%s], len:%i, pos:\"%s:%i:%i\">"
        t.m_trigger (_body_list_repr t.m_body) t.m_param_cnt t.pos.pos_fname
        t.pos.pos_lnum
        (t.pos.pos_cnum - t.pos.pos_bol + 1)

    let _patch_token_list_with_space tok_lst =
      let rec _patch tt =
        match tt with
        | [] -> []
        | (PPToken.Identifier _ as i1) :: (Identifier _ as i2) :: tl ->
            PPToken.token_repr i1 :: " " :: _patch (i2 :: tl)
        | a :: tl -> PPToken.token_repr a :: _patch tl
      in
      String.concat "" (_patch tok_lst)

    (** From params to body *)
    let _expand_to_body (t : t) (ptoks : PPToken.pp_token list list) =
      let f macro_body =
        match macro_body with
        | PLACEHOLDER n ->
            let target = List.nth ptoks (n - 1) in
            List.map (fun tk -> TOKEN tk) target
        | SPLACEHOLDER n ->
            print_char 'd';
            let target = List.nth ptoks (n - 1) in
            let ss = _patch_token_list_with_space target in
            [ TOKEN (PPToken.PPstr ('\"', ss)) ]
        | VA_ARGS ->
            if not t.m_variadic then
              raise (Lexer.SyntaxError "__VA_ARGS__ in not variadic macro")
            else
              let rec _drop n lst =
                match n with 0 -> lst | n -> _drop (n - 1) (List.tl lst)
              in
              let rec _place_comma lst =
                match lst with
                | [] -> []
                | [ x ] -> [ x ]
                | hd :: tl ->
                    hd :: [ PPToken.Punctuator "," ] :: _place_comma tl
              in
              let last_elms = _drop t.m_param_cnt ptoks in
              (* form last elements into one *)
              let merged = List.concat (_place_comma last_elms) in
              List.map (fun tk -> TOKEN tk) merged
        | k -> [ k ]
      in

      List.concat_map f t.m_body

    let _body_to_tokens (body : body_token list) =
      let rec f b acc =
        match b with
        | [] -> acc
        | TOKEN k1 :: CONCATE :: TOKEN k2 :: tl ->
            f tl
              (match PPToken.token_concate k1 k2 with
              | Some k -> k :: acc
              | None -> k2 :: k1 :: acc)
        | TOKEN k :: tl -> f tl (k :: acc)
        | b :: _ -> raise (ExpansionError (_body_repr b))
      in
      List.rev (f body [])

    (** Expand macro [self] 
        with parameter of token lists [ptoks], 
        returns expanded token list. *)
    let expand self (ptoks : PPToken.pp_token list list) =
      let tmp = _expand_to_body self ptoks in
      _body_to_tokens tmp

    let repr (t : t) =
      let expanded = expand t [] in
      let tmpstr = _patch_token_list_with_space expanded in
      tmpstr
  end

  module SymbolTable = Set.Make (Macro)

  type t = { mutable sym_table : SymbolTable.t; mutable active_state : bool }

  let make () = { sym_table = SymbolTable.empty; active_state = true }

  let has_symbol self sym =
    SymbolTable.exists (fun x -> x.m_trigger = sym) self.sym_table

  let add_symbol self sym pos =
    self.sym_table <-
      SymbolTable.add
        {
          m_trigger = sym;
          m_body = [];
          m_param_cnt = 0;
          m_variadic = false;
          pos;
        }
        self.sym_table

  let add_macro_parsed self sym syms pos =
    self.sym_table <-
      SymbolTable.add
        {
          m_trigger = sym;
          m_body = List.map (fun tk -> Macro.TOKEN tk) syms;
          m_param_cnt = 0;
          m_variadic = false;
          pos;
        }
        self.sym_table

  let add_macro self sym param reps v pos =
    self.sym_table <-
      SymbolTable.add
        {
          m_trigger = sym;
          m_body = Macro.parse_from_tokens param reps;
          m_param_cnt = List.length param;
          m_variadic = v;
          pos;
        }
        self.sym_table

  let remove_symbol self sym =
    self.sym_table <-
      SymbolTable.filter (fun x -> x.m_trigger <> sym) self.sym_table

  let state_enable self = self.active_state <- true
  let state_disable self = self.active_state <- false
  let state_invert self = self.active_state <- not self.active_state
  let state_ifdef self sym = self.active_state <- has_symbol self sym
  let state_ifndef self sym = self.active_state <- not (has_symbol self sym)
end
