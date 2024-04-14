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
      | TOKEN of PPToken.pp_token (* Will remain *)
      | CONCATE
      | STRINGFY
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

    let parse_from_tokens parameters replacement =
      let rec check_param param (nth : int) (token : PPToken.pp_token) =
        match param with
        | [] -> (
            match token with
            | Punctuator "##" -> CONCATE
            | Punctuator "#" -> STRINGFY
            | Identifier "__VA_ARGS__" -> VA_ARGS
            | _ -> TOKEN token)
        | hd :: tl ->
            if hd = token then PLACEHOLDER nth
            else check_param tl (nth + 1) token
      in
      let f = check_param parameters 1 in
      List.map f replacement

    let _body_repr b =
      match b with
      | PLACEHOLDER n -> Printf.sprintf "_%d" n
      | TOKEN k -> Printf.sprintf "\"%s\"" (PPToken.token_repr k)
      | CONCATE -> "_CON_"
      | STRINGFY -> "_STR"
      | VA_ARGS -> "_VA_ARGS_"

    let rec _body_list_repr body =
      match body with
      | [] -> ""
      | [ lst ] -> Printf.sprintf "%s" (_body_repr lst)
      | hd :: tl -> Printf.sprintf "%s, %s" (_body_repr hd) (_body_list_repr tl)

    let repr t =
      Printf.sprintf "<sym:\"%s\", body:[%s], len:%i, pos:\"%s:%i:%i\">"
        t.m_trigger (_body_list_repr t.m_body) t.m_param_cnt t.pos.pos_fname
        t.pos.pos_lnum
        (t.pos.pos_cnum - t.pos.pos_bol + 1)

    (** From params to body *)
    let _expand_to_body (t : t) (ptoks : PPToken.pp_token list list) =
      let f =
        if t.m_variadic then fun macro_body ->
          match macro_body with
          | PLACEHOLDER n ->
              let target = List.nth ptoks (n - 1) in
              List.map (fun tk -> TOKEN tk) target
          | VA_ARGS ->
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
        else fun macro_body ->
          match macro_body with
          | PLACEHOLDER n ->
              List.map (fun tk -> TOKEN tk) (List.nth ptoks (n - 1))
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
        | STRINGFY :: TOKEN k :: tl -> (
            match k with
            | Identifier i -> f tl (PPstr ('\"', i) :: acc)
            | _ -> raise (ExpansionError "Not Stringfy-able"))
        | TOKEN k :: tl -> f tl (k :: acc)
        | b :: _ -> raise (ExpansionError (_body_repr b))
      in
      List.rev (f body [])

    let expand (t : t) (ptoks : PPToken.pp_token list list) =
      let tmp = _expand_to_body t ptoks in
      _body_to_tokens tmp
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
