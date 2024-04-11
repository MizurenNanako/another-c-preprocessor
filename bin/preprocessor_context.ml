module PPCtx = struct
  open Preprocessor_token

  module Macro = struct
    type t = { m_trigger : string; m_body : body_token list; m_param_cnt : int }

    and body_token =
      | PLACEHOLDER of int (* Will be replaced when expand *)
      | TOKEN of PPToken.pp_token (* Will remain *)

    let compare m1 m2 = Stdlib.compare m1.m_trigger m2.m_trigger

    let parse_from_tokens parameters replacement =
      let rec check_param param (nth : int) token =
        match param with
        | [] -> TOKEN token
        | hd :: tl ->
            if hd = token then PLACEHOLDER nth
            else check_param tl (nth + 1) token
      in
      let f = check_param parameters 1 in
      List.map f replacement

    let parse_tokens_from_raw raw =
      let lexbuf = Lexing.from_string raw in
      let rec loop (tok : PPToken.pp_token) lst =
        match tok with
        | Eof -> lst
        | x -> loop (Lexer.pp_token lexbuf) (x :: lst)
      in
      loop (Lexer.pp_token lexbuf) []

    let _body_repr b =
      match b with
      | PLACEHOLDER n -> Printf.sprintf "_%d" n
      | TOKEN k -> Printf.sprintf "\"%s\"" (PPToken.token_repr k)

    let repr t =
      let rec _repr body =
        match body with
        | [] -> ""
        | [ lst ] -> Printf.sprintf "%s" (_body_repr lst)
        | hd :: tl -> Printf.sprintf "%s, %s" (_body_repr hd) (_repr tl)
      in
      Printf.sprintf "<sym:\"%s\", body:[%s], len:%i>" t.m_trigger
        (_repr t.m_body) t.m_param_cnt
  end

  module SymbolTable = Set.Make (Macro)

  type t = { mutable sym_table : SymbolTable.t }

  let make () = { sym_table = SymbolTable.empty }

  let has_symbol (self : t) sym =
    SymbolTable.exists (fun x -> x.m_trigger = sym) self.sym_table

  let add_symbol (self : t) sym =
    self.sym_table <-
      SymbolTable.add
        { m_trigger = sym; m_body = []; m_param_cnt = 0 }
        self.sym_table

  let add_macro_parsed (self : t) sym syms =
    self.sym_table <-
      SymbolTable.add
        {
          m_trigger = sym;
          m_body = List.map (fun tk -> Macro.TOKEN tk) syms;
          m_param_cnt = 0;
        }
        self.sym_table

  let add_macro (self : t) sym param reps =
    self.sym_table <-
      SymbolTable.add
        {
          m_trigger = sym;
          m_body = Macro.parse_from_tokens param reps;
          m_param_cnt = List.length param;
        }
        self.sym_table

  let remove_symbol (self : t) sym =
    self.sym_table <-
      SymbolTable.filter (fun x -> x.m_trigger <> sym) self.sym_table
end
