module PPCtx = struct
  open Preprocessor_token

  module Macro = struct
    type t = {
      m_trigger : Preprocessor_token.PPToken.pp_token;
      m_body : body_token list;
    }

    and body_token =
      | PLACEHOLDER of int (* Will be replaced when expand *)
      | TOKEN of Preprocessor_token.PPToken.pp_token (* Will remain *)

    let compare m1 m2 = Stdlib.compare m1.m_trigger m2.m_trigger

    let parse parameters replacement =
      let rec check_param param (nth : int) token =
        match param with
        | [] -> TOKEN token
        | hd :: tl ->
            if hd = token then PLACEHOLDER nth
            else check_param tl (nth + 1) token
      in
      let f = check_param parameters 1 in
      List.map f replacement

    let _body_repr b =
      match b with
      | PLACEHOLDER n -> Printf.sprintf "_%d" n
      | TOKEN k -> PPToken.token_repr k

    let repr t =
      let rec _repr body =
        match body with
        | [] -> ""
        | [ lst ] -> Printf.sprintf "%s" (_body_repr lst)
        | hd :: tl -> Printf.sprintf "%s, %s" (_body_repr hd) (_repr tl)
      in
      Printf.sprintf "<sym:\"%s\", body:\"%s\">"
        (PPToken.token_repr t.m_trigger)
        (_repr t.m_body)
  end

  module SymbolTable = Set.Make (Macro)

  type t = { mutable sym_table : SymbolTable.t }

  let make () = { sym_table = SymbolTable.empty }

  let has_symbol (self : t) sym =
    match
      SymbolTable.find_opt { m_trigger = sym; m_body = [] } self.sym_table
    with
    | Some _ -> true
    | None -> false

  let add_symbol (self : t) sym =
    self.sym_table <-
      SymbolTable.add { m_trigger = sym; m_body = [] } self.sym_table

  let add_macro_parsed (self : t) sym syms =
    self.sym_table <-
      SymbolTable.add { m_trigger = sym; m_body = syms } self.sym_table

  let add_macro (self : t) sym param reps =
    self.sym_table <-
      SymbolTable.add
        { m_trigger = sym; m_body = Macro.parse param reps }
        self.sym_table

  let remove_symbol (self : t) sym =
    self.sym_table <-
      SymbolTable.remove { m_trigger = sym; m_body = [] } self.sym_table
end
