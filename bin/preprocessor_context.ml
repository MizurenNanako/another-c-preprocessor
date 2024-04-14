module PPCtx = struct
  open Preprocessor_token

  module Macro = struct
    exception ExpansionError of string

    type t = {
      m_trigger : string;
      m_body : macro_token list;
      m_param_cnt : int;
      m_variadic : bool;
      m_pos : Lexing.position;
    }

    and macro_token =
      (* parameter placeholder *)
      | PLACEHOLDER of int (* index starts from 1 *)
      (* stringfied parameter placeholder *)
      | SPLACEHOLDER of int (* index starts from 1 *)
      | TOKEN of string (* raw string *)
      (* placeholders to form a new one *)
      | CONCATE of int list
      | VA_ARGS

    let dummy =
      {
        m_trigger = "";
        m_body = [];
        m_param_cnt = 0;
        m_pos = Lexing.dummy_pos;
        m_variadic = false;
      }

    let compare m1 m2 = Stdlib.compare m1.m_trigger m2.m_trigger

    let _macro_token_dump b =
      match b with
      | PLACEHOLDER n -> Printf.sprintf "_%d" n
      | SPLACEHOLDER n -> Printf.sprintf "#%d" n
      | TOKEN k -> Printf.sprintf "\"%s\"" k
      | CONCATE l ->
          Printf.sprintf "_CON_(%s)"
            (String.concat ", " (List.map (fun i -> string_of_int i) l))
      | VA_ARGS -> "_VA_ARGS_"

    let dump t =
      let dumped_body =
        String.concat ", " (List.map (fun k -> _macro_token_dump k) t.m_body)
      in
      Printf.sprintf "<sym:\"%s\", body:[%s], cnt:%i, pos:\"%s:%i:%i\">"
        t.m_trigger dumped_body t.m_param_cnt t.m_pos.pos_fname t.m_pos.pos_lnum
        (t.m_pos.pos_cnum - t.m_pos.pos_bol + 1)

    (** Replace parameter placeholders 
        in token list [replacement],
        respecting to token parameter list [parameters],
        returning macro body *)
    let parse_from_tokens (parameters : PPToken.pp_token list)
        (replacement : PPToken.pp_token list) =
      ignore (parameters, replacement);
      [TOKEN ""]

    (** Expand macro [self] 
        with parameter of token lists [ptoks], 
        returns expanded string. *)
    let expand (self : t) (ptoks : PPToken.pp_token list list) =
      ignore (self, ptoks);
      ""

    (** Expand macro [self] 
        with parameter of strings [stoks], 
        returns expanded string. *)
    let expand_with_str (self : t) (stoks : string list) =
      ignore (self, stoks);
      ""
  end

  module SymbolTable = Set.Make (Macro)

  type t = { mutable sym_table : SymbolTable.t; mutable active_state : bool }

  let make () = { sym_table = SymbolTable.empty; active_state = true }

  let has_symbol self sym =
    SymbolTable.exists (fun x -> x.m_trigger = sym) self.sym_table

  let add_symbol_empty self sym pos =
    self.sym_table <-
      SymbolTable.add
        {
          m_trigger = sym;
          m_body = [];
          m_param_cnt = 0;
          m_variadic = false;
          m_pos = pos;
        }
        self.sym_table

  let add_symbol self sym toks pos =
    self.sym_table <-
      SymbolTable.add
        {
          m_trigger = sym;
          m_body = List.map (fun tk -> Macro.TOKEN (PPToken.token_repr tk)) toks;
          m_param_cnt = 0;
          m_variadic = false;
          m_pos = pos;
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
          m_pos = pos;
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
