
open Foundation
open Elements
open Lexer

(** A grammar rule attached to a token. *)
type ('e, 'p) rule = {
  sym : literal;
  lbp : int;
  led : ('e -> 'p) option;
  nud : 'p option;
}

let rule ?(lbp = 0) ?led ?nud sym =
  { sym; lbp; led; nud }

(** Scope encapsulates the definitions of the grammar rules. *)
module Scope = struct

  (** Can hold the various kinds of scope definitons. *)
  module Map = Map.Make(String)

  type ('e, 'p) t = {
    lbp : int Map.t;
    led : ('e -> 'p) Map.t;
    nud : 'p Map.t;
  }

  let empty = {
    lbp = Map.empty;
    led = Map.empty;
    nud = Map.empty;
  }

  let show scope =
    fmt "{ lbp = [%s];\n  led  = [%s];\n  prefix = [%s] }"
     (join ", " (List.map ~f:fst (Map.bindings scope.lbp)))
     (join ", " (List.map ~f:fst (Map.bindings scope.led)))
     (join ", " (List.map ~f:fst (Map.bindings scope.nud)))

  let is_defined_led scope name =
    Map.mem name scope.led

  let is_defined_nud scope name =
    Map.mem name scope.nud

  let is_defined_lbp scope name =
    Map.mem name scope.lbp

  let lookup_led scope name =
    Map.find name scope.led

  let lookup_nud scope name =
    Map.find name scope.nud

  let lookup_lbp scope name =
    Map.find name scope.lbp

  let define rule scope =
    let name = show_literal rule.sym in
    let scope'1 = { scope with lbp = Map.add name rule.lbp scope.lbp } in
    let scope'2 = match rule.led with
      | Some led -> (if Map.mem name scope.led then
                       print @ fmt "Redefinition of led symbol %s" name);
        { scope'1 with led = Map.add name led scope.led }
      | None -> scope'1 in
    let scope'3 = match rule.nud with
      | Some nud -> (if Map.mem name scope.nud then
                       print @ fmt "Redefinition of nud symbol %s" name);
        { scope'2 with nud = Map.add name nud scope.nud }
      | None -> scope'2 in
    scope'3

  let define_led rule scope : ('e, 'p) t =
    let name = show_literal rule.sym in
    if Map.mem name scope.led then
       Log.wrn (fmt "Redefinition of led symbol `%s`." name);
    match rule.led with
    | Some led -> { scope with lbp = Map.add name rule.lbp scope.lbp;
                               led = Map.add name led      scope.led }
    | None -> raise (Invalid_argument "rule has no led code")

  let define_nud rule scope : ('e, 'p) t =
    let name = show_literal rule.sym in
    if Map.mem name scope.nud then
       Log.wrn (fmt "Redefinition of nud symbol `%s`." name);
    match rule.nud with
    | Some nud -> { scope with nud = Map.add name nud scope.nud }
    | None -> raise (Invalid_argument "rule has no nud code")
end

type ('e, 'p) grammar = {
  env     : ('e, 'p) Scope.t list;
  default : literal -> ('e, 'p) rule;
}

let grammar ~main ~default = {
  env = [main];
  default;
}

let show_grammar g =
  join "\n" (List.map ~f:Scope.show g.env)

let rec lookup_led g name =
  match g.env with
  | s::env' -> if Scope.is_defined_led s name
                 then Some (Scope.lookup_led s name)
                 else lookup_led { g with env = env' } name
  | [] -> None

let rec lookup_nud g name =
  match g.env with
  | s::env' -> if Scope.is_defined_nud s name
                 then Some (Scope.lookup_nud s name)
                 else lookup_nud { g with env = env' } name
  | [] -> None

let rec lookup_lbp g name =
  match g.env with
  | s::env' -> if Scope.is_defined_lbp s name
                 then Some (Scope.lookup_lbp s name)
                 else lookup_lbp { g with env = env' } name
  | [] -> None

let lookup_rule g t =
  let sym  = t.value in
  let name = show_literal sym in
  let lbp = lookup_lbp g name || (g.default sym).lbp in
  let led = lookup_led g name in
  let nud = lookup_nud g name in
  (* let led = Some (match lookup_led g name with *)
  (*   | Some led -> led *)
  (*   | None -> Option.value_exn (g.default sym).led) in *)
  (* let nud = Some (match lookup_nud g name with *)
  (*   | Some nud -> nud *)
  (*   | None -> Option.value_exn (g.default sym).nud) in *)

  { sym; lbp; led; nud }

