
open Pratt.Syntax
open Pratt.Lexer

module Data = Map.Make(String)

type env = {
  next : env option;
  data : expr Data.t ref;
}

let make next = { next; data = ref Data.empty }

let set env sym value =
  match sym with
  | Atom (Sym key) -> env.data := Data.add key value !(env.data)
  | _ -> raise (Invalid_argument "set requires a Symbol for its key")

let rec find env sym =
  match sym with
  | Atom (Sym key) ->
    if Data.mem key !(env.data) then
      Some env
    else begin
      match env.next with
      | Some next -> find next sym
      | None -> None
    end
  | _ -> raise (Invalid_argument "find requires a Symbol for its key")

let get env sym =
  match sym with
  | Atom (Sym key) ->
    begin match find env sym with
      | Some found_env -> Data.find key !(found_env.data)
      | None -> raise (Invalid_argument ("name `" ^ key ^ " is not defined"))
    end
  | _ -> raise (Invalid_argument "get requires a Symbol for its key")

