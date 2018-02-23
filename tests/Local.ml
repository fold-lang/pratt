
module Char = Astring.Char

let identity x = x

module String = struct
  include String

  let implode l =
    let arr = Array.of_list l in
    String.init (Array.length arr) (Array.get arr)

  let join ?(sep = "") self =
    concat sep self
end


module Int = struct
  let unsafe_of_string =
    int_of_string
end

module Result = struct
  let map f = function Ok x -> Ok (f x) | Error e -> Error e
end

