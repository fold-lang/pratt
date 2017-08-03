
module Char = Astring.Char

module String = struct
  include String

  let implode l =
    let arr = Array.of_list l in
    String.init (Array.length arr) (Array.get arr)

  let join ?(sep = "") self =
    concat sep self
end


module Int = struct
  include Int
  let force_of_string =
    int_of_string
end
