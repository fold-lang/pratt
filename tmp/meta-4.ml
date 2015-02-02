
f -> (split ~on: "." ~reverse: true ~repeat: 1 f) # 0


interface Comparable {a}
  function compare :: a -> a -> ordering
end


module Comparable {int}
  compare a b =
    ? | a > b : GT  
      | a < b : LT  
      | ...   : EQ
end


module Celsius
  init from fahrenheit =
end


{a} :: if: bool code -> then: a code -> else: a code -> a code

if a_number > 10
  then: "It is big."
  else: "It is not that big."



:: view: dom.view
-> did update context: app.context
-> unit 


encode "Wie heißt du?"
  encoding: str.UTF-8
    errors: str.strict

init_with :: response: URL_Response
      -> data: Data
      -> user_info: Map{string, string}
      -> storage_policy: URL_Cache_Storage_Policy
      -> cached_response


cached_url_response =
  init_with response: cached_response.response
          data
       user_info: cached_response.user_info
    storage_policy: URL_Cache_Storage_Allowed


  initWithResponse: (NSURLResponse *)response
        data: (NSData *)data
      userInfo: (NSDictionary *)userInfo
     storagePolicy: (NSURLCacheStoragePolicy)storagePolicy


[NSCachedURLResponse alloc
  initWithResponse: cachedResponse.response
        data: cachedResponse.data
      userInfo: cachedResponse.userInfo
     storagePolicy: NSURLCacheStorageAllowed];



module Celsius
  temperature_in_celsius :: double mutable =
end


type Celsius {
  var temperature_in_celsius: double
  init(from_fahrenheit fahrenheit: double) {
    temperature_in_celsius = (fahrenheit - 32.0) / 1.8
  }
  init(from_kelvin kelvin: double) {
    temperature_in_celsius = kelvin - 273.15
  }
  init(_ celsius: double) {
    temperature_in_celsius = celsius
  }
}

let boiling_point_of_water = Celsius(from_fahrenheit: 212.0)
// boilingpointofwater.temperatureincelsius is 100.0

let freezing_point_of_water = Celsius(from_kelvin: 273.15)
// freezingpointofwater.temperatureincelsius is 0.0

let body_temperature = Celsius(37.0)
// bodytemperature.temperatureincelsius is 37.0


interface Sequence
  Empty :: t
  (&) :: x -> t -> t
end

interface Graph
  nodes :: a
end


type Node = Integer
type Adj b = [(b, Node)]
type Context a b = (Adj b, Node, a, Adj b)

type Graph a b =
  | Empty
  | Context a b & Graph a b

def &:[C >: A, D >: B](c: Context[C, D]): Graph[C, D] = new &:(c, this)


type Tree{A} = Option{Tree{A}} * A * Option{Tree{A}}


type Tree[A] = (A, Tree[A]?, Tree[A]?)


nud_provider :: {a} => Token -> Integer -> Parser a =
  | Token.Integer x -> atomic : Expression.literal x

type Tree a = Empty | Node a (Tree a) (Tree a)

map f :: {a, b} => (a -> b) -> Tree a -> Tree b =
  | Empty      -> Empty
  | Node x l r -> Node (f x) (map f l) (map f r)

module Functor {Tree}
  map f = Empty      -> Empty
        | Node x l r -> Node (f x) (map f l) (map f r)
end


module Show {Tree}
  show = Empty      -> "()"
       | Node x l r -> "" (f x) (map f l) (map f r)
end




def map[A, B] :: Tree[A] -> (A -> B) -> Tree[B]

case class Tree[+A](value: A, left: Option[Tree[A]], right: Option[Tree[A]]) [
  def map[B](f: A => B): Tree[B] =
    Tree(f(value), left map (_.map(f)), right map (_.map(f)))
}

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree
 
(** val map_tree : ('a -> 'b) -> 'a tree -> 'b tree *)
let rec map_tree f = function
  | Empty        -> Empty
  | Node (x,l,r) -> Node (f x, map_tree f l, map_tree f r)


class List<T>

push_back<T>(T a)

function dfs :: {a, b} => Graph a b -> int

type ('a, 'b) graph =
  | Empty
  | Context of (('a * 'b) * ('a, 'b) graph


type Point = (int, int)

type Pair{a} = (a, a)

type Points = Pair int int

-- Sequence builder notation. The type must be annotated.
squares :: int list = [ n^2 : 1 .. 100 ]

-- Sequence head element notation.
head :: int = [ 1 .. 100 ]

-- Record update notation
g :: Graph a b = { Context | Empty }

{}             : empty
{x}            : singleton
{head & tail}  : compound

-- Inductive Constructors

interface Inductive {a}
  
end

head # tail
head : tail
head ~ tail
head & tail
@#$%^&*_+
head | tail

([("left", 2), ("up", 3)], 1, "a", [("right", 2)]) &
                      ([], 2, "b", [("down", 3)]) &
                      ([], 3, "c", []) & Empty

sum_of_squared_odd_numbers upper =
  [1 .. ]                           -- All natural numbers
    => map (\n -> n * n)             -- Squared
    >> take_while (\n -> n < upper)  -- Below upper limit
    >> filter is_odd                -- That are odd
    >> reduce sum                   -- Sum them




(find "Smith" in: people).first_name greet "George"

send msg: "Hello" to: (find "Waldo" in: addressBook.names)
