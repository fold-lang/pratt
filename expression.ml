
open Foundation

type t =
    | Addition of t * t
    | Subtraction of t * t
    | Multiplication of t * t
    | Assignment of t * t
    | Sequence of t * t
    | Integer of int
    | Variable of string
    | Negation of t
    | Start of t
    | End

let addition a b = Addition (a, b)
let subtraction a b = Subtraction (a, b)
let multiplication a b = Multiplication (a, b)
let assignment a b = Assignment (a, b)
let sequence a b = Sequence (a, b)
let integer a = Integer a
let variable a = Variable a
let negation a = Negation a
let start a = Start a
let fim = End

let rec show = function
  | Addition       (a, b) -> format "(%s + %s)" (show a) (show b)
  | Subtraction    (a, b) -> format "(%s - %s)" (show a) (show b)
  | Multiplication (a, b) -> format "(%s * %s)" (show a) (show b)
  | Assignment     (a, b) -> format "(%s = %s)" (show a) (show b)
  | Sequence       (a, b) -> format "(%s; %s)"  (show a) (show b)
  | Integer             a -> format "%d" a
  | Variable            a -> a
  | Negation            a -> "-" ^ (show a)
  | Start               a -> show a
  | End                   -> "(fim)"


