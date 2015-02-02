
module TopDownParserState where

-- to stop a collision with record field id (records are kind of odd in Haskell)
import Prelude hiding (id, error, lookup)
import qualified Prelude

-- unlike Javascript there's no inbuilt data map support
import Data.Map (Map, (!), lookup, insert, member)
import qualified Data.Map as Map

-- support stateful function style
import Control.Monad.State

import Tokeniser

type Parser a = State Env a

-- Something which isn't obvious from the original variable names is
-- that 'tokens' is input yet 'token' belongs to the completely
-- different output type.
data Env = Env { scope        :: Scope,
                 symbol_table :: SymbolTable,
                 token        :: Symbol,
                 tokens       :: [Token] }

itself = return

-- This could be a simple list of scopes but I'll try to keep closely to
-- the structure of the Javascript code
data Scope = Scope { def    :: SymbolTable,
                     parent :: Scope }
           | TopScope

type SymbolTable = Map Value Symbol

define n@Symbol {value = value} = do
  this <- gets scope
  let t = def this ! value
  when (member value $ def this) $
    error n $ if reserved t
              then "Already reserved."
              else "Already defined."
  let n' = n { reserved = False,
               nudf     = itself,
               -- why redefine led here?
               ledf     = \this _ -> error this "Undefined operator.",
               std      = Nothing,
               lbp      = 0,
               skope    = this }
  env <- get
  put env { scope = this {
    def = insert value n' $ def this
  } }
  return n'

find env@Env {scope = Scope {def = def, parent = e}} n =
  case lookup n def of
    Just t -> t
    _      -> find env { scope = e} n

find Env { symbol_table = st } n =
  case lookup n st of
    Just t -> t
    _      -> st ! "(name)"

pop = do
  env <- get
  put env { scope = parent $ scope env }

reserve n@Symbol {arity = Name, reserved = False, value = value} = do
  this <- gets scope
  let t = def this ! value
  when (member value $ def this) $ do
    when (reserved t) $
      return ()
    when (arity t == Name) $
      error n "Unreserved is already defined."
  env <- get
  put env { scope = this {
    def = insert value n { reserved = True } $ def this
  } }

reserve _ = return ()

new_scope = do
  s <- gets scope
  let s' = Scope { def    = Map.empty,
                   parent = s }
  env <- get
  put env { scope = s' }
  return s'

advanceIf requiredId = do
  token <- gets token
  when (id token /= requiredId) $
    error token $ "Expected '" ++ requiredId ++ "'."
  advance

advance = do
  this <- get
  let (t, ts) = case tokens this of
        []
          -> (symbol_table this ! "(end)", [])
        t@(Token a v):tokens'
          -> let (o, a') = case a of
                   NameType
                     -> (find this $ v, Name)
                   OperatorType
                     -> case lookup v $ symbol_table this of
                          Just t' -> (t', Operator)
                          _       -> error t "Unknown operator."
                   NumberType
                     -> (symbol_table this ! "(literal)", Literal)
                   StringType
                     -> (symbol_table this ! "(literal)", Literal)
                   -- the next case can't happen and ghc throws a warning
                   -- _         -> error t "Unexpected token."
             in (o { value = v, arity = a' }, tokens')

  put this { token = t, tokens = ts }
  return t

expression rbp = do
  t <- gets token
  advance
  left <- nud t
  let walkRight left = do
        t <- gets token
        if rbp < lbp t then do
            advance
            left <- led t left
            walkRight left
          else return left
  walkRight left

type NudFun = This -> Parser Symbol
type LedFun = This -> Symbol -> Parser Symbol

statement = do
  n <- gets token
  case n of
    Symbol { std = Just std } -> do
      advance
      reserve n
      std n
    otherwise -> do
      v <- expression 0
      when (not (isAssignment v) && id v /= "(") $
        error v "Bad expression statement."
      advanceIf ";"
      return [v]

type StdFun = This -> Parser [Symbol]

-- For this function and all like it we don't change the return type
-- but instead make the pretty printer treat empty lists as null and
-- single element lists as the element.  To simplify the structure
-- of the Symbol data structure we also apply the equivilent
-- transformation there which means that single element lists appear
-- in many places where the Javascript uses just the element.
-- Because we apply this transformation uniformly there are cases
-- where our output is slightly different from the original.
statements = do
  token <- gets token
  if id token == "}" || id token == "(end)"
    then return []
    else do
      s <- statement
      ss <- statements
      return $ s ++ ss

block = do
  t <- gets token
  advanceIf "{"
  case std t of
    Just s -> s t

data Symbol = Symbol { id    :: Id,
                       arity :: Arity,
                       value :: Value,
                       lbp   :: BindingPower,
                       reserved, isAssignment :: Bool,
                       nudf  :: NudFun,
                       ledf  :: LedFun,
                       std   :: Maybe StdFun,
                       skope :: Scope,
                       key   :: Maybe Value,
                       first, second, third :: [Symbol] }

data Arity = Name | Operator | Literal | Unary | Binary | Ternary
           | Statement | This
           | Function { name :: Maybe Value }
           deriving (Eq, Show)

original_symbol = Symbol {
  nudf = \this   -> error this "Undefined.",
  ledf = \this _ -> error this "Missing operator.",
  std  = Nothing,
  first = [], second = [], third = [],
  id = undefined, arity = undefined, value = undefined, lbp = undefined,
  isAssignment = False, skope = undefined, reserved = False,
  key = Nothing
}

-- helper functions to access nudf/ledf with correct "object"
nud s = nudf s s
led s = ledf s s

symbol0 id = symbol1 id NilT
symbol1    = flip symbol 0

-- rather than make Symbol mutable this binds the function during
-- the symbol creation
-- symbol :: Id -> BindingPower -> SymbolType -> State SymbolTable Symbol
symbol id bp typ = do
  st <- get
  let s' = bind typ $
           case lookup id st of
             Just s -> if bp >= lbp s
                       then s { lbp = bp }
                       else s
             _ -> original_symbol { id    = id,
                                    value = id,
                                    lbp   = bp }
  put $ insert id s' st
  return s'
    where
      bind (Nud f) s = s { nudf = f }
      bind (Led f) s = s { ledf = f }
      bind (Std f) s = s { std  = Just f }
      bind _       s = s

data SymbolType = NilT 
                | Nud NudFun
                | Led LedFun
                | Std StdFun

-- this constant doesn't use the value because that would require a
-- datatype for all the kinds of javascript types it could be set to
constant0 s v = constant s v $ \this -> do
  reserve this
  symbol_table <- gets symbol_table
  return this { value = value $ symbol_table ! id this,
                arity = Literal }

constant s _ f = symbol1 s $ Nud f

-- infix is a keyword
inphix0 s bp = inphix s bp $ \this left -> do
  right <- expression bp
  return this { first  = [left],
                second = [right],
                arity  = Binary }

inphix s bp f = symbol s bp $ Led f

inphixr0 s bp = inphixr s bp $ \this left -> do
  right <- expression $ bp-1
  return this { first  = [left],
                second = [right],
                arity  = Binary }

-- infixr is a keyword
inphixr = inphix

assignment s = inphixr s 10 $ \this left -> do
  when (id left /= "." && id left /= "[" && arity left /= Name) $
    error left "Bad lvalue."
  right <- expression 9
  return this { first  = [left],
                second = [right],
                arity  = Binary,
                isAssignment = True }

-- prefix isn't a keyword but to named to match inphix and inphixr
prephix0 s = prephix s $ \this -> do
  reserve this
  expr <- expression 70
  return this { first = [expr],
                arity = Unary }

prephix s f = symbol1 s $ Nud f

stmt s f = symbol1 s $ Std f

initial_symbol_table = execState ist Map.empty
    where 
      ist = do

      symbol0 "(end)"
      symbol0 "(name)"

      symbol0 ":"
      symbol0 ";"
      symbol0 ")"
      symbol0 "]"
      symbol0 "}"
      symbol0 ","
      symbol0 "else"

      constant0 "true" True
      constant0 "false" False
      constant0 "null" undefined
      constant0 "pi" 3.141592653589793
      constant0 "Object" Map.empty
      constant0 "Array" []

      symbol1 "(literal)" $ Nud itself

      symbol1 "this" $ Nud $ \this -> do
        reserve this
        return this { arity = This }

      assignment "="
      assignment "+="
      assignment "-="

      inphix "?" 20 $ \this left -> do
        whenTrue <- expression 0
        advanceIf ":"
        whenFalse <- expression 0
        return this { first  = [left],
                      second = [whenTrue],
                      third  = [whenFalse],
                      arity  = Ternary }

      inphixr0 "&&" 30
      inphixr0 "||" 30

      inphixr0 "===" 40
      inphixr0 "!==" 40
      inphixr0 "<" 40
      inphixr0 "<=" 40
      inphixr0 ">" 40
      inphixr0 ">=" 40

      inphix0 "+" 50
      inphix0 "-" 50

      inphix0 "*" 60
      inphix0 "/" 60

      inphix "." 80 $ \this left -> do
        token <- gets token
        when (arity token /= Name) $
          error token "Expected a property name."
        -- Even though the Javascript updates the token it is then
        -- immediately discaded by 'advance' so we won't bother
        advance
        return this { first  = [left],
                      second = [token { arity = Literal }],
                      arity  = Binary }

      inphix "[" 80 $ \this left -> do
        s <- expression 0
        advanceIf "]"
        return this { first  = [left],
                      second = [s],
                      arity  = Binary }

      inphix "(" 80 $ \this left -> do
        t <- gets token
        a <- if id t /= ")" then
               let vars = do
                     e <- expression 0
                     token <- gets token
                     if id token /= ","
                       then return [e]
                       else do
                         advanceIf ","
                         v <- vars
                         return $ e:v
               in vars
             else return []
        -- can't use a before it's been populated
        let this' = if id left == "." || id left == "["
                    then this { first  = first left,
                                second = second left,
                                third  = a,
                                arity  = Ternary }
                    else if (arity left /= Unary || id left /= "function") &&
                            arity left /= Name && id left /= "(" &&
                            id left /= "&&" && id left /= "||" && id left /= "?"
                         then error left "Expected a variable name."
                         else this { first  = [left],
                                     second = a,
                                     arity  = Binary }
        advanceIf ")"
        return this'

      prephix0 "!"
      prephix0 "-"
      prephix0 "typeof"

      prephix "(" $ \this -> do
        e <- expression 0
        advanceIf ")"
        return e

      prephix "function" $ \this -> do
        new_scope
        t <- gets token
        n <- if arity t == Name then do
               define t
               advance
               return $ Just $ value t
             else return Nothing
        t <- advanceIf "("
        a <- if id t /= ")" then
               let params = do
                     t <- gets token
                     when (arity t /= Name) $
                       error t "Expected a parameter name."
                     define t
                     token <- advance
                     if id token /= ","
                       then return [t]
                       else do
                         advanceIf ","
                         p <- params
                         return $ t:p
               in params
             else return []
        advanceIf ")"
        advanceIf "{"
        s <- statements
        advanceIf "}"
        pop
        return this { first  = a,
                      second = s,
                      arity  = Function { name = n } }

      prephix "[" $ \this -> do
        t <- gets token
        a <- if id t /= "]" then
               let entries = do
                     v <- expression 0
                     token <- gets token
                     if id token /= ","
                       then return [v]
                       else do
                         advanceIf ","
                         e <- entries
                         return $ v:e
               in entries
             else return []
        advanceIf "]"
        return this { first = a,
                      arity = Unary }

      prephix "{" $ \this -> do
        t <- gets token
        a <- if id t /= "}" then
               let entries = do
                     n <- gets token
                     when (arity n /= Name && arity n /= Literal) $
                       error n "Bad property name."
                     advance
                     advanceIf ":"
                     v <- expression 0
                     let v' = v { key = Just $ value n }
                     token <- gets token
                     if id token /= ","
                       then return [v']
                       else do
                         advanceIf ","
                         e <- entries
                         return $ v':e
               in entries
             else return []
        advanceIf "}"
        return this { first = a, 
                      arity = Unary }

      stmt "{" $ \this -> do
        new_scope
        a <- statements
        advanceIf "}"
        pop
        return a

      stmt "var" $ \this -> do
        let vars = do
              n <- gets token
              when (arity n /= Name) $
                error n "Expected a new variable name."
              define n
              t <- advance
              a <- if id t == "=" then do
                     advanceIf "="
                     s <- expression 0
                     let t' = t { first  = [n],
                                  second = [s],
                                  arity  = Binary,
                                  isAssignment = True }
                     return [t']
                   else return []
              t <- gets token
              if id t /= ","
                then return a
                else do
                  advanceIf ","
                  v <- vars
                  return $ a++v

        a <- vars
        advanceIf ";"
        return a

      stmt "if" $ \this -> do
        advanceIf "("
        test <- expression 0
        advanceIf ")"
        body <- block
        token <- gets token
        els <- if id token == "else" then do
                 reserve token
                 token <- advanceIf "else"
                 if id token == "if" then statement else block
               else return []
        return [this { first  = [test],
                       second = body,
                       third  = els,
                       arity  = Statement }]

      stmt "return" $ \this -> do
        t <- gets token
        first <- if id t /= ";" then do
                   e <- expression 0
                   return [e]
                 else return []
        t <- advanceIf ";"
        when (id t /= "}") $
          error t "Unreachable statement."
        return [this { first = first,
                       arity = Statement }]

      stmt "break" $ \this -> do
        t <- advanceIf ";"
        when (id t /= "}") $
          error t "Unreachable statement."
        return [this { arity = Statement }]

      stmt "while" $ \this -> do
        advanceIf "("
        f <- expression 0
        advanceIf ")"
        s <- block
        return [this { first  = [f],
                       second = s,
                       arity  = Statement }]

parse source = 
  evalState ( do
    new_scope
    advance
    s <- statements
    advanceIf "(end)"
    return s
  ) Env { tokens       = tokenise source,
          scope        = TopScope,
          token        = original_symbol { id = "(start)" },
          symbol_table = initial_symbol_table }

type Value        = String
type Id           = String
type BindingPower = Int
type This         = Symbol

error t msg = Prelude.error $ msg ++ " " ++ show t

instance Show Symbol where
    show Symbol { value = value, arity = arity } =
      "{value: " ++ show value ++ " " ++ show arity ++ "}"
      
