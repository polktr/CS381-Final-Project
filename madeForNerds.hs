-- FileName: madeForNerds.hs
-- Date: Milestone Due: Thursday, February 27th, 2020 @ 11PM

module MadeForNerds where


-- Syntax:
--
--   int ::= (any integer)
--   float ::= (any floating-point number)
--   var ::= (any variable name)
--
--   expr ::= int                             -- integer literal
--          | float                           -- float literal
--          | expr `+` expr                   -- addition
--          | expr `-` expr                   -- subtraction
--          | expr `*` expr                   -- multiplication
--          | expr `/` expr                   -- division
--          | expr `==` expr                  -- check if two values are equal
-- Add some sort of conditional expression????
--          | `let` var `=` expr `in` expr    -- variable declaration and binding
--          | var                             -- variable reference


-- Abstract syntax as Haskell data type
type Var = String

data Expr
    = LitI Int
    | LitF Float
    | Add Expr Expr
    | Sub Expr Expr
    | Mult Expr Expr
    | Div Expr Expr
    | Equ Expr Expr
    | Let Var Expr Expr
    | Ref Var
    deriving (Eq, Show)


-- Semantic domains 
--
--   Possible values:
--      Int
--      Float
--      Bool
--      String
--      Error

data Value
    = I Int
    | F Float
    | B Bool
    | S String
    | Error
    deriving (Eq, Show)

-- Alternate domains: Using Maybe:
--   data Maybe a = Nothing | Just a
--   data Either a b c d = One a | Two b | Three c | Four d
--
--   type Value = Maybe (Either Int Float Bool String)


-- Valuation function