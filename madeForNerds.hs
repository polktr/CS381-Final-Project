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


-- Valuation function and helper functions
sem :: Expr -> Value
sem (LitI i) = I i
sem (LitF f) = F f
sem (Add a b) = case (sem a, sem b) of
                    (I a, I b) -> I (a+b)
                    (I a, F b) -> F ((fromInteger(toInteger a))+b)
                    (F a, I b) -> F ((fromInteger(toInteger b))+a)
                    (F a, F b) -> F (a+b)
                    _ -> Error
sem (Sub a b) = case (sem a, sem b) of
                    (I a, I b) -> I (a-b)
                    (I a, F b) -> F ((fromInteger(toInteger a))-b)
                    (F a, I b) -> F (a-(fromInteger(toInteger b)))
                    (F a, F b) -> F (a-b)
                    _ -> Error
sem (Mult a b) = case (sem a, sem b) of
                    (I a, I b) -> I (a*b)
                    (I a, F b) -> F ((fromInteger(toInteger a))*b)
                    (F a, I b) -> F (a*(fromInteger(toInteger b)))
                    (F a, F b) -> F (a*b)
                    _ -> Error
--sem (Div a b) = 
--sem (Equ a b) = 
--sem (Let v a b) = 
--sem (Ref v) = v


-- Syntactic Sugar
-- Goals:
--    