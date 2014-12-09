-- Experimentação com a biblioteca "hatt", para lógica proposicional.
module Propositional where


import Data.Logic.Propositional

import Data.Functor ((<$>))
import Data.Map (fromList)
import Text.Parsec.Error (ParseError)


evaluate :: [(Char, Bool)] -> String -> Either ParseError Bool
evaluate mapping expression = interpret' <$> expression'
    where
        interpret' expr = interpret expr mapping'
        mapping' = fromList [(Var var, value) | (var, value) <- mapping]
        expression' = parseExpr "" expression


main = do
    print expression
    print assignments
    print $ evaluate assignments expression
    where
        expression = "~ (p & q)"
        assignments = [('p', True), ('q', False)]
