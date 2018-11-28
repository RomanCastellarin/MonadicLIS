
module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

import System.Random


-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do 
                  whiteSpace lis
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , opLetter      = char '='
                                  , reservedNames = [
                                                     "true","false","skip","if",
                                                     "then","else","end", "while","do"
                                                    ]
                                  --, reservedOpNames = [ ":=", "<", ">", "?", ":" ]
                                  })

-- operator = reservedOp lis

----------------------------------
--- Parser de expresiones enteras
-----------------------------------

intexp, intterm, intfact :: Parser IntExp

inttop = (reservedOp lis "+" >> return Plus)
     <|> (reservedOp lis "-" >> return Minus)
     
intfop = (reservedOp lis "*" >> return Times)
     <|> (reservedOp lis "/" >> return Div)

intexp = chainl1 intterm inttop

intterm = chainl1 intfact intfop

intfact = parens lis intexp
      <|> do reservedOp lis "-"
             f <- intfact
             return $ UMinus f
      <|> do n <- integer lis
             return $ Const n
      <|> do reservedOp lis "?" 
             b <- boolexp
             reservedOp lis ":"            
             n1 <- intexp
             reservedOp lis ":"
             n2 <- intexp
             return $ TerCond b n1 n2
      <|> do v <- identifier lis
             return $ Var v
      
-----------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp, boolterm, boolfact :: Parser BoolExp

boolexp = chainl1 boolterm (reservedOp lis "|" >> return Or)

boolterm = chainl1 boolfact (reservedOp lis "&" >> return And)

boolfact = (reserved lis "true" >> return BTrue)
       <|> (reserved lis "false" >> return BFalse)
       <|> parens lis boolexp
       <|> do reservedOp lis "~"
              b <- boolfact
              return $ Not b
       <|> do e1 <- intexp
              co <- do op <- operator lis   -- is this patter correct or should I use a
                       case op of           -- chain of (reservedOp lis x >> x') <|>'d ?
                        "<" -> return Lt
                        ">" -> return Gt
                        "=" -> return Eq
              e2 <- intexp
              return $ co e1 e2

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm

comm = chainl1 commatom (reservedOp lis ";" >> return Seq)

commatom = (reserved lis "skip" >> return Skip)
       <|> do reserved lis "if"
              b <- boolexp
              reserved lis "then"
              c1 <- comm
              reserved lis "else"
              c2 <- comm
              reserved lis "end"
              return $ Cond b c1 c2
       <|> do reserved lis "repeat"
              c <- comm
              reserved lis "until"
              b <- boolexp
              reserved lis "end"
              return $ Repeat c b
       <|> do v <- identifier lis
              reservedOp lis ":="
              x <- intexp
              return $ Let v x

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

parseBool = parse (totParser boolexp)
