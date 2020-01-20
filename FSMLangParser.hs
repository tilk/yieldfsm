module FSMLangParser where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as THQ
import qualified Language.Haskell.Meta as HM
import FSMLang
import Text.Trifecta
import Text.Parser.Token.Style
import Control.Applicative
import Control.Monad
import Prelude
import qualified Data.HashSet as HS

e2m (Left s) = fail s
e2m (Right r) = return r

newlineOrEof = (newline *> return ()) <|> eof

stringToHsExp s = HM.toExp <$> HM.parseHsExp s
stringToHsPat s = HM.toPat <$> HM.parseHsPat s
parseToEOL p = e2m . p =<< manyTill anyChar newlineOrEof
parseHsExpToEOL = parseToEOL stringToHsExp
parseHsPatToEOL = parseToEOL stringToHsPat

idStyle = haskellIdents { _styleReserved = HS.fromList ["var", "let", "emit", "ret", "call", "if", "fun", "else", "endif", "endfun"] }

singleSymbol s = runUnlined (symbol s) *> newlineOrEof

parseName = TH.mkName <$> ident idStyle

parseVar = SVar <$> runUnlined (symbol "var" *> parseName <* symbolic '=')
                <*> parseHsExpToEOL

parseAssign = SAssign <$> runUnlined (parseName <* symbolic '=')
                      <*> parseHsExpToEOL

parseLet = SLet <$> runUnlined (symbol "let" *> parseName <* symbolic '=')
                <*> parseHsExpToEOL

parseEmit = SEmit <$> (runUnlined (symbol "emit") *> parseHsExpToEOL)

parseRet = SRet <$> (runUnlined (symbol "ret") *> parseHsExpToEOL)

parseCall = SCall <$> runUnlined (symbol "call" *> parseName)
                  <*> runUnlined (symbolic '=' *> parseName)
                  <*> parseHsExpToEOL

parseIf = SIf <$> (runUnlined (symbol "if") *> parseHsExpToEOL)
              <*> parseStmt
              <*> (singleSymbol "else" *> parseStmt <* singleSymbol "endif")

parseFun = SFun <$> runUnlined (symbol "fun" *> parseName)
                <*> parseHsPatToEOL
                <*> (parseStmt <* singleSymbol "endfun")

parseBasicStmt = parseVar
             <|> parseLet
             <|> parseEmit
             <|> parseRet
             <|> parseCall
             <|> parseIf
             <|> parseFun
             <|> parseAssign

parseStmt = foldr SSeq SNop <$> many parseBasicStmt

parseProg = Prog <$> (runUnlined (symbol "inputs") *> parseHsPatToEOL)
                 <*> parseStmt

