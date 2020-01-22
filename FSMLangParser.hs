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
import qualified Data.Map.Strict as M

e2m (Left s) = fail s
e2m (Right r) = return r

newlineOrEof = (newline *> return ()) <|> eof

stringToHsExp s = HM.toExp <$> HM.parseHsExp s
stringToHsPat s = HM.toPat <$> HM.parseHsPat s
parseToEOL p = e2m . p =<< manyTill anyChar newlineOrEof
parseHsExpToEOL = parseToEOL stringToHsExp
parseHsPatToEOL = parseToEOL stringToHsPat

idStyle = haskellIdents { _styleReserved = HS.fromList ["var", "let", "emit", "ret", "call", "if", "fun", "else", "begin", "end"] }

singleSymbol s = runUnlined (symbol s) *> newlineOrEof

parseName = TH.mkName <$> ident idStyle

parseVar = SVar <$> runUnlined (symbol "var" *> parseName <* symbolic '=')
                <*> parseVStmt

parseAssign = SAssign <$> runUnlined (parseName <* symbolic '=')
                      <*> parseHsExpToEOL

parseLet = SLet <$> runUnlined (symbol "let" *> parseName <* symbolic '=')
                <*> parseVStmt

parseEmit = SEmit <$> (runUnlined (symbol "emit") *> parseHsExpToEOL)

parseRet = SRet <$> (runUnlined (symbol "ret") *> parseVStmt)

parseIf = SIf <$> (runUnlined (symbol "if") *> parseHsExpToEOL)
              <*> parseBasicStmt
              <*> ((singleSymbol "else" *> parseBasicStmt) <|> return SNop)

parseFun1 = f <$> runUnlined (symbol "fun" *> parseName)
              <*> parseHsPatToEOL
              <*> parseBasicStmt
    where f a b c = (a, (b, c))

parseFun = SFun . M.fromList <$> some parseFun1

parseBlock = SBlock <$> (singleSymbol "begin" *> parseStmt <* singleSymbol "end")

parseBasicStmt = parseVar
             <|> parseLet
             <|> parseEmit
             <|> parseRet
             <|> parseIf
             <|> parseFun
             <|> parseBlock
             <|> parseAssign

parseStmt = many parseBasicStmt

parseVCall = VCall <$> runUnlined (symbol "call" *> parseName)
                   <*> parseHsExpToEOL

parseVExp = VExp <$> parseHsExpToEOL

parseVStmt = parseVCall
         <|> parseVExp

parseProg = Prog <$> (runUnlined (symbol "inputs") *> parseHsPatToEOL)
                 <*> parseStmt

