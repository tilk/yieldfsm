{-# LANGUAGE TupleSections #-}
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

ssymbol s = (try $ whiteSpace *> symbol s) <?> s

stringToHsExp s = HM.toExp <$> HM.parseHsExp s
stringToHsPat s = HM.toPat <$> HM.parseHsPat s
parseToEOL p = e2m . p =<< manyTill anyChar newlineOrEof
parseHsExpToEOL = parseToEOL stringToHsExp
parseHsPatToEOL = parseToEOL stringToHsPat

idStyle = haskellIdents { _styleReserved = HS.fromList ["var", "let", "emit", "ret", "call", "if", "fun", "else", "begin", "end", "case"] }

singleSymbol s = runUnlined (ssymbol s) *> newlineOrEof

parseName = TH.mkName <$> ident idStyle

parseVar = SVar <$> runUnlined (ssymbol "var" *> parseName <* symbolic '=')
                <*> parseVStmt
                <*> parseBasicStmt

parseAssign = SAssign <$> runUnlined (parseName <* symbolic '=')
                      <*> parseHsExpToEOL

parseLet = SLet <$> runUnlined (ssymbol "let" *> parseName <* symbolic '=')
                <*> parseVStmt
                <*> parseBasicStmt

parseEmit = SEmit <$> (runUnlined (ssymbol "emit") *> parseHsExpToEOL)

parseRet = SRet <$> (runUnlined (ssymbol "ret") *> parseVStmt)

parseIf = SIf <$> (runUnlined (ssymbol "if") *> parseHsExpToEOL)
              <*> parseBasicStmt
              <*> ((singleSymbol "else" *> parseBasicStmt) <|> return SNop)

parseFun1 = f <$> runUnlined (ssymbol "fun" *> parseName)
              <*> parseHsPatToEOL
              <*> parseBasicStmt
    where f a b c = (a, (b, c))

parseFun = SFun <$> (M.fromList <$> some parseFun1)
                <*> parseBasicStmt

parseBlock = SBlock <$> (singleSymbol "begin" *> parseStmt <* singleSymbol "end")

parseCase = SCase <$> (runUnlined (ssymbol "case") *> parseHsExpToEOL)
                  <*> some parseCase1

parseCase1 = (,) <$> (runUnlined (ssymbol "|") *> parseHsPatToEOL)
                 <*> parseBasicStmt

parseBasicStmt = parseVar
             <|> parseLet
             <|> parseEmit
             <|> parseRet
             <|> parseIf
             <|> parseCase
             <|> parseFun
             <|> parseBlock
             <|> parseAssign

parseStmt = many parseBasicStmt

parseVCall = VCall <$> runUnlined (ssymbol "call" *> parseName)
                   <*> parseHsExpToEOL

parseVExp = VExp <$> parseHsExpToEOL

parseVStmt = parseVCall
         <|> parseVExp

parseProg = Prog <$> (runUnlined (ssymbol "inputs") *> parseHsPatToEOL)
                 <*> parseBasicStmt

