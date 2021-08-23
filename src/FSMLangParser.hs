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

newlineOrEof :: Parser ()
newlineOrEof = (newline *> return ()) <|> eof

ssymbol s = (try $ whiteSpace *> symbol s) <?> s

stringToHsExp s = HM.toExp <$> HM.parseHsExp s
stringToHsPat s = HM.toPat <$> HM.parseHsPat s
parseToEOL :: ([Char] -> Either String a) -> Parser a
parseToEOL p = e2m . p =<< manyTill anyChar newlineOrEof
parseHsExpToEOL = parseToEOL stringToHsExp
parseHsPatToEOL = parseToEOL stringToHsPat

idStyle = haskellIdents { _styleReserved = HS.fromList ["nop", "var", "let", "emit", "ret", "call", "if", "fun", "else", "begin", "end", "case"] }

singleSymbol s = runUnlined (ssymbol s) *> newlineOrEof

parseName = TH.mkName <$> ident idStyle

parseVar :: Parser Stmt
parseVar = SVar <$> runUnlined (ssymbol "var" *> parseName <* symbolic '=')
                <*> parseVStmt
                <*> parseBasicStmt

parseAssign :: Parser Stmt
parseAssign = SAssign <$> runUnlined (parseName <* symbolic '=')
                      <*> parseHsExpToEOL

parseLet :: Parser Stmt
parseLet = SLet <$> runUnlined (ssymbol "let" *> parseName <* symbolic '=')
                <*> parseVStmt
                <*> parseBasicStmt

parseEmit :: Parser Stmt
parseEmit = SEmit <$> (runUnlined (ssymbol "emit") *> parseHsExpToEOL)

parseRet :: Parser Stmt
parseRet = SRet <$> (runUnlined (ssymbol "ret") *> parseVStmt)

parseIf :: Parser Stmt
parseIf = SIf <$> (runUnlined (ssymbol "if") *> parseHsExpToEOL)
              <*> parseBasicStmt
              <*> ((singleSymbol "else" *> parseBasicStmt) <|> return SNop)

parseFun1 = f <$> runUnlined (ssymbol "fun" *> parseName)
              <*> parseHsPatToEOL
              <*> parseBasicStmt
    where f a b c = (a, (b, c))

parseFun :: Parser Stmt
parseFun = SFun <$> (M.fromList <$> some parseFun1)
                <*> parseBasicStmt

parseBlock :: Parser Stmt
parseBlock = SBlock <$> (singleSymbol "begin" *> parseStmt <* singleSymbol "end")

parseCase :: Parser Stmt
parseCase = SCase <$> (runUnlined (ssymbol "case") *> parseHsExpToEOL)
                  <*> some parseCase1

parseCase1 = (,) <$> (runUnlined (ssymbol "|") *> parseHsPatToEOL)
                 <*> parseBasicStmt

parseNop :: Parser Stmt
parseNop = singleSymbol "nop" *> return SNop

parseBasicStmt :: Parser Stmt
parseBasicStmt = parseVar
             <|> parseLet
             <|> parseEmit
             <|> parseRet
             <|> parseIf
             <|> parseCase
             <|> parseFun
             <|> parseBlock
             <|> parseNop
             <|> parseAssign

parseStmt :: Parser [Stmt]
parseStmt = many parseBasicStmt

parseVCall :: Parser VStmt
parseVCall = VCall <$> runUnlined (ssymbol "call" *> parseName)
                   <*> parseHsExpToEOL

parseVExp :: Parser VStmt
parseVExp = VExp <$> parseHsExpToEOL

parseVStmt :: Parser VStmt
parseVStmt = parseVCall
         <|> parseVExp

parseProg = Prog <$> (runUnlined (ssymbol "inputs") *> parseHsPatToEOL)
                 <*> parseBasicStmt

