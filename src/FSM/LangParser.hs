{-# LANGUAGE TupleSections #-}
module FSM.LangParser where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as THQ
import qualified Language.Haskell.Meta as HM
import FSM.Lang
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative hiding (many, some)
import Control.Monad
import Prelude
import Data.Void
import Data.Char(isSpace)
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M

type Parser = Parsec Void String

isHSpace :: Char -> Bool
isHSpace x = isSpace x && x /= '\n' && x /= '\r'

myspace :: (MonadParsec e s m, Token s ~ Char) => m ()
myspace = void $ takeWhileP (Just "white space") isHSpace
lexeme = L.lexeme myspace
symbol = L.symbol myspace
symbolic = lexeme . char
ident = lexeme $ (:) <$> letterChar <*> many alphaNumChar

e2m (Left s) = fail s
e2m (Right r) = return r

newlineOrEof :: Parser ()
newlineOrEof = (newline *> return ()) <|> eof

ssymbol s = (try $ space *> symbol s) <?> s

stringToHsExp s = HM.toExp <$> HM.parseHsExp s
stringToHsPat s = HM.toPat <$> HM.parseHsPat s
stringToHsType s = HM.toType <$> HM.parseHsType s
parseToEOL :: ([Char] -> Either String a) -> Parser a
parseToEOL p = e2m . p =<< manyTill anySingle newlineOrEof
parseHsExpToEOL = parseToEOL stringToHsExp
parseHsPatToEOL = parseToEOL stringToHsPat
parseHsTypeToEOL = parseToEOL stringToHsType

--idStyle = haskellIdents { _styleReserved = HS.fromList ["nop", "var", "let", "emit", "ret", "call", "if", "fun", "else", "begin", "end", "case"] }

singleSymbol s = ssymbol s *> newlineOrEof

parseName = TH.mkName <$> ident

parseVar :: Parser Stmt
parseVar = SVar <$> (ssymbol "var" *> parseName <* symbolic '=')
                <*> parseVStmt
                <*> parseBasicStmt

parseAssign :: Parser Stmt
parseAssign = SAssign <$> (parseName <* symbolic '=')
                      <*> parseHsExpToEOL

parseLet :: Parser Stmt
parseLet = SLet <$> (ssymbol "let" *> parseName <* symbolic '=')
                <*> parseVStmt
                <*> parseBasicStmt

parseEmit :: Parser Stmt
parseEmit = SEmit <$> (ssymbol "emit" *> parseHsExpToEOL)

parseRet :: Parser Stmt
parseRet = SRet <$> (ssymbol "ret" *> parseVStmt)

parseIf :: Parser Stmt
parseIf = SIf <$> (ssymbol "if" *> parseHsExpToEOL)
              <*> parseBasicStmt
              <*> ((singleSymbol "else" *> parseBasicStmt) <|> return SNop)

parseFun1 = f <$> (ssymbol "fun" *> parseName)
              <*> parseHsPatToEOL
              <*> parseBasicStmt
    where f a b c = (a, (b, c))

parseFun :: Parser Stmt
parseFun = SFun <$> (M.fromList <$> some parseFun1)
                <*> parseBasicStmt

parseBlock :: Parser Stmt
parseBlock = SBlock <$> (singleSymbol "begin" *> parseStmt <* singleSymbol "end")

parseCase :: Parser Stmt
parseCase = SCase <$> (ssymbol "case" *> parseHsExpToEOL)
                  <*> some parseCase1

parseCase1 = (,) <$> (ssymbol "|" *> parseHsPatToEOL)
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
parseVCall = VCall <$> (ssymbol "call" *> parseName)
                   <*> parseHsExpToEOL

parseVExp :: Parser VStmt
parseVExp = VExp <$> parseHsExpToEOL

parseVStmt :: Parser VStmt
parseVStmt = parseVCall
         <|> parseVExp

parseProg = Prog <$> (parseName <* ssymbol "::")
                 <*> parseHsTypeToEOL
                 <*> many (ssymbol "param" *> parseHsPatToEOL)
                 <*> (ssymbol "inputs" *> parseHsPatToEOL)
                 <*> parseBasicStmt

