{-# LANGUAGE TupleSections #-}
module FSM.LangParser(runParseProg) where

import qualified Language.Haskell.TH as TH
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
import qualified Data.Map.Strict as M

type Parser = Parsec Void String

isHSpace :: Char -> Bool
isHSpace x = isSpace x && x /= '\n' && x /= '\r'

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineComment empty

myspace :: (MonadParsec e s m, Token s ~ Char) => m ()
myspace = void $ takeWhileP (Just "white space") isHSpace

symbol :: String -> Parser String
symbol = L.symbol myspace

symbolic :: Parser () -> Char -> Parser Char
symbolic sc' = L.lexeme sc' . char

ident :: Parser () -> Parser String
ident sc' = L.lexeme sc' $ (:) <$> letterChar <*> many alphaNumChar

e2m :: MonadFail m => Either String a -> m a
e2m (Left s) = fail s
e2m (Right r) = return r

newlineOrEof :: Parser ()
newlineOrEof = (newline *> return ()) <|> eof

ssymbol :: String -> Parser String
ssymbol s = (try $ scn *> symbol s) <?> s

stringToHsExp :: String -> Either String TH.Exp
stringToHsExp s = HM.toExp <$> HM.parseHsExp s

stringToHsPat :: String -> Either String TH.Pat
stringToHsPat s = HM.toPat <$> HM.parseHsPat s

stringToHsType :: String -> Either String TH.Type
stringToHsType s = HM.toType <$> HM.parseHsType s

parseHsFoldGen :: (Parser () -> Parser a) -> (Parser () -> Parser (b -> c)) -> (a -> Either String b) -> Parser c
parseHsFoldGen rest pfx p = try (lookAhead (scn *> pfx scn)) *> L.lineFold scn (\sc' ->
    (($) <$> pfx sc' <*> (e2m =<< p <$> rest sc')) <* sc)

parseHsFold :: (Parser () -> Parser (a -> b)) -> (String -> Either String a) -> Parser b
parseHsFold = parseHsFoldGen (\sc' -> unwords <$> some (noneOf "\r\n") `sepBy1` try sc')

parseHsFoldColon :: (Parser () -> Parser (a -> b)) -> (String -> Either String a) -> Parser b
parseHsFoldColon = parseHsFoldGen (\sc' -> unwords <$> some mysingle `sepBy1` try sc' <* single ':')
    where
    mysingle = try $ noneOf "\r\n" >>= \c -> return c <* (if c == ':' then notFollowedBy newline else return ())

parseHsFoldSymbol :: String -> (String -> Either String a) -> Parser a
parseHsFoldSymbol s = parseHsFold (\sc' -> L.symbol sc' s >> return id)

--idStyle = haskellIdents { _styleReserved = HS.fromList ["nop", "var", "let", "emit", "ret", "call", "if", "fun", "else", "begin", "end", "case"] }

singleSymbol :: String -> Parser ()
singleSymbol s = ssymbol s *> newlineOrEof

parseName :: Parser () -> Parser TH.Name
parseName sc' = TH.mkName <$> ident sc'

parseVar :: Parser Stmt
parseVar = do
    (lvl, i, v) <- parseVStmt (\sc' -> (,,) <$> L.indentLevel <*> (L.symbol sc' "var" *> parseName sc' <* symbolic sc' '='))
    SLet VarMut i v <$> (L.indentGuard scn EQ lvl *> parseStmt)

parseAssign :: Parser Stmt
parseAssign = uncurry SAssign <$> parseVStmt (\sc' -> (,) <$> parseName sc' <* symbolic sc' '=')

parseLet :: Parser Stmt
parseLet = do
    (lvl, i, v) <- parseVStmt (\sc' -> (,,) <$> L.indentLevel <*> (L.symbol sc' "let" *> parseName sc' <* symbolic sc' '='))
    SLet VarLet i v <$> (L.indentGuard scn EQ lvl *> parseStmt)

parseEmit :: Parser Stmt
parseEmit = SEmit <$> parseHsFoldSymbol "emit" stringToHsExp

parseRet :: Parser Stmt
parseRet = parseVStmt (\sc' -> L.symbol sc' "ret" >> return SRet)

parseIf :: Parser Stmt
parseIf = do
    (lvl, e) <- parseHsFoldColon (\sc' -> (,) <$> L.indentLevel <* L.symbol sc' "if") stringToHsExp
    SIf e <$> (L.indentGuard scn GT lvl *> parseStmt)
          <*> ((try (L.indentGuard scn EQ lvl *> singleSymbol "else") *> L.indentGuard scn GT lvl *> parseStmt) <|> return SNop)

parseFun :: Parser Stmt
parseFun = do
    (lvl, n, p) <- parseHsFoldColon (\sc' -> (,,) <$> L.indentLevel <* L.symbol sc' "fun" <*> parseName sc') stringToHsPat
    s <- (L.indentGuard scn GT lvl *> parseStmt)
    rest <- many (f <$> parseHsFoldColon (\sc' -> (,) <$> (L.indentGuard scn EQ lvl *> L.symbol sc' "fun" *> parseName sc')) stringToHsPat
                    <*> (L.indentGuard scn GT lvl *> parseStmt))
    SFun (M.fromList $ (n, (p, s)):rest) <$> (L.indentGuard scn EQ lvl *> parseStmt)
    where
    f (n, p) s = (n, (p, s))

parseBlock :: Parser Stmt
parseBlock = do
    lvl <- scn *> L.indentLevel <* singleSymbol "begin"
    SBlock <$> many (try $ L.indentGuard scn GT lvl *> parseBasicStmt)

parseCase :: Parser Stmt
parseCase = do
    (lvl, e) <- parseHsFold (\sc' -> (,) <$> L.indentLevel <* L.symbol sc' "case") stringToHsExp
    cs <- some $ (,) <$> parseHsFoldColon (\sc' -> L.indentGuard scn EQ lvl *> L.symbol sc' "|" *> return id) stringToHsPat
                     <*> (L.indentGuard scn GT lvl *> parseStmt)
    return $ SCase e cs

parseNop :: Parser Stmt
parseNop = singleSymbol "nop" *> return SNop

parseCall :: Parser Stmt
parseCall = (\vs -> SLet VarLet (TH.mkName "_") vs SNop) <$> parseHsFold (\sc' -> VCall <$> (L.symbol sc' "call" *> parseName sc')) stringToHsExp

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
             <|> parseCall

mkStmt :: [Stmt] -> Stmt
mkStmt [] = SNop
mkStmt [s] = s
mkStmt ss = SBlock ss

parseStmt :: Parser Stmt
parseStmt = do
    lvl <- scn *> L.indentLevel
    mkStmt <$> (some $ try $ L.indentGuard scn EQ lvl *> parseBasicStmt)

parseVStmt :: (Parser () -> Parser (VStmt -> a)) -> Parser a
parseVStmt pfx = parseHsFold (\sc' -> (.) <$> pfx sc' <*> (VCall <$> (L.symbol sc' "call" *> parseName sc') <|> return VExp)) stringToHsExp

parseProg :: Parser Prog
parseProg = prog <$> parseHsFold (\sc' -> L.indentGuard (return ()) EQ pos1 *> ((,) <$> parseName sc' <* L.symbol sc' "::")) stringToHsType
                 <*> many (parseHsFoldSymbol "param" stringToHsPat)
                 <*> parseHsFoldSymbol "inputs" stringToHsPat
                 <*> parseBasicStmt
    where prog = uncurry Prog

runParseProg :: String -> Either (ParseErrorBundle String Void) Prog
runParseProg = runParser parseProg ""

