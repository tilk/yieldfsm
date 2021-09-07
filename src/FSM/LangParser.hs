{-# LANGUAGE TupleSections #-}
module FSM.LangParser(runParseProg) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.Exts as HE
import qualified Language.Haskell.Meta as HM
import FSM.Lang
import FSM.FreeVars
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative hiding (many, some)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Lens hiding (noneOf)
import Prelude
import Data.Void
import Data.Char(isSpace)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

data PRData = PRData {
    _prDataVars :: M.Map TH.Name VarKind,
    _prDataLoop :: Bool
}

prData :: PRData
prData = PRData M.empty False

data PWData = PWData {
    _pwDataRet :: Bool
}

pwData :: PWData
pwData = PWData False

$(makeLenses ''PRData)
$(makeLenses ''PWData)

boundVarsEnv :: FreeVarsPat a => a -> M.Map TH.Name VarKind
boundVarsEnv = M.fromSet (const VarLet) . boundVars

instance Semigroup PWData where
    (PWData r1) <> (PWData r2) = PWData (r1 || r2)

instance Monoid PWData where
    mempty = pwData

type Parser = ReaderT PRData (WriterT PWData (ParsecT Void String TH.Q))

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

qlift :: TH.Q a -> Parser a
qlift = lift . lift . lift

e2m :: FreeVars a => Either String a -> Parser a
e2m (Left s) = fail s
e2m (Right r) = do
    vm <- view prDataVars
    forM_ (freeVars r `S.difference` M.keysSet vm) $ qlift . TH.reify
    return r

newlineOrEof :: Parser ()
newlineOrEof = (newline *> return ()) <|> eof

ssymbol :: String -> Parser String
ssymbol s = (try $ scn *> symbol s) <?> s

myDefaultParseMode :: HE.ParseMode
myDefaultParseMode = HE.defaultParseMode
  {HE.parseFilename = []
  ,HE.baseLanguage = HE.Haskell2010
  ,HE.extensions = map HE.EnableExtension myDefaultExtensions
  }

myDefaultExtensions :: [HE.KnownExtension]
myDefaultExtensions = [
    HE.PostfixOperators,
    HE.QuasiQuotes,
    HE.UnicodeSyntax,
    HE.PatternSignatures,
    HE.MagicHash,
    HE.ForeignFunctionInterface,
    HE.TemplateHaskell,
    HE.RankNTypes,
    HE.MultiParamTypeClasses,
    HE.RecursiveDo,
    HE.TypeApplications,
    HE.DataKinds]

parseHsType :: String -> Either String (HE.Type HE.SrcSpanInfo)
parseHsType = HM.parseResultToEither . HE.parseTypeWithMode myDefaultParseMode

parseHsExp :: String -> Either String (HE.Exp HE.SrcSpanInfo)
parseHsExp = HM.parseResultToEither . HE.parseExpWithMode myDefaultParseMode

parseHsPat :: String -> Either String (HE.Pat HE.SrcSpanInfo)
parseHsPat = HM.parseResultToEither . HE.parsePatWithMode myDefaultParseMode

stringToHsExp :: String -> Either String TH.Exp
stringToHsExp s = HM.toExp <$> parseHsExp s

stringToHsPat :: String -> Either String TH.Pat
stringToHsPat s = HM.toPat <$> parseHsPat s

stringToHsType :: String -> Either String TH.Type
stringToHsType s = HM.toType <$> parseHsType s

parseHsFoldGen :: FreeVars b => (Parser () -> Parser a) -> (Parser () -> Parser (b -> c)) -> (a -> Either String b) -> Parser c
parseHsFoldGen rest pfx p = try (lookAhead (scn *> pfx scn)) *> L.lineFold scn (\sc' ->
    (($) <$> pfx sc' <*> (e2m =<< p <$> rest sc')) <* sc)

parseHsFold :: FreeVars a => (Parser () -> Parser (a -> b)) -> (String -> Either String a) -> Parser b
parseHsFold = parseHsFoldGen (\sc' -> unwords <$> some (noneOf "\r\n") `sepBy1` try sc')

parseHsFoldColon :: FreeVars a => (Parser () -> Parser (a -> b)) -> (String -> Either String a) -> Parser b
parseHsFoldColon = parseHsFoldGen (\sc' -> unwords <$> some mysingle `sepBy1` try sc' <* single ':')
    where
    mysingle = try $ noneOf "\r\n" >>= \c -> return c <* (if c == ':' then notFollowedBy newline else return ())

parseHsFoldSymbol :: FreeVars a => String -> (String -> Either String a) -> Parser a
parseHsFoldSymbol s = parseHsFold (\sc' -> L.symbol sc' s >> return id)

--idStyle = haskellIdents { _styleReserved = HS.fromList ["nop", "var", "let", "emit", "ret", "call", "if", "fun", "else", "begin", "end", "case"] }

singleSymbol :: String -> Parser ()
singleSymbol s = ssymbol s *> newlineOrEof

parseName :: Parser () -> Parser TH.Name
parseName sc' = TH.mkName <$> ident sc'

parseVar :: Parser Stmt
parseVar = do
    (lvl, i, v) <- parseVStmt (\sc' -> (,,) <$> L.indentLevel <*> (L.symbol sc' "var" *> parseName sc' <* symbolic sc' '='))
    locally prDataVars (M.insert i VarMut) $ SLet VarMut i v <$> (L.indentGuard scn EQ lvl *> parseStmt)

parseAssign :: Parser Stmt
parseAssign = do
    (i, vs) <- parseVStmt (\sc' -> (,) <$> parseName sc' <* symbolic sc' '=')
    vm <- view prDataVars
    unless (M.lookup i vm == Just VarMut) $ fail $ "Invalid assignment target " ++ TH.nameBase i
    return $ SAssign i vs

parseLet :: Parser Stmt
parseLet = do
    (lvl, i, v) <- parseVStmt (\sc' -> (,,) <$> L.indentLevel <*> (L.symbol sc' "let" *> parseName sc' <* symbolic sc' '='))
    locally prDataVars (M.insert i VarLet) $ SLet VarLet i v <$> (L.indentGuard scn EQ lvl *> parseStmt)

parseEmit :: Parser Stmt
parseEmit = SEmit <$> parseHsFoldSymbol "emit" stringToHsExp

parseRet :: Parser Stmt
parseRet = mkRet =<< parseVStmt (\sc' -> L.symbol sc' "ret" >> return id)

mkRet :: VStmt -> Parser Stmt
mkRet vs = do
    b <- view prDataLoop
    scribe pwDataRet True
    if b then error "Return in loops currently unsupported" else return $ SRet vs

parseIf :: Parser Stmt
parseIf = do
    (lvl, e) <- parseHsFoldColon (\sc' -> (,) <$> L.indentLevel <* L.symbol sc' "if") stringToHsExp
    SIf e <$> (L.indentGuard scn GT lvl *> parseStmt)
          <*> ((try (L.indentGuard scn EQ lvl *> singleSymbol "else") *> L.indentGuard scn GT lvl *> parseStmt) <|> return SNop)

parseFun :: Parser Stmt
parseFun = do
    (lvl, n, p) <- parseHsFoldColon (\sc' -> (,,) <$> L.indentLevel <* L.symbol sc' "fun" <*> parseName sc') stringToHsPat
    first <- (L.indentGuard scn GT lvl *> parseFunBody n p)
    rest <- locally prDataLoop (const False) $ many $ do
        (n', p') <- parseHsFoldColon (\sc' -> (,) <$> (L.indentGuard scn EQ lvl *> L.symbol sc' "fun" *> parseName sc')) stringToHsPat
        L.indentGuard scn GT lvl *> parseFunBody n' p'
    SFun (M.fromList $ first:rest) <$> (L.indentGuard scn EQ lvl *> parseStmt)

parseFunBody :: TH.Name -> TH.Pat -> Parser (TH.Name, (TH.Pat, Stmt))
parseFunBody n p = do
    (s, r) <- locally prDataVars (M.union $ boundVarsEnv p) $ listening pwDataRet $ parseStmt
    return (n, (p, if r then s else SBlock [s, SRet $ VExp $ TH.TupE []]))

parseBlock :: Parser Stmt
parseBlock = do
    lvl <- scn *> L.indentLevel <* singleSymbol "begin"
    SBlock <$> many (try $ L.indentGuard scn GT lvl *> parseBasicStmt)

parseForever :: Parser Stmt
parseForever = censoring pwDataRet (const False) $ locally prDataLoop (const True) $ do
    lvl <- scn *> L.indentLevel <* singleSymbol "forever"
    ss <- many (try $ L.indentGuard scn GT lvl *> parseBasicStmt) -- TODO: ret handling
    f <- qlift $ TH.newName "forever"
    let scall = SRet $ VCall f $ TH.TupE []
    return $ SFun (M.singleton f (TH.TupP [], SBlock $ ss ++ [scall])) scall

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
             <|> parseForever
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
parseProg = do
    (i, t) <- parseHsFold (\sc' -> L.indentGuard (return ()) EQ pos1 *> ((,) <$> parseName sc' <* L.symbol sc' "::")) stringToHsType
    ps <- many (parseHsFoldSymbol "param" stringToHsPat)
    is <- parseHsFoldSymbol "inputs" stringToHsPat
    s <- locally prDataVars (M.union $ boundVarsEnv $ is:ps) $ parseBasicStmt
    return $ Prog i t ps is s

runParseProg :: String -> TH.Q (Either (ParseErrorBundle String Void) Prog)
runParseProg s = fmap fst <$> (runParserT (runWriterT (runReaderT parseProg prData)) "" s)

