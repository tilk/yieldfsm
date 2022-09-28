{-# LANGUAGE TupleSections #-}
{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

The parser.
-}
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
import Control.Lens hiding (noneOf)
import Prelude
import Data.Void
import qualified Data.Set as S
import qualified Data.Map.Strict as M

data PRData = PRData {
    _prDataInputs :: S.Set TH.Name,
    _prDataVars :: M.Map TH.Name VarKind
}

prData :: PRData
prData = PRData S.empty M.empty

$(makeLenses ''PRData)

boundVarsEnv :: FreeVarsPat a => a -> M.Map TH.Name VarKind
boundVarsEnv = M.fromSet (const VarLet) . boundVars

type Parser = ParsecT Void String (ReaderT PRData TH.Q)

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineComment empty

symbol :: Parser () -> String -> Parser String
symbol sc' = L.symbol sc' 

symbolic :: Parser () -> Char -> Parser Char
symbolic sc' = L.lexeme sc' . char

ident :: Parser () -> Parser String
ident sc' = L.lexeme sc' $ (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')

qlift :: TH.Q a -> Parser a
qlift = lift . lift

withoutPrims :: TH.Name -> TH.Name
withoutPrims = TH.mkName . reverse . dropWhile (== '\'') . reverse . TH.nameBase

e2m :: FreeVars a => Either String a -> Parser a
e2m (Left s) = fail s
e2m (Right r) = do
    vm <- view prDataVars
    is <- view prDataInputs
    forM_ (S.filter (not . (`S.member` is) . withoutPrims) $ freeVars r `S.difference` M.keysSet vm) $ qlift . TH.reify
    return r

newlineOrEof :: Parser ()
newlineOrEof = (newline *> return ()) <|> eof

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

hsParser :: FreeVars b => (Parser () -> Parser a) -> (a -> Either String b) -> Parser () -> Parser b
hsParser rest p sc' = e2m =<< p <$> rest sc'

hsRest :: Parser () -> Parser String
hsRest sc' = unwords <$> (some (noneOf "\r\n") `sepBy1` try sc')

hsRestColon :: Parser () -> Parser String
hsRestColon sc' = unwords <$> (some mysingle `sepBy1` try sc' <* single ':')
    where
    mysingle = try $ noneOf "\r\n" >>= \c -> return c <* (if c == ':' then notFollowedBy newline else return ())

parseHsFoldGen :: FreeVars b => (Parser () -> Parser b) -> (Parser () -> Parser (b -> c)) -> Parser c
parseHsFoldGen hp pfx = try (lookAhead (pfx scn)) *> L.lineFold scn (\sc' ->
    (($) <$> pfx sc' <*> hp sc') <* sc)

parseHsFold :: FreeVars a => (String -> Either String a) -> (Parser () -> Parser (a -> b)) -> Parser b
parseHsFold p = parseHsFoldGen (hsParser hsRest p)

parseHsFoldColon :: FreeVars a => (String -> Either String a) -> (Parser () -> Parser (a -> b)) -> Parser b
parseHsFoldColon p = parseHsFoldGen (hsParser hsRestColon p)

parseHsFoldSymbol :: FreeVars a => String -> (String -> Either String a) -> Parser a
parseHsFoldSymbol s = flip parseHsFold (\sc' -> L.symbol sc' s >> return id)

singleSymbol :: String -> Parser ()
singleSymbol s = symbol sc s *> newlineOrEof

singleSymbolColon :: String -> Parser ()
singleSymbolColon s = symbol sc s *> single ':' *> newlineOrEof

parseName :: Parser () -> Parser TH.Name
parseName sc' = TH.mkName <$> ident sc'

vStmtToExp :: (TH.Exp -> Stmt LvlSugared) -> VStmt -> Parser (Stmt LvlSugared)
vStmtToExp s (VExp e) = return $ s e
vStmtToExp s vs = do
    n <- qlift $ TH.newName "e"
    return $ SLet VarLet n vs (s $ TH.VarE n)

parseVar :: Parser (Stmt LvlSugared)
parseVar = do
    (lvl, i, ov) <- parseVStmtOptAssign (\sc' -> (,,) <$> L.indentLevel <*> (L.symbol sc' "var" *> parseName sc))
    locally prDataVars (M.insert i VarMut) $ SLet VarMut i (maybe (VExp $ TH.VarE 'undefined) id ov) <$> (L.indentGuard scn EQ lvl *> parseStmt)

parseAssign :: Parser (Stmt LvlSugared)
parseAssign = do
    (i, vs) <- parseVStmt (\sc' -> (,) <$> parseName sc' <* symbolic sc' '=')
    vm <- view prDataVars
    unless (M.lookup i vm == Just VarMut) $ fail $ "Invalid assignment target " ++ TH.nameBase i
    vStmtToExp (SAssign i) vs

parseLet :: Parser (Stmt LvlSugared)
parseLet = do
    (lvl, i, v) <- parseVStmt (\sc' -> (,,) <$> L.indentLevel <*> (L.symbol sc' "let" *> parseName sc' <* symbolic sc' '='))
    locally prDataVars (M.insert i VarLet) $ SLet VarLet i v <$> (L.indentGuard scn EQ lvl *> parseStmt)

parseNameList :: Parser () -> Parser (Maybe [TH.Name])
parseNameList sc' = Just <$> (symbolic sc' '<' *> parseName sc' `sepBy` symbolic sc' ',' <* symbolic sc' '>')
                <|> return Nothing

parseYield :: Parser (Stmt LvlSugared)
parseYield = do
    (ons, ovs) <- parseVStmtOpt (\_sc' -> (,) <$> (L.symbol sc "yield" *> parseNameList sc))
    case (ons, ovs) of
        (Nothing, Nothing) -> return $ SYieldO [] $ tupE []
        (Nothing, Just vs) -> vStmtToExp (SYieldO [TH.mkName "__default"]) vs
        (Just ns, _) -> vStmtToExp (SYieldO ns) $ maybe (VExp $ tupE []) id ovs

parseOutputStmt :: Parser (Stmt LvlSugared)
parseOutputStmt = do
    (ons, vs) <- parseVStmt (\sc' -> (,) <$> (L.symbol sc' "output" *> parseNameList sc'))
    case ons of
        Nothing -> vStmtToExp (SOutput [TH.mkName "__default"]) vs
        Just ns -> vStmtToExp (SOutput ns) vs

parseRet :: Parser (Stmt LvlSugared)
parseRet = SRet <$> parseVStmt (\sc' -> L.symbol sc' "ret" >> return id)

parseIf :: Parser (Stmt LvlSugared)
parseIf = do
    (lvl, e) <- parseHsFoldColon stringToHsExp (\sc' -> (,) <$> L.indentLevel <* L.symbol sc' "if")
    SIf e <$> (L.indentGuard scn GT lvl *> parseStmt)
          <*> parseElifElse lvl

parseElif :: Pos -> Parser (Stmt LvlSugared)
parseElif lvl = do
    SIf <$> parseHsFoldColon stringToHsExp (\sc' -> L.indentGuard scn EQ lvl *> L.symbol sc' "elif" *> return id)
        <*> (L.indentGuard scn GT lvl *> parseStmt)
        <*> parseElifElse lvl

parseElse :: Pos -> Parser (Stmt LvlSugared)
parseElse lvl = (try (L.indentGuard scn EQ lvl *> singleSymbolColon "else") *> L.indentGuard scn GT lvl *> parseStmt)

parseElifElse :: Pos -> Parser (Stmt LvlSugared)
parseElifElse lvl = parseElif lvl <|> parseElse lvl <|> return SNop

parseFun :: Parser (Stmt LvlSugared)
parseFun = do
    (lvl, n, p) <- parseHsFoldColon stringToHsPat (\sc' -> (,,) <$> L.indentLevel <* L.symbol sc' "fun" <*> parseName sc')
    first <- (L.indentGuard scn GT lvl *> parseFunBody n p)
    rest <- many $ do
        (n', p') <- parseHsFoldColon stringToHsPat (\sc' -> (,) <$> (L.indentGuard scn EQ lvl *> L.symbol sc' "fun" *> parseName sc'))
        L.indentGuard scn GT lvl *> parseFunBody n' p'
    SFun (M.fromList $ first:rest) <$> (L.indentGuard scn EQ lvl *> parseStmt)

parseFunBody :: TH.Name -> TH.Pat -> Parser (TH.Name, (TH.Pat, Stmt LvlSugared))
parseFunBody n p = do
    s <- locally prDataVars (M.union $ boundVarsEnv p) $ parseStmt
    return (n, (p, SBlock [s, SRet $ VExp $ TH.VarE 'undefined]))

parseBlock :: Parser (Stmt LvlSugared)
parseBlock = do
    lvl <- L.indentLevel <* singleSymbolColon "begin"
    SBlock <$> many (try $ L.indentGuard scn GT lvl *> parseBasicStmt)

parseForever :: Parser (Stmt LvlSugared)
parseForever = do
    lvl <- L.indentLevel <* singleSymbolColon "forever"
    ss <- parseLoopBody lvl
    return $ SLoop LoopForever $ SBlock ss

parseLoopBody :: Pos -> Parser [Stmt LvlSugared]
parseLoopBody lvl = many (try $ L.indentGuard scn GT lvl *> parseBasicStmt)

parseRepeat :: Parser (Stmt LvlSugared)
parseRepeat = do
    (lvl, e) <- parseHsFoldColon stringToHsExp (\sc' -> (,) <$> L.indentLevel <* L.symbol sc' "repeat")
    ss <- parseLoopBody lvl
    return $ SLoop (LoopRepeat IterWhile e) $ SBlock ss

parseRepeat1 :: Parser (Stmt LvlSugared)
parseRepeat1 = do
    (lvl, e) <- parseHsFoldColon stringToHsExp (\sc' -> (,) <$> L.indentLevel <* L.symbol sc' "repeat1")
    ss <- parseLoopBody lvl
    return $ SLoop (LoopRepeat IterDoWhile e) $ SBlock ss

parseWhileUntilHelp :: Parser () -> Parser (TH.Exp -> (WhileType, TH.Exp))
parseWhileUntilHelp sc' = (L.symbol sc' "while" *> return (WhileWhile,)) <|> (L.symbol sc' "until" *> return (WhileUntil,))

parseWhile :: Parser (Stmt LvlSugared)
parseWhile = do
    (lvl, (wt, e)) <- parseHsFoldColon stringToHsExp (\sc' -> (\a k -> (a,) . k) <$> L.indentLevel <*> parseWhileUntilHelp sc')
    ss <- parseLoopBody lvl
    return $ SLoop (LoopWhile IterWhile wt e) $ SBlock ss

parseDoWhile :: Parser (Stmt LvlSugared)
parseDoWhile = do
    lvl <- L.indentLevel <* singleSymbolColon "do"
    ss <- parseLoopBody lvl
    (wt, e) <- L.indentGuard scn EQ lvl *> parseHsFold stringToHsExp parseWhileUntilHelp
    return $ SLoop (LoopWhile IterDoWhile wt e) $ SBlock ss

parseCase :: Parser (Stmt LvlSugared)
parseCase = do
    (lvl, e) <- parseHsFold stringToHsExp (\sc' -> (,) <$> L.indentLevel <* L.symbol sc' "case")
    cs <- some $ do
        pat <- parseHsFoldColon stringToHsPat (\sc' -> L.indentGuard scn EQ lvl *> L.symbol sc' "|" *> return id)
        (pat,) <$> (locally prDataVars (M.union $ boundVarsEnv pat) $ L.indentGuard scn GT lvl *> parseStmt)
    return $ SCase e (addWildP cs)

addWildP :: [(TH.Pat, Stmt LvlSugared)] -> [(TH.Pat, Stmt LvlSugared)]
addWildP cs | any isCatchAllPat (map fst cs) = cs
            | otherwise = cs ++ [(TH.WildP, SNop)]

isCatchAllPat :: TH.Pat -> Bool
isCatchAllPat TH.WildP = True
isCatchAllPat (TH.VarP _) = True
isCatchAllPat (TH.TildeP _) = True
isCatchAllPat (TH.ParensP p) = isCatchAllPat p
isCatchAllPat (TH.BangP p) = isCatchAllPat p
isCatchAllPat (TH.AsP _ p) = isCatchAllPat p
isCatchAllPat (TH.SigP p _) = isCatchAllPat p
isCatchAllPat _ = False

parseNop :: Parser (Stmt LvlSugared)
parseNop = singleSymbol "skip" *> return SNop

parseContinue :: Parser (Stmt LvlSugared)
parseContinue = singleSymbol "continue" >> return (SBreak BrkCont)

parseBreak :: Parser (Stmt LvlSugared)
parseBreak = singleSymbol "break" >> return (SBreak BrkBrk)

parseCall :: Parser (Stmt LvlSugared)
parseCall = (\vs -> SLet VarLet (TH.mkName "_") vs SNop) <$> parseHsFold stringToHsExp (\sc' -> VCall <$> (L.symbol sc' "call" *> parseName sc'))

parseBasicStmt :: Parser (Stmt LvlSugared)
parseBasicStmt = parseVar
             <|> parseLet
             <|> parseYield
             <|> parseOutputStmt
             <|> parseRet
             <|> parseIf
             <|> parseCase
             <|> parseFun
             <|> parseForever
             <|> parseRepeat1
             <|> parseRepeat
             <|> parseBlock
             <|> parseNop
             <|> parseAssign
             <|> parseCall
             <|> parseContinue
             <|> parseBreak
             <|> parseWhile
             <|> parseDoWhile

mkStmt :: [Stmt LvlSugared] -> Stmt LvlSugared
mkStmt [] = SNop
mkStmt [s] = s
mkStmt ss = SBlock ss

parseStmt :: Parser (Stmt LvlSugared)
parseStmt = do
    lvl <- L.indentLevel
    mkStmt <$> (some $ try (L.indentGuard scn EQ lvl >> notFollowedBy eof) *> parseBasicStmt)

parseVStmtOptAssign :: (Parser () -> Parser (Maybe VStmt -> a)) -> Parser a
parseVStmtOptAssign pfx = parseHsFoldGen
    (\sc' -> (Just <$> (L.symbol sc' "=" *> hsParser hsRest stringToHsExp sc')) <|> return Nothing)
    (\sc' -> (.) <$> pfx sc' <*> (fmap . VCall <$> (L.symbol sc' "call" *> parseName sc') <|> return (fmap VExp)))

parseVStmtOpt :: (Parser () -> Parser (Maybe VStmt -> a)) -> Parser a
parseVStmtOpt pfx = parseHsFoldGen
    (\sc' -> (Just <$> hsParser hsRest stringToHsExp sc') <|> return Nothing)
    (\sc' -> (.) <$> pfx sc' <*> (fmap . VCall <$> (L.symbol sc' "call" *> parseName sc') <|> return (fmap VExp)))

parseVStmt :: (Parser () -> Parser (VStmt -> a)) -> Parser a
parseVStmt pfx = parseHsFold stringToHsExp (\sc' -> (.) <$> pfx sc' <*> (VCall <$> (L.symbol sc' "call" *> parseName sc') <|> return VExp))

parseOutput :: Parser (TH.Name, Output)
parseOutput = try $ L.nonIndented scn $ parseHsFoldGen
    (\sc' -> (Just <$> (L.symbol sc' "=" *> hsParser hsRest stringToHsExp sc')) <|> return Nothing)
    (\sc' -> (\a oe -> (a, Output $ maybe (TH.VarE 'undefined) id oe)) <$> (L.symbol sc' "output" *> parseName sc))

parseProg :: Parser (Prog LvlSugared)
parseProg = do
    (i, t) <- parseHsFold stringToHsType (\sc' -> L.indentGuard (return ()) EQ pos1 *> ((,) <$> parseName sc' <* L.symbol sc' "::"))
    ps <- many (try $ L.nonIndented scn $ parseHsFoldSymbol "param" stringToHsPat)
    is <- (try $ L.nonIndented scn $ Just <$> parseHsFoldSymbol "input" stringToHsPat) <|> return Nothing
    os <- many parseOutput
    s <- locally prDataInputs (S.union $ boundVars is) $ locally prDataVars (M.union $ boundVarsEnv is `M.union` boundVarsEnv ps) $ L.nonIndented scn $ parseBasicStmt
    scn *> eof
    return $ Prog i t ps is (if null os then [(TH.mkName "__default", Output (TH.VarE 'undefined))] else os) s

-- | Parser function.
runParseProg :: String -> TH.Q (Either (ParseErrorBundle String Void) (Prog LvlSugared))
runParseProg s = runReaderT (runParserT parseProg "" s) prData

