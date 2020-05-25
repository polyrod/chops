{-# Language TypeApplications #-}
module Chops.Parser where


import           Chops.AST
import           Data.List                              (mapAccumL)
import qualified Data.Map                               as M


import           Text.Parsec                            hiding (runParser, try)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "bgn"
                                     --, "para"
                                     , "sel"
                                     , "ld"
                                     , "bpm"
                                     , "sig"
                                     , "srst"
                                     , "fit"
                                     , "mrk"
                                     , "set"
                                     , "playf"
                                     , "playb"
                                     , "seek"
                                     , "wait"
                                     , "waitt"
                                     , "jdnz"
                                     , "jmp"
                                     , "onbt"
                                     , "stop"
                                     , "hlt"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/"]
           , Token.caseSensitive   = True
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
natural    = Token.natural    lexer -- parses an integer
float      = Token.float      lexer -- parses an integer
comma      = Token.comma      lexer -- parses an integer
colon      = Token.colon      lexer -- parses an integer
whiteSpace = Token.whiteSpace lexer -- parses whitespace
stringLiteral = Token.stringLiteral lexer -- parses whitespace

data MyParseState = MPS { _linenr   :: Integer
                        , _labelMap :: M.Map String Integer
                        }


prs = runParser program (MPS (-1) M.empty) "prog"  

program :: GenParser Char MyParseState [(Integer,Stmt)]
program = do
          whiteSpace
          ss <- many line
          eof
          m <- _labelMap <$> getState
          case resolveRefs m ss of
              Left x  ->  fail x
              Right x -> return x


resolveRefs :: M.Map String Integer
            -> [(Integer ,Either (String,String->Maybe Integer->(String,Stmt)) Stmt)]
            -> Either String [(Integer,Stmt)]
resolveRefs labelmap ss = let (acc,ss') = mapAccumL resolv [] ss
                          in if null acc
                              then Right ss'
                              else Left acc
  where
    resolv acc (i,Right x) = (acc,(i,x))
    resolv acc (i,Left (lab,f)) = let (acc',stmt') = f acc (M.lookup lab labelmap)
                                  in (acc',(i,stmt'))

line :: GenParser Char MyParseState (Integer,Either (String,String->Maybe Integer->(String,Stmt)) Stmt)
line = do
          modifyState (\mps -> mps { _linenr = _linenr mps +1 } )
          ln <- _linenr <$> getState
          optional lbl
          s <- stmt
          return (ln,s)

lbl = do
  lab <- identifier
  colon
  modifyState (\mps -> mps { _labelMap = M.insert lab (_linenr mps) (_labelMap mps) } )

stmt =   stmtJMP
     <|> stmtJDNZ
     <|> stmtBGN
     <|> stmtLD
     <|> stmtBPM
     <|> stmtSIG
     <|> stmtSEL
     <|> stmtSET
     <|> stmtWAITT
     <|> stmtWAIT
     <|> stmtHLT
     <|> stmtSRST
     <|> stmtFIT
     <|> stmtSEEK
     <|> stmtMRK
     <|> stmtPLAYF
     <|> stmtPLAYB
     <|> stmtSTOP

data TExpr = IE (Expr Integer) | FE (Expr Float)  | SE (Expr String)   

expr = try (FE <$> fexpr)
    <|> try (IE <$> iexpr)

fexpr = buildExpressionParser  foperators fterm
iexpr = buildExpressionParser  ioperators iterm
--sexpr = buildExpressionParser soperators sterm
--
foperators :: (Show a,Real a,Num a,Fractional a,VType a) => OperatorTable Char st (Expr a)
foperators = [-- [Prefix (reservedOp "-"   >> return (Neg             ))          ]
               [Infix  (reservedOp "*"   >> return (:*:)) AssocLeft ,
                Infix  (reservedOp "/"   >> return (:/:)) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (:+:)) AssocLeft,
                Infix  (reservedOp "-"   >> return (:-:)) AssocLeft]
             ]

ioperators :: (Show a,Real a,Num a,Integral a,VType a) => OperatorTable Char st (Expr a)
ioperators = [-- [Prefix (reservedOp "-"   >> return (Neg             ))          ]
               [Infix  (reservedOp "*"   >> return (:*:)) AssocLeft ,
                Infix  (reservedOp "/"   >> return (://:)) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (:+:)) AssocLeft,
                Infix  (reservedOp "-"   >> return (:-:)) AssocLeft]
             ]

fterm :: GenParser Char st (Expr Float)
fterm = choice [parens fexpr
               ,Con <$> do try $ ( realToFrac <$> float)
               ,Var <$> identifier]

iterm :: GenParser Char st (Expr Integer)
iterm = choice [parens iexpr
        ,Con <$> integer  
        ,Var <$> identifier
        ,fail "Expecting ITERM"]


stmtHLT = do
  reserved "hlt"
  return $ Right HLT

stmtSRST = do
  reserved "srst"
  return $ Right SRST

stmtBGN = do
  reserved "bgn"
  progname <- stringLiteral
  return $ Right $ BGN progname

stmtFIT = do
  reserved "fit"
  factor <- fexpr
  return $ Right $ FIT factor

stmtLD = do
  reserved "ld"
  progname <- stringLiteral
  return $ Right $ LD $ Con progname

intAsFloat = do
  i <- integer
  return $ realToFrac i

stmtBPM = do
  reserved "bpm"
  b <- try (realToFrac <$> float)
        <|> intAsFloat
        
  return $ Right $ BPM b

stmtSIG = do
  reserved "sig"
  beats <- integer
  comma
  res   <- integer
  return $ Right $ SIG (fromInteger beats) (fromInteger res)

stmtWAIT = do 
  reserved "wait"
  bars <- natural
  colon
  beats <- natural
  colon
  pulses <- natural
  return $ Right $ WAIT (bars,beats,pulses) 

stmtWAITT = do 
  reserved "waitt"
  bars <- natural
  colon
  beats <- natural
  colon
  pulses <- natural
  return $ Right $ WAITT (bars,beats,pulses) 

stmtJMP = do
  reserved "jmp"
  pos <- getPosition
  lab <- identifier
  m <- _labelMap <$> getState
  case M.lookup lab m of
    Nothing -> return $ Left (lab,\acc mi ->
                                    case mi of
                                      Nothing -> (acc ++ "\n" ++ show pos ++ ":\n" ++ "Label '" ++ lab ++ ":' does not exist !!! ",undefined)
                                      Just a -> (acc,JMP a))
    Just l -> return $  Right $ JMP l

stmtJDNZ = do
  reserved "jdnz"
  pos <- getPosition
  var <- identifier
  comma
  lab <- identifier
  m <- _labelMap <$> getState
  case M.lookup lab m of
    Nothing -> return $ Left (lab,\acc mi ->
                                    case mi of
                                      Nothing -> (acc ++ "\n" ++ show pos ++ ":\n" ++ "Label '" ++ lab ++ ":' does not exist !!! ",undefined)
                                      Just a -> (acc,JDNZ var a))
    Just l -> return $  Right $ JDNZ var l

stmtSEL = do
  reserved "sel"
  b <- identifier
  return $ Right $ SEL b

stmtSET = do
  reserved "set"
  b <- identifier
  comma
  v <- expr
  case v of
    FE a -> return $ Right $ SET @Float b a 
    IE a -> return $ Right $ SET @Integer b a 


stmtMRK = do
  reserved "mrk"
  b <- identifier
  comma
  v <- fexpr
  return $ Right $ MRK b v 

stmtPLAYF = do
  reserved "playf"
  m <- identifier
  return $ Right $ PLAYF m

stmtPLAYB = do
  reserved "playb"
  m <- identifier
  return $ Right $ PLAYB m

stmtSEEK = do
  reserved "seek"
  f <- fexpr
  return $ Right $ SEEK f

stmtSTOP = do
  reserved "stop"
  return $ Right $ STOP


