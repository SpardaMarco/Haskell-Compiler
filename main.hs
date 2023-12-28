-- PFL 2023/24 - Haskell practical assignment quickstart

import Data.Char
import Data.List
import Distribution.Compat.CharParsing (CharParsing (string))
import Distribution.Simple.Setup (programDbOptions)

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst
  = Push Integer
  | Add
  | Mult
  | Sub
  | Tru
  | Fals
  | Equ
  | Le
  | And
  | Neg
  | Fetch String
  | Store String
  | Noop
  | Branch Code Code
  | Loop Code Code
  deriving (Show)

type Code = [Inst]

data StackData
  = I Integer
  | B Bool
  deriving (Show, Eq)

type Stack = [StackData]

instance Ord StackData where
  compare (I n1) (I n2) = compare n1 n2
  compare (B b1) (B b2) = compare b1 b2
  compare (I _) (B _) = error "Run-time error"
  compare (B _) (I _) = error "Run-time error"

push :: StackData -> Stack -> Stack
push val stack = val : stack

pop :: Stack -> Stack
pop [] = error "Run-time error"
pop (h : t) = t

top :: Stack -> StackData
top [] = error "Run-time error"
top (h : t) = h

type StateData = (String, StackData)

type State = [StateData]

fetch :: String -> State -> StackData
fetch var [] = error "Run-time error"
fetch var ((var', value) : t) = if var == var' then value else fetch var t

createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [h] = case h of
  I i -> show i
  B b -> show b
stack2Str (h : t) = case h of
  I i -> show i ++ "," ++ stack2Str t
  B b -> show b ++ "," ++ stack2Str t

createEmptyState :: State
createEmptyState = []

orderState :: State -> State
orderState [] = []
orderState s = sort s

state2StrAux :: State -> String
state2StrAux [] = ""
state2StrAux [(var, value)] = case value of
  I i -> var ++ "=" ++ show i
  B b -> var ++ "=" ++ show b
state2StrAux ((var, value) : state) = case value of
  I i -> var ++ "=" ++ show i ++ "," ++ state2StrAux state
  B b -> var ++ "=" ++ show b ++ "," ++ state2StrAux state

state2Str :: State -> String
state2Str state = state2StrAux (orderState state)

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (inst : code, stack, state)
  | Push n <- inst = run (code, I n : stack, state)
  | Add <- inst = run (code, arithmeticOp (+) stack, state)
  | Mult <- inst = run (code, arithmeticOp (*) stack, state)
  | Sub <- inst = run (code, arithmeticOp (-) stack, state)
  | Tru <- inst = run (code, B True : stack, state)
  | Fals <- inst = run (code, B False : stack, state)
  | Equ <- inst = run (code, comparisonOp (==) stack, state)
  | Le <- inst = run (code, comparisonOp (<=) stack, state)
  | And <- inst = run (code, logicalOp (&&) stack, state)
  | Neg <- inst = run (code, unaryOp not stack, state)
  | Fetch var <- inst = run (code, fetch var state : stack, state)
  | Store var <- inst = run (code, tail stack, store var (head stack) state)
  | Noop <- inst = run (code, stack, state)
  | Branch code1 code2 <- inst = if head stack == B True then run (code1 ++ code, tail stack, state) else run (code2 ++ code, tail stack, state)
  | Loop code1 code2 <- inst = run (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]] ++ code, stack, state)
  where
    arithmeticOp :: (Integer -> Integer -> Integer) -> Stack -> Stack
    arithmeticOp op (I n1 : I n2 : stack) = I (n1 `op` n2) : stack
    arithmeticOp op _ = error "Run-time error"

    comparisonOp :: (StackData -> StackData -> Bool) -> Stack -> Stack
    comparisonOp op (v1 : v2 : stack) = case (v1, v2) of
      (I _, B _) -> error "Run-time error"
      (B _, I _) -> error "Run-time error"
      _ -> B (v1 `op` v2) : stack

    logicalOp :: (Bool -> Bool -> Bool) -> Stack -> Stack
    label op (B b1 : B b2 : stack) = B (b1 `op` b2) : stack
    logicalOp op _ = error "Run-time error"

    unaryOp :: (Bool -> Bool) -> Stack -> Stack
    unaryOp op (B b : stack) = B (op b) : stack
    unaryOp op _ = error "Run-time error"

    store :: String -> StackData -> State -> State
    store var value [] = [(var, value)]
    store var value ((var', value') : state)
      | var == var' = (var', value) : state
      | otherwise = (var', value') : store var value state

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where
    (_, stack, state) = run (code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

data Aexp
  = AddAx Aexp Aexp
  | MultAx Aexp Aexp
  | SubAx Aexp Aexp
  | NumAx Integer
  | VarAx String
  deriving (Eq, Show)

data Bexp
  = AndBx Bexp Bexp
  | EqBx Bexp Bexp
  | EqAx Aexp Aexp
  | NegBx Bexp
  | LeqAx Aexp Aexp
  | Bx Bool
  | VarBx String
  deriving (Eq, Show)

data Stm
  = AssignBx String Bexp
  | AssignAx String Aexp
  | Conditional Bexp Program Program
  | While Bexp Program
  | Seq Stm Stm
  deriving (Show)

type Program = [Stm]

compA :: Aexp -> Code
compA aexp
  | (AddAx aexp1 aexp2) <- aexp = compA aexp2 ++ compA aexp1 ++ [Add]
  | (SubAx aexp1 aexp2) <- aexp = compA aexp2 ++ compA aexp1 ++ [Sub]
  | (MultAx aexp1 aexp2) <- aexp = compA aexp2 ++ compA aexp1 ++ [Mult]
  | (NumAx n) <- aexp = [Push n]
  | (VarAx v) <- aexp = [Fetch v]

compB :: Bexp -> Code
compB bexp
  | (AndBx b1 b2) <- bexp = compB b2 ++ compB b1 ++ [And]
  | (EqBx b1 b2) <- bexp = compB b2 ++ compB b1 ++ [Equ]
  | (EqAx a1 a2) <- bexp = compA a2 ++ compA a1 ++ [Equ]
  | (NegBx b) <- bexp = compB b ++ [Neg]
  | (LeqAx a1 a2) <- bexp = compA a2 ++ compA a1 ++ [Le]
  | (Bx b) <- bexp = if b then [Tru] else [Fals]
  | (VarBx v) <- bexp = [Fetch v]

compile :: Program -> Code
compile [] = []
compile (stm : program)
  | (AssignBx var bexp) <- stm = compB bexp ++ [Store var] ++ compile program
  | (AssignAx var aexp) <- stm = compA aexp ++ [Store var] ++ compile program
  | (Conditional bexp stm1 stm2) <- stm = compB bexp ++ [Branch (compile stm1) (compile stm2)] ++ compile program
  | (While bexp stm) <- stm = Loop (compB bexp) (compile stm) : compile program
  | (Seq stm1 stm2) <- stm = compile [stm1] ++ compile [stm2] ++ compile program

data Token
  = TokAssign
  | TokSemicolon
  | TokOpenBracket
  | TokCloseBracket
  | TokIf
  | TokThen
  | TokElse
  | TokWhile
  | TokDo
  | TokNot
  | TokAnd
  | TokOr
  | TokTrue
  | TokFalse
  | TokAEq
  | TokBEq
  | TokLeq
  | TokPlus
  | TokMinus
  | TokMult
  | TokNum Integer
  | TokVar String
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (':' : '=' : rest) = TokAssign : lexer rest
lexer ('+' : rest) = TokPlus : lexer rest
lexer ('-' : rest) = TokMinus : lexer rest
lexer ('*' : rest) = TokMult : lexer rest
lexer (';' : rest) = TokSemicolon : lexer rest
lexer ('(' : rest) = TokOpenBracket : lexer rest
lexer (')' : rest) = TokCloseBracket : lexer rest
lexer ('=' : '=' : rest) = TokAEq : lexer rest
lexer ('<' : '=' : rest) = TokLeq : lexer rest
lexer ('=' : rest) = TokBEq : lexer rest
lexer ('i' : 'f' : rest) = TokIf : lexer rest
lexer ('t' : 'h' : 'e' : 'n' : rest) = TokThen : lexer rest
lexer ('e' : 'l' : 's' : 'e' : rest) = TokElse : lexer rest
lexer ('w' : 'h' : 'i' : 'l' : 'e' : rest) = TokWhile : lexer rest
lexer ('d' : 'o' : rest) = TokDo : lexer rest
lexer ('n' : 'o' : 't' : rest) = TokNot : lexer rest
lexer ('a' : 'n' : 'd' : rest) = TokAnd : lexer rest
lexer ('o' : 'r' : rest) = TokOr : lexer rest
lexer ('T' : 'r' : 'u' : 'e' : rest) = TokTrue : lexer rest
lexer ('F' : 'a' : 'l' : 's' : 'e' : rest) = TokFalse : lexer rest
lexer (c : rest)
  | isDigit c = TokNum (read (c : takeWhile isDigit rest)) : lexer (dropWhile isDigit rest)
  | isSpace c = lexer rest
  | isLower c && isAlpha c = TokVar (c : takeWhile isAlphaNum rest) : lexer (dropWhile isAlphaNum rest) -- deviamos aceitar variaveis tipo x12?
  | otherwise = error "Run-time error"

parseInt :: [Token] -> Maybe (Aexp, [Token])
parseInt (TokNum n : restTokens) =
  Just (NumAx n, restTokens)
parseInt (TokVar v : restTokens) =
  Just (VarAx v, restTokens)
parseInt tokens =
  Nothing

parseProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseProdOrInt tokens =
  case parseInt tokens of
    Just (expr1, TokMult : restTokens1) -> case parseProdOrInt restTokens1 of
      Just (expr2, restTokens2) -> Just (MultAx expr1 expr2, restTokens2)
      Nothing -> Nothing
    result -> result

parseSumOrDiffOrProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseSumOrDiffOrProdOrInt tokens =
  case parseProdOrInt tokens of
    Just (expr1, TokPlus : restTokens1) -> case parseSumOrDiffOrProdOrInt restTokens1 of
      Just (expr2, restTokens2) -> Just (AddAx expr1 expr2, restTokens2)
      Nothing -> Nothing
    Just (expr1, TokMinus : restTokens1) -> case parseSumOrDiffOrProdOrInt restTokens1 of
      Just (expr2, restTokens2) -> Just (SubAx expr1 expr2, restTokens2)
      Nothing -> Nothing
    result -> result

parseIntOrParenExpr :: [Token] -> Maybe (Aexp, [Token])
parseIntOrParenExpr (TokOpenBracket : restTokens1) =
  case parseSumOrDiffOrProdOrInt restTokens1 of
    Just (expr, TokCloseBracket : restTokens2) -> Just (expr, restTokens2)
    Just _ -> Nothing
    Nothing -> Nothing
parseIntOrParenExpr tokens = parseInt tokens

parseProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrIntOrPar tokens =
  case parseIntOrParenExpr tokens of
    Just (expr1, TokMult : restTokens1) ->
      case parseProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (MultAx expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

parseSumOrDiffOrProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseSumOrDiffOrProdOrIntOrPar tokens =
  case parseProdOrIntOrPar tokens of
    Just (expr1, TokPlus : restTokens1) ->
      case parseSumOrDiffOrProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (AddAx expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr1, TokMinus : restTokens1) ->
      case parseSumOrDiffOrProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (SubAx expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

parseBool :: [Token] -> Maybe (Bexp, [Token])
parseBool (TokTrue : restTokens) = Just (Bx True, restTokens)
parseBool (TokFalse : restTokens) = Just (Bx False, restTokens)
parseBool (TokVar v : restTokens) = Just (VarBx v, restTokens)
parseBool tokens =
  case parseSumOrDiffOrProdOrIntOrPar tokens of
    Just (expr1, TokLeq : restTokens1) -> case parseSumOrDiffOrProdOrIntOrPar restTokens1 of
      Just (expr2, restTokens2) -> Just (LeqAx expr1 expr2, restTokens2)
    Just (expr1, TokAEq : restTokens1) -> case parseSumOrDiffOrProdOrIntOrPar restTokens1 of
      Just (expr2, restTokens2) -> Just (EqAx expr1 expr2, restTokens2)
    Nothing -> Nothing

parseNotOrBool :: [Token] -> Maybe (Bexp, [Token])
parseNotOrBool (TokNot : tokens) =
  case parseBool tokens of
    Just (expr1, restTokens1) -> Just (NegBx expr1, restTokens1)
    result -> result
parseNotOrBool tokens =
  case parseBool tokens of
    Just (expr1, restTokens1) -> Just (expr1, restTokens1)
    result -> result

parseEqOrNotOrBool :: [Token] -> Maybe (Bexp, [Token])
parseEqOrNotOrBool tokens =
  case parseNotOrBool tokens of
    Just (expr1, TokBEq : restTokens1) -> case parseEqOrNotOrBool restTokens1 of
      Just (expr2, restTokens2) -> Just (EqBx expr1 expr2, restTokens2)
      Nothing -> Nothing
    result -> result

parseAndOrEqOrNotOrBool :: [Token] -> Maybe (Bexp, [Token])
parseAndOrEqOrNotOrBool tokens =
  case parseEqOrNotOrBool tokens of
    Just (expr1, TokAnd : restTokens1) -> case parseAndOrEqOrNotOrBool restTokens1 of
      Just (expr2, restTokens2) -> Just (AndBx expr1 expr2, restTokens2)
      Nothing -> Nothing
    result -> result

parseBoolOrParExpr :: [Token] -> Maybe (Bexp, [Token])
parseBoolOrParExpr (TokOpenBracket : restTokens1) =
  case parseAndOrEqOrNotOrBool restTokens1 of
    Just (expr, TokCloseBracket : restTokens2) -> Just (expr, restTokens2)
    Just _ -> Nothing
    Nothing -> Nothing
parseBoolOrParExpr tokens = parseBool tokens

parseNotOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseNotOrBoolOrPar (TokNot : restTokens1) =
  case parseBoolOrParExpr restTokens1 of
    Just (expr, restTokens2) -> Just (NegBx expr, restTokens2)
    Nothing -> Nothing
parseNotOrBoolOrPar tokens = parseBool tokens

parseBEqOrNotOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseBEqOrNotOrBoolOrPar tokens =
  case parseNotOrBoolOrPar tokens of
    Just (expr1, TokBEq : restTokens1) -> case parseBEqOrNotOrBoolOrPar restTokens1 of
      Just (expr2, restTokens2) -> Just (EqBx expr1 expr2, restTokens2)
      Nothing -> Nothing
    result -> result

parseAndOrBEqOrNotOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseAndOrBEqOrNotOrBoolOrPar tokens =
  case parseBEqOrNotOrBoolOrPar tokens of
    Just (expr1, TokAnd : restTokens1) -> case parseAndOrBEqOrNotOrBoolOrPar restTokens1 of
      Just (expr2, restTokens2) -> Just (AndBx expr1 expr2, restTokens2)
      Nothing -> Nothing
    result -> result

buildBool :: [Token] -> Bexp
buildBool tokens =
  case parseAndOrBEqOrNotOrBoolOrPar tokens of
    Just (expr, []) -> expr
    _ -> error "Run-time error"

buildArithmetic :: [Token] -> Aexp
buildArithmetic tokens =
  case parseSumOrDiffOrProdOrIntOrPar tokens of
    Just (expr, []) -> expr
    _ -> error "Run-time error"

isBooleanExp :: [Token] -> Bool
isBooleanExp [] = False
isBooleanExp (exp : restTokens)
  | exp `elem` [TokTrue, TokFalse, TokNot, TokAnd, TokAEq, TokBEq, TokLeq] = True
  | otherwise = isBooleanExp restTokens

isBooleanExp2 :: [Token] -> Bool
isBooleanExp2 = any (\x -> x `elem` [TokTrue, TokFalse, TokNot, TokAnd, TokAEq, TokBEq, TokLeq])

buildExpression :: [Token] -> String -> Stm
buildExpression tokens var
  | isBooleanExp2 tokens = AssignBx var (buildBool tokens)
  | otherwise = AssignAx var (buildArithmetic tokens)

buildData :: [Token] -> Program
buildData [] = []
buildData tokens
  | (TokSemicolon : restTokens1) <- tokens = buildData restTokens1
  | (TokVar var : TokAssign : restTokens1) <- tokens = buildExpression (takeWhile (/= TokSemicolon) restTokens1) var : buildData (dropWhile (/= TokSemicolon) restTokens1)

parse :: String -> Program
parse = buildData . lexer

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where
    (_, stack, state) = run (compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")