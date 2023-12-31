-- PFL 2023/24 - Haskell Practical Assignment Quickstart

import Data.Char
import Data.List
import GHC.Conc (STM(STM))

-- Part 1

-- Do not modify our definition of Inst and Code

-- Inst represents all available instructions to be run by the machine
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

-- Code is a list of Inst. It represents a set of instructions to be executed
type Code = [Inst]

-- StackData represents the different data types that can be held in the Stack type
data StackData
  = I Integer
  | B Bool
  deriving (Show, Eq)

-- Stack represents a custom stack-like data structure
type Stack = [StackData]

-- Ord StackData establishes how the StackData type is ordered
instance Ord StackData where
  compare :: StackData -> StackData -> Ordering
  compare (I n1) (I n2) = compare n1 n2
  compare (B b1) (B b2) = compare b1 b2
  compare _ _ = error "Run-time error"

-- StateData is a tuple that contains a String and a StackData, where the String represents a variable's name, and the StackData corresponds to its value
type StateData = (String, StackData)

-- State is a list of StateData. It represents the storage
type State = [StateData]

-- Creates an empty Stack
createEmptyStack :: Stack
createEmptyStack = []

-- Converts a stack into a string
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [h] = case h of
  I i -> show i
  B b -> show b
stack2Str (h : t) = case h of
  I i -> show i ++ "," ++ stack2Str t
  B b -> show b ++ "," ++ stack2Str t

-- Creates an empty State
createEmptyState :: State
createEmptyState = []

-- Converts a State into a String
state2StrAux :: State -> String
state2StrAux [] = ""
state2StrAux [(var, value)] = case value of
  I i -> var ++ "=" ++ show i
  B b -> var ++ "=" ++ show b
state2StrAux ((var, value) : state) = case value of
  I i -> var ++ "=" ++ show i ++ "," ++ state2StrAux state
  B b -> var ++ "=" ++ show b ++ "," ++ state2StrAux state

-- Sorts a State and converts it into a String
state2Str :: State -> String
state2Str = state2StrAux . sort

-- Executes a program (a list of instructions) for a given stack and state, returning the resulting stack and state
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (inst : code, stack, state) =
  case inst of
    Push n -> run (code, I n : stack, state)
    Add -> run (code, arithmeticOp (+) stack, state)
    Mult -> run (code, arithmeticOp (*) stack, state)
    Sub -> run (code, arithmeticOp (-) stack, state)
    Tru -> run (code, B True : stack, state)
    Fals -> run (code, B False : stack, state)
    Equ -> run (code, comparisonOp (==) stack, state)
    Le -> run (code, comparisonOp (<=) stack, state)
    And -> run (code, logicalOp (&&) stack, state)
    Neg -> run (code, unaryOp not stack, state)
    Fetch var -> run (code, fetch var state : stack, state)
    Store var -> run (code, tail stack, store var (head stack) state)
    Noop -> run (code, stack, state)
    Branch code1 code2 -> if head stack == B True then run (code1 ++ code, tail stack, state) else run (code2 ++ code, tail stack, state)
    Loop code1 code2 -> run (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]] ++ code, stack, state)
  where
    -- Auxiliary functions

    -- Executes an arithmetic operation on the top two elements of the stack
    arithmeticOp :: (Integer -> Integer -> Integer) -> Stack -> Stack
    arithmeticOp op (I n1 : I n2 : stack) = I (n1 `op` n2) : stack
    arithmeticOp op _ = error "Run-time error"

    -- Executes a comparison operation on the top two elements of the stack
    comparisonOp :: (StackData -> StackData -> Bool) -> Stack -> Stack
    comparisonOp op (v1 : v2 : stack) = case (v1, v2) of
      (I _, B _) -> error "Run-time error"
      (B _, I _) -> error "Run-time error"
      _ -> B (v1 `op` v2) : stack

    -- Executes a binary logical operation on the top two elements of the stack
    logicalOp :: (Bool -> Bool -> Bool) -> Stack -> Stack
    logicalOp op (B b1 : B b2 : stack) = B (b1 `op` b2) : stack
    logicalOp op _ = error "Run-time error"

    -- Executes an unary logical operation on the top element of the stack
    unaryOp :: (Bool -> Bool) -> Stack -> Stack
    unaryOp op (B b : stack) = B (op b) : stack
    unaryOp op _ = error "Run-time error"

    -- Stores a variable and its corresponding value in the storage. If the variable is already stored, its value is updated
    store :: String -> StackData -> State -> State
    store var value [] = [(var, value)]
    store var value ((var', value') : state)
      | var == var' = (var', value) : state
      | otherwise = (var', value') : store var value state

    -- Fetches a given variable's value from the storage. If the variable does not exist in the storage, raises an exception "Run-time error"
    fetch :: String -> State -> StackData
    fetch var [] = error "Run-time error"
    fetch var ((var', value) : t) = if var == var' then value else fetch var t

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

-- Aexp represents all possible arithmetic expressions
data Aexp
  = AddAx Aexp Aexp
  | MultAx Aexp Aexp
  | SubAx Aexp Aexp
  | NumAx Integer
  | VarAx String
  deriving (Eq, Show)

-- Bexp represents all possible boolean expressions
data Bexp
  = AndBx Bexp Bexp
  | EqBx Bexp Bexp
  | EqAx Aexp Aexp
  | NegBx Bexp
  | LeqAx Aexp Aexp
  | Bx Bool
  | VarBx String
  deriving (Eq, Show)

-- Stm represents all available statements
data Stm
  = AssignBx String Bexp
  | AssignAx String Aexp
  | Conditional Bexp Stm Stm
  | While Bexp Stm
  | Seq [Stm]
  deriving (Show)

-- Program is a list of Stm. It represents a set of statements
type Program = [Stm]

-- Compiles an arithmetic expression into a list of instructions
compA :: Aexp -> Code
compA aexp =
  case aexp of
    AddAx aexp1 aexp2 -> compA aexp2 ++ compA aexp1 ++ [Add]
    SubAx aexp1 aexp2 -> compA aexp2 ++ compA aexp1 ++ [Sub]
    MultAx aexp1 aexp2 -> compA aexp2 ++ compA aexp1 ++ [Mult]
    NumAx n -> [Push n]
    VarAx v -> [Fetch v]

-- Compiles a boolean expression into a list of instructions
compB :: Bexp -> Code
compB bexp =
  case bexp of
    AndBx b1 b2 -> compB b2 ++ compB b1 ++ [And]
    EqBx b1 b2 -> compB b2 ++ compB b1 ++ [Equ]
    EqAx a1 a2 -> compA a2 ++ compA a1 ++ [Equ]
    NegBx b -> compB b ++ [Neg]
    LeqAx a1 a2 -> compA a2 ++ compA a1 ++ [Le]
    Bx b -> if b then [Tru] else [Fals]
    VarBx v -> [Fetch v]

-- Compiles a program into a list of instructions
compile :: Program -> Code
compile [] = []
compile (stm : program) =
  case stm of
    AssignBx var bexp -> compB bexp ++ [Store var] ++ compile program
    AssignAx var aexp -> compA aexp ++ [Store var] ++ compile program
    Conditional bexp stm1 stm2 ->
      compB bexp ++ [Branch (compile [stm1]) (compile [stm2])] ++ compile program
    While bexp stm -> Loop (compB bexp) (compile [stm]) : compile program
    Seq stms -> compile stms ++ compile program

-- Token represents the type of tokens used by the lexer to parse the program
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

-- Converts a given string into a list of tokens
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
  | isLower c && isAlpha c = TokVar (c : takeWhile isAlphaNum rest) : lexer (dropWhile isAlphaNum rest)
  | otherwise = error "Run-time error"

-- Parser functions

-- The parser functions convert a given list of tokens into a tuple containing a parsed expression and the remaining tokens. If the parser functions fail, it returns Nothing

-- Parses integers or variables
-- Returns a tuple containing the parsed arithmetic expression and the remaining tokens
parseInt :: [Token] -> Maybe (Aexp, [Token])
parseInt (TokNum n : restTokens) = Just (NumAx n, restTokens)
parseInt (TokVar v : restTokens) = Just (VarAx v, restTokens)
parseInt _ = Nothing

-- Parses integers or parenthesized expressions
-- Returns a tuple containing the parsed arithmetic expression and the remaining tokens
parseIntOrParenExpr :: [Token] -> Maybe (Aexp, [Token])
parseIntOrParenExpr (TokCloseBracket : restTokens1) =
  case parseSumOrDiffOrProdOrIntOrPar restTokens1 of
    Just (expr, TokOpenBracket : restTokens2) -> Just (expr, restTokens2)
    Just _ -> Nothing
    Nothing -> Nothing
parseIntOrParenExpr tokens = parseInt tokens

-- Parses multiplications, integers, or parenthesized expressions
-- Returns a tuple containing the parsed arithmetic expression and the remaining tokens
parseProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrIntOrPar tokens =
  case parseIntOrParenExpr tokens of
    Just (expr1, TokMult : restTokens1) ->
      case parseProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (MultAx expr2 expr1, restTokens2)
        Nothing -> Nothing
    result -> result

-- Parses additions, subtractions, multiplications, integers, or parenthesized expressions
-- Returns a tuple containing the parsed arithmetic expression and the remaining tokens
parseSumOrDiffOrProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseSumOrDiffOrProdOrIntOrPar tokens =
  case parseProdOrIntOrPar tokens of
    Just (expr1, TokPlus : restTokens1) ->
      case parseSumOrDiffOrProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (AddAx expr2 expr1, restTokens2)
        Nothing -> Nothing
    Just (expr1, TokMinus : restTokens1) ->
      case parseSumOrDiffOrProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (SubAx expr2 expr1, restTokens2)
        Nothing -> Nothing
    result -> result

-- Boolean parser functions

-- Parses boolean literals, variables, or boolean expressions related to arithmetic expressions
-- Returns a tuple containing the parsed boolean expression and the remaining tokens
parseBool :: [Token] -> Maybe (Bexp, [Token])
parseBool (TokTrue : restTokens) = Just (Bx True, restTokens)
parseBool (TokFalse : restTokens) = Just (Bx False, restTokens)
parseBool (TokVar v : restTokens) = Just (VarBx v, restTokens)
parseBool tokens =
  case parseSumOrDiffOrProdOrIntOrPar tokens of
    Just (expr1, TokLeq : restTokens1) ->
      case parseSumOrDiffOrProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (LeqAx expr2 expr1, restTokens2)
    Just (expr1, TokAEq : restTokens1) ->
      case parseSumOrDiffOrProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (EqAx expr2 expr1, restTokens2)
    Nothing -> Nothing

-- Parses boolean literals, boolean expressions related to arithmetic expressions, or parenthesized boolean expressions
-- Returns a tuple containing the parsed boolean expression and the remaining tokens
parseBoolOrParExpr :: [Token] -> Maybe (Bexp, [Token])
parseBoolOrParExpr (TokCloseBracket : restTokens1) =
  case parseAndOrBEqOrNotOrBoolOrPar restTokens1 of
    Just (expr, TokOpenBracket : restTokens2) -> Just (expr, restTokens2)
    Just _ -> Nothing
    Nothing -> Nothing
parseBoolOrParExpr tokens = parseBool tokens

-- Parses boolean negations, boolean literals, boolean expressions related to arithmetic expressions, or parenthesized boolean expressions
-- Returns a tuple containing the parsed boolean expression and the remaining tokens
parseNotOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseNotOrBoolOrPar tokens =
  case parseBoolOrParExpr tokens of
    Just (expr1, TokNot : restTokens1) -> Just (NegBx expr1, restTokens1)
    result -> result

-- Parses boolean equalities, boolean negations, boolean literals, boolean expressions related to arithmetic expressions, or parenthesized boolean expressions
-- Returns a tuple containing the parsed boolean expression and the remaining tokens
parseBEqOrNotOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseBEqOrNotOrBoolOrPar tokens =
  case parseNotOrBoolOrPar tokens of
    Just (expr1, TokBEq : restTokens1) ->
      case parseBEqOrNotOrBoolOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (EqBx expr2 expr1, restTokens2)
        Nothing -> Nothing
    result -> result

-- Parses boolean conjunctions, boolean equalities, boolean negations, boolean literals, boolean expressions related to arithmetic expressions, or parenthesized boolean expressions
-- Returns a tuple containing the parsed boolean expression and the remaining tokens
parseAndOrBEqOrNotOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseAndOrBEqOrNotOrBoolOrPar tokens =
  case parseBEqOrNotOrBoolOrPar tokens of
    Just (expr1, TokAnd : restTokens1) ->
      case parseAndOrBEqOrNotOrBoolOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (AndBx expr2 expr1, restTokens2)
        Nothing -> Nothing
    result -> result

-- Top-level parser functions

-- Builds a boolean expression from a given list of tokens
buildBool :: [Token] -> Bexp
buildBool tokens =
  case parseAndOrBEqOrNotOrBoolOrPar (reverse tokens) of
    Just (expr, []) -> expr
    _ -> error "Run-time error"

-- Builds an arithmetic expression from a given list of tokens
buildArithmetic :: [Token] -> Aexp
buildArithmetic tokens =
  case parseSumOrDiffOrProdOrIntOrPar (reverse tokens) of
    Just (expr, []) -> expr
    _ -> error "Run-time error"

-- Checks if a given list of tokens represents a boolean expression
isBooleanExp :: [Token] -> Bool
isBooleanExp = any (\x -> x `elem` [TokTrue, TokFalse, TokNot, TokAnd, TokAEq, TokBEq, TokLeq])

-- Builds an assignment statement from a given list of tokens
buildAssignment :: [Token] -> String -> Stm
buildAssignment tokens var
  | isBooleanExp tokens = AssignBx var (buildBool tokens)
  | otherwise = AssignAx var (buildArithmetic tokens)

-- Builds a program from a given list of tokens
buildData :: [Token] -> [Stm]
buildData [] = []
buildData tokens =
  case tokens of
    (TokVar var : TokAssign : restTokens) -> case currentStatement restTokens [] of
      (stm, restTokens1) -> buildAssignment (init stm) var : buildData restTokens1
    (TokOpenBracket : restTokens) -> case currentStatement restTokens [TokOpenBracket] of
      (stm, restTokens1) -> Seq (buildData (init (init stm))) : buildData restTokens1
    (TokIf : restTokens) -> case getCondition TokThen restTokens of
      (bexp, restTokens1) -> case getCurrentStatement restTokens1 of
        (stm1, restTokens2) -> case getCurrentStatement restTokens2 of
          (stm2, restTokens3) -> Conditional bexp (head stm1) (head stm2) : buildData restTokens3
    (TokWhile : restTokens) -> case getCondition TokDo restTokens of
      (bexp, restTokens1) -> case getCurrentStatement restTokens1 of
        (stm, restTokens2) -> While bexp (head stm) : buildData restTokens2
    _ -> error "Run-time error" 
  
  where
    -- Auxiliary functions

    currentStatement :: [Token] -> [Token] -> ([Token], [Token])
    currentStatement (TokSemicolon : restTokens) [] = ([TokSemicolon], restTokens)
    currentStatement (TokOpenBracket : restTokens) stack = 
      let (current, next) = currentStatement restTokens (TokOpenBracket : stack)
      in (TokOpenBracket : current, next)
    currentStatement (TokCloseBracket : restTokens) (TokOpenBracket : stack) =
      let (current, next) = currentStatement restTokens stack
      in (TokCloseBracket : current, next)
    currentStatement (TokCloseBracket : restTokens) _ = error "Run-time error"
    currentStatement (TokIf : restTokens) stack = 
      let (current, next) = currentStatement restTokens (TokIf : stack)
      in (TokIf : current, next)
    currentStatement (TokThen : restTokens) (TokIf : stack) = 
      let (current, next) = currentStatement restTokens (TokThen : TokIf : stack)
      in (TokThen : current, next)
    currentStatement (TokThen : restTokens) _ = error "Run-time error"
    currentStatement (TokElse : restTokens) (TokThen : TokIf : stack) = 
      let (current, next) = currentStatement restTokens stack
      in (TokElse : current, next)
    currentStatement (TokElse : restTokens) _ = error "Run-time error"
    currentStatement (TokWhile : restTokens) stack = 
      let (current, next) = currentStatement restTokens (TokWhile : stack)
      in (TokWhile : current, next)
    currentStatement (TokDo : restTokens) (TokWhile : stack) = 
      let (current, next) = currentStatement restTokens stack
      in (TokDo : current, next)
    currentStatement (TokDo : restTokens) _ = error "Run-time error"
    currentStatement (token : restTokens) stack = 
      let (current, next) = currentStatement restTokens stack
      in (token : current, next)
    currentStatement [] stack = error "Run-time error"

    -- Wrapper functions to get the condition, the current statement and the following statements for conditional and while statements

    getCondition :: Token -> [Token] -> (Bexp, [Token])
    getCondition diffToken tokens = (buildBool (takeWhile (/= diffToken) tokens), dropWhile (/= diffToken) tokens)

    getCurrentStatement :: [Token] -> ([Stm], [Token])
    getCurrentStatement tokens = 
      let (current, rest) = currentStatement (tail tokens) []
      in (buildData current, rest)


-- Parses a given string into a program (list of statements) for the compiler to execute
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
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;); else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

-- Custom Tests:
-- testParser "x := not True;" == ("", "x=False")
-- testParser "x := True and False;" == ("", "x=False")
-- testParser "x := True and not False;"      == ("", "x=True")
-- testParser "x := not (True and False);" == ("", "x=True")
-- testParser "x := (True and not False) and (not False);" == ("", "x=True")
-- testParser "x := 2 <= 5;" == ("", "x=True")
-- testParser "x := 2 == 5;" == ("", "x=False")
-- testParser "x := 2 + 3 == 5;" == ("", "x=True")
-- testParser "x := not (3 <= 1) and 4 == 2+2;" == ("", "x=True")
-- testParser "x := not (3 <= 1);" == ("", "x=True")
-- testParser "x := True = False;" == ("", "x=False")
-- testParser "x := True = False and True = False;" == ("", "x=False")
-- testParser "x := True = (1 <= 2);" == ("", "x=True")
-- testParser "x := 1+2-3+10;" == ("","x=10")
-- testParser "x := ((1)+(2) * 3 - (4 * 5) + (((6)))) * 7;" == ("","x=-49")
-- testParser "x := (1 + 2 * 3 - 4 * 5 + 6) * 7;" == ("","x=-49")
-- testParser "x := (1 + (2 * 3) - (4 * 5) + 6) * 7;" == ("","x=-49")
-- testParser "x := ((1)+(2) * 3 - ((4 * 5) + (((6))))) * 7;" == ("","x=-133")
-- testParser "x := (1 + 2 * 3 - (4 * 5 + 6)) * 7;" == ("","x=-133")
-- testParser "if True then if False then x := 1; else x := 2; else x := 3;" == ("","x=2")
-- testParser "x := 0; while x <= 5 do (y := 0; while y <= 5 do (y := y + 1;); x := x + 1;);" == ("","x=6,y=6")
-- testParser "x := 1; while x <= 5 do if True then x := x + 1; else x := x + 2;" == ("","x=6")
-- testParser "x := 1; if False then x := 1; else while x <= 5 do x := x + 1;" == ("","x=6")
-- testParser "x := 1; if True then while x <= 5 do x := x + 1; else x := 1;" == ("","x=6")
-- testParser "(x := 1; y := 1; while (x <= 3) do (y := 2 * y; x := x + 1; z := 0; while (z <= 2) do (y := y + 1; z := z + 1;);););" == ("","x=4,y=29,z=3")
-- testParser "if True then (if False then x:=1; else ((x := 1; y := 1; while (x <= 3) do (y := 2 * y; x := x + 1; z := 0; while (z <= 2) do (y := y + 1; z := z + 1;);););w:=1;);); else y:=1;" == ("","w=1,x=4,y=29,z=3")


-- buildData :: [Token] -> [Stm]
-- buildData [] = []
-- buildData tokens =
--   case tokens of
--     (TokVar var : TokAssign : restTokens) ->
--       buildAssignment (init (currentStatementWrapper restTokens)) var
--         : buildData (nextStatementsWrapper restTokens)
--     (TokOpenBracket : restTokens) ->
--       Seq (buildData (init (init (tail (currentStatementWrapper (TokOpenBracket : restTokens))))))
--         : buildData (nextStatementsWrapper (TokOpenBracket : restTokens))
--     (TokIf : restTokens) ->
--       Conditional (getCondition TokThen restTokens) (Seq (buildData (getPartialStatementWrapper TokThen (TokIf : restTokens)))) (Seq (buildData (getPartialStatementWrapper TokElse (TokIf : restTokens))))
--         : buildData (nextStatementsWrapper (TokIf : restTokens))
--     (TokWhile : restTokens) ->
--       While (getCondition TokDo restTokens) (Seq (buildData (getPartialStatementWrapper TokDo (TokWhile : restTokens))))
--         : buildData (nextStatementsWrapper (TokWhile : restTokens))
--     _ -> error "Run-time error"

--   where
--     -- Auxiliary functions
--     nextStatementsWrapper :: [Token] -> [Token]
--     nextStatementsWrapper = nextStatements []

--     currentStatementWrapper :: [Token] -> [Token]
--     currentStatementWrapper = currentStatement []

--     -- Returns a list of tokens corresponding to the statements after the current statement
--     nextStatements :: [Token] -> [Token] -> [Token]
--     nextStatements [] (TokSemicolon : restTokens) = restTokens
--     nextStatements stack (TokSemicolon : restTokens) = nextStatements stack restTokens
--     nextStatements stack (TokOpenBracket : restTokens) = nextStatements (TokOpenBracket : stack) restTokens
--     nextStatements (TokOpenBracket : stack) (TokCloseBracket : restTokens) = nextStatements stack restTokens
--     nextStatements _ (TokCloseBracket : restTokens) = error "Run-time error"
--     nextStatements stack (TokIf : restTokens) = nextStatements (TokThen : stack) restTokens
--     nextStatements (TokThen : stack) (TokThen : restTokens) = nextStatements ( TokElse : stack) restTokens
--     nextStatements _ (TokThen : restTokens) = error "Run-time error"
--     nextStatements (TokElse : stack) (TokElse : restTokens) = nextStatements stack restTokens
--     nextStatements _ (TokElse : restTokens) = error "Run-time error"
--     nextStatements stack (TokWhile : restTokens) = nextStatements (TokDo : stack) restTokens
--     nextStatements (TokDo : stack) (TokDo : restTokens) = nextStatements stack restTokens
--     nextStatements _ (TokDo : restTokens) = error "Run-time error"
--     nextStatements stack (_ : restTokens) = nextStatements stack restTokens
--     nextStatements _ [] = error "Run-time error"

--     -- Returns a list of tokens corresponding to the current statement
--     currentStatement :: [Token] -> [Token] -> [Token]
--     currentStatement [] (TokSemicolon : restTokens) = [TokSemicolon]
--     currentStatement stack (TokSemicolon : restTokens) = TokSemicolon : currentStatement stack restTokens
--     currentStatement [] (TokOpenBracket : restTokens) = TokOpenBracket : currentStatement [TokOpenBracket] restTokens
--     currentStatement stack (TokOpenBracket : restTokens) = TokOpenBracket : currentStatement (TokOpenBracket : stack) restTokens
--     currentStatement (TokOpenBracket : stack) (TokCloseBracket : restTokens) = TokCloseBracket : currentStatement stack restTokens
--     currentStatement _ (TokCloseBracket : restTokens) = error "Run-time error"
--     currentStatement stack (TokIf : restTokens) = TokIf : currentStatement (TokThen : stack) restTokens
--     currentStatement (TokThen : stack) (TokThen : restTokens) = TokThen : currentStatement (TokElse : stack) restTokens
--     currentStatement _ (TokThen : restTokens) = error "Run-time error"
--     currentStatement (TokElse : stack) (TokElse : restTokens) = TokElse : currentStatement stack restTokens
--     currentStatement _ (TokElse : restTokens) = error "Run-time error"
--     currentStatement stack (TokWhile : restTokens) = TokWhile : currentStatement (TokDo : stack) restTokens
--     currentStatement (TokDo : stack) (TokDo : restTokens) = TokDo : currentStatement stack restTokens
--     currentStatement _ (TokDo : restTokens) = error "Run-time error"
--     currentStatement stack (token : restTokens) = token : currentStatement stack restTokens
--     currentStatement _ [] = error "Run-time error"

--     getCondition :: Token -> [Token] -> Bexp
--     getCondition diffToken tokens = buildBool (takeWhile (/= diffToken) tokens)

--     getPartialStatementWrapper :: Token -> [Token] -> [Token]
--     getPartialStatementWrapper diffToken (TokIf : restTokens) = currentStatementWrapper (getPartialStatement [TokThen] diffToken restTokens)
--     getPartialStatementWrapper diffToken (TokWhile : restTokens) = currentStatementWrapper (getPartialStatement [TokDo] diffToken restTokens)

--     getPartialStatement :: [Token] -> Token -> [Token] -> [Token]
--     getPartialStatement  [] _ tokens = tokens
--     getPartialStatement stack diffToken (TokSemicolon : restTokens) = getPartialStatement stack diffToken restTokens
--     getPartialStatement stack diffToken (TokOpenBracket : restTokens) = getPartialStatement (TokOpenBracket : stack) diffToken restTokens
--     getPartialStatement (TokOpenBracket : stack) diffToken (TokCloseBracket : restTokens) = getPartialStatement stack diffToken restTokens
--     getPartialStatement _ _ (TokCloseBracket : restTokens) = error "Run-time error"
--     getPartialStatement stack diffToken (TokIf : restTokens) = getPartialStatement (TokThen : stack) diffToken restTokens
--     getPartialStatement (TokThen : stack) TokThen (TokThen : restTokens) = getPartialStatement stack TokThen restTokens
--     getPartialStatement (TokThen : stack) diffToken (TokThen : restTokens) = getPartialStatement (TokElse : stack) diffToken restTokens
--     getPartialStatement _ _ (TokThen : restTokens) = error "Run-time error"
--     getPartialStatement (TokElse : stack) TokElse (TokElse : restTokens) = getPartialStatement stack TokElse restTokens
--     getPartialStatement (TokElse : stack) diffToken (TokElse : restTokens) = getPartialStatement stack diffToken restTokens
--     getPartialStatement _ _ (TokElse : restTokens) = error "Run-time error"
--     getPartialStatement stack diffToken (TokWhile : restTokens) = getPartialStatement (TokDo : stack) diffToken restTokens
--     getPartialStatement (TokDo : stack) TokDo (TokDo : restTokens) = getPartialStatement stack TokDo restTokens
--     getPartialStatement (TokDo : stack) diffToken (TokDo : restTokens) = getPartialStatement stack diffToken restTokens
--     getPartialStatement _ _ (TokDo : restTokens) = error "Run-time error"
--     getPartialStatement stack diffToken (token : restTokens) = getPartialStatement stack diffToken restTokens
--     getPartialStatement _ _ [] = error "Run-time error"

