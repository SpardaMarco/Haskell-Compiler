-- PFL 2023/24 - Haskell practical assignment quickstart

import Data.Char
import Data.List
import Distribution.Compat.CharParsing (CharParsing (string))
import Distribution.Simple.Setup (programDbOptions)

-- Part 1

-- Do not modify our definition of Inst and Code
-- Inst is the type of instructions.
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

-- The code of the machine is a list of instructions
type Code = [Inst]

-- StackData is a type that can hold either an Integer or a Bool
data StackData
  = I Integer
  | B Bool
  deriving (Show, Eq)

-- Stack is a list of StackData
type Stack = [StackData]

-- We define the order of StackData so that we can use "<=" operator with StackData
instance Ord StackData where
  compare (I n1) (I n2) = compare n1 n2
  compare (B b1) (B b2) = compare b1 b2
  compare (I _) (B _) = error "Run-time error"
  compare (B _) (I _) = error "Run-time error"

-- StateData is a pair of a String and a StackData, where the String is the name of a variable and the StackData is its value
type StateData = (String, StackData)

-- State is a list of StateData
type State = [StateData]

-- Creates an empty stack
createEmptyStack :: Stack
createEmptyStack = []

-- Transforms a stack into a string.
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [h] = case h of
  I i -> show i
  B b -> show b
stack2Str (h : t) = case h of
  I i -> show i ++ "," ++ stack2Str t
  B b -> show b ++ "," ++ stack2Str t

-- Creates an empty state
createEmptyState :: State
createEmptyState = []

-- Auxiliary function to transform a state into a string.
state2StrAux :: State -> String
state2StrAux [] = ""
state2StrAux [(var, value)] = case value of
  I i -> var ++ "=" ++ show i
  B b -> var ++ "=" ++ show b
state2StrAux ((var, value) : state) = case value of
  I i -> var ++ "=" ++ show i ++ "," ++ state2StrAux state
  B b -> var ++ "=" ++ show b ++ "," ++ state2StrAux state

-- Transforms a state into a string.
state2Str :: State -> String
state2Str state = state2StrAux (sort state)

-- Executes a program (a list of instructions) on a stack and a state, returning a new stack and a new state, and possibly raising an exception "Run-time error". 
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

    -- Executes a logical operation on the top two elements of the stack
    logicalOp :: (Bool -> Bool -> Bool) -> Stack -> Stack
    logicalOp op (B b1 : B b2 : stack) = B (b1 `op` b2) : stack
    logicalOp op _ = error "Run-time error"

    -- Executes a unary operation on the top element of the stack
    unaryOp :: (Bool -> Bool) -> Stack -> Stack
    unaryOp op (B b : stack) = B (op b) : stack
    unaryOp op _ = error "Run-time error"

    -- Stores a variable value pair in the state (if the variable already exists, its value is updated).
    store :: String -> StackData -> State -> State
    store var value [] = [(var, value)]
    store var value ((var', value') : state)
      | var == var' = (var', value) : state
      | otherwise = (var', value') : store var value state

    -- Fetches a value from the state. If the variable does not exist, raises an exception "Run-time error".
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
-- Aexp is the type of arithmetic expressions.
data Aexp
  = AddAx Aexp Aexp
  | MultAx Aexp Aexp
  | SubAx Aexp Aexp
  | NumAx Integer
  | VarAx String
  deriving (Eq, Show)

-- Bexp is the type of boolean expressions.
data Bexp
  = AndBx Bexp Bexp
  | EqBx Bexp Bexp
  | EqAx Aexp Aexp
  | NegBx Bexp
  | LeqAx Aexp Aexp
  | Bx Bool
  | VarBx String
  deriving (Eq, Show)

-- Stm is the type of statements, this encompasses both arithmetic and boolean assignments, conditionals, while loops and sequences of statements.
data Stm
  = AssignBx String Bexp
  | AssignAx String Aexp
  | Conditional Bexp Program Program
  | While Bexp Program
  | Seq [Stm]
  deriving (Show)

-- Program is a list of statements.
type Program = [Stm]

-- Compiles an arithmetic expression into a list of instructions.
compA :: Aexp -> Code
compA aexp =
  case aexp of
    AddAx aexp1 aexp2 -> compA aexp2 ++ compA aexp1 ++ [Add]
    SubAx aexp1 aexp2 -> compA aexp2 ++ compA aexp1 ++ [Sub]
    MultAx aexp1 aexp2 -> compA aexp2 ++ compA aexp1 ++ [Mult]
    NumAx n -> [Push n]
    VarAx v -> [Fetch v]

-- Compiles a boolean expression into a list of instructions.
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

-- Compiles a program into a list of instructions.
compile :: Program -> Code
compile [] = []
compile (stm : program) =
  case stm of
    AssignBx var bexp -> compB bexp ++ [Store var] ++ compile program
    AssignAx var aexp -> compA aexp ++ [Store var] ++ compile program
    Conditional bexp stm1 stm2 ->
      compB bexp ++ [Branch (compile stm1) (compile stm2)] ++ compile program
    While bexp stm -> Loop (compB bexp) (compile stm) : compile program
    Seq stms -> compile stms ++ compile program

-- Token is the type of tokens used by the lexer to parse the program.
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

-- Function that receives a string and returns a list of tokens. This function is used by the parser to parse the program.
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
-- The parser functions receive a list of tokens and return a pair of an expression and the remaining tokens.
-- If the parser fails, it returns Nothing.

-- Parses an integer or variable token.
-- Returns a tuple containing the parsed arithmetic expression and the remaining tokens.
parseInt :: [Token] -> Maybe (Aexp, [Token])
parseInt (TokNum n : restTokens) = Just (NumAx n, restTokens)
parseInt (TokVar v : restTokens) = Just (VarAx v, restTokens)
parseInt _ = Nothing

-- Parses an integer or a parenthesized expression.
--Returns a tuple containing the parsed arithmetic expression and the remaining tokens.
parseIntOrParenExpr :: [Token] -> Maybe (Aexp, [Token])
parseIntOrParenExpr (TokCloseBracket : restTokens1) =
  case parseSumOrDiffOrProdOrIntOrPar restTokens1 of
    Just (expr, TokOpenBracket : restTokens2) -> Just (expr, restTokens2)
    Just _ -> Nothing
    Nothing -> Nothing
parseIntOrParenExpr tokens = parseInt tokens

-- Parses multiplication, division, or an integer or a parenthesized expression.
-- Returns a tuple containing the parsed arithmetic expression and the remaining tokens.
parseProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrIntOrPar tokens =
  case parseIntOrParenExpr tokens of
    Just (expr1, TokMult : restTokens1) ->
      case parseProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (MultAx expr2 expr1, restTokens2)
        Nothing -> Nothing
    result -> result

-- Parses addition, subtraction, multiplication, division, integer, or parenthesized expression.
-- Returns a tuple containing the parsed arithmetic expression and the remaining tokens.
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

-- Parses boolean literals, variable tokens or boolean expressions related to arithmetic expressions.
-- Returns a tuple containing the parsed boolean expression and the remaining tokens.
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

-- Parses boolean literals, variable tokens, boolean expressions related to arithmetic expressions, or a parenthesized boolean expression.
-- Returns a tuple containing the parsed boolean expression and the remaining tokens.
parseBoolOrParExpr :: [Token] -> Maybe (Bexp, [Token])
parseBoolOrParExpr (TokCloseBracket : restTokens1) =
  case parseAndOrBEqOrNotOrBoolOrPar restTokens1 of
    Just (expr, TokOpenBracket : restTokens2) -> Just (expr, restTokens2)
    Just _ -> Nothing
    Nothing -> Nothing
parseBoolOrParExpr tokens = parseBool tokens

-- Parses negation or boolean literals, variable tokens, boolean expressions related to arithmetic expressions, or a parenthesized boolean expression.
-- Returns a tuple containing the parsed boolean expression and the remaining tokens.
parseNotOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseNotOrBoolOrPar tokens =
  case parseBoolOrParExpr tokens of
    Just (expr1, TokNot : restTokens1) -> Just (NegBx expr1, restTokens1)
    result -> result

-- Parses boolean equality, negation, boolean literals, variable tokens, boolean expressions related to arithmetic expressions, or a parenthesized boolean expression.
-- Returns a tuple containing the parsed boolean expression and the remaining tokens.
parseBEqOrNotOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseBEqOrNotOrBoolOrPar tokens =
  case parseNotOrBoolOrPar tokens of
    Just (expr1, TokBEq : restTokens1) ->
      case parseBEqOrNotOrBoolOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (EqBx expr2 expr1, restTokens2)
        Nothing -> Nothing
    result -> result

-- Parses AND, boolean equality, negation, boolean literals, variable tokens, boolean expressions related to arithmetic expressions, or a parenthesized boolean expression.
-- Returns a tuple containing the parsed boolean expression and the remaining tokens.
parseAndOrBEqOrNotOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseAndOrBEqOrNotOrBoolOrPar tokens =
  case parseBEqOrNotOrBoolOrPar tokens of
    Just (expr1, TokAnd : restTokens1) ->
      case parseAndOrBEqOrNotOrBoolOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (AndBx expr2 expr1, restTokens2)
        Nothing -> Nothing
    result -> result

-- Top-level parser functions

-- Builds a boolean expression from a list of tokens.
buildBool :: [Token] -> Bexp
buildBool tokens =
  case parseAndOrBEqOrNotOrBoolOrPar (reverse tokens) of
    Just (expr, []) -> expr
    _ -> error "Run-time error"

-- Builds an arithmetic expression from a list of tokens.
buildArithmetic :: [Token] -> Aexp
buildArithmetic tokens =
  case parseSumOrDiffOrProdOrIntOrPar (reverse tokens) of
    Just (expr, []) -> expr
    _ -> error "Run-time error"

-- Auxiliary function to check if a list of tokens contains a boolean expression. This is used to decide whether to build a boolean assignment or an arithmetic assignment.
isBooleanExp :: [Token] -> Bool
isBooleanExp = any (\x -> x `elem` [TokTrue, TokFalse, TokNot, TokAnd, TokAEq, TokBEq, TokLeq])

-- Builds an assignment from a list of tokens.
buildAssignment :: [Token] -> String -> Stm
buildAssignment tokens var
  | isBooleanExp tokens = AssignBx var (buildBool tokens)
  | otherwise = AssignAx var (buildArithmetic tokens)

-- Builds a program from a list of tokens. This is responsible for calling the other functions to build the program. 
buildData :: [Token] -> Program
buildData [] = []
buildData tokens =
  case tokens of
    (TokVar var : TokAssign : restTokens) ->
      buildAssignment (init (currentStatement 0 restTokens)) var
        : buildData (nextStatements 0 restTokens)
    (TokOpenBracket : restTokens) ->
      Seq (buildData (init (init (currentStatement 1 restTokens))))
        : buildData (nextStatements 1 restTokens)
    (TokIf : restTokens) ->
      Conditional (getCondition TokThen restTokens) (getCurrentStatement TokThen restTokens) (getCurrentStatement TokElse restTokens)
        : getnextStatements TokElse restTokens
    (TokWhile : restTokens) ->
      While (getCondition TokDo restTokens) (getCurrentStatement TokDo restTokens)
        : getnextStatements TokDo restTokens
    _ -> error "Run-time error"

  where
    -- Auxiliary functions
    -- Returns the list of tokens corresponding the statements after the current statement.
    nextStatements :: Int -> [Token] -> [Token]
    nextStatements 0 (TokSemicolon : restTokens) = restTokens
    nextStatements n (TokOpenBracket : restTokens) = nextStatements (n + 1) restTokens
    nextStatements n (TokCloseBracket : restTokens) = nextStatements (n - 1) restTokens
    nextStatements n (_ : restTokens) = nextStatements n restTokens
    nextStatements _ [] = error "Run-time error"

    -- Returns the list of tokens corresponding to the current statement.
    currentStatement :: Int -> [Token] -> [Token]
    currentStatement 0 (TokSemicolon : restTokens) = [TokSemicolon]
    currentStatement n (TokOpenBracket : restTokens) = TokOpenBracket : currentStatement (n + 1) restTokens
    currentStatement n (TokCloseBracket : restTokens) = TokCloseBracket : currentStatement (n - 1) restTokens
    currentStatement n (token : restTokens) = token : currentStatement n restTokens 
    currentStatement _ [] = error "Run-time error"

    -- Wrapper functions to get the condition, current statement and next statements for conditional and while statements.
    getCondition :: Token -> [Token] -> Bexp
    getCondition diffToken tokens = buildBool (takeWhile (/= diffToken) tokens)

    getCurrentStatement :: Token -> [Token] -> [Stm]
    getCurrentStatement diffToken tokens =
      buildData (currentStatement 0 (tail (dropWhile (/= diffToken) tokens)))

    getnextStatements :: Token -> [Token] -> [Stm]
    getnextStatements diffToken tokens =
      buildData (nextStatements 0 (tail (dropWhile (/= diffToken) tokens)))

-- Parses a string (representing a program) into a program for the compiler.
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
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34") ********************************
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
-- testParser "x := ((1)+(2) * 3 - (4 * 5) + (((6)))) * 7;" == ("","x=-49")
-- testParser "x := (1 + 2 * 3 - 4 * 5 + 6) * 7;" == ("","x=-49")
-- testParser "x := (1 + (2 * 3) - (4 * 5) + 6) * 7;" == ("","x=-49")

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
-- testParser "x := ((1)+(2) * 3 - ((4 * 5) + (((6))))) * 7;" == ("","x=-133")
-- testParser "x := (1 + 2 * 3 - (4 * 5 + 6)) * 7;" == ("","x=-133")