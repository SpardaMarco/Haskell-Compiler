import Data.List

-- PFL 2023/24 - Haskell practical assignment quickstart

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data StackData = I Integer | B Bool deriving (Show, Eq)
type Stack = [StackData]

instance Ord StackData where
  compare (I n1) (I n2) = compare n1 n2
  compare (B b1) (B b2) = compare b1 b2
  compare (I _) (B _) = LT
  compare (B _) (I _) = GT

type StateData = (String, StackData)
type State = [StateData]

createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str (h:t) = case h of
    I i -> show i ++ "," ++ stack2Str t
    B b -> show b ++ "," ++ stack2Str t

createEmptyState :: State
createEmptyState = []

orderState :: State -> State
orderState [] = []
orderState s = sort s

state2Str :: State -> String
state2Str [] = ""
state2Str s =
    show (var, value) ++ "," ++ state2Str t
    where ((var,value) : t) = orderState s

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (inst : code, stack, state)
  | Push n <- inst = run (code, I n : stack, state)
  | Add <- inst = run (code, runBinaryOp (+) stack, state)
  | Mult <- inst = run (code, runBinaryOp (*) stack, state)
  | Sub <- inst = run (code, runBinaryOp (-) stack, state)
  | Tru <- inst = run (code, B True : stack, state)
  | Fals <- inst = run (code, B False : stack, state)
  | Equ <- inst = run (code, runBinaryBoolOp (==) stack, state)
  | Le <- inst = run (code, runBinaryBoolOp (<=) stack, state)
  | And <- inst = run (code, runBinaryBoolOp (&&) stack, state)
  | Neg <- inst = run (code, runUnaryOp not stack, state)
  | Fetch var <- inst = run (code, fetch var state : stack, state)
  | Store var <- inst = run (code, stack, (var, head stack) : state)
  | Noop <- inst = run (code, stack, state)
  | Branch code1 code2 <- inst = if head stack == B True then run (code1 ++ code, tail stack, state) else run (code2 ++ code, tail stack, state)
  | Loop code1 code2 <- inst = run (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]] ++ code, stack, state)
  | otherwise = error "Run-time error"

  where
    runBinaryOp :: (Integer -> Integer -> Integer) -> Stack -> Stack
    runBinaryOp op (I n1 : I n2 : stack) = I (n1 `op` n2) : stack
    runBinaryOp op _ = error "Run-time error"

    runBinaryBoolOp :: (Bool -> Bool -> Bool) -> Stack -> Stack
    runBinaryBoolOp op (B b1 : B b2 : stack) = B (b1 `op` b2) : stack
    runBinaryBoolOp op _ = error "Run-time error"

    runUnaryOp :: (Bool -> Bool) -> Stack -> Stack
    runUnaryOp op (B b : stack) = B (op b) : stack
    runUnaryOp op _ = error "Run-time error"

    fetch :: String -> State -> StackData
    fetch var [] = error "Run-time error"
    fetch var ((var', value) : t) = if var == var' then value else fetch var t


-- run :: (Code, Stack, State) -> (Code, Stack, State)
-- run ([], stack, state) = ([], stack, state)
-- run (inst : code, stack, state)
--   | Push n <- inst = run (code, push (I n) stack, state)
--   | Add <- inst = run (code, push (I (top stack + top (pop stack))) (pop (pop stack)), state)
--   | Mult <- inst = run (code, push (I (top stack * top (pop stack))) (pop (pop stack)), state)
--   | Sub <- inst = run (code, push (I (top stack - top (pop stack))) (pop (pop stack)), state)
--   | Tru <- inst = run (code, push (B True) stack, state)
--   | Fals <- inst = run (code, push (B False) stack, state)
--   | Equ <- inst = run (code, push (B (top stack == top (pop stack))) (pop (pop stack)), state)
--   | Le <- inst = run (code, push (B (top stack <= top (pop stack))) (pop (pop stack)), state)
--   | And <- inst = run (code, push (B (top stack && top (pop stack))) (pop (pop stack)), state)
--   | Neg <- inst = run (code, push (B (not (top stack))) (pop stack), state)
--   | Fetch var <- inst = run (code, push (fetch var state) stack, state)
--   | Store var <- inst = run (code, stack, (var, top stack) : state)
--   | Noop <- inst = run (code, stack, state)
--   | Branch code1 code2 <- inst = if top stack then run (code1 ++ code, pop stack, state) else run (code2 ++ code, pop stack, state)
--   | Loop code1 code2 <- inst = run (code1 ++ [Branch [code2, Loop code1 code2] [Noop]] ++ code, stack, state)
--   | otherwise = error "Run-time error"

--   where
--     push :: StackData -> Stack -> Stack
--     push val stack = val : stack

--     pop :: Stack -> Stack
--     pop [] = error "Run-time error"
--     pop (h:t) = t

--     top :: Stack -> StackData
--     top [] = error "Run-time error"
--     top (h:t) = h

--     fetch :: String -> State -> StackData
--     fetch var [] = error "Run-time error"
--     fetch var ((var', value) : t) = if var == var' then value else fetch var t


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)


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
-- testAssembler [Push 1,Push 2,And] == "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")