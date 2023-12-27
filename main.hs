import Data.List
-- PFL 2023/24 - Haskell practical assignment quickstart

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data StackData = I Integer | B Bool deriving Show
type Stack = [StackData]

-- create type StateData and State, where state is a hashmap which the key is a string and the value is a value from StackData
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
    show (var, value) ++ "," state2Str t
    where ((var,value) : t) = orderState s

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (inst : code, stack, state)
  | inst == Push  = run (code, push (top stack + top (pop stack)) (pop (pop stack)), state)
  | inst == Add = run (code, push (top stack + top (pop stack)) (pop (pop stack)), state)
  | inst == Mult = run (code, push (top stack * top (pop stack)) (pop (pop stack)), state)
  | inst == Sub = run (code, push (top stack - top (pop stack)) (pop (pop stack)), state)
  | inst == Tru = run (code, push True stack, state)
  | inst == Fals = run (code, push False stack, state)
  | inst == Equ = run (code, push (top stack == top (pop stack) (pop (pop stack)), state))
  | inst == Le = run (code, push (top stack <= top (pop stack)) (pop (pop stack)), state)
  | inst == And = run (code, push (top stack <= top (pop stack) (pop (pop stack))), state)
  | inst == Neg = run (code, push (not (top stack)) (pop stack), state)


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

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
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")