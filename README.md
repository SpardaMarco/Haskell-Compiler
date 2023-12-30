# Functional and Logic Programming - 2nd Pratical Assignment

## Group Description

Group Name: **T03_G10**

Group Members:

- **[João Filipe Oliveira Ramos](https://github.com/11Ramos11)** - up202108743 - 33,(3)% contribution
- **[Marco André Pereira da Costa](https://github.com/SpardaMarco)** - up202108821 - 33,(3)% contribution
- **[Tiago Filipe Castro Viana](https://github.com/tiagofcviana)** - up201807126 - 33,(3)% contribution

## Low-level Machine Description

### Data Types

The low-level machine we are implementing consistes in a tuple of 3 eleements, where the first element is the *code*, the second element is the *stack* and the third element is the *state*.

The *code* type was already defined by the problem statement. 

For the *stack* we decided to create a new type called *StackData* which can hold either an Integer or a Boolean. The *stack* would then be defined as a list of *StackData*.

These types are defined as follows:

```haskell
-- StackData represents the different data types that can be held in the Stack type
data StackData
  = I Integer
  | B Bool
  deriving (Show, Eq)

-- Stack represents a custom stack-like data structure
type Stack = [StackData]
```

This definition allows us to process the different types of data that can be stored in the **stack** abstractly, without having to worry about the type of data that is being processed. This is useful because it allows us to implement the **stack** operations in a generic way, without having to implement them for each type of data.

However, in order to implement the *<=* operation, we need to be able to compare the values of the **stack** elements. For this reason, we decided to implement the **Ord** typeclass for the **StackData** type, as follows:

```haskell
-- Ord StackData establishes how the StackData type is ordered
instance Ord StackData where
  compare :: StackData -> StackData -> Ordering
  compare (I n1) (I n2) = compare n1 n2
  compare (B b1) (B b2) = compare b1 b2
  compare _ _ = error "Run-time error"
```

In this way, we can compare the values of the **stack** elements, and if the values are of different types, we throw a *run-time error*.

As for the **state**, we decided to create a new type called **StateData** which is a pair of **String** and **StackData**. The **state** would then be defined as a list of **StateData**.

These types are defined as follows:

```haskell
-- StateData is a tuple that contains a String and a StackData, where the String represents a variable's name, and the StackData corresponds to its value
type StateData = (String, StackData)

-- State is a list of StateData. It represents the storage
type State = [StateData]
```

This definition allows us to identify the tuples by the name of the variable, which is useful when we need to update the value of a variable. This is also useful for the implementation of ***state2Str*** function, which will need to print the *state* ordered by the name of the variables.

### Functions

In order to create a empty **stack** and **state**, we implemented the following functions:

```haskell
-- Creates an empty Stack
createEmptyStack :: Stack
createEmptyStack = []

-- Creates an empty State
createEmptyState :: State
createEmptyState = []
```

In order to convert a **stack** to a **string**, where the elements of the **stack** are separated by a comma (without spaces), we implemented the following function:

```haskell
-- Converts a stack into a string
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [h] = case h of
  I i -> show i
  B b -> show b
stack2Str (h : t) = case h of
  I i -> show i ++ "," ++ stack2Str t
  B b -> show b ++ "," ++ stack2Str t
```

In order to convert a **state** to a **string**, where the elements of the **state** are separated by a comma (without spaces), we implemented the following function:

```haskell
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
```

In this function, we use the **sort** function to sort the **state** by the name of the variables, so that the **state** is printed in alphabetical order.

Finally, in order to process the **code**, we implemented the following function:

```haskell
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
```

This function receives a tuple of 3 elements, where the first element is the **code**, the second element is the **stack** and the third element is the **state**. It then processes the **code** and returns a tuple of 3 elements, where the first element is the remaining **code** (should be empty), the second element is the resulting **stack** and the third element is the resulting **state**.

Each instruction is processed one at a time, and the resulting **code**, **stack** and **state** are passed to the next instruction. This is done recursively until the **code** is empty. Each case of the **run** function corresponds to a different instruction, indentified by pattern matching. The **run** function also uses auxiliary functions to process the different types of instructions.

The *op* auxiliary functions are used to process the arithmetic, comparison and logical operations. These functions receive a function that corresponds to the operation to be performed, and the **stack**. The *op* functions then apply the operation to the top elements of the **stack** and return the resulting **stack**. If the **stack** does not have enough elements to perform the operation, a *run-time error* is thrown.

The *store* auxiliary function is used to store a variable and its corresponding value (top of the stack) in the **state**. If the variable is already stored, its value is updated. This function receives the variable name, the value to be stored and the **state**. The *store* function then searches for the variable in the **state** and updates its value if it exists, or adds the variable and its value to the **state** if it does not exist.

The *fetch* auxiliary function is used to fetch a variable's value from the **state**. If the variable does not exist in the **state**, a *run-time error* is thrown. This function receives the variable name and the **state**. The *fetch* function then searches for the variable in the **state** and returns its value if it exists, or throws a *run-time error* if it does not exist.

## Compiler Description

### Data Types

For the second 