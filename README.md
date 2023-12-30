# Functional and Logic Programming - 2nd Pratical Assignment
## Group Description
Group Name: **Tactigon_4**

Group Members:
- **[João Filipe Oliveira Ramos](https://github.com/11Ramos11)** - up202108743 - 33% contribution
- **[Marco André Pereira da Costa](https://github.com/SpardaMarco)** - up202108821 - 33% contribution
- **[Tiago Filipe Castro Viana](https://github.com/tiagofcviana)** - up201807126 - 33% contribution

## Low-level Machine Description

### Data Types
The low-level machine we are implementing consistes in a tuple of 3 eleements, where the first element is the *code*, the second element is the *stack* and the third element is the *state*.

The *code* type was already defined by the problem statement. 

For the *stack* we decided to create a new type called *StackData* which can hold either an Integer or a Boolean. The *stack* would then be defined as a list of *StackData*.

These types are defined as follows:

```haskell
-- StackData is a type that can hold either an Integer or a Bool
data StackData
  = I Integer
  | B Bool
  deriving (Show, Eq)

-- Stack is a list of StackData
type Stack = [StackData]
```

This definition allows us to process the different types of data that can be stored in the **stack** abstractly, without having to worry about the type of data that is being processed. This is useful because it allows us to implement the **stack** operations in a generic way, without having to implement them for each type of data.

However, in order to implement the *<=* operation, we need to be able to compare the values of the **stack** elements. For this reason, we decided to implement the **Ord** typeclass for the **StackData** type, as follows:

```haskell
-- We define the order of StackData so that we can use "<=" operator with StackData
instance Ord StackData where
  compare (I n1) (I n2) = compare n1 n2
  compare (B b1) (B b2) = compare b1 b2
  compare (I _) (B _) = error "Run-time error"
  compare (B _) (I _) = error "Run-time error"
```

In this way, we can compare the values of the **stack** elements, and if the values are of different types, we throw a *run-time error*.

As for the **state**, we decided to create a new type called **StateData** which is a pair of **String** and **StackData**. The **state** would then be defined as a list of **StateData**.

These types are defined as follows:

```haskell
-- StateData is a pair of a String and a StackData, where the String is the name of a variable and the StackData is its value
type StateData = (String, StackData)

-- State is a list of StateData
type State = [StateData]
```

This definition allows us to identify the tuples by the name of the variable, which is useful when we need to update the value of a variable. This is also useful for the implementation of ***state2Str*** function, which will need to print the *state* ordered by the name of the variables.

### Functions

