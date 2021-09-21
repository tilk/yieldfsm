# YieldFSM

This project is a library for the hardware description language Clash which allows to generate finite state machines from simple imperative descriptions.
For example, the following code implements a simple counter circuit:

```
[fsm|counter :: (HiddenClockResetEnable dom, Num a, NFDataX a)
             => Signal dom a
var x = 0
forever:
    yield x
    x = x + 1
|]
```

Standard language constructs like `while` loops, function calls etc. are supported, which allows to describe finite state machines in a way intuitive for a programmer.
This is not a high level synthesis (HLS) language. 
A direct relationship exists between the written code and the timing behavior of the resulting circuit.
YieldFSM code is compiled to a single `mealy` call and some companion datatypes.

## Tutorial

### Yields and infinite loops

Automata descriptions written in YieldFSM are similar to [generators](https://en.wikipedia.org/wiki/Generator_(computer_programming)) in other programming languages.
The most important statement is `yield`, which denotes generating a new output of the automaton, which is held for a single clock cycle.
Each consecutive `yield` describes one cycle of the automaton.

As automata can't "finish execution", the second most important statement is the `forever` loop.
Statements inside the `forever` loop, as the name implies, will be executed forever.
At least one `yield` statement must be executed in each iteration of the loop, as automata can't loop without producing output.
For example, the following code describes an automaton generating a signal which toggles for every clock cycle.

```
[fsm|toggle :: HiddenClockResetEnable dom => Signal dom Bit
forever:
    yield low
    yield high
|]
```

### Mutable variables

In order to store some values in the automaton's state, variables can be used.
A variable is defined using a `var` statement.
After definition, it can be changed using an assignment statement.
The following code has the same behavior as the previous example, but it uses a variable to achieve this result.

```
[fsm|toggle1 :: HiddenClockResetEnable dom => Signal dom Bit
var b = high
forever:
    b = complement b
    yield b
|]
```

An alternative way to achieve the same behavior is the following:

```
[fsm|toggle2 :: HiddenClockResetEnable dom => Signal dom Bit
var b = low
forever:
    yield b
    b = complement b
|]
```

Please note that, even though the behavior is the same, the resulting implementation is different.
They are equivalent to the following `mealy` calls:

```haskell
toggle1 = mealy (\b _ -> let b' = complement b in (b', b')) high (pure ())
toggle2 = mealy (\b _ -> (complement b, b)) low (pure ())
```

In this way, the structure of YieldFSM code can influence the structure of the generated circuit.

### Inputs

The automata defined in previous examples always exhibit the same behavior.
More interesting automata have a dependence on input signals.
To add a dependence on input signals, an `input` line should be added at the beginning of the code, right after the type signature.
An input specification is a Haskell pattern, which is matched to the automaton's input.
Any variables bound by the pattern will be available as (immutable) variables.
The value of those variables depends on the state of the input in the present clock cycle.
Recall that a `yield` ends a clock cycle, and therefore statements after a `yield` will read input values from the next clock cycle.

For example, a toggling automaton which is enabled using an input signal can be described as follows:

```
[fsm|entoggle1 :: HiddenClockResetEnable dom => Signal dom Bit -> Signal dom Bit
input en
var b = low
forever:
    b = b `xor` en
    yield b
|]
```

This code corresponds to the following Mealy automaton:

```haskell
entoggle1 = mealy (\b en -> let b' = b `xor` en in (b', b')) low
```

The corresponding Moore automaton can be described as follows:

```
[fsm|entoggle2 :: HiddenClockResetEnable dom => Signal dom Bit -> Signal dom Bit
input en
var b = low
forever:
    let en0 = en
    yield b
    b = b `xor` en0
|]
```

Notice that a `let` statement had to be used to capture the value of the `en` signal before the `yield`.
A `let` statement is similar to `var`, but the bound variable cannot be modified by assignment statements.
The resulting automaton is as follows:

```haskell
entoggle2 = mealy (\b en -> (b `xor` en, b)) low
```

Please also notice that the following YieldFSM code represents neither a Moore or Mealy style toggling automaton.
It instead describes a circuit which in the first cycle outputs `low`, and then behaves like the Mealy style automaton `entoggle1`.

```
[fsm|entoggle_wrong :: HiddenClockResetEnable dom => Signal dom Bit -> Signal dom Bit
input en
var b = low
forever:
    yield b
    b = b `xor` en
|]
```

In other words, the `let` statement is crucial to achieve Moore-style behavior.

### Conditions and loops

Up until now, we were using only one control structure: the `forever` loop.
Here we will introduce additional control structures: conditional statements and `while` loops.

The conditional statement (written `if`) can be used to make some behavior dependent on a condition.
For example, the `entoggle1` example can be rewritten using an `if` statement as follows:

```
[fsm|entoggle1_if :: HiddenClockResetEnable dom => Signal dom Bit -> Signal dom Bit
input en
var b = low
forever:
    if bitToBool en:
        b = complement b
    yield b
|]
```

The `if` statement can have an optional `else` block.
For example, the following code describes a counter which counts up or down depending on the input.

```
[fsm|counter_updown :: (HiddenClockResetEnable dom, KnownNat n)
                    => Signal dom Bit -> Signal dom (Unsigned n)
input d
var x = 0
forever:
    if d:
        x = x - 1
    else:
        x = x + 1
    yield x
```

TODO

### Parameters

TODO

### Functions

TODO

## Syntax

The syntax is indentation based.
It distinguishes between statements, which execute in order, and expressions, which denote combinatorial circuits.
Each statement takes one or more lines.
The expressions are standard Haskell expressions, which makes integration with standard Clash code easy.



