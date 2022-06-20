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

### Magic primes

Please notice that the following YieldFSM code represents neither a Moore or Mealy style toggling automaton.
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

In other words, the `let` statement in the example `entoggle2` is crucial to achieve Moore-style behavior.

This occurs often in typical automata, so YieldFSM has a special syntax for this.
If you write a name of an input variable with a prime (a *magic prime*), it represents the value of this input before the last `yield`.
For example, the following code represents a Moore style toggling automaton.

```
[fsm|entoggle2_prime :: HiddenClockResetEnable dom => Signal dom Bit -> Signal dom Bit
input en
var b = low
forever:
    yield b
    b = b `xor` en'
|]
```

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
    if bitToBool d:
        x = x - 1
    else:
        x = x + 1
    yield x
```

Both `while` and `do`-`while` are available in YieldFSM.
However, the `do`-`while` variant is often more useful because using `while` can lead to yield-less iterations of the top level loop.
There are also `until` and `do`-`until` loops, which negate the condition.
The following example has the same behavior as `entoggle2`:

```
[fsm|entoggle2_until :: (CP.HiddenClockResetEnable dom)
                     => CP.Signal dom Bit -> CP.Signal dom Bit
input en
var b = low
forever:
    do:
        yield x
    until bitToBool en'
    b = complement b
|]
```

### Parameters

If the automaton needs to be configured in some manner, parameters are useful.
Parameters allow to make the automaton dependent on a value which is not a signal.
The following code describes a counter with a configurable maximum value.

```
[fsm|counter_param :: (HiddenClockResetEnable dom, Num a, NFDataX a)
                   => a -> Signal dom a
param m
var x = 0
forever:
    yield x
    if x == m:
        x = 0
    else:
        x = x + 1
|]
```

### Functions

YieldFSM allows to define functions for managing control flow and factoring out common functionality.
In fact, functions are the core abstraction of YieldFSM: the loops described above are a syntactic sugar for certain forms of functions.
Because of syntactic limitations, function calls can be used only as separate statements or on the top level of assignments, `let` statements and function returns.

Function calls at function returns are tail calls.
They can be used to specify control flow.
For example, the following code is equivalent to the `toggle2` example described earlier.

```
[fsm|toggle2_tail :: HiddenClockResetEnable dom => Signal dom Bit
fun f b:
    yield b
    ret call f (complement b)
ret call f b
|]
```

Multiple functions defined at the same level can call each other recursively.
For example, the following code describes an automaton which toggles its output every two cycles.

```
[fsm|toggletwo_tail :: HiddenClockResetEnable dom => Signal dom Bit
fun f b:
    yield b
    ret call g b
fun g b:
    yield b
    ret call f (complement b)
ret call f b
|]
```

Non-tail calls allow to factor out common functionality.
Recursion using non-tail calls is disallowed, as automata in general don't have a stack to store call frames.
The following code is a different description of an automaton presented above.

```
[fsm|toggletwo :: HiddenClockResetEnable dom => Signal dom Bit
fun f b:
    yield b
    yield b
forever:
    call f low
    call f high
|]
```

### Named outputs

It is often the case that an automaton being designed has multiple outputs, each of which is actively used only in some subset of its states.
For example, a controller circuit might have one output connected to a common bus, and another to the device being controlled.
These outputs typically have an idle state, which is a default value that can be overridden in some states.
YieldFSM allows to have named outputs to cover this use case.

Each named output has a definition in the header, which names the output and defines its initial value.
The default value can be omitted - it is then assumed to be undefined ("don't care").
On each `yield`, one names the outputs to be overridden inside angle brackets, and specifies their values using a tuple.
Omitted outputs have the default value.
For example, the automaton described below yields `[(1,0), (1,1), (0,0)]` in a cycle:

```
[fsm|cyclethree :: HiddenClockResetEnable dom => Signal dom (Unsigned 1, Unsigned 1)
output a = 0
output b = 0
forever:
    yield<a> 1
    yield<a, b> (1, 1)
    yield
|]
```

YieldFSM also introduces the `output` statement. Its role is to override the value of an output in
the current cycle without yielding immediately. This feature exists for the purpose of separation of
concerns: using `output`, different outputs can be managed in separate parts of the program. When
appended to the previous example, the following two lines yield `(0, 1)`:

```
    output<b> 1
    yield
```

## Syntax

The syntax is indentation based.
It distinguishes between statements, which execute in order, and expressions, which denote combinatorial circuits.
Each statement takes one or more lines.
The expressions are standard Haskell expressions, which makes integration with standard Clash code easy.

## Project structure

The source of the YieldFSM compiler is present in the `src/FSM` directory.

* Language definition is in `Lang.hs`.
* Parsing and pretty-printing is in `LangParser.hs` and `LangPretty.hs`.
* Target language is in `Desc.hs`.

Various transformations are defined in the `src/FSM/Process` directory, including:

* Desugaring - `DesugarOutputs.hs`, `DesugarLoops.hs`, some desugaring also occurs in the parser.
* Local mutable variables - `MakeLocalVars.hs`.
* Lambda lifting - `LambdaLift.hs`.
* Normalization - `CutBlocks.hs`.
* Call stack reification - `MakeTailCalls.hs`.
* Eliminating non-emitting transitions - `RemoveEpsilon.hs`.

Most of the remaining transformations are various optimizations.

