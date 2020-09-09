# Prolog Interpreter

In this project, I have written a simplified version of the Prolog interpreter in OCaml.

## Data Types

- **Program**: List of Clauses
- **Clause**: Fact or Rule
- **Fact**: contains Head
- **Rule**: contains Head and Body
- **Head**: single Atomic Formula
- **Body**: sequence of Atomic Formulas
- **Atomic Formula**: k-ary predicate Symbol followed by k Terms (k ≥ 0)
- **Term**: either a variable, a number, or a k-ary function symbol with k sub-terms (k ≥ 0)
- **Goal**: list of Atomic Formulas

## Working of Interpreter

Unification has been used as the parameter-passing mechanism. By pretending the predicate symbol is a function symbol, resolution of goals and program clauses have been performed.

A back-tracking strategy has also been developed to explore the resolution search space. A goal has been replaced by subgoals, as found by applying a unifier to the body of a program clause whose head is unified with the chosen subgoal.

## Example Programs

### Program 1

Program:

```prolog
fact(0, 1).
fact(X, Y) :-
        X > 0,
        Z = X - 1,
        fact(Z, W),
        Y =  W * X.
```

Query:

```prolog
fact(8, X).
```

Result:

```prolog
X = 40320 ;
```

### Program 2

Program:

```prolog
min(X, X, X).
min(X, Y, Z) :- X > Y, Z = Y.
min(X, Y, Z) :- Y > X, Z = X.

mergesort([], []).
mergesort([A], [A]).
mergesort([A,B|R], S) :-
    split([A,B|R], L1, L2),
    mergesort(L1, S1),
    mergesort(L2, S2),
    merge(S1, S2, S).

split([], [], []).
split([A], [A], []).
split([A,B|R], [A|Ra], [B|Rb]) :- split(R, Ra, Rb).

merge(A, [], A).
merge([], B, B).
merge([A|Ra], [B|Rb], [A|M]) :-  min(A, B, A), merge(Ra, [B|Rb], M).
merge([A|Ra], [B|Rb], [B|M]) :-  B < A, merge([A|Ra], Rb, M).
```

Query:

```prolog
mergesort([4, 7, 1, 9, 17, 3, 4, 9, 5, 2, 2], X).
```

Result:

```prolog
X = [ 1 , 2 , 2 , 3 , 4 , 4 , 5 , 7 , 9 , 9 , 17 ] ;
```

## Usage

### Compile

```bash
make
```

### Execute

```bash
./prolog <program_file>
```

### Example

```bash
./prolog test8.pl
```

### Clean

```bash
make clean
```
