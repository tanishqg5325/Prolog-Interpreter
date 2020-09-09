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
