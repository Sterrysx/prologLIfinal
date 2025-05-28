# CLP(FD) Quick-Reference Cheat Sheet

_All CLP(FD) exercises follow this 4-step strategy:_

1. **Variables & Domains**  
2. **Constraints**  
3. **Labeling (Search)**  
4. **Show the Results**

---

## 1. Variables & Domains

### 1.1 `length/2`  
**Purpose:**  
Generate or inspect the length of a list—often used to create a list of fresh FD variables.  

**Examples & Explanations:**

1. **Create 3 fresh variables**  
   ```prolog
   ?- length(Vars, 3).
   Vars = [_, _, _].
   ```

• `Vars` becomes a list of three new Prolog variables.

2. **Ask for the length of a ground list**

   ```prolog
   ?- length([a,b,c,d], N).
   N = 4.
   ```

   • Succeeds with `N=4`, the list’s length.

3. **Failure on mismatch**

   ```prolog
   ?- length([1,2,3], 2).
   false.
   ```

   • Fails because `[1,2,3]` has length 3, not 2.

---

### 1.2 `ins/2`

**Purpose:**
Constrain one or more FD variables to lie within a finite integer domain.

**Examples & Explanations:**

1. **Single variable domain**

   ```prolog
   ?- X ins 1..10.
   X in 1..10.
   ```

   • `X` can now be any integer 1 through 10.

2. **List of variables same domain**

   ```prolog
   ?- Vars = [A,B,C], Vars ins 0..5.
   Vars = [A,B,C],
   A in 0..5,
   B in 0..5,
   C in 0..5.
   ```

   • All three share the domain 0…5.

3. **Failure on empty domain**

   ```prolog
   ?- X ins 5..3.
   false.
   ```

   • Fails because `5..3` is an empty interval.

---

## 2. Constraints

### 2.1 `#=/2`

**Purpose:**
Enforce arithmetic equality between two FD expressions.

**Examples & Explanations:**

1. **Simple sum**

   ```prolog
   ?- X + Y #= 10, X ins 0..10, Y ins 0..10.
   X in 0..10,
   Y in 0..10,
   X+Y#=10.
   ```

   • Constrains `X+Y` to equal 10, pruning domains accordingly.

2. **Relational use**

   ```prolog
   ?- 3 #= Y + 2.
   Y = 1.
   ```

   • Works “backwards”: solves `Y+2=3` even though `is/2` would error.

3. **Failure when impossible**

   ```prolog
   ?- A ins 1..3, A + 2 #= 10.
   false.
   ```

   • No solution because adding 2 to any of 1..3 cannot reach 10.

---

### 2.2 `all_distinct/1`

**Purpose:**
Force all variables in a list to take pairwise different values.

**Examples & Explanations:**

1. **Basic permutation check**

   ```prolog
   ?- Vs = [X,Y,Z], Vs ins 1..3, all_distinct(Vs), label(Vs).
   Vs = [1,2,3] ;
   Vs = [1,3,2] ;
   Vs = [2,1,3] ;
   Vs = [2,3,1] ;
   Vs = [3,1,2] ;
   Vs = [3,2,1].
   ```

   • Generates all 6 permutations of 1..3.

2. **Pruning domains**

   ```prolog
   ?- [X,Y] ins 1..2, all_distinct([X,Y]).
   false.
   ```

   • Fails: two variables can’t both pick from the two-element set uniquely.

3. **Mixed ground & FD**

   ```prolog
   ?- all_distinct([1, X, 3]).
   X in inf..0\/2..2\/4..sup.
   ```

   • Prunes `X≠1` and `X≠3`, leaving all other integers.

---

## 3. Labeling (Search)

### 3.1 `labeling/2`

**Purpose:**  
Trigger search: assign concrete values to your FD variables according to a chosen strategy.

**Options categories:**  
- **Variable‐selection:** which variable to label next  
  (`leftmost`, `ff`, `ffc`, `min`, `max`)  
- **Value‐order:** in what order to try values  
  (`up`, `down`)  
- **Branching:** how to split a variable’s domain into choices  
  (`step`, `enum`, `bisect`)  
- **Optimization:** order solutions by an expression  
  (`min(Expr)`, `max(Expr)`)

#### 3.1.1 Variable‐selection strategies

1. **leftmost** (the default)  
   ```prolog
   ?- Vars = [X,Y], Vars ins 1..3, X+Y #=4,
      labeling([leftmost], Vars).
   Vs = [1,3] ;
   Vs = [2,2] ;
   Vs = [3,1].
   ```

• Always picks `X` before `Y`.

2. **ff** (“first‐fail”)

   ```prolog
   ?- Vars = [X,Y], X ins 1..3, Y ins 1..1, X+Y #=4,
      labeling([ff], Vars).
   Y = 1,
   X = 3.
   ```

   • Chooses `Y` first because its domain size (1) is smallest.

3. **ffc** (first‐fail + constraint‐counting)

   ```prolog
   ?- Vars = [A,B,C], Vars ins 1..3,
      A+B #=4, B+C #=5,
      labeling([ffc], Vars).
   A = 1,
   B = 3,
   C = 2.
   ```

   • Of the smallest‐domain variables, picks the one in most constraints (`B` here).

4. **min(Var)**

   ```prolog
   ?- Vs = [X,Y], Vs ins 1..5, X+Y #=6,
      labeling([min(X)], Vs).
   Vs = [1,5] ;
   Vs = [2,4] ;
   Vs = [3,3] ;
   Vs = [4,2] ;
   Vs = [5,1].
   ```

   • Labels variables in order to **minimize** `X`.

5. **max(Var)**

   ```prolog
   ?- Vs = [X,Y], Vs ins 1..5, X+Y #=6,
      labeling([max(Y)], Vs).
   Vs = [5,1] ;
   Vs = [4,2] ;
   Vs = [3,3] ;
   Vs = [2,4] ;
   Vs = [1,5].
   ```

   • Labels to **maximize** `Y`.

#### 3.1.2 Value‐order

1. **up** (default: ascending)

   ```prolog
   ?- Z ins 1..3, labeling([up], [Z]).
   Z = 1 ;
   Z = 2 ;
   Z = 3.
   ```

2. **down** (descending)

   ```prolog
   ?- Z ins 1..3, labeling([down], [Z]).
   Z = 3 ;
   Z = 2 ;
   Z = 1.
   ```

#### 3.1.3 Branching strategy

1. **step** (default)

   ```prolog
   ?- X in 1..3, Y in 1..3, X+Y #=4,
      labeling([step], [X,Y]).
   X = 1, Y = 3 ;
   X = 2, Y = 2 ;
   X = 3, Y = 1.
   ```

2. **enum** (enumerate all values)

   ```prolog
   ?- X in 1..3, Y #= X+1,
      labeling([enum], [X,Y]).
   X = 1, Y = 2 ;
   X = 2, Y = 3 ;
   X = 3, Y = 4.
   ```

3. **bisect** (binary‐split domains)

   ```prolog
   ?- X in 1..8, labeling([bisect], [X]).
   X = 4 ;
   X = 2 ;
   X = 6 ;
   X = 1 ;
   X = 3 ;
   X = 5 ;
   X = 7 ;
   X = 8.
   ```

   • Splits 1..8 at midpoint (4), then recursively bisects each subinterval.

---

### 3.2 `label/1`

**Purpose:**
Shorthand for `labeling([], Vars)`—use default options.

**Examples & Explanations:**

1. **Equivalent to `labeling([],Vars)`**

   ```prolog
   ?- Vs=[X,Y], Vs ins 1..2, X#\=Y, label(Vs).
   Vs = [1,2] ;
   Vs = [2,1] ;
   false.
   ```
2. **Single variable**

   ```prolog
   ?- X ins 0..3, label([X]).
   X = 0 ;
   X = 1 ;
   X = 2 ;
   X = 3.
   ```
3. **Failure on infinite domain**

   ```prolog
   ?- X ins 0..sup, label([X]).
   false.
   ```

   • Can’t enumerate an unbounded domain.

---

## 4. Show the Results

### 4.1 `write/1`

**Purpose:**
Write a term to the current output without a newline.

**Examples & Explanations:**

1. **Simple atom**

   ```prolog
   ?- write('Hello').
   Hello
   ```
2. **Variable output**

   ```prolog
   ?- X = 42, write(X).
   42
   ```
3. **Uninstantiated variable**

   ```prolog
   ?- write(Y).
   _G1234
   ```

   • Prints the internal name of the fresh variable.

---

### 4.2 `format/2`

**Purpose:**
Formatted output, similar to `printf` in C.

**Examples & Explanations:**

1. **String + newline**

   ```prolog
   ?- format("~w world~n", ['Hello']).
   Hello world
   ```
2. **Integer insertion**

   ```prolog
   ?- Sum = 7, format("Sum = ~d~n", [Sum]).
   Sum = 7
   ```
3. **Type error on mismatch**

   ```prolog
   ?- format("Value: ~d~n", [hello]).
   ERROR: Type error: `integer’ expected, found `hello’ (an atom)
   ```

---

