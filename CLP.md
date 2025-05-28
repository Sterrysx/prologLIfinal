````markdown
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
````

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
Trigger search: assign concrete values to FD variables in a specified order/strategy.

**Examples & Explanations:**

1. **Default search**

   ```prolog
   ?- Vs = [X,Y], Vs ins 1..3, X #< Y,
      labeling([], Vs).
   Vs = [1,2] ;
   Vs = [1,3] ;
   Vs = [2,3] ;
   false.
   ```

   • Finds all `(X,Y)` with `1≤X<Y≤3`.

2. **First-fail heuristic**

   ```prolog
   ?- Vs = [A,B,C], Vs ins 1..5,
      A+B+C #= 7,
      labeling([ff], Vs).
   ```

   • Chooses the variable with the smallest domain next, often speeding up search.

3. **Optimization**

   ```prolog
   ?- Vs = [S1,S2], Vs ins 0..10,
      S1+S2 #= 10,
      labeling([min(S1)], Vs).
   Vs = [0,10] ;
   Vs = [1,9] ;
   …         % in ascending order of S1
   ```

   • `min(S1)` directs the solver to generate solutions increasing in `S1`.

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

```
```
