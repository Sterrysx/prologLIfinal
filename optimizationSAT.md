
# SAT Constraint Cheatsheet for Prolog-based SAT Solver

## Table of Contents

1. [Clause Generators](#clause-generators)  
   1.1. [`writeOneClause/1`](#11-writeoneclause)  
   1.2. [`atLeast/2`](#12-atleast2)  
   1.3. [`atMost/2`](#13-atmost2)  
   1.4. [`exactly/2`](#14-exactly2)  
   1.5. [Default Comment Template for Clause Generation](#15-default-comment-template)  

2. [Clause Skeletons](#clause-skeletons)  
   2.1. [`relateXwithY` helper-variable equivalence](#21-relatexwithy-helper-variable-equivalence)  

---

## 1. Clause Generators <a name="clause-generators"></a>

### 1.1 `writeOneClause/1` <a name="11-writeoneclause"></a>

```prolog
% -------------------------------------------------------------
% 1. Declare Clause:
% 2. Iterate Over All Variables:
% 3. (Optional) Extra Conditions:
% 4. Write the Clause:
% 5. Fail + Base Case:
% -------------------------------------------------------------
````

#### Overview

The predicate **`writeOneClause/1`** outputs a single clause (a disjunction of literals) in Conjunctive Normal Form (CNF). It translates symbolic literals (e.g., `x(I,J)`, `-x(I,J)`) into a numeric clause for the SAT solver.

#### The 5-Step Pattern

1. **Declare Clause:**
   Define the constraint predicate.
   *Example:*

   ```prolog
   myConstraint :- 
       ...,
       fail.
   myConstraint.
   ```

2. **Iterate Over All Variables:**
   Loop over the relevant variables.
   *Example:*

   ```prolog
   myConstraint :-
       slot(I),
       song(J),
       ...,
       fail.
   myConstraint.
   ```

3. **The Constraints:**
   Apply any conditions to decide when the clause should be generated.
   *Example:*

   ```prolog
   someCondition(I,J),
   ```

4. **Write the Clause:**
   Call **`writeOneClause/1`** with a list of literals representing the CNF clause. For instance, the implication *A → B* translates to *¬A ∨ B*.
   *Example:*

   ```prolog
   writeOneClause([ -x(I,J), x(I2,J2) ]),
   ```

5. **Fail + Base Case:**
   End with `fail.` to force backtracking over all possibilities, then provide a base case so the predicate succeeds eventually.
   *Example:*

   ```prolog
   myConstraint :-
       slot(I),
       song(J),
       someCondition(I,J),
       writeOneClause([ -x(I,J), x(I+1,J) ]),
       fail.
   myConstraint.
   ```

---

### 1.2 `atLeast/2` <a name="12-atleast2"></a>

```prolog
% -------------------------------------------------------------
% 1. Declare Clause:
% 2. Iterate Over All Variables:
% 3. The Constraints (IF NECESSARY):
% 4. Findall + atLeast:
% 5. Fail + Base Case:
% -------------------------------------------------------------
```

#### Overview

The predicate **`atLeast/2`** ensures that at least *K* literals in a given list are true.

#### The 5-Step Pattern

1. **Declare Clause:**
   Define your constraint predicate (e.g., to ensure each participant sings at least one song).
   *Example:*

   ```prolog
   everyParticipantAtLeastOneSong :- 
       ...,
       fail.
   everyParticipantAtLeastOneSong.
   ```

2. **Iterate Over All Variables:**
   Loop over all participants.
   *Example:*

   ```prolog
   everyParticipantAtLeastOneSong :-
       participant(P),
       ...
   ```

3. **The Constraints:**
   Identify and collect the candidate literals (e.g., instances where participant *P* sings a song).
   *Example:*

   ```prolog
   findall(x(I,J),
           (slot(I), song(J), songParticipant(J,P)),
           Lits),
   ```

4. **Write the Clause Using `atLeast`:**
   Enforce the cardinality constraint that at least one literal is true.
   *Example:*

   ```prolog
   atLeast(1, Lits),
   ```

5. **Fail + Base Case:**
   Use `fail.` to ensure the predicate iterates over all participants, then close with a base case.
   *Example:*

   ```prolog
   everyParticipantAtLeastOneSong :-
       participant(P),
       findall(x(I,J),
               (slot(I), song(J), songParticipant(J,P)),
               Lits),
       atLeast(1, Lits),
       fail.
   everyParticipantAtLeastOneSong.
   ```

---

### 1.3 `atMost/2` <a name="13-atmost2"></a>

```prolog
% -------------------------------------------------------------
% 1. Declare Clause:
% 2. Iterate Over All Variables:
% 3. The Constraints:
% 4. Findall + atMost:
% 5. Fail + Base Case:
% -------------------------------------------------------------
```

#### Overview

The predicate **`atMost/2`** guarantees that no more than *K* literals in a list are true.

#### The 5-Step Pattern

1. **Declare Clause:**
   Define your predicate (e.g., to ensure each song appears in at most one slot).
   *Example:*

   ```prolog
   everySongAtMostOneSlot :- 
       ...,
       fail.
   everySongAtMostOneSlot.
   ```

2. **Iterate Over All Variables:**
   Loop over each song.
   *Example:*

   ```prolog
   everySongAtMostOneSlot :-
       song(J),
       ...
   ```

3. **The Constraints:**
   Collect all slot literals for the given song.
   *Example:*

   ```prolog
   findall(x(I,J), slot(I), Lits),
   ```

4. **Write the Clause Using `atMost`:**
   Enforce that at most one of these literals can be true.
   *Example:*

   ```prolog
   atMost(1, Lits),
   ```

5. **Fail + Base Case:**
   Finalize with `fail.` to ensure backtracking, then a base case.
   *Example:*

   ```prolog
   everySongAtMostOneSlot :-
       song(J),
       findall(x(I,J), slot(I), Lits),
       atMost(1, Lits),
       fail.
   everySongAtMostOneSlot.
   ```

---

### 1.4 `exactly/2` <a name="14-exactly2"></a>

```prolog
% -------------------------------------------------------------
% 1. Declare Clause:
% 2. Iterate Over All Variables:
% 3. The Constraints:
% 4. Findall + exactly:
% 5. Fail + Base Case:
% -------------------------------------------------------------
```

#### Overview

The predicate **`exactly/2`** enforces that exactly *K* literals in a list are true. It combines **`atLeast/2`** and **`atMost/2`**.

#### The 5-Step Pattern

1. **Declare Clause:**
   Define your predicate (e.g., to ensure each slot has exactly one song).
   *Example:*

   ```prolog
   everySlotExactlyOneSong :- 
       ...,
       fail.
   everySlotExactlyOneSong.
   ```

2. **Iterate Over All Variables:**
   Loop over each slot.
   *Example:*

   ```prolog
   everySlotExactlyOneSong :-
       slot(I),
       ...
   ```

3. **The Constraints:**
   Gather all song literals that could be in slot *I*.
   *Example:*

   ```prolog
   findall(x(I,J), song(J), Lits),
   ```

4. **Write the Clause Using `exactly`:**
   Enforce that exactly one literal in this list is true by calling:

   ```prolog
   exactly(1, Lits),
   ```

   *(Note: internally, `exactly/2` invokes `atLeast/2` and `atMost/2`.)*

5. **Fail + Base Case:**
   Use `fail.` to iterate over all slots and finish with a base case.
   *Example:*

   ```prolog
   everySlotExactlyOneSong :-
       slot(I),
       findall(x(I,J), song(J), Lits),
       exactly(1, Lits),
       fail.
   everySlotExactlyOneSong.
   ```

---

### 1.5 Default Comment Template for Clause Generation <a name="15-default-comment-template"></a>

```prolog
% -------------------------------------------------------------
% 1. Declare Clause:
%    - Name your constraint predicate. This marks the beginning
%      of the clause definition.
%
% 2. Iterate Over All Variables:
%    - Loop through all relevant variables using generators (e.g.,
%      slot(I), song(J), participant(P), etc.).
%
% 3. The Constraints:
%    - Apply any conditions or filtering that determine when this
%      clause should be generated.
%      (For example, check that one score is greater than another,
%       or that a certain symbol exists.)
%
% 4. Write the Clause:
%    - Construct the clause by assembling a list of literals.
%      For an implication A -> B, use its CNF equivalent: [-A, B].
%      For cardinality constraints, use findall to collect the
%      relevant literals:
%           findall(Literal, (Condition on variables), Lits),
%           atLeast/atMost/exactly(N, Lits).
%
% 5. Fail + Base Case:
%    - End the loop with a 'fail' to force backtracking and generate
%      all possible clauses.
%    - Provide a base case (an empty clause) to ensure the predicate
%      eventually succeeds.
% -------------------------------------------------------------
```

---

## 2. Clause Skeletons <a name="clause-skeletons"></a>

### 2.1 `relateXwithY` helper-variable equivalence <a name="21-relatexwithy-helper-variable-equivalence"></a>

> **Goal:** introduce a helper SAT variable **Y(Shared…)** that is true exactly WHEN some (OR all) of a family of literals **X(Shared, Extra…)** hold.

#### 2.1.1 Generic Skeleton (OR version)

```prolog
relateXwithY :-
    % 1) Iterate over the shared indices:
    shared1(A), shared2(B), ...,

    % 2) Collect all X-literals that share those indices but differ
    %    in the extra ones (Extra1, Extra2, …):
    findall(
      X(A,B,Extra1,Extra2,...),      % the X-literal template
      ( extraIdx1(Extra1),
        extraIdx2(Extra2),
        ...                          % generators for un-shared indices
      ),
      Lits
    ),

    % 3) Emit the equivalence Y <--> OR(Lits):
    expressOr(Y(A,B), Lits),

    % 4) Force backtracking:
    fail.
relateXwithY.                       % base case
```

> **Why put only the un-shared indices inside `findall/3`?**
> Because those are the dimensions present in `X(...)` but **absent** in `Y(...)`; ranging over them enumerates every way to make `Y` true.

---

#### 2.1.2 Example 1: `busyAtHour` vs. `does`

**SAT vars:**

```prolog
satVariable( does(G,T,H) )      :- gangster(G), task(T), hour(H).
satVariable( busyAtHour(G,H) )  :- gangster(G), hour(H).
```

**Relation:**

```
busyAtHour(G,H) <--> ∨ₜ does(G,T,H)
```

```prolog
relateDoesBusy :-
    % shared indices G,H:
    gangster(G), hour(H),

    % un-shared = T:
    findall( does(G,T,H),
             task(T),
             Lits ),

    expressOr( busyAtHour(G,H), Lits ),
    fail.
relateDoesBusy.
```

* **Shared:** G, H
* **Un-shared:** T

---

#### 2.1.3 Example 2: `c(J)` vs. `bc(I,J)`

**SAT vars:**

```prolog
satVariable( bc(I,J) ) :- bar(I),    container(J).
satVariable( c(J)    ) :-            container(J).
```

**Relation:**

```
c(J) <--> ∨ᵢ bc(I,J)
```

```prolog
relateBarContainer :-
    % shared index J:
    container(J),

    % un-shared = I:
    findall( bc(I,J),
             bar(I),
             Lits ),

    expressOr( c(J), Lits ),
    fail.
relateBarContainer.
```

* **Shared:** J
* **Un-shared:** I

---



---

