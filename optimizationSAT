# SAT Constraint Cheatsheet for Prolog-based SAT Solver

## Table of Contents

1. [writeOneClause/1](#writeoneclause)
2. [atLeast/2](#atleast2)
3. [atMost/2](#atmost2)
4. [exactly/2](#exactly2)
5. [Default Comment Template for Clause Generation](#default-comment-template)

---

## 1. writeOneClause/1 <a name="writeoneclause"></a>

```prolog
% -------------------------------------------------------------
% 1. Declare Clause:
% 2. Iterate Over All Variables:
% 3. The Constraints (IF NECESSARY):
% 4. Write the Clause:
% 5. Fail + Base Case:
% -------------------------------------------------------------
```

### Overview
The predicate **`writeOneClause/1`** outputs a single clause (a disjunction of literals) in Conjunctive Normal Form (CNF). It translates symbolic literals (e.g., `x(I,J)`, `-x(I,J)`) into a numeric clause for the SAT solver.

### The 5-Step Pattern

1. **Declare Clause:**  
   Define the constraint predicate.  
   _Example:_  
   ```prolog
   myConstraint :- 
       ...,
       fail.
   myConstraint.
   ```

2. **Iterate Over All Variables:**  
   Loop over the relevant variables.  
   _Example:_  
   ```prolog
   myConstraint :-
       slot(I),
       song(J),
       ...
       fail.
   myConstraint.
   ```

3. **The Constraints:**  
   Apply any conditions to decide when the clause should be generated.  
   _Example:_  
   ```prolog
   someCondition(I,J),
   ```

4. **Write the Clause:**  
   Call **`writeOneClause/1`** with a list of literals representing the CNF clause. For instance, the implication _A → B_ translates to _¬A ∨ B_.  
   _Example:_  
   ```prolog
   writeOneClause([ -x(I,J), x(I2,J2) ]),
   ```

5. **Fail + Base Case:**  
   End with `fail.` to force backtracking over all possibilities, then provide a base case so the predicate succeeds eventually.  
   _Example:_  
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

## 2. atLeast/2 <a name="atleast2"></a>

```prolog
% -------------------------------------------------------------
% 1. Declare Clause:
% 2. Iterate Over All Variables:
% 3. The Constraints (IF NECESSARY):
% 4. Findall+constraint:
% 5. Fail + Base Case:
% -------------------------------------------------------------
```

### Overview
The predicate **`atLeast/2`** ensures that at least _N_ literals in a given list are true.

### The 5-Step Pattern

1. **Declare Clause:**  
   Define your constraint predicate (e.g., to ensure each participant sings at least one song).  
   _Example:_  
   ```prolog
   everyParticipantAtLeastOneSong :- 
       ...,
       fail.
   everyParticipantAtLeastOneSong.
   ```

2. **Iterate Over All Variables:**  
   Loop over all participants.  
   _Example:_  
   ```prolog
   everyParticipantAtLeastOneSong :-
       participant(P),
       ...
   ```

3. **The Constraints:**  
   Identify and collect the candidate literals (e.g., instances where participant _P_ sings a song).  
   _Example:_  
   ```prolog
   findall(x(I,J),
           (slot(I), song(J), songParticipant(J,P)),
           Lits),
   ```

4. **Write the Clause Using atLeast:**  
   Enforce the cardinality constraint that at least one literal is true.  
   _Example:_  
   ```prolog
   atLeast(1, Lits),
   ```

5. **Fail + Base Case:**  
   Use `fail.` to ensure the predicate iterates over all participants, then close with a base case.  
   _Example:_  
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


## Explanation of `findall/3` in the SAT Encoding Exercise

In our SAT encoding exercise, each SAT variable is defined by:
```prolog
satVariable( x(I, J) ) :- slot(I), song(J).
```
This means that the SAT variable `x(I, J)` represents that in slot `I`, song `J` is chosen.

---

## Mathematical Explanation of `findall/3` in this Context

The Prolog predicate `findall/3` is used to collect **all** possible solutions to a given query into a list. Its general form is:

```prolog
findall(Template, Goal, List)
```

- **Template:** This is what you want to collect. In our exercise, the template is always a SAT variable, such as `x(I, J)`.
- **Goal:** The condition that must be satisfied. For instance, `song(J)` iterates over all valid songs.
- **List:** The resulting list that will contain every instance of the Template for which the Goal succeeds.

---


prolog
findall(x, P(x), List)


can be understood as constructing the set:

{x such that P(x) is true}


and returning it as the list List.



## Example in Our Exercise

Consider the clause for ensuring that every slot has exactly one song:

```prolog
everySlotExactlyOneSong :-
    slot(I),
    findall(x(I, J), song(J), Lits),
    exactly(1, Lits),
    fail.
everySlotExactlyOneSong.
```

### What is `findall` Collecting Here?

1. **Iteration over Slots:**  
   The predicate first iterates over every slot `I` using `slot(I)`.

2. **Collecting SAT Variables:**  
   For each slot `I`, the call  
   ```prolog
   findall(x(I, J), song(J), Lits)
   ```  
   collects all SAT variables `x(I, J)` such that `song(J)` is true. In other words, it gathers the list `Lits` of all possible literals corresponding to “song `J` is chosen in slot `I`” for every valid song `J`.

3. **Enforcing the Constraint:**  
   The next line  
   ```prolog
   exactly(1, Lits)
   ```  
   uses the collected list `Lits` and enforces that **exactly one** literal in this list is true. This means that for slot `I`, exactly one song is selected.

4. **Fail + Base Case:**  
   The `fail` ensures that Prolog backtracks to process every slot, and the base case (`everySlotExactlyOneSong.`) allows the predicate to succeed after all slots have been processed.



## 3. atMost/2 <a name="atmost2"></a>

### Overview
The predicate **`atMost/2`** guarantees that no more than _N_ literals in a list are true.

### The 5-Step Pattern

1. **Declare Clause:**  
   Define your predicate (e.g., to ensure each song appears in at most one slot).  
   _Example:_  
   ```prolog
   everySongAtMostOneSlot :- 
       ...,
       fail.
   everySongAtMostOneSlot.
   ```

2. **Iterate Over All Variables:**  
   Loop over each song.  
   _Example:_  
   ```prolog
   everySongAtMostOneSlot :-
       song(J),
       ...
   ```

3. **The Constraints:**  
   Collect all slot literals for the given song.  
   _Example:_  
   ```prolog
   findall(x(I,J), slot(I), Lits),
   ```

4. **Write the Clause Using atMost:**  
   Enforce that at most one of these literals can be true.  
   _Example:_  
   ```prolog
   atMost(1, Lits),
   ```

5. **Fail + Base Case:**  
   Finalize with `fail.` to ensure backtracking, then a base case.  
   _Example:_  
   ```prolog
   everySongAtMostOneSlot :-
       song(J),
       findall(x(I,J), slot(I), Lits),
       atMost(1, Lits),
       fail.
   everySongAtMostOneSlot.
   ```

---

## 4. exactly/2 <a name="exactly2"></a>

### Overview
The predicate **`exactly/2`** enforces that exactly _N_ literals in a list are true. It is generally implemented as a combination of **`atLeast/2`** and **`atMost/2`**.

### The 5-Step Pattern

1. **Declare Clause:**  
   Define your predicate (e.g., to ensure each slot has exactly one song).  
   _Example:_  
   ```prolog
   everySlotExactlyOneSong :- 
       ...,
       fail.
   everySlotExactlyOneSong.
   ```

2. **Iterate Over All Variables:**  
   Loop over each slot.  
   _Example:_  
   ```prolog
   everySlotExactlyOneSong :-
       slot(I),
       ...
   ```

3. **The Constraints:**  
   Gather all song literals that could be in slot _I_.  
   _Example:_  
   ```prolog
   findall(x(I,J), song(J), Lits),
   ```

4. **Write the Clause Using exactly:**  
   Enforce that exactly one literal in this list is true by calling:
   ```prolog
   exactly(1, Lits),
   ```
   *(Note: `exactly(1, Lits)` is implemented as `atLeast(1, Lits)` combined with `atMost(1, Lits)`.)*

5. **Fail + Base Case:**  
   Use `fail.` to iterate over all slots and finish with a base case.  
   _Example:_  
   ```prolog
   everySlotExactlyOneSong :-
       slot(I),
       findall(x(I,J), song(J), Lits),
       exactly(1, Lits),
       fail.
   everySlotExactlyOneSong.
   ```

---

## 5. Default Comment Template for Clause Generation <a name="default-comment-template"></a>

Include the following template as a header in your Prolog clause definitions to outline the five standard steps:

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
