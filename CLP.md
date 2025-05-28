
#### 0 · Setup

```prolog
:- use_module(library(clpfd)).   % load once at the top of every file
```

---

#### 1 · The 4-Step “recipe”

1. **Variables & Domains** – create logic variables and give them domains (`ins/2`, `in/2`).
2. **Constraints** – relate the variables with arithmetic (`#=/2 …`) or global constraints (`all_distinct/1`, `cumulative/2`, …).
3. **Labeling (Search)** – `labeling/2` or `label/1` to turn constraints into concrete solutions; add `[min(Expr)]` or `[max(Expr)]` to optimise.
4. **Presentation** – print, verify, or post-process the solution.

---

#### 2 · Domain & Membership Syntax

| Example               | Meaning                                 |
| --------------------- | --------------------------------------- |
| `X ins 1..9`          | integer in 1-9                          |
| `Xs ins 0..sup`       | non-negative integers                   |
| `Y in -5..-1 \/ 3..7` | union of intervals                      |
| `element(I, List, V)` | *V* equals the *I*-th element of *List* |

---

#### 3 · Core Arithmetic Constraints

Replace `is`, `=:=`, `<` … with their **relational** counterparts:

```prolog
#=   #\=   #<   #>   #=<   #>=
```

They work **in all directions**, so

```prolog
3 #= Y+2.     % Y = 1    (works!)
```

---

#### 4 · Global Constraints (most used)

| Predicate                       | Quick use-case hint                        |
| ------------------------------- | ------------------------------------------ |
| `all_distinct(Vs)`              | rows/cols in Sudoku, colours, permutations |
| `global_cardinality(Vs, K-N)`   | “colour *K* appears *N* times”             |
| `cumulative(Tasks,[limit(L)])`  | scheduling with resource limit *L*         |
| `serialized(Ss, Ds)`            | non-overlapping tasks (no resources)       |
| `circuit(Vs)`                   | Hamiltonian cycle / TSP skeleton           |
| `tuples_in(Ts, Relation)`       | lookup tables, transition rules            |
| `lex_chain(Lists)`              | symmetry-breaking lex order                |
| `sum(Vs, #=, Expr)`             | linear equations ∑Vs = Expr                |
| `scalar_product(Cs,Vs,#=,Expr)` | weighted sums, knap-sack                   |

---

#### 5 · Labeling & Optimisation

```prolog
labeling([ff,          % first-fail var choice
          bisect,      % split domains
          min(Cost)],  % minimise Cost
         Vars).
```

*Var-selection* `ff | ffc | min | max | leftmost`
*Value-order* `up | down`
*Branching* `enum | step | bisect`
*Optimisers* `min(Expr) | max(Expr)` – cascade them left→right.

---

#### 6 · Reification (turn a constraint into a bit)

```prolog
X #= Y #<==> B.   % B∈{0,1}
#\  P             % not  P
P #/\ Q           % P and Q
P #\/ Q           % P or  Q
P #==> Q          % P ⇒ Q
```

Great for “count how many…” patterns.

---

#### 7 · Reflection Predicates

| Purpose               | Call             |
| --------------------- | ---------------- |
| current domain of `X` | `fd_dom(X,D)`    |
| domain size           | `fd_size(X,Sz)`  |
| # constraints on `X`  | `fd_degree(X,D)` |

---

#### 8 · 10 Tiny “Ready-to-Copy” Examples

<details><summary>① Coin-change (min #coins)</summary>

```prolog
min_coins(Amount, CoinVals, Vars) :-
        length(CoinVals, N),
        length(Vars, N), Vars ins 0..Amount,
        scalar_product(CoinVals, Vars, #=, Amount),
        sum(Vars, #=, Num),
        labeling([min(Num)], Vars).     % ← optimisation
/*
?- min_coins(26,[1,2,5,10], Vars).
Vars = [1, 0, 1, 2].        % 1×1 + 0×2 + 1×5 + 2×10 = 26
*/
```

</details>

<details><summary>② 8-Queens (general N)</summary>

```prolog
n_queens(N, Qs) :-
        length(Qs,N), Qs ins 1..N,
        all_distinct(Qs),
        diagonals(Qs,Ds1,Ds2),
        all_distinct(Ds1), all_distinct(Ds2),
        labeling([ff], Qs).

diagonals([],[],[]).
diagonals([Q|Qs],[D1|Ds1],[D2|Ds2]) :-
        length(Qs,K), I #= K+1,
        D1 #= Q+I, D2 #= Q-I,
        diagonals(Qs,Ds1,Ds2).
```

</details>

<details><summary>③ SEND + MORE = MONEY</summary>

```prolog
send_more_money(Vars) :-
 Vars = [S,E,N,D,M,O,R,Y],
 Vars ins 0..9, all_distinct(Vars),
 S #\= 0, M #\= 0,
   1000*S +100*E +10*N +D
 + 1000*M +100*O +10*R +E
 #=10000*M +1000*O +100*N +10*E +Y,
 labeling([], Vars).
```

</details>

<details><summary>④ Sudoku (9×9)</summary>

```prolog
sudoku(Rows) :-
        length(Rows,9), maplist(same_length(Rows),Rows),
        append(Rows,Vs), Vs ins 1..9,
        maplist(all_distinct,Rows),
        transpose(Rows,Cols), maplist(all_distinct,Cols),
        blocks(Rows), labeling([ff],Vs).

blocks([]).
blocks([A,B,C|Rs]) :- rows3(A,B,C), blocks(Rs).
rows3([],[],[]).
rows3([A1,A2,A3|As],[B1,B2,B3|Bs],[C1,C2,C3|Cs]) :-
        all_distinct([A1,A2,A3,B1,B2,B3,C1,C2,C3]),
        rows3(As,Bs,Cs).
```

</details>

<details><summary>⑤ Simple Job-Shop with <code>cumulative/2</code></summary>

```prolog
jobs([task(S1,3,_,1,_),
      task(S2,2,_,1,_),
      task(S3,4,_,1,_)
     ], Limit, Starts) :-
        Starts = [S1,S2,S3], Starts ins 0..Limit,
        cumulative(Starts,[limit(1)]),      % one machine
        maximum(End, [S1+3,S2+2,S3+4]),
        labeling([min(End)], Starts).
```

</details>

<details><summary>⑥ Latin Square *N*×*N*</summary>

```prolog
latin(N, Matrix) :-
        length(Matrix,N), maplist(length_list(N),Matrix),
        append(Matrix,Vs), Vs ins 1..N,
        maplist(all_distinct,Matrix),
        transpose(Matrix,Cols), maplist(all_distinct,Cols),
        labeling([ff], Vs).

length_list(N,L) :- length(L,N).
```

</details>

<details><summary>⑦ Graph Colouring (k-colours)</summary>

```prolog
colour_graph(Edges, K, Colors) :-
        vertices(Edges,Vs), length(Vs,N),
        length(Colors,N), Colors ins 1..K,
        maplist(no_same_colour(Colors),Edges),
        labeling([ff],Colors).

no_same_colour(Cs,V1-V2) :-
        nth1(V1,Cs,C1), nth1(V2,Cs,C2), C1 #\= C2.
vertices(E,V) :- findall(X,(member(A-B,E),(X=A;X=B)),Xs),
                 sort(Xs,V).
```

</details>

<details><summary>⑧ Magic Square (3×3)</summary>

```prolog
magic3x3(Square) :-
 Square = [A,B,C,D,E,F,G,H,I],
 Square ins 1..9, all_distinct(Square),
 S #= 15,
 A+B+C #= S, D+E+F #= S, G+H+I #= S,
 A+D+G #= S, B+E+H #= S, C+F+I #= S,
 A+E+I #= S, C+E+G #= S,
 labeling([],Square).
```

</details>

<details><summary>⑨ Bin-Packing (serialised)</summary>

```prolog
bin_pack(Weights,BinCap,Bins) :-
        length(Weights,N), length(Bins,N),
        Bins ins 1..N,                        % assign item→bin
        findall(task(S,Wi,_,Wi,_),
                (nth1(I,Weights,Wi), nth1(I,Bins,S)), Tasks),
        cumulative(Tasks,[limit(BinCap)]),
        labeling([min(max(Bins))],Bins).
```

</details>

<details><summary>⑩ Futoshiki “ &gt; ” Clue</summary>

```prolog
futoshiki(N,Rows, BiggerPairs) :-
        latin(N,Rows),                      % reuse example ⑥
        maplist(>, BiggerPairs).            % each A>B enforced

A > B :- A #> B.           % tiny helper
```

</details>

---

#### 9 · Performance & Debugging Tips

* **Propagate early:** state strong global constraints before labeling.
* **Break symmetries:** `lex_chain/1`, add fixed first element, order pairs.
* **Watch residual goals:** wrap queries in `call_residue_vars/2` to see remaining domains.
* **Use `once/1` only to *cut* after finding the first optimal solution.**
* **Experiment with labeling heuristics** – one extra `[ffc]` can cut search time dramatically.

---

#### 10 · Beyond Integers

* **`library(clpb)`** – Boolean (bit-vector, SAT-like) constraints.
* **`library(clpq)` / `clpr`** – rationals / reals (linear).
* **`simplex`** – linear programming with objective functions.

---
