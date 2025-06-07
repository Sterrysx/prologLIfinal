# Prolog Cheat Sheet

A comprehensive reference for Prolog concepts—from basic syntax through advanced list processing and SAT/CLP optimizations. Use this as your quick lookup guide when you need to recall definitions, syntax, or usage tips.

---

## Table of Contents

1. [Prolog Basic Syntax](#prolog-basic-syntax)  
   1\.1. [Basic Prolog (Facts, Rules, Variables, Predicates, Comments, Control Constructs)](#basic-prolog-facts-rules-variables-predicates-comments-control-constructs)  
   1\.2. [Debugging with write and writeln](#debugging-with-write-writeln) 1.3. [If-Then-Else in Prolog](#if-then-else-in-prolog)
2. [Operators](#operators)  
   2\.1. [Arithmetic Operators](#arithmetic-operators)  
   2\.2. [Comparison Operators](#comparison-operators)  
   2\.3. [Logical Operators](#logical-operators)  
   2\.4. [Unification and Equality Operators](#unification-and-equality-operators)  
   2\.5. [Arithmetic Evaluation (](#arithmetic-evaluation-is)`is`)  
   2\.6. [Operator Precedence & Associativity](#operator-precedence--associativity)  
   2\.7. [Logical Implications (](#logical-implications)`:-`)
3. [Lists](#lists)  
   3\.1. [List Basics](#list-basics-syntax-headtail-decomposition)  
   3\.2. [Head and Tail Decomposition](#head-tail-decomposition)  
   3\.3. [Common List Predicates](#common-list-predicates)  
       3\.3.1. [member/2 and memberchk/2](#member2-and-memberchk2)  
       3\.3.2. [append/3](#append3)  
       3\.3.3. [length/2](#length2)  
       3\.3.4. [nth1/3](#nth13)  
       3\.3.5. [findall/3](#findall3)  
       3\.3.6. [select/3](#select3)  
       3\.3.7. [reverse/2](#reverse2)  
       3\.3.8. [last/2](#last2)  
       3\.3.9. [delete/3](#delete3)  
       3\.3.10. [union/3 and intersection/3](#union3-and-intersection3)  
   3\.4. [Advanced List Predicates](#advanced-list-predicates)  
       3\.4.1. [sort/2 and msort/2](#sort2-and-msort2)  
       3\.4.2. [Custom predicates: unique/2 and intersect/3](#custom-predicates-unique2-and-intersect3)  
   3\.5. [Haskell-like List Functions](#haskell-like-list-functions)  
       3\.5.1. [take/3 and drop/3](#take3-and-drop3)  
       3\.5.2. [takewhile/3 and dropwhile/3](#takewhile3-and-dropwhile3)  
       3\.5.3. [split_at/4](#split_at4)  
       3\.5.4. [zip/3](#zip3)  
   3\.6. [Recursive List Processing](#recursive-list-processing)  
       3\.6.1. [Sum of Elements](#sum-of-elements)  
       3\.6.2. [Maximum Element](#maximum-element)  
       3\.6.3. [Minimum Element](#minimum-element)  
       3\.6.4. [Product of Elements](#product-of-elements)  
       3\.6.5. [Average of Elements](#average-of-elements)  
   3\.7. [Haskell-like Higher-Order Functions](#haskell-like-higher-order-functions)  
       3\.7.1. [map/3](#map3)  
       3\.7.2. [filter/3](#filter3)  
       3\.7.3. [foldl/4 and foldr/4](#foldl4-and-foldr4)  
       3\.7.4. [scanl/4 and scanr/4](#scanl4-and-scanr4)  
       3\.7.5. [all/2 and any/2](#all2-and-any2)  
   3\.8. [Typical List-Based Examples](#typical-list-based-examples)  
       3\.8.1. [Factorial](#factorial)  
       3\.8.2. [Fibonacci](#fibonacci)  
       3\.8.3. [Reverse a List](#reverse-a-list)  
       3\.8.4. [Palindrome Check](#palindrome-check)  
       3\.8.5. [Merge Sort](#merge-sort)  
       3\.8.6. [Flatten a Nested List](#flatten-a-nested-list)  
       3\.8.7. [Sum of Elements](#sum-of-elements-1)  
       3\.8.8. [Product of Elements](#product-of-elements-1)  
       3\.8.9. [List Length (Recursive)](#list-length-recursive)  
       3\.8.10. [Count Occurrences](#count-occurrences)  
       3\.8.11. [Maximum Element (Recursive)](#maximum-element-recursive)  
       3\.8.12. [Minimum Element (Recursive)](#minimum-element-recursive)  
       3\.8.13. [Check if a List is Sorted](#check-if-a-list-is-sorted)  
       3\.8.14. [Quick Sort](#quick-sort)  
       3\.8.15. [Reverse a List with an Accumulator](#reverse-a-list-with-an-accumulator)  
       3\.8.16. [Concatenate a List of Lists](#concatenate-a-list-of-lists)  
       3\.8.17. [Interleave Two Lists](#interleave-two-lists)  
       3\.8.18. [Sublist Check (Contiguous)](#sublist-check-contiguous)  
       3\.8.19. [Permutations of a List](#permutations-of-a-list)  
       3\.8.20. [Zip Three Lists](#zip-three-lists)  
4. [Advanced Predicates](#advanced-predicates)  
   4\.1. [Using findall/3 for Collecting Solutions](#using-findall3-for-collecting-solutions)  
   4\.2. [SAT & Constraint Encodings](#sat--constraint-encodings)  
       4\.2.1. [expressOr/2 and expressAnd/2](#expressor2-and-expressand2)  
       4\.2.2. [Cardinality Constraints: atLeast/2, atMost/2, exactly/2](#cardinality-constraints)  
   4\.3. [Clause Generation with writeOneClause/1](#clause-generation-with-writeoneclause1)  
   4\.4. [Higher-Order Predicates: maplist/2, maplist/3, include/3, exclude/3](#higher-order-predicates)  
   4\.5. [Defining Custom Predicates](#defining-custom-predicates)  
   4\.6. [Recursion Techniques](#recursion-techniques)  
   4\.7. [Error Handling and Guards](#error-handling-and-guards)  
   4\.8. [Generator Predicates](#generator-predicates)  
       4\.8.1. [between/3](#between3)  
       4\.8.2. [repeat/0](#repeat0)  
       4\.8.3. [bagof/3](#bagof3)  
       4\.8.4. [setof/3](#setof3)  
       4\.8.5. [succ/2](#succ2)
5. [Extra Self-Declarated Predicates](#5-extra-self-declarated-predicates)  
   5\.1. [inside/2](#51-inside2)  
   5\.2. [prefix/2](#52-prefix2)  
   5\.3. [suffix/2](#53-suffix2)  
   5\.4. [rotate_left/2](#54-rotate_left2)  
   5\.5. [rotate_right/2](#55-rotate_right2)  
   5\.6. [remove_duplicates/2](#56-remove_duplicates2)  
   5\.7. [substitute/4](#57-substitute4)  
   5\.8. [replace_at/4](#58-replace_at4)  
   5\.9. [nth0/3](#59-nth03)  
   5\.10. [duplicate/3](#510-duplicate3)

---

## 1. Prolog Basic Syntax

<a id="prolog-basic-syntax"></a>

### 1.1 Basic Prolog (Facts, Rules, Variables, Predicates, Comments, Control Constructs)

<a id="basic-prolog-facts-rules-variables-predicates-comments-control-constructs"></a>

* **Facts**  
  *Definition:* Simple assertions about the world.  
  *Syntax:*

  ```
  gangster(g01).
  notAvailable(g01, [6,13,14,16,21,35,37,41,59]).
  
  ```
* **Rules**  
  *Definition:* Implications that derive new information from facts.  
  *Syntax:*

  ```
  available(G, H) :- hour(H), gangster(G), \+ blocked(G, H).
  
  ```
* **Variables**  
  *Naming:* Begin with an uppercase letter or underscore.  
  *Examples:*

  ```
  G, H, Cost, _Temp, X1, Y_variable.
  
  ```
* **Predicates**  
  *Definition:* Relations defined by facts and rules.  
  *Example:*

  ```
  task(T).
  needed(T, H, N).
  
  ```
* **Comments**  
  *Single-line:* Use `%`

  ```
  % This is a comment.
  gangster(g01).  % Declares gangster g01
  
  ```

  *Multi-line:* Use `/* ... */`

  ```
  /*
    This is a multi-line comment.
    It spans several lines.
  */
  
  ```
* **Control Constructs**  
  *Cut (*`!`): Prunes the search tree (prevents backtracking).

  ```
  max(X, Y, X) :- X >= Y, !.
  max(X, Y, Y) :- X < Y.
  
  ```

  *Failure (*`fail`): Forces a predicate to fail (often used in iterative printing).

  ```
  print_all_fail(List) :-
      member(X, List),
      write(X), nl,
      fail.
  print_all_fail(_).
  
  ```

---

### 1.2 Debugging with write and writeln

<a id="debugging-with-write-writeln"></a>

When writing Prolog code, it’s often helpful to display the values of variables during execution to understand what’s going on. Prolog provides built-in predicates such as `write/1`, `writeln/1`, and `format/2` to print information to the console.

Below are some common scenarios demonstrating how to use `write` and `writeln`:

#### Example 1: Display a Single Variable Value

```
rotate(L, K, T) :-
    length(L, N),
    Aux is K mod N,
    write('Aux is: '), writeln(Aux),
    split_at(Aux, L, L2, L1),
    append(L1, L2, T).
```

* **What happens here?**
  * We calculate `Aux` as `K mod N`.
  * We immediately display the value of `Aux` using `write/1` followed by `writeln/1`.
    * `write('Aux is: ')` prints the text without a newline.
    * `writeln(Aux)` prints the actual value of `Aux` and then moves to a new line.

---

#### Example 2: Display Multiple Variables in a Single Line

```
check_and_print(A, B) :-
    A < B,
    write('A = '), write(A),
    write(', B = '), writeln(B),
    writeln('A is less than B!').
```

* **What happens here?**
  * We check if `A < B`.
  * We print both variables on one line, then print a message on a new line.
  * Using multiple `write/1` calls in a row allows you to control exactly how you space or format your output.

---

#### Example 3: Debugging Recursive Calls

```
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, TailSum),
    Sum is H + TailSum,
    write('Current head: '), write(H),
    write(', Tail sum: '), write(TailSum),
    write(', Total so far: '), writeln(Sum).
```

* **What happens here?**
  * Each time `sum_list/2` processes an element, it prints out:
    1. The current head of the list (`H`).
    2. The sum of the tail (`TailSum`).
    3. The total sum so far (`Sum`).
  * This is especially useful for understanding how recursion unfolds.

---

### 1.3 If-Then-Else in Prolog

<a id="if-then-else-in-prolog"></a>

Prolog’s if-then-else construct lets you execute one branch of code if a condition is met and another branch if it is not. The syntax is:

```
( Condition -> ThenClause ; ElseClause )
```

* **How it works:**
  1. **Evaluate Condition:** If the condition succeeds, Prolog commits to the `ThenClause` and ignores the `ElseClause`.
  2. **Else Clause:** If the condition fails, the `ElseClause` is executed.

This construct is useful within predicates to decide between alternatives based on certain tests.

#### Example 1: Basic Numeric Check

Determine whether a number is even or odd:

```
even_or_odd(Number, Result) :-
    ( 0 is Number mod 2 ->
        Result = even
    ; 
        Result = odd
    ).
```

* **Explanation:**
  * The predicate checks if `Number mod 2` equals 0.
  * If true, `Result` is unified with `even`.
  * Otherwise, `Result` is unified with `odd`.

---

#### Example 2: Grading System

Assign a letter grade based on a score:

```
grade(Score, Grade) :-
    ( Score >= 90 ->
        Grade = 'A'
    ; Score >= 80 ->
        Grade = 'B'
    ; Score >= 70 ->
        Grade = 'C'
    ; Score >= 60 ->
        Grade = 'D'
    ;
        Grade = 'F'
    ).
```

* **Explanation:**
  * This predicate uses multiple if-then-else checks.
  * It assigns a grade by testing the score range in descending order.
  * The first true condition’s clause is executed, and the rest are ignored.

---

#### Example 3: Determine the Maximum of Two Numbers

Find the maximum of two numbers using an if-then-else construct:

```
max(X, Y, Max) :-
    ( X >= Y ->
        Max = X
    ;
        Max = Y
    ).
```

* **Explanation:**
  * The predicate compares `X` and `Y`.
  * If `X` is greater than or equal to `Y`, then `Max` is set to `X`.
  * Otherwise, `Max` is set to `Y`.

---

## 2. Operators

<a id="operators"></a>

### 2.1 Arithmetic Operators

<a id="arithmetic-operators"></a>

* **Addition (**`+`):

  ```
  ?- X is 2 + 3.  % X = 5.
  
  ```
* **Subtraction (**`-`):

  ```
  ?- X is 5 - 2.  % X = 3.
  
  ```
* **Multiplication (**`*`):

  ```
  ?- X is 4 * 3.  % X = 12.
  
  ```
* **Division (**`/`):

  ```
  ?- X is 10 / 4.  % X = 2.5.
  
  ```
* **Integer Division (**`//`):

  ```
  ?- X is 10 // 4.  % X = 2.
  
  ```
* **Modulus (**`mod`):

  ```
  ?- X is 10 mod 4.  % X = 2.
  
  ```
* **Exponentiation (**`^`):

  ```
  ?- X is 2 ^ 3.  % X = 8.
  
  ```

### 2.2 Comparison Operators

<a id="comparison-operators"></a>

* **Equal (**`=:=`):

  ```
  ?- 2 + 3 =:= 5.  % true.
  
  ```
* **Not Equal (**`=\=`):

  ```
  ?- 2 + 2 =\= 5.  % true.
  
  ```
* **Less Than (**`<`), Greater Than (`>`), Less Than or Equal (`=<`), Greater Than or Equal (`>=`):

  ```
  ?- 3 < 5.  % true.
  ?- 5 >= 3. % true.
  
  ```

### 2.3 Logical Operators

<a id="logical-operators"></a>

* **Logical AND (**`,`):

  ```
  eligible(Person) :- over_18(Person), has_valid_id(Person).
  
  ```
* **Logical OR (**`;`):

  ```
  role(Person, student) :- student(Person).
  role(Person, teacher) :- teacher(Person).
  
  ```
* **Logical NOT (**`\+`):

  ```
  not_student(Person) :- \+ student(Person).
  
  ```

### 2.4 Unification and Equality Operators

<a id="unification-and-equality-operators"></a>

* **Unification (**`=`):

  ```
  ?- X = 5.  % X becomes 5.
  
  ```
* **Strict Equality (**`==`):

  ```
  ?- 5 == 5.  % true.
  
  ```
* **Strict Inequality (**`\==`):

  ```
  ?- 5 \== 3.  % true.
  
  ```

### 2.5 Arithmetic Evaluation (`is`)

<a id="arithmetic-evaluation-is"></a>

* *Usage:* Evaluate the right-hand expression and bind the result.

  ```
  ?- X is 2 + 3.  % X = 5.
  
  ```

### 2.6 Operator Precedence and Associativity

<a id="operator-precedence--associativity"></a>

* *Example:*

  ```
  ?- X is 2 + 3 * 4.  % X = 14.
  ?- Y is (2 + 3) * 4.  % Y = 20.
  
  ```
* *Key Table:*

  | Operator | Precedence | Associativity |
  |----------|------------|---------------|
  | `is` | 700 | xfx |
  | `=:=`, `=\=` | 700 | xfx |
  | `<`, `>`, `=<`, `>=` | 700 | xfx |
  | `+`, `-` | 500 | yfx |
  | `*`, `/`, `//`, `mod`, `^` | 400 | yfx |
  | `\+` | 200 | fy |
  | `,` | 100 | xfy |
  | `;` | 100 | xfy |

### 2.7 Logical Implications (`:-`)

<a id="logical-implications"></a>

* *Usage:* Defines a rule where the head is true if the body is true.

  ```
  happy(Person) :- has_money(Person), healthy(Person).
  
  ```

---

## 3. Lists

<a id="lists"></a>

Lists in Prolog are fundamental data structures representing ordered sequences of elements. They can contain numbers, atoms, strings, even other lists, and can be partially instantiated (with variables).

---

### 3.1 List Basics

<a id="list-basics-syntax-headtail-decomposition"></a>

**Definition:**  
Lists are written using square brackets and elements are separated by commas.

**Examples & Explanations:**

1. **Empty List:**

   ```
   [].
   
   ```

   *Explanation:*  
   This is the simplest list, containing no elements. It is useful as the base case in recursive predicates.
2. **List of Integers:**

   ```
   [1, 2, 3, 4].
   
   ```

   *Explanation:*  
   This list contains four integer elements. When processing such a list, you might use arithmetic operations on the elements.
3. **List of Atoms:**

   ```
   [apple, banana, cherry].
   
   ```

   *Explanation:*  
   Atoms (like `apple`) are constants. This list might represent names or labels.
4. **Mixed List:**

   ```
   [g01, "task", 3, [sublist]].
   
   ```

   *Explanation:*  
   Here, the list contains an atom (`g01`), a string (`"task"`), an integer (`3`), and even another list (`[sublist]`). Prolog lists are heterogeneous.
5. **List with Variables:**

   ```
   [X, Y, Z].
   
   ```

   *Explanation:*  
   When elements are variables, Prolog can later instantiate them based on rules or queries. This is useful when generating or matching lists dynamically.

---

### 3.2 Head and Tail Decomposition

<a id="head-tail-decomposition"></a>

**Concept:**  
Prolog uses the notation `[Head|Tail]` to split a list into its first element (Head) and the remaining list (Tail).

**Examples & Explanations:**

1. **Decomposing a Non-Empty List:**

   ```
   [Head|Tail] = [a, b, c, d].
   
   ```

   *Explanation:*
   * **Head:** Unifies with `a` (the first element).
   * **Tail:** Unifies with `[b, c, d]` (the remainder).  
     *Why:*  
     This technique is central for recursion; you process the head and then recursively process the tail.
2. **Single-Element List:**

   ```
   [Head|Tail] = [a].
   
   ```

   *Explanation:*
   * **Head:** Unifies with `a`.
   * **Tail:** Unifies with `[]` (an empty list) because there are no further elements.  
     *Why:*  
     This shows that even a single-element list is decomposed into an element and an empty tail.
3. **Using Variables:**

   ```
   [X|Rest] = [1, 2, 3].
   
   ```

   *Explanation:*
   * **X:** Will be instantiated to `1`.
   * **Rest:** Will be instantiated to `[2, 3]`.  
     *Why:*  
     This pattern is frequently used in recursive predicates (e.g., printing each element).
4. **Recursive Predicate Example – Print List:**

   ```
   print_list([]).
   print_list([Head|Tail]) :-
       write(Head), nl,
       print_list(Tail).
   
   ```

   *Explanation:*
   * **Base Case:** When the list is empty (`[]`), the predicate succeeds without printing.
   * **Recursive Case:** The head is printed, and the predicate recursively processes the tail.  
     *Why:*  
     Demonstrates how head–tail decomposition drives recursion.

---

### 3.3 Common List Predicates

<a id="common-list-predicates"></a>

Below are the core predicates for list manipulation. Each predicate can work in different modes depending on which arguments are known (instantiated) and which are variables.

---

#### 3.3.1 `member/2`

<a id="member2-and-memberchk2"></a>

**Purpose:**
Determines if an element is a member of a list or enumerates all elements.

**Examples & Explanations:**

1. **Check Membership (Element Known, List Known):**

   ```prolog
   ?- member(b, [a, b, c]).
   ```

   *Explanation:*

   * The query asks, "Is `b` in the list `[a, b, c]`?"
   * Since `b` is present, it succeeds (`true`).

2. **Enumerate Members (Element Variable, List Known):**

   ```prolog
   ?- member(X, [a, b, c]).
   ```

   *Explanation:*

   * Prolog assigns `X = a`, then on backtracking `X = b`, then `X = c`.
   * **Output:**

     * `X = a`
     * `X = b`
     * `X = c`
       *Why:*
       Useful for generating all elements one by one.

3. **Using with Duplicates:**

   ```prolog
   ?- member(a, [a, b, a, c]).
   ```

   *Explanation:*

   * `a` appears twice, but `member/2` succeeds on the first occurrence.
   * On backtracking, it can yield the second occurrence if needed.

4. **Generating Lists (List Variable):**

   ```prolog
   ?- member(1, L).
   ```

   *Explanation:*

   * Here, `L` is a variable. Prolog attempts to generate lists that contain `1`.
   * **Caution:** This can lead to infinitely many solutions unless you constrain `L` further.

5. **Not a Member (Using Negation):**

   ```prolog
   ?- \+ member(d, [a, b, c]).
   ```

   *Explanation:*

   * The query asks, "Is `d` not in the list `[a, b, c]`?"
   * Because `d` is absent, `\+ member(d, [a, b, c])` succeeds (`true`).
   * If you tried `\+ member(b, [a, b, c])`, it would fail because `b` *is* a member.

---

#### 3.3.2 `memberchk/2`

**Purpose:**
A deterministic version of `member/2` that stops after the first successful match.

**Examples & Explanations:**

1. **Check First Occurrence:**

   ```prolog
   ?- memberchk(b, [a, b, c, b]).
   ```

   *Explanation:*

   * Finds `b` and stops immediately.
   * **Output:** `true`.

2. **Enumerating with Variable (Less Useful):**

   ```prolog
   ?- memberchk(X, [a, b, c]).
   ```

   *Explanation:*

   * `X` is unified with `a` (the first element) and then stops.
   * **Why:**
     Use `memberchk/2` when you only need a yes/no answer about membership (no backtracking).

---

#### 3.3.3 `append/3`

<a id="append3"></a>

**Purpose:**  
Used for concatenating lists or splitting a list into two parts. It works in multiple modes.

**Examples & Explanations:**

1. **Concatenating Two Lists:**

   ```
   ?- append([a, b], [c, d], X).
   
   ```

   *Explanation:*
   * **Input:** First list `[a,b]`, second list `[c,d]`.
   * **Output:** `X` becomes `[a, b, c, d]`.
   * *Why:*  
     Directly joins two lists.
2. **Splitting a List into Two Parts:**

   ```
   ?- append(X, Y, [1, 2, 3]).
   
   ```

   *Explanation:*
   * Both `X` and `Y` are variables. Prolog finds all ways to split `[1,2,3]`:
     * `X = []`, `Y = [1,2,3]`
     * `X = [1]`, `Y = [2,3]`
     * `X = [1,2]`, `Y = [3]`
     * `X = [1,2,3]`, `Y = []`
   * *Why:*  
     This is useful for deconstructing a list or generating candidate splits.
3. **Using with a Partially Instantiated List:**

   ```
   ?- append([a, b], Y, [a, b, c, d]).
   
   ```

   *Explanation:*
   * Given that `[a, b]` is a prefix of `[a,b,c,d]`, Prolog determines `Y` must be `[c, d]`.
   * *Why:*  
     Constrains the solution by providing known parts of the list.
4. **Reverse-Mode Usage Example:**  
   You can use `append/3` to “guess” a list when provided with its parts. For instance, in writing a predicate that splits and then recombines a list in a different order.

---

#### 3.3.4 `length/2`

<a id="length2"></a>

**Purpose:**  
Determines the number of elements in a list, or can generate a list of a specified length.

**Examples & Explanations:**

1. **Finding the Length of a Given List:**

   ```
   ?- length([a, b, c], N).
   
   ```

   *Explanation:*
   * Prolog counts the elements.
   * **Output:** `N = 3`.
2. **Generating a List of a Given Length:**

   ```
   ?- length(L, 3).
   
   ```

   *Explanation:*
   * Here, `L` is a variable and is instantiated to a list with exactly 3 elements (e.g., `[ _G1, _G2, _G3 ]`).
   * *Why:*  
     Useful when you need a list of a particular size for further processing.
3. **Empty List Case:**

   ```
   ?- length([], N).
   
   ```

   *Explanation:*
   * Since the list is empty, `N = 0`.
4. **Using as a Constraint:**

   ```
   ?- L = [a, b | Tail], length(L, 5).
   
   ```

   *Explanation:*
   * The list starts with `[a,b]` and then an unknown tail. Constraining the length to 5 forces `Tail` to be a list of 3 elements.

---

#### 3.3.5 `nth1/3`

<a id="nth13"></a>

**Purpose:**  
Accesses the N-th element of a list using 1-based indexing.

**Examples & Explanations:**

1. **Retrieve the Second Element:**

   ```
   ?- nth1(2, [a, b, c], X).
   
   ```

   *Explanation:*
   * Index `2` means the second element, so `X = b`.
2. **Find the Position of an Element:**

   ```
   ?- nth1(Pos, [a, b, c], b).
   
   ```

   *Explanation:*
   * Prolog finds that `b` is at position `2` (since indexing starts at 1).
3. **First Element Access:**

   ```
   ?- nth1(1, [apple, banana, cherry], X).
   
   ```

   *Explanation:*
   * `X` is unified with `apple` because it is the first element.
4. **Out-of-Bounds Query:**

   ```
   ?- nth1(4, [a, b, c], X).
   
   ```

   *Explanation:*
   * The list has only 3 elements, so the query fails.
   * *Why:*  
     The predicate expects the index to be within the valid range (1 to length of list).

---

#### 3.3.6 `findall/3`

<a id="findall3"></a>

**Purpose:**  
Collects all solutions for a given goal and returns them as a list.

**Examples & Explanations:**

1. **Collect All Elements of a List:**

   ```
   ?- findall(X, member(X, [a, b, c]), L).
   
   ```

   *Explanation:*
   * The goal `member(X, [a, b, c])` generates `X = a`, then `b`, then `c`.
   * **Output:** `L = [a, b, c]`.
2. **Computing Squares:**

   ```
   ?- findall(Square, (between(1, 5, N), Square is N * N), Squares).
   
   ```

   *Explanation:*
   * `between(1,5,N)` generates numbers from 1 to 5.
   * For each `N`, `Square is N*N` computes its square.
   * **Output:** `Squares = [1, 4, 9, 16, 25]`.
3. **No Solutions Scenario:**

   ```
   ?- findall(X, (member(X, [a,b]), X == c), L).
   
   ```

   *Explanation:*
   * There is no `X` in `[a, b]` such that `X == c`.
   * **Output:** `L = []`.
4. **Using findall with Complex Goals:**

   ```
   ?- findall([X, Y], (member(X, [1,2]), member(Y, [a,b])), Pairs).
   
   ```

   *Explanation:*
   * Generates all pairs `[X, Y]` where `X` is from `[1,2]` and `Y` is from `[a,b]`.
   * **Output:** `Pairs = [[1,a], [1,b], [2,a], [2,b]]`.

---

#### 3.3.7 `select/3`

<a id="select3"></a>

**Purpose:**  
Removes one occurrence of an element from a list, returning the remainder.

**Examples & Explanations:**

1. **Remove a Specific Element:**

   ```
   ?- select(b, [a, b, c], L).
   
   ```

   *Explanation:*
   * Removes the first occurrence of `b` from the list.
   * **Output:** `L = [a, c]`.
2. **Enumerate All Removals (With Duplicates):**

   ```
   ?- select(X, [a, b, a, c], L).
   
   ```

   *Explanation:*
   * Prolog will first choose `X = a` and `L = [b, a, c]`.
   * On backtracking, it may choose `X = b` with `L = [a, a, c]`, then again `X = a` with a different removal order, etc.
   * *Why:*  
     This illustrates how `select/3` can be used to enumerate different ways of removing one occurrence.
3. **Using select to Split a List:**

   ```
   ?- select(X, [1,2,3,4], Rest).
   
   ```

   *Explanation:*
   * This will generate all pairs where one element is selected and the remaining list (`Rest`) is returned.
   * **Outputs include:**
     * `X = 1, Rest = [2,3,4]`
     * `X = 2, Rest = [1,3,4]`
     * etc.

---

#### 3.3.8 `reverse/2`

<a id="reverse2"></a>

**Purpose:**  
Reverses the order of elements in a list.

**Examples & Explanations:**

1. **Reversing a Given List:**

   ```
   ?- reverse([a, b, c], X).
   
   ```

   *Explanation:*
   * The predicate produces `X = [c, b, a]`.
2. **Using reverse in the Opposite Mode:**

   ```
   ?- reverse(X, [c, b, a]).
   
   ```

   *Explanation:*
   * Prolog infers that `X` must be `[a, b, c]` because reversing `[a, b, c]` gives `[c, b, a]`.
3. **Both Arguments Variables (Use with Caution):**

   ```
   ?- reverse(X, Y).
   
   ```

   *Explanation:*
   * With both variables free, Prolog may generate many pairs where one list is the reverse of the other. Typically, at least one list is provided.

---

#### 3.3.9 `last/2`

<a id="last2"></a>

**Purpose:**  
Retrieves the last element of a list.

**Examples & Explanations:**

1. **Simple Case:**

   ```
   ?- last([a, b, c], X).
   
   ```

   *Explanation:*
   * The last element of `[a,b,c]` is `c`, so `X = c`.
2. **Using in Verification:**

   ```
   ?- last([1, 2, 3, 4], 4).
   
   ```

   *Explanation:*
   * This succeeds because `4` is indeed the last element of the list.
3. **Failure on Empty List:**

   ```
   ?- last([], X).
   
   ```

   *Explanation:*
   * The predicate fails since there is no last element in an empty list.

---

#### 3.3.10 `delete/3`

<a id="delete3"></a>

**Purpose:**  
Removes all occurrences of a specified element from a list.

**Examples & Explanations:**

1. **Removing a Single Occurrence (All Occurrences):**

   ```
   ?- delete([a, b, c, b], b, L).
   
   ```

   *Explanation:*
   * All `b` elements are removed from the list.
   * **Output:** `L = [a, c]`.
2. **With Multiple Duplicates:**

   ```
   ?- delete([a, b, a, c, a], a, L).
   
   ```

   *Explanation:*
   * Removes every occurrence of `a`.
   * **Output:** `L = [b, c]`.
3. **Using delete in a Goal:**

   ```
   remove_item(Item, List, Result) :-
       delete(List, Item, Result).
   
   ```

   *Explanation:*
   * This predicate wraps `delete/3` to remove all instances of `Item` from `List`.

---

#### 3.3.11 `union/3` and `intersection/3`

<a id="union3-and-intersection3"></a>

**Purpose:**  
Compute the union (merging lists without duplicates) and intersection (common elements) of two lists.

**Examples & Explanations:**

1. **Union of Two Lists:**

   ```
   ?- union([a, b, c], [b, c, d], X).
   
   ```

   *Explanation:*
   * The union of `[a,b,c]` and `[b,c,d]` is `[a,b,c,d]`.
   * *Note:* Order is typically preserved from the first list, with new elements appended.
2. **Intersection of Two Lists:**

   ```
   ?- intersection([a, b, c], [b, c, d], X).
   
   ```

   *Explanation:*
   * Only the elements present in both lists are returned.
   * **Output:** `X = [b, c]`.
3. **Handling Duplicates:**
   * **Union Example with Duplicates:**

     ```
     ?- union([a, b, a], [b, c, c], X).
     
     ```

     *Explanation:*
     * Despite duplicates in the inputs, the output is unique: `X = [a, b, c]`.
   * **Intersection Example with Duplicates:**

     ```
     ?- intersection([a, a, b, c], [a, c, d], X).
     
     ```

     *Explanation:*
     * The intersection returns unique common elements: `X = [a, c]`.

---

### 3.4 Advanced List Predicates

<a id="advanced-list-predicates"></a>

#### 3.4.1 `sort/2` vs. `msort/2`

<a id="sort2-and-msort2"></a>

Both predicates sort lists, but with a crucial difference regarding duplicates.

##### `sort/2`

* **Purpose:** Sorts a list in ascending order while removing duplicates.
* **Example 1 – Basic Sorting:**

  ```
  ?- sort([3, 1, 2, 3, 4, 2], X).
  
  ```

  **Explanation:**
  * **Input:** `[3,1,2,3,4,2]`
  * **Process:** The list is sorted to `[1,2,3,4]`, and duplicate values are removed.
  * **Output:** `X = [1,2,3,4]`
* **Example 2 – With Atoms and Mixed Types:**

  ```
  ?- sort([banana, apple, cherry, apple], Sorted).
  
  ```

  **Explanation:**
  * Atoms are compared lexicographically.
  * **Output:** `Sorted = [apple, banana, cherry]`
  * **Note:** Duplicates (e.g., `apple`) are removed.

##### `msort/2`

* **Purpose:** Sorts a list in ascending order but *preserves* all duplicates.
* **Example 1 – Basic Sorting with Duplicates:**

  ```
  ?- msort([3, 1, 2, 3, 4, 2], X).
  
  ```

  **Explanation:**
  * **Input:** `[3,1,2,3,4,2]`
  * **Process:** The list is sorted, but every instance is kept.
  * **Output:** `X = [1,2,2,3,3,4]`
* **Example 2 – Mixed Data:**

  ```
  ?- msort([banana, apple, cherry, apple], Sorted).
  
  ```

  **Explanation:**
  * **Output:** `Sorted = [apple, apple, banana, cherry]`
  * Duplicates are preserved in the order given by the sorting algorithm.

---

#### 3.4.2 Custom Predicates

<a id="custom-predicates-unique2-and-intersect3"></a>

Custom predicates allow you to implement specific behaviors not built into Prolog. Two common examples are `unique/2` and `intersect/3`.

##### `unique/2`

* **Purpose:** Removes duplicate elements while preserving the order of the *first occurrences*.
* **Implementation:**

  ```
  unique([], []).
  unique([H|T], [H|UniqueT]) :-
      \+ member(H, T),
      unique(T, UniqueT).
  unique([H|T], UniqueT) :-
      member(H, T),
      unique(T, UniqueT).
  
  ```
* **Example 1 – Simple Duplicate Removal:**

  ```
  ?- unique([1,2,3,2,4,1], X).
  
  ```

  **Explanation:**
  * The predicate processes the list from left to right.
  * **Output:** `X = [1,2,3,4]`
  * The first occurrence of `1` is kept; subsequent `1`’s are removed.
* **Example 2 – List of Atoms:**

  ```
  ?- unique([apple, banana, apple, cherry, banana], Unique).
  
  ```

  **Explanation:**
  * **Output:** `Unique = [apple, banana, cherry]`
  * Maintains the order of first appearances.

##### `intersect/3`

* **Purpose:** Finds the intersection between two lists, i.e., elements that appear in both lists. Depending on the implementation, duplicates may or may not be handled.
* **Implementation:**

  ```
  intersect([], _, []).
  intersect([H|T], L2, [H|I]) :-
      member(H, L2),
      intersect(T, L2, I).
  intersect([H|T], L2, I) :-
      \+ member(H, L2),
      intersect(T, L2, I).
  
  ```
* **Example 1 – Basic Intersection:**

  ```
  ?- intersect([1,2,3,4], [3,4,5,6], X).
  
  ```

  **Explanation:**
  * **Input:** First list is `[1,2,3,4]` and second list is `[3,4,5,6]`.
  * **Process:** Only elements present in both lists are retained.
  * **Output:** `X = [3,4]`
* **Example 2 – With Duplicates in First List:**

  ```
  ?- intersect([a, a, b, c], [a, c, d], Y).
  
  ```

  **Explanation:**
  * **Output:** `Y = [a, c]`
  * The predicate will include `a` only once if it checks uniqueness or as many times as it appears in the first list if duplicates are not explicitly handled.

---

## 3.5 Haskell-like List Functions

<a id="haskell-like-list-functions"></a>

This section discusses a series of list-processing predicates reminiscent of common functions in Haskell. Each predicate is accompanied by a thorough explanation of its inner workings. Where relevant, we also illustrate **partial application** (fixing some arguments of a predicate while leaving others free) and why it matters in predicates like `takewhile/3` and `dropwhile/3`.

---

### 3.5.1 `take/3`

<a id="take3"></a>

**Purpose:**  
Extracts the first N elements from a list, or fewer if the list is shorter than N.

**Implementation:**

```
take(0, _, []).
take(_, [], []).
take(N, [H|T], [H|Taken]) :-
    N > 0,
    N1 is N - 1,
    take(N1, T, Taken).
```

1. The **base cases**:
   * If N = 0, no elements are taken.
   * If the list is empty, the result is also empty.
2. The **recursive case**:
   * Take one element `H` if `N > 0`.
   * Decrement N and recurse down the tail of the list.

#### Example: Select Only a Few Items

```
?- take(3, [apple, banana, cherry, date, fig], X).
```

* **Process**:
  1. We start with N=3. The head of the list is `apple`, so it is included.
  2. Decrement N to 2, move on to `banana`.
  3. Decrement N to 1, move on to `cherry`.
  4. Decrement N to 0. At this point, the recursion stops taking new elements.
* **Outcome**: Prolog unifies `X` with `[apple, banana, cherry]`.

#### Example: Limiting an Already Short List

```
?- take(5, [red, green], X).
```

* **Process**:
  1. The list has only two elements. By the time we run out of elements, we have already taken `[red, green]`.
  2. The predicate stops once the list is empty.
* **Outcome**: `X` is `[red, green]`.

---

### 3.5.2 `drop/3`

<a id="drop3"></a>

**Purpose:**  
Discards the first N elements of a list, returning whatever remains.

**Implementation:**

```
drop(0, List, List).
drop(_, [], []).
drop(N, [_|T], Dropped) :-
    N > 0,
    N1 is N - 1,
    drop(N1, T, Dropped).
```

1. If N = 0, none are discarded, so the result is the original list.
2. If the list is empty, nothing remains.
3. Otherwise, consume elements while decrementing N.

#### Example: Skipping a Header

```
?- drop(2, [header1, header2, data1, data2, data3], Remainder).
```

* **Process**:
  1. With `N = 2`, Prolog discards the first two items (`header1`, `header2`).
  2. It returns whatever is left, `[data1, data2, data3]`.
* **Outcome**: `Remainder = [data1, data2, data3]`.

#### Example: Discarding Beyond the List Size

```
?- drop(10, [a, b, c], Remainder).
```

* **Process**:
  1. The predicate discards `a`, `b`, and `c`.
  2. There are no more elements to process, so the result is the empty list.
* **Outcome**: `Remainder = []`.

---

### 3.5.3 `takewhile/3` and `dropwhile/3`

<a id="takewhile-dropwhile"></a>

Both predicates rely on `call(P, Elem)` to test whether an element satisfies a condition. This requires that the predicate `P` be a unary predicate (i.e. take one argument). If you have a predicate with more arguments—one or more of which must be provided dynamically—you must “partially apply” it (wrap it so that some arguments are fixed by variables) so that it becomes unary.

Below, we define a couple of higher-arity predicates and show examples where the extra arguments are not fixed as constants but are taken from dynamic variables.

#### Definitions

```
% takewhile/3: Collects elements from the front of a list as long as P succeeds.
takewhile(_, [], []).
takewhile(P, [H|T], [H|Taken]) :-
    call(P, H),
    takewhile(P, T, Taken).
takewhile(P, [H|_], []) :-
    \+ call(P, H).

% dropwhile/3: Discards elements from the front of a list as long as P succeeds.
dropwhile(_, [], []).
dropwhile(P, [H|T], Dropped) :-
    call(P, H),
    dropwhile(P, T, Dropped).
dropwhile(P, List, List) :-
    List \= [],
    \+ (List = [H|_], call(P, H)).
```

#### Example Setup: A 2-Argument Predicate with a Dynamic First Argument

Imagine a predicate that compares two numbers:

```
% less_than/2: Succeeds if Y is less than X.
less_than(X, Y) :- Y < X.
```

In many cases, you want the first argument (the threshold) to come from the context of your data rather than a fixed number.

**Example 1 – Using** `takewhile/3` with `less_than/2`

We want to collect elements from a list that are less than the first element. The first element is dynamic and becomes the threshold.

```
?- List = [10, 8, 6, 12, 4],
   List = [Threshold|Tail],
   takewhile(less_than(Threshold), Tail, Result).
```

* **Explanation:**
  1. The list is deconstructed so that `Threshold = 10` and `Tail = [8, 6, 12, 4]`.
  2. The call `less_than(Threshold)` becomes a unary predicate equivalent to:

     ```
     ElemSatisfies :- less_than(10, Elem).
     
     ```
  3. As `takewhile/3` processes `Tail`, it tests each element:
     * `8 < 10` succeeds,
     * `6 < 10` succeeds,
     * `12 < 10` fails (stopping the process).
  4. **Outcome:** `Result = [8, 6]`.

**Example 2 – Using** `dropwhile/3` with `less_than/2`

Now we drop elements that are less than the first element.

```
?- List = [10, 8, 6, 12, 4],
   List = [Threshold|Tail],
   dropwhile(less_than(Threshold), Tail, Result).
```

* **Explanation:**
  1. Again, `Threshold = 10` and `Tail = [8, 6, 12, 4]`.
  2. `dropwhile/3` tests each element with `less_than(10, Elem)`:
     * It drops `8` and `6` because both satisfy the condition.
     * When it reaches `12` (since `12 < 10` fails), it stops and returns the remainder.
  3. **Outcome:** `Result = [12, 4]`.

---

#### Example Setup: A 3-Argument Predicate with Two Dynamic Arguments

Now consider a predicate that checks if a number is within an interval:

```
% in_interval/3: Succeeds if X is between Min and Max (inclusive).
in_interval(Min, Max, X) :- X >= Min, X =< Max.
```

We want both `Min` and `Max` to be provided dynamically (for example, as the first two elements of a list).

**Example 3 – Using** `takewhile/3` with `in_interval/3`

We deconstruct a list so that the first two elements become the dynamic bounds.

```
?- List = [3, 7, 4, 5, 6, 8, 2],
   List = [Min, Max | Tail],
   takewhile(in_interval(Min, Max), Tail, Result).
```

* **Explanation:**
  1. Here, `Min = 3`, `Max = 7`, and `Tail = [4, 5, 6, 8, 2]`.
  2. The partial application `in_interval(Min, Max)` acts as a unary predicate testing:

     ```
     ElemSatisfies :- in_interval(3, 7, Elem).
     
     ```
  3. The predicate collects `4, 5, 6` (all between 3 and 7). It stops at `8` because `8` is not ≤ 7.
  4. **Outcome:** `Result = [4, 5, 6]`.

**Example 4 – Using** `dropwhile/3` with `in_interval/3`

Using the same dynamic bounds, now drop the elements that satisfy the interval.

```
?- List = [3, 7, 4, 5, 6, 8, 2],
   List = [Min, Max | Tail],
   dropwhile(in_interval(Min, Max), Tail, Result).
```

* **Explanation:**
  1. With `Min = 3`, `Max = 7`, and `Tail = [4, 5, 6, 8, 2]`, the predicate checks each element:
     * It drops `4, 5, 6` because they lie within 3 and 7.
     * It stops at `8`, since `8` is not in the interval.
  2. **Outcome:** `Result = [8, 2]`.

---

#### Example Setup: A 2-Argument Predicate with a Dynamic First Argument (Different Relation)

Consider a predicate that checks if a value is greater than a dynamic value:

```
% greater_than/2: Succeeds if Y is greater than X.
greater_than(X, Y) :- Y > X.
```

We use it to collect or drop elements relative to a dynamic reference.

**Example 5 – Using** `takewhile/3` with `greater_than/2`

Suppose you want to take elements from a list that are greater than the first element.

```
?- List = [5, 7, 9, 4, 8],
   List = [Threshold|Tail],
   takewhile(greater_than(Threshold), Tail, Result).
```

* **Explanation:**
  1. Here, `Threshold = 5` and `Tail = [7, 9, 4, 8]`.
  2. The call `greater_than(Threshold)` is interpreted as:

     ```
     ElemSatisfies :- greater_than(5, Elem).
     
     ```
  3. It collects `7` and `9` because both are greater than 5; it stops at `4` (which is not > 5).
  4. **Outcome:** `Result = [7, 9]`.

**Example 6 – Using** `dropwhile/3` with `greater_than/2`

Similarly, dropping the elements that are greater than the dynamic threshold:

```
?- List = [5, 7, 9, 4, 8],
   List = [Threshold|Tail],
   dropwhile(greater_than(Threshold), Tail, Result).
```

* **Explanation:**
  1. With `Threshold = 5` and `Tail = [7, 9, 4, 8]`, the predicate drops `7` and `9`.
  2. It stops when it finds `4`, which does not satisfy `greater_than(5, 4)`.
  3. **Outcome:** `Result = [4, 8]`.

---

### 3.5.4 `split_at/4`

<a id="split_at4"></a>

**Purpose:**  
Divides a list into two parts at a given index **N**.

**Implementation:**

```
split_at(0, List, [], List).
split_at(_, [], [], []).
split_at(N, [H|T], [H|Left], Right) :-
    N > 0,
    N1 is N - 1,
    split_at(N1, T, Left, Right).
```

1. If `N` is 0, the left part is empty, and the right part is the entire list.
2. If the list is empty, both parts are empty regardless of N.
3. Otherwise, decrement N and move the head to the left part until N hits 0.

#### Example: Splitting a Student List

```
?- split_at(3, [alice, bob, charlie, diana, eric], GroupA, GroupB).
```

* **Process**:
  1. The first three names move to `GroupA`: `[alice, bob, charlie]`.
  2. The rest end up in `GroupB`: `[diana, eric]`.
* **Outcome**: `GroupA = [alice, bob, charlie]`, `GroupB = [diana, eric]`.

#### Example: Splitting an Inventory Beyond Its Length

```
?- split_at(10, [pen, pencil, eraser], Part1, Part2).
```

* **Process**:
  1. We move all items into the left group because we never run out of index steps before the list ends.
  2. When the list is empty, recursion stops.
* **Outcome**: `Part1 = [pen, pencil, eraser]`, `Part2 = []`.

---

### 3.5.5 `zip/3`

<a id="zip3"></a>

**Purpose:**  
Combines two lists **element by element** into a new list of pairs. The process ceases when the shortest list is exhausted.

**Implementation:**

```
zip([], _, []).
zip(_, [], []).
zip([H1|T1], [H2|T2], [[H1,H2]|Zipped]) :-
    zip(T1, T2, Zipped).
```

1. If either list is empty, the zipping ends immediately.
2. Pairs up the heads of both lists, then recurses on the tails.

#### Example: Pairing Names and Ages

```
?- zip([alice, bob, charlie], [20, 21, 22], Paired).
```

* **Process**:
  1. Combine `alice` with `20`, `bob` with `21`, `charlie` with `22`.
  2. End of both lists reached simultaneously.
* **Outcome**: `Paired = [[alice, 20], [bob, 21], [charlie, 22]]`.

#### Example: Combining Days of the Week and Weather

```
?- zip([monday, tuesday, wednesday], [sunny, cloudy], Output).
```

* **Process**:
  1. `monday` is paired with `sunny`, `tuesday` is paired with `cloudy`.
  2. The second list is now empty, so the process stops.
* **Outcome**: `Output = [[monday, sunny], [tuesday, cloudy]]`.

---

### Key Takeaways

1. **Recursion and Base Cases**: Each of these list predicates relies on defining clear base cases (e.g., when a list is empty or a counter is zero) and then handling the recursive step.
2. **call/2 and Partial Application**:
   * `takewhile/3` and `dropwhile/3` use `call(P, Element)` to apply a predicate `P` to `Element`.
   * `P` must have arity 1. If you have a predicate of higher arity, **partially apply** it by fixing some arguments in advance.
3. **Early Stopping**:
   * `takewhile/3` and `dropwhile/3` can terminate the traversal early depending on whether the predicate passes or fails.
   * `zip/3` finishes once at least one of the input lists is exhausted.
4. **Splitting vs. Taking/Dropping**:
   * `split_at/4` divides the list at a specific index, while `take/3` and `drop/3` each cover only one half of that logic.

### 3.6 Recursive List Processing

<a id="recursive-list-processing"></a>

Recursive predicates are central to list processing in Prolog. Here we illustrate several examples with detailed explanations.

#### 3.6.1 Sum of Elements

<a id="sum-of-elements"></a>

* **Implementation:**

  ```
  sum_list([], 0).
  sum_list([H|T], Sum) :-
      sum_list(T, Rest),
      Sum is H + Rest.
  
  ```
* **Example:**

  ```
  ?- sum_list([1,2,3,4], Sum).
  
  ```

  **Explanation:**
  * **Base Case:** An empty list sums to 0.
  * **Recursive Case:** For `[1,2,3,4]`, it computes `Sum = 1 + (2 + (3 + (4 + 0)))`.
  * **Output:** `Sum = 10`.

---

#### 3.6.2 Maximum Element

<a id="maximum-element"></a>

* **Implementation:**

  ```
  max_list([X], X).
  max_list([H|T], Max) :-
      max_list(T, TempMax),
      Max is max(H, TempMax).
  
  ```
* **Example:**

  ```
  ?- max_list([3,1,4,2], Max).
  
  ```

  **Explanation:**
  * Compares each element recursively.
  * **Output:** `Max = 4`.

---

#### 3.6.3 Minimum Element

<a id="minimum-element"></a>

* **Implementation:**

  ```
  min_list([X], X).
  min_list([H|T], Min) :-
      min_list(T, TempMin),
      Min is min(H, TempMin).
  
  ```
* **Example:**

  ```
  ?- min_list([3,1,4,2], Min).
  
  ```

  **Explanation:**
  * **Output:** `Min = 1`.

---

#### 3.6.4 Product of Elements

<a id="product-of-elements"></a>

* **Implementation:**

  ```
  product_list([], 1).
  product_list([H|T], Product) :-
      product_list(T, Rest),
      Product is H * Rest.
  
  ```
* **Example:**

  ```
  ?- product_list([1,2,3,4], Product).
  
  ```

  **Explanation:**
  * Multiplies all elements together.
  * **Output:** `Product = 24`.

---

#### 3.6.5 Average of Elements

<a id="average-of-elements"></a>

* **Implementation:**

  ```
  average_list(List, Avg) :-
      sum_list(List, Sum),
      length(List, Len),
      Len > 0,
      Avg is Sum / Len.
  
  ```
* **Example:**

  ```
  ?- average_list([1,2,3,4], Avg).
  
  ```

  **Explanation:**
  * Computes the sum and divides by the length.
  * **Output:** `Avg = 2.5`.

---

### 3.7 Haskell-like Higher-Order Functions

<a id="haskell-like-higher-order-functions"></a>

These predicates enable a higher–level functional programming style with lists.

#### 3.7.1 `map/3`

<a id="map3"></a>

**Purpose:**  
Applies a given predicate to each element of the input list to produce an output list.

**Implementation:**

```
map([], [], _F).
map([H|T], [H1|T1], F) :-
    call(F, H, H1),
    map(T, T1, F).
```

**Examples:**

* **Squaring Numbers:**

  ```
  square(X, Y) :- Y is X * X.
  ?- map([1,2,3,4], Squares, square).
  
  ```

  *Explanation:* Each element is squared.  
  *Output:* `Squares = [1,4,9,16]`
* **Converting Atoms to Uppercase:**  
  (Assuming you have `upcase_atom/2` available)

  ```
  ?- map([apple, banana], Upper, upcase_atom).
  
  ```

  *Explanation:* Converts each atom to its uppercase equivalent.  
  *Output:* Possibly `Upper = [APPLE, BANANA]`
* **Converting Numbers to Atoms:**  
  Define a helper predicate:

  ```
  number_to_atom(X, Atom) :- atom_number(Atom, X).
  ?- map([1,2,3,4], Atoms, number_to_atom).
  
  ```

  *Explanation:* Each number is converted to its atom representation.  
  *Output:* `Atoms = ['1','2','3','4']`

---

#### 3.7.2 `filter/3`

<a id="filter3"></a>

**Purpose:**  
Selects only those elements from the list that satisfy a given predicate.

**Implementation:**

```
filter([], [], _Pred).
filter([H|T], [H|Filtered], Pred) :-
    call(Pred, H),
    filter(T, Filtered, Pred).
filter([_|T], Filtered, Pred) :-
    filter(T, Filtered, Pred).
```

**Examples:**

* **Filtering Even Numbers:**

  ```
  is_even(X) :- 0 is X mod 2.
  ?- filter([1,2,3,4,5,6], Evens, is_even).
  
  ```

  *Output:* `Evens = [2,4,6]`
* **Filtering Numbers Greater Than 3:**

  ```
  greater_than_3(X) :- X > 3.
  ?- filter([1,2,3,4,5], Result, greater_than_3).
  
  ```

  *Output:* `Result = [4,5]`

---

#### 3.7.3 `foldl/4` and `foldr/4`

<a id="foldl4-and-foldr4"></a>

These predicates process lists while carrying an accumulator.

##### `foldl/4` (Left Fold)

**Purpose:**  
Processes a list from left to right.

**Implementation:**

```
foldl(_, [], Acc, Acc).
foldl(P, [H|T], Acc, Result) :-
    call(P, H, Acc, NewAcc),
    foldl(P, T, NewAcc, Result).
```

**Examples:**

* **Summing Elements:**

  ```
  add(X, Acc, NewAcc) :- NewAcc is Acc + X.
  ?- foldl(add, [1,2,3,4], 0, Sum).
  
  ```

  *Output:* `Sum = 10`
* **Concatenating Atoms:**  
  (Assuming all list elements are atoms)

  ```
  concatenate(H, Acc, NewAcc) :- atom_concat(Acc, H, NewAcc).
  ?- foldl(concatenate, [hello, world], '', Result).
  
  ```

  *Explanation:* Starting with the empty atom, concatenates each atom in order.  
  *Output:* `Result = helloworld`

##### `foldr/4` (Right Fold)

**Purpose:**  
Processes a list from right to left.

**Implementation:**

```
foldr(_, [], Acc, Acc).
foldr(P, [H|T], Acc, Result) :-
    foldr(P, T, Acc, NewAcc),
    call(P, H, NewAcc, Result).
```

**Examples:**

* **Multiplying Elements:**

  ```
  multiply(X, Acc, NewAcc) :- NewAcc is X * Acc.
  ?- foldr(multiply, [1,2,3,4], 1, Product).
  
  ```

  *Output:* `Product = 24`
* **Subtraction Example (Right–Associative):**  
  (This computes `1 - (2 - (3 - 0))`)

  ```
  subtract(X, Acc, Result) :- Result is X - Acc.
  ?- foldr(subtract, [1,2,3], 0, Result).
  
  ```

  *Output:* `Result = 2`

---

#### 3.7.4 `scanl/4` and `scanr/4`

These predicates are similar to folds but return the entire list of intermediate accumulator values.

##### `scanl/4` (Left Scan)

**Purpose:**  
Returns a list of successive accumulator states as the list is processed from left to right.

**Implementation:**

```
scanl(_, Acc, [], [Acc]).
scanl(P, [H|T], Acc, [Acc|Rest]) :-
    call(P, H, Acc, NewAcc),
    scanl(P, T, NewAcc, Rest).
```

**Example – Summing Elements:**

```
add(X, Acc, NewAcc) :- NewAcc is Acc + X.
?- scanl(add, [1,2,3,4], 0, Result).
```

*Explanation:*

* Start with `0`
* 0 + 1 = 1
* 1 + 2 = 3
* 3 + 3 = 6
* 6 + 4 = 10  
  *Output:* `Result = [0,1,3,6,10]`

##### `scanr/4` (Right Scan)

**Purpose:**  
Returns a list of successive accumulator states as the list is processed from right to left.

**Implementation:**

```
scanr(_, Acc, [], [Acc]).
scanr(P, [H|T], Acc, [Result|Rest]) :-
    scanr(P, T, Acc, Rest),
    Rest = [NewAcc|_],
    call(P, H, NewAcc, Result).
```

**Example – Multiplying Elements:**

```
multiply(X, Acc, NewAcc) :- NewAcc is X * Acc.
?- scanr(multiply, [1,2,3,4], 1, Result).
```

*Explanation:*  
Evaluates as in Haskell's `scanr (*) 1 [1,2,3,4]`, producing the intermediate products.  
*Output:* `Result = [24,24,12,4,1]`

---

#### 3.7.5 `all/2` and `any/2`

<a id="all2-and-any2"></a>

These predicates check if all or any elements of a list satisfy a given condition.

##### `all/2`

**Purpose:**  
Succeeds if every element in the list satisfies the predicate.

**Implementation:**

```
all(_Pred, []).
all(Pred, [H|T]) :-
    call(Pred, H),
    all(Pred, T).
```

**Examples:**

* **Checking Even Numbers:**

  ```
  is_even(X) :- 0 is X mod 2.
  ?- all(is_even, [2,4,6,8]).
  
  ```

  *Output:* Succeeds (all numbers are even).
* **Checking Positivity:**

  ```
  greater_than_zero(X) :- X > 0.
  ?- all(greater_than_zero, [1,2,3,4]).
  
  ```

  *Output:* Succeeds.
* **Failure Case:**

  ```
  ?- all(is_even, [2,3,6]).
  
  ```

  *Output:* Fails (since 3 is not even).

##### `any/2`

**Purpose:**  
Succeeds if at least one element in the list satisfies the predicate.

**Implementation:**

```
any(_Pred, []) :- false.
any(Pred, [H|_T]) :-
    call(Pred, H), !.
any(Pred, [_|T]) :-
    any(Pred, T).
```

**Examples:**

* **Checking for an Even Number:**

  ```
  ?- any(is_even, [1,3,5,7]).
  
  ```

  *Output:* Fails (none are even).

  ```
  ?- any(is_even, [1,3,4,7]).
  
  ```

  *Output:* Succeeds (4 is even).
* **Checking for a Negative Number:**

  ```
  is_negative(X) :- X < 0.
  ?- any(is_negative, [1,-1,2,3]).
  
  ```

  *Output:* Succeeds (since -1 is negative).

---

### 3.8 Typical List-Based Examples

<a id="typical-list-based-examples"></a>

These examples demonstrate common algorithms implemented with list recursion. They showcase how recursive patterns, pattern matching, and backtracking work in Prolog when processing lists.

#### 3.8.1 Factorial

<a id="factorial"></a> **What It Does:**  
The factorial predicate computes the factorial of a non-negative integer using recursion. The base case is defined for 0 (0! = 1), and for any N > 0, it recursively computes factorial(N-1) and multiplies it by N.

**Implementation:**

```
factorial(0, 1).
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.
```

**Examples:**

1. **Base Case:**

   ```
   ?- factorial(0, F).
   
   ```

   *Explanation:* Since 0! is defined as 1, Prolog returns `F = 1`.
2. **Small Number:**

   ```
   ?- factorial(5, F).
   
   ```

   *Explanation:* Computes 5! as `5 * 4 * 3 * 2 * 1 = 120`.  
   **Output:** `F = 120`.
3. **Larger Number:**

   ```
   ?- factorial(7, F).
   
   ```

   *Explanation:* Computes 7! as `7 * 6 * 5 * 4 * 3 * 2 * 1 = 5040`.  
   **Output:** `F = 5040`.

---

#### 3.8.2 Fibonacci

<a id="fibonacci"></a> **What It Does:**  
The Fibonacci predicate calculates the N-th Fibonacci number recursively. The base cases are defined for N=0 (result 0) and N=1 (result 1). For N > 1, it sums the (N-1)-th and (N-2)-th Fibonacci numbers.

**Implementation:**

```
fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, F) :-
    N > 1,
    N1 is N - 1, N2 is N - 2,
    fibonacci(N1, F1), fibonacci(N2, F2),
    F is F1 + F2.
```

**Examples:**

1. **Base Case 0:**

   ```
   ?- fibonacci(0, F).
   
   ```

   *Explanation:* Returns the 0th Fibonacci number: `F = 0`.
2. **Base Case 1:**

   ```
   ?- fibonacci(1, F).
   
   ```

   *Explanation:* Returns the 1st Fibonacci number: `F = 1`.
3. **Recursive Case:**

   ```
   ?- fibonacci(7, F).
   
   ```

   *Explanation:* Computes the sequence up to the 7th term (0,1,1,2,3,5,8,13).  
   **Output:** `F = 13`.

---

#### 3.8.3 Reverse a List

<a id="reverse-a-list"></a> **What It Does:**  
The reverse predicate recursively reverses the elements of a list by first reversing the tail and then appending the head to the end of the reversed tail.

**Implementation:**

```
reverse_list([], []).
reverse_list([H|T], Rev) :-
    reverse_list(T, RevT),
    append(RevT, [H], Rev).
```

**Examples:**

1. **Basic List:**

   ```
   ?- reverse_list([a,b,c], X).
   
   ```

   *Explanation:* Reverses `[a,b,c]` to `[c,b,a]`.  
   **Output:** `X = [c,b,a]`.
2. **Numeric List:**

   ```
   ?- reverse_list([1,2,3,4], X).
   
   ```

   *Explanation:* Reverses `[1,2,3,4]` to `[4,3,2,1]`.
3. **Empty List:**

   ```
   ?- reverse_list([], X).
   
   ```

   *Explanation:* The reverse of an empty list is also an empty list.  
   **Output:** `X = []`.

---

#### 3.8.4 Palindrome Check

<a id="palindrome-check"></a> **What It Does:**  
The predicate checks whether a list is a palindrome by comparing the list with its reverse.

**Implementation:**

```
is_palindrome(List) :-
    reverse(List, List).
```

**Examples:**

1. **Alphabetic Palindrome:**

   ```
   ?- is_palindrome([r,a,c,e,c,a,r]).
   
   ```

   *Explanation:* The list is the same forwards and backwards.  
   **Output:** Succeeds.
2. **Numeric Palindrome:**

   ```
   ?- is_palindrome([1,2,3,2,1]).
   
   ```

   *Explanation:* The list reads the same in reverse.  
   **Output:** Succeeds.
3. **Non-Palindrome:**

   ```
   ?- is_palindrome([a,b,c]).
   
   ```

   *Explanation:* `[a,b,c]` ≠ `[c,b,a]`, so the predicate fails.

---

#### 3.8.5 Merge Sort

<a id="merge-sort"></a> **What It Does:**  
Merge sort is a divide-and-conquer algorithm that splits a list into halves, recursively sorts each half, and then merges the sorted halves.

**Implementation:**

```
merge_sort([], []).
merge_sort([X], [X]).
merge_sort(List, Sorted) :-
    List \= [],
    List \= [_],
    split(List, L1, L2),
    merge_sort(L1, Sorted1),
    merge_sort(L2, Sorted2),
    merge(Sorted1, Sorted2, Sorted).

split([], [], []).
split([X], [X], []).
split([X,Y|T], [X|L1], [Y|L2]) :-
    split(T, L1, L2).

merge([], L, L).
merge(L, [], L).
merge([H1|T1], [H2|T2], [H1|T]) :-
    H1 =< H2,
    merge(T1, [H2|T2], T).
merge([H1|T1], [H2|T2], [H2|T]) :-
    H1 > H2,
    merge([H1|T1], T2, T).
```

**Examples:**

1. **Even-Length List:**

   ```
   ?- merge_sort([3,1,4,1,5,9], Sorted).
   
   ```

   *Explanation:* Splits and sorts `[3,1,4,1,5,9]` to get `[1,1,3,4,5,9]`.
2. **Odd-Length List:**

   ```
   ?- merge_sort([10,2,7,3,6], Sorted).
   
   ```

   *Explanation:* Returns `[2,3,6,7,10]` after recursively sorting and merging.
3. **Empty List:**

   ```
   ?- merge_sort([], Sorted).
   
   ```

   *Explanation:* The empty list remains empty.  
   **Output:** `Sorted = []`.

---

#### 3.8.6 Flatten a Nested List

<a id="flatten-a-nested-list"></a> **What It Does:**  
The flatten predicate converts a nested list (a list that contains other lists) into a single-level list by recursively processing each element.

**Implementation:**

```
flatten_list([], []).
flatten_list([H|T], Flat) :-
    flatten_list(H, FlatH),
    flatten_list(T, FlatT),
    append(FlatH, FlatT, Flat).
flatten_list(L, [L]) :-
    \+ is_list(L).
```

**Examples:**

1. **Simple Nested List:**

   ```
   ?- flatten_list([a, [b, c], d], X).
   
   ```

   *Explanation:* Converts `[a, [b, c], d]` to `[a,b,c,d]`.
2. **Deeply Nested List:**

   ```
   ?- flatten_list([[1,2], [3, [4,5]], 6], X).
   
   ```

   *Explanation:* Recursively flattens to `[1,2,3,4,5,6]`.
3. **Nested with Empty List:**

   ```
   ?- flatten_list([[[a]], b, []], X).
   
   ```

   *Explanation:* Returns `[a,b]` as the empty list contributes nothing.

---

#### 3.8.7 Sum of Elements

<a id="sum-of-elements-1"></a> **What It Does:**  
This predicate recursively computes the sum of all elements in a numeric list.

**Implementation:**

```
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Rest),
    Sum is H + Rest.
```

**Examples:**

1. **Basic Sum:**

   ```
   ?- sum_list([1,2,3,4,5], Sum).
   
   ```

   *Explanation:* Sums `1+2+3+4+5` to yield `15`.
2. **Single Element:**

   ```
   ?- sum_list([10], Sum).
   
   ```

   *Explanation:* The sum of a single-element list is the element itself (`10`).
3. **Empty List:**

   ```
   ?- sum_list([], Sum).
   
   ```

   *Explanation:* By definition, the sum of an empty list is `0`.

---

#### 3.8.8 Product of Elements

<a id="product-of-elements-1"></a> **What It Does:**  
This predicate multiplies all elements in a list together recursively.

**Implementation:**

```
product_list([], Product) :- Product = 1.
product_list([H|T], Product) :-
    product_list(T, Rest),
    Product is H * Rest.
```

**Examples:**

1. **Basic Product:**

   ```
   ?- product_list([2,3,4], Product).
   
   ```

   *Explanation:* Computes `2*3*4 = 24`.
2. **Single Element:**

   ```
   ?- product_list([5], Product).
   
   ```

   *Explanation:* The product of a one-element list is `5`.
3. **Empty List:**

   ```
   ?- product_list([], Product).
   
   ```

   *Explanation:* By definition, the product of an empty list is `1` (multiplicative identity).

---

#### 3.8.9 List Length (Recursive)

<a id="list-length-recursive"></a> **What It Does:**  
Calculates the number of elements in a list using recursion.

**Implementation:**

```
my_length([], 0).
my_length([_|T], Len) :-
    my_length(T, L1),
    Len is L1 + 1.
```

**Examples:**

1. **Standard List:**

   ```
   ?- my_length([a,b,c,d], L).
   
   ```

   *Explanation:* There are 4 elements, so `L = 4`.
2. **Empty List:**

   ```
   ?- my_length([], L).
   
   ```

   *Explanation:* An empty list has length `0`.
3. **Numeric List:**

   ```
   ?- my_length([1,2,3], L).
   
   ```

   *Explanation:* Returns `L = 3`.

---

#### 3.8.10 Count Occurrences

<a id="count-occurrences"></a> **What It Does:**  
Counts the number of times a specific element appears in a list.

**Implementation:**

```
count_occurrences(_, [], 0).
count_occurrences(X, [X|T], Count) :-
    count_occurrences(X, T, Count1),
    Count is Count1 + 1.
count_occurrences(X, [_|T], Count) :-
    count_occurrences(X, T, Count).
```

**Examples:**

1. **Counting Letters:**

   ```
   ?- count_occurrences(a, [a,b,a,c,a], Count).
   
   ```

   *Explanation:* `a` appears 3 times; thus, `Count = 3`.
2. **Counting Numbers:**

   ```
   ?- count_occurrences(1, [1,2,1,3,1,1], Count).
   
   ```

   *Explanation:* `1` appears 4 times.
3. **Element Not Present:**

   ```
   ?- count_occurrences(x, [a,b,c], Count).
   
   ```

   *Explanation:* Since `x` is absent, `Count = 0`.

---

#### 3.8.11 Maximum Element (Recursive)

<a id="maximum-element-recursive"></a> **What It Does:**  
Finds the largest element in a numeric list by recursively comparing elements.

**Implementation:**

```
max_list([X], X).
max_list([H|T], Max) :-
    max_list(T, TempMax),
    Max is max(H, TempMax).
```

**Examples:**

1. **Mixed Numbers:**

   ```
   ?- max_list([3,7,2,9,5], Max).
   
   ```

   *Explanation:* Returns `Max = 9` after comparing all elements.
2. **Single Element:**

   ```
   ?- max_list([10], Max).
   
   ```

   *Explanation:* With only one element, `Max = 10`.
3. **Duplicate Maximums:**

   ```
   ?- max_list([1,3,3,2], Max).
   
   ```

   *Explanation:* The maximum value is `3`.

---

#### 3.8.12 Minimum Element (Recursive)

<a id="minimum-element-recursive"></a> **What It Does:**  
Finds the smallest element in a list by recursively comparing elements.

**Implementation:**

```
min_list([X], X).
min_list([H|T], Min) :-
    min_list(T, TempMin),
    Min is min(H, TempMin).
```

**Examples:**

1. **Mixed Numbers:**

   ```
   ?- min_list([3,7,2,9,5], Min).
   
   ```

   *Explanation:* Returns `Min = 2` as the smallest element.
2. **Single Element:**

   ```
   ?- min_list([10], Min).
   
   ```

   *Explanation:* With one element, `Min = 10`.
3. **All Equal Elements:**

   ```
   ?- min_list([4,4,4], Min).
   
   ```

   *Explanation:* The minimum is `4`.

---

#### 3.8.13 Check if a List is Sorted

<a id="check-if-a-list-is-sorted"></a> **What It Does:**  
Determines whether a list is sorted in non-decreasing order by comparing adjacent elements recursively.

**Implementation:**

```
is_sorted([]).
is_sorted([_]).
is_sorted([X, Y|T]) :-
    X =< Y,
    is_sorted([Y|T]).
```

**Examples:**

1. **Sorted List:**

   ```
   ?- is_sorted([1,2,2,3,4]).
   
   ```

   *Explanation:* All adjacent pairs satisfy `X =< Y`. Succeeds.
2. **Unsorted List:**

   ```
   ?- is_sorted([3,2,1]).
   
   ```

   *Explanation:* `3 =< 2` fails, so the predicate fails.
3. **Empty List:**

   ```
   ?- is_sorted([]).
   
   ```

   *Explanation:* An empty list is considered sorted. Succeeds.

---

#### 3.8.14 Quick Sort

<a id="quick-sort"></a> **What It Does:**  
Quick sort partitions a list around a pivot, recursively sorts the partitions, and concatenates them. It is an alternative sorting algorithm with different performance characteristics compared to merge sort.

**Implementation:**

```
quicksort([], []).
quicksort([Pivot|T], Sorted) :-
    partition(T, Pivot, L, R),
    quicksort(L, SortedL),
    quicksort(R, SortedR),
    append(SortedL, [Pivot|SortedR], Sorted).

partition([], _, [], []).
partition([H|T], Pivot, [H|L], R) :-
    H =< Pivot,
    partition(T, Pivot, L, R).
partition([H|T], Pivot, L, [H|R]) :-
    H > Pivot,
    partition(T, Pivot, L, R).
```

**Examples:**

1. **General Case:**

   ```
   ?- quicksort([3,6,2,5,4,1], Sorted).
   
   ```

   *Explanation:* Partitions around a pivot and recursively sorts.  
   **Output:** `Sorted = [1,2,3,4,5,6]`.
2. **List with Negative Numbers:**

   ```
   ?- quicksort([10, -1, 4, 3], Sorted).
   
   ```

   *Explanation:* Returns `Sorted = [-1,3,4,10]`.
3. **Empty List:**

   ```
   ?- quicksort([], Sorted).
   
   ```

   *Explanation:* An empty list remains empty.  
   **Output:** `Sorted = []`.

---

#### 3.8.15 Reverse a List with an Accumulator

<a id="reverse-a-list-with-an-accumulator"></a> **What It Does:**  
This tail-recursive version of list reversal uses an accumulator to efficiently build the reversed list without using append repeatedly.

**Implementation:**

```
reverse_acc(List, Reversed) :-
    reverse_acc(List, [], Reversed).

reverse_acc([], Acc, Acc).
reverse_acc([H|T], Acc, Reversed) :-
    reverse_acc(T, [H|Acc], Reversed).
```

**Examples:**

1. **Standard List:**

   ```
   ?- reverse_acc([a,b,c,d], X).
   
   ```

   *Explanation:* Builds reversed list using an accumulator.  
   **Output:** `X = [d,c,b,a]`.
2. **Empty List:**

   ```
   ?- reverse_acc([], X).
   
   ```

   *Explanation:* Reversing an empty list returns an empty list.
3. **Numeric List:**

   ```
   ?- reverse_acc([1,2,3], X).
   
   ```

   *Explanation:* Returns `X = [3,2,1]`.

---

#### 3.8.16 Concatenate a List of Lists

<a id="concatenate-a-list-of-lists"></a> **What It Does:**  
Concatenates multiple lists into a single list by recursively appending each sublist.

**Implementation:**

```
concat_lists([], []).
concat_lists([L|Ls], Result) :-
    concat_lists(Ls, Rest),
    append(L, Rest, Result).
```

**Examples:**

1. **Multiple Sublists:**

   ```
   ?- concat_lists([[a,b], [c], [d,e,f]], X).
   
   ```

   *Explanation:* Concatenates to produce `[a,b,c,d,e,f]`.
2. **Including Empty Sublists:**

   ```
   ?- concat_lists([[], [1,2], [3]], X).
   
   ```

   *Explanation:* Returns `[1,2,3]`.
3. **Single Sublist:**

   ```
   ?- concat_lists([[x,y,z]], X).
   
   ```

   *Explanation:* Simply returns `[x,y,z]`.

---

#### 3.8.17 Interleave Two Lists

<a id="interleave-two-lists"></a> **What It Does:**  
Interleaves two lists by pairing corresponding elements into a list of pairs. It stops when the shorter list is exhausted.

**Implementation:**

```
interleave([], [], []).
interleave([H1|T1], [H2|T2], [[H1,H2]|Rest]) :-
    interleave(T1, T2, Rest).
```

**Examples:**

1. **Equal Length Lists:**

   ```
   ?- interleave([a,b,c], [1,2,3], X).
   
   ```

   *Explanation:* Combines to `[[a,1],[b,2],[c,3]]`.
2. **One List Shorter:**

   ```
   ?- interleave([x,y], [true,false,true], X).
   
   ```

   *Explanation:* Stops after pairing `x` and `y`, output: `X = [[x,true],[y,false]]`.
3. **Empty First List:**

   ```
   ?- interleave([], [1,2,3], X).
   
   ```

   *Explanation:* With an empty first list, result is `X = []`.

---

#### 3.8.18 Sublist Check (Contiguous)

<a id="sublist-check-contiguous"></a> **What It Does:**  
Verifies that a given list (the sublist) appears contiguously within another list by splitting the larger list around it.

**Implementation:**

```
sublist(Sub, List) :-
    append(_, L2, List),
    append(Sub, _, L2).
```

**Examples:**

1. **Found Sublist:**

   ```
   ?- sublist([b,c], [a,b,c,d]).
   
   ```

   *Explanation:* `[b,c]` appears contiguously in `[a,b,c,d]`.
2. **Another Valid Case:**

   ```
   ?- sublist([c,d], [a,b,c,d,e]).
   
   ```

   *Explanation:* Succeeds since `[c,d]` is present.
3. **Sublist Not Present:**

   ```
   ?- sublist([x,y], [a,b,c]).
   
   ```

   *Explanation:* Fails because `[x,y]` does not appear.

---

#### 3.8.19 Permutations of a List

<a id="permutations-of-a-list"></a> **What It Does:**  
Generates all possible orderings (permutations) of a list by recursively selecting an element and permuting the remainder.

**Implementation:**

```
permutation([], []).
permutation(List, [H|Perm]) :-
    select(H, List, Rest),
    permutation(Rest, Perm).
```

**Examples:**

1. **Small List:**

   ```
   ?- permutation([1,2,3], P).
   
   ```

   *Explanation:* Generates permutations like `[1,2,3]`, `[1,3,2]`, etc.
2. **List of Atoms:**

   ```
   ?- permutation([a,b], P).
   
   ```

   *Explanation:* Yields `[a,b]` and `[b,a]`.
3. **Empty List:**

   ```
   ?- permutation([], P).
   
   ```

   *Explanation:* The only permutation is `[]`.

---

#### 3.8.20 Zip Three Lists

<a id="zip-three-lists"></a> **What It Does:**  
Combines three lists element-wise into a list of triples. The process stops when any one of the lists is exhausted.

**Implementation:**

```
zip3([], [], [], []).
zip3([H1|T1], [H2|T2], [H3|T3], [[H1,H2,H3]|Rest]) :-
    zip3(T1, T2, T3, Rest).
```

**Examples:**

1. **All Lists Non-Empty:**

   ```
   ?- zip3([a,b], [1,2], [x,y], X).
   
   ```

   *Explanation:* Combines to produce `X = [[a,1,x],[b,2,y]]`.
2. **Longer Lists:**

   ```
   ?- zip3([a,b,c], [1,2,3], [p,q,r], X).
   
   ```

   *Explanation:* Yields `X = [[a,1,p],[b,2,q],[c,3,r]]`.
3. **One Empty List:**

   ```
   ?- zip3([], [1,2], [x,y], X).
   
   ```

   *Explanation:* With an empty first list, the result is `X = []`.

---

## 4. Advanced Predicates

<a id="advanced-predicates"></a>

### 4.1 `findall/3`

<a id="using-findall3-for-collecting-solutions"></a>

* *Purpose:* Collect all possible solutions for a given goal.
* *Syntax:*

  ```
  findall(Template, Goal, List).
  
  ```
* *Examples:*

  ```
  ?- findall(N, between(1,5,N), L).  
  % L = [1,2,3,4,5].
  
  ```

   For SAT encodings:

  ```
  relateDoesVarsWithBusyAtHourVars :-
      available(G,H),
      findall(does(G,T,H), task(T), Lits),
      expressOr(busyAtHour(G,H), Lits),
      fail.
  relateDoesVarsWithBusyAtHourVars.
  
  ```

### 4.2 SAT & Constraint Encodings

<a id="sat--constraint-encodings"></a>

#### 4.2.1 `expressOr/2` and `expressAnd/2`

<a id="expressor2-and-expressand2"></a>

* *Purpose:* Define logical relationships for SAT solving.
* *Syntax & Example:*

  ```
  expressOr(Var, Lits).
  % Means Var <--> (Literal1 ∨ Literal2 ∨ ...)
  
  expressAnd(Var, Lits).
  % Means Var <--> (Literal1 ∧ Literal2 ∧ ...)
  
  ```

   You can implement these by writing out the logic (or by interfacing with a SAT solver).

#### 4.2.2 Cardinality Constraints

<a id="cardinality-constraints"></a>

* `atLeast(K, Lits)`: At least K literals are true.
* `atMost(K, Lits)`: At most K literals are true.
* `exactly(K, Lits)`: Exactly K literals are true (often defined as a combination of `atLeast/2` and `atMost/2`).

*Example:*

```
atLeast(2, [does(g01,T,H), does(g02,T,H), does(g03,T,H)]).
atMost(1, [does(G,killing,H), does(G,countingMoney,H), does(G,politics,H)]).
exactly(3, [does(g01,T,H), does(g02,T,H), does(g03,T,H), does(g04,T,H)]).
```

### 4.3 `writeOneClause/1`

<a id="clause-generation-with-writeoneclause1"></a>

* *Purpose:* Outputs a single clause (a disjunction of literals) for SAT or CNF generation.
* *Example Implementation:*

  ```
  writeOneClause([]) :- write('0'), nl.
  writeOneClause([Lit|Lits]) :-
      write(Lit), write(' '),
      writeOneClause(Lits).
  
  ```

### 4.4 Higher-Order Predicates

<a id="higher-order-predicates"></a>

* `maplist/2` and `maplist/3`:  
  Apply a predicate across a list (or multiple lists).

  ```
  % Example: Doubling each element
  double(X, Y) :- Y is X * 2.
  ?- maplist(double, [1,2,3], Doubled).  % Doubled = [2,4,6].
  
  ```
* `include/3` and `exclude/3`:  
  Filter lists by including or excluding elements that satisfy a predicate.

  ```
  is_even(X) :- 0 is X mod 2.
  ?- include(is_even, [1,2,3,4], Evens).  % Evens = [2,4].
  ?- exclude(is_even, [1,2,3,4], Odds).    % Odds = [1,3].
  
  ```

### 4.5 Defining Custom Predicates

<a id="defining-custom-predicates"></a>

* *Example – Checking Availability:*

  ```
  available(G, H) :-
      gangster(G),
      hour(H),
      \+ blocked(G, H).
  
  ```
* *Example – Assigning Tasks:*

  ```
  assign_task(G, T, H) :-
      available(G, H),
      does(G, T, H).
  
  ```

### 4.6 Recursion Techniques

<a id="recursion-techniques"></a>

* Break down complex operations into helper predicates and ensure base cases are covered (see examples under recursive list processing).

### 4.7 Error Handling and Guards

<a id="error-handling-and-guards"></a>

* *Example – Safe Division:*

  ```
  safe_divide(_, 0, _) :-
      write('Error: Division by zero.'), nl, fail.
  safe_divide(X, Y, Z) :-
      Z is X / Y.
  
  ```

  *Usage:*

  ```
  ?- safe_divide(10, 2, Z).  % Z = 5.
  ?- safe_divide(10, 0, Z).  % Error printed, fails.
  
  ```

---

### 4.8 Generator Predicates

<a id="generator-predicates"></a>

These predicates help generate values and iterate over ranges. They are particularly useful when you need to create choice points or enumerate all possibilities under given constraints.

#### 4.8.1 between/3

<a id="between3"></a> *Purpose:* Generates an integer `N` such that `Low =< N =< High`. Prolog instantiates `N` one value at a time as you backtrack.

**Example 1 – Generating a Range with a Non-Trivial Lower Bound:**

```
?- between(4, 7, N).
```

*Explanation:*

* First, Prolog unifies `N` with 4.
* On backtracking, `N` becomes 5, then 6, and finally 7.
* This is useful for iterating over the numbers 4 to 7.

**Example 2 – Single-Value Range:**

```
?- between(0, 0, N).
```

*Explanation:*

* The lower and upper bounds are both 0, so there is only one possible value: `N = 0`.

**Example 3 – Combining with a Computation:**

```
?- between(3, 5, N), M is N * 10.
```

*Explanation:*

* For `N = 3`, the computation yields `M = 30`;
* then for `N = 4`, `M = 40`;
* and for `N = 5`, `M = 50`.
* This example shows how `between/3` can be used to drive calculations in a loop-like fashion.

---

#### 4.8.2 repeat/0

<a id="repeat0"></a> *Purpose:* `repeat/0` always succeeds and creates an infinite choice point. It’s used to drive loops until a condition is met, at which point a cut (`!`) stops further backtracking.

**Example 1 – Interactive Loop Until a Specific Word Is Entered:**

```
?- repeat, write('Type stop to end: '), read(Input), Input == stop, !.
```

*Explanation:*

* The loop repeatedly prompts the user to type a word.
* It continues until the user enters `stop`.
* When `Input == stop` succeeds, the cut (`!`) prevents further iterations.

**Example 2 – Simulated Menu Loop:**

```
menu_loop :-
    repeat,
    write('Menu: 1. Continue  2. Quit'), nl,
    read(Choice),
    (Choice = 2 -> ! ; (write('You chose to continue.'), nl, fail)).
```

*Explanation:*

* This predicate shows a menu that repeats until the user selects option 2.
* When `Choice = 2` is true, the cut stops the loop; otherwise, it writes a message and fails to loop again.

**Example 3 – Retry Until a Random Condition is Met:**

```
?- repeat, random_between(1, 10, N), write('Generated: '), write(N), nl, N = 8, !.
```

*Explanation:*

* Prolog repeatedly generates a random number between 1 and 10 and prints it.
* The loop stops when the generated number is 8 (thanks to the cut).
* This illustrates using `repeat/0` for retrying a goal until a certain condition is satisfied.

---

#### 4.8.3 bagof/3

<a id="bagof3"></a> *Purpose:* Collects all solutions for a goal into a list. It preserves duplicates and groups solutions by any free variables that are not explicitly quantified.

**Example 1 – Collecting Numbers from a List:**

```
?- bagof(X, member(X, [10,20,30,20]), L).
```

*Explanation:*

* This gathers each solution for `X` that is a member of the list.
* **Output:** `L = [10,20,30,20]` (notice the duplicate 20 is preserved).

**Example 2 – Filtering with a Condition:**

```
?- bagof(N, (member(N, [5,10,15,20]), N > 10), L).
```

*Explanation:*

* Only numbers greater than 10 are collected.
* **Output:** `L = [15,20]`.

**Example 3 – Grouping by Ignoring a Variable:**

```
?- bagof(Y, X^(member((X, Y), [(red, apple), (yellow, banana), (red, cherry)])), L).
```

*Explanation:*

* The `X^` operator tells Prolog to ignore `X` when grouping results.
* This collects all `Y` values from the pairs regardless of their corresponding color.
* **Output:** `L = [apple, banana, cherry]`.

---

#### 4.8.4 setof/3

<a id="setof3"></a> *Purpose:* Like `bagof/3`, but returns a sorted list of unique solutions. It fails if no solution exists.

**Example 1 – Unique Sorted Numbers:**

```
?- setof(X, member(X, [4,2,8,4,6,2]), L).
```

*Explanation:*

* Prolog collects all members, removes duplicates, and sorts them.
* **Output:** `L = [2,4,6,8]`.

**Example 2 – Unique Sorted Words:**

```
?- setof(Word, member(Word, [hello, world, hello, prolog]), L).
```

*Explanation:*

* Collects the words, removes the duplicate `hello`, and sorts them alphabetically.
* **Output:** `L = [hello, prolog, world]`.

**Example 3 – Grouping Tuples:**

```
?- setof((X, Y), member((X, Y), [(a,1), (b,2), (a,3), (c,1)]), L).
```

*Explanation:*

* This gathers all unique pairs (tuples) and sorts them according to Prolog’s standard term order.
* **Output:** `L = [(a,1), (a,3), (b,2), (c,1)]`.

---

#### 4.8.5 succ/2

<a id="succ2"></a> *Purpose:* Establishes a successor relationship between two integers. The predicate `succ(N, M)` succeeds if `M` is exactly one more than `N` (i.e., `M = N + 1`).

**Example 1 – Direct Successor Calculation:**

```
?- succ(7, N).
```

*Explanation:*

* Prolog unifies `N` with `8`, because 8 is the immediate successor of 7.

**Example 2 – Reverse Query to Find a Predecessor:**

```
?- succ(X, 5).
```

*Explanation:*

* This query asks for a value `X` such that `X + 1 = 5`.
* Prolog unifies `X` with `4`.

**Example 3 – Chaining Successors in a Calculation:**

```
?- succ(A, B), succ(B, C), A = 10.
```

*Explanation:*

* First, with `A = 10`, Prolog sets `B = 11` (since `succ(10, 11)` succeeds).
* Then, `succ(11, C)` unifies `C` with `12`.
* This shows how you can chain successor relationships to generate a sequence.

---

## 5. Extra Self-Declarated Predicates

<a id="5-extra-self-declarated-predicates"></a>

### 5.1 inside/2

<a id="51-inside2"></a>

*Purpose:* Checks whether the first list is a subsequence of the second (i.e. all its elements appear in order, though not necessarily consecutively).

**Definition:**

```
inside([], _).
inside([X|S], L) :-
    append(_, [X|R], L),
    inside(S, R).
```

**Examples:**

1. **Simple Positive Case:**

   ```
   ?- inside([2,4], [1,2,3,4,5]).
   
   ```

   *Explanation:* 2 appears in the list, and after it 4 also appears. The query succeeds.
2. **Non-Consecutive Elements:**

   ```
   ?- inside([a,c,e], [a,b,c,d,e]).
   
   ```

   *Explanation:* Even though the elements aren’t adjacent, they occur in order (a then c then e). Succeeds.
3. **Order Violation (Failing Case):**

   ```
   ?- inside([3,2], [1,2,3,4]).
   
   ```

   *Explanation:* Although both 3 and 2 are present, they do not occur in the order [3,2]. The query fails.

---

### 5.2 prefix/2

<a id="52-prefix2"></a>

*Purpose:* Determines if the first list is a prefix of the second list.

**Definition:**

```
prefix([], _).
prefix([H|T], [H|Rest]) :-
    prefix(T, Rest).
```

**Examples:**

1. **Valid Prefix:**

   ```
   ?- prefix([a,b], [a,b,c,d]).
   
   ```

   *Explanation:* The list [a,b] exactly appears at the start of [a,b,c,d]. Succeeds.
2. **Empty List as Prefix:**

   ```
   ?- prefix([], [1,2,3]).
   
   ```

   *Explanation:* An empty list is a prefix of any list. Succeeds.
3. **Non-Prefix Case:**

   ```
   ?- prefix([2,3], [1,2,3]).
   
   ```

   *Explanation:* [2,3] does not appear at the beginning of [1,2,3]. The query fails.

---

### 5.3 suffix/2

<a id="53-suffix2"></a>

*Purpose:* Checks whether the first list is a suffix (ending segment) of the second list.

**Definition:**

```
suffix(S, S).
suffix(S, [_|Tail]) :-
    suffix(S, Tail).
```

**Examples:**

1. **Valid Suffix:**

   ```
   ?- suffix([c,d], [a,b,c,d]).
   
   ```

   *Explanation:* The list [c,d] appears at the end of [a,b,c,d]. Succeeds.
2. **Single-Element Suffix:**

   ```
   ?- suffix([d], [a,b,c,d]).
   
   ```

   *Explanation:* [d] is the ending element. Succeeds.
3. **Not a Suffix:**

   ```
   ?- suffix([b,d], [a,b,c,d]).
   
   ```

   *Explanation:* Although both b and d occur in the list, they aren’t contiguous at the end. The query fails.

---

### 5.4 rotate_left/2

<a id="54-rotate_left2"></a>

*Purpose:* Rotates a list left by moving the first element to the end.

**Definition:**

```
rotate_left([], []).
rotate_left([H|T], Rotated) :-
    append(T, [H], Rotated).
```

**Examples:**

1. **Standard Rotation:**

   ```
   ?- rotate_left([1,2,3,4], X).
   
   ```

   *Explanation:* Moves 1 to the end resulting in [2,3,4,1].
2. **Single-Element List:**

   ```
   ?- rotate_left([a], X).
   
   ```

   *Explanation:* A one-element list remains the same ([a]). Succeeds.
3. **Empty List:**

   ```
   ?- rotate_left([], X).
   
   ```

   *Explanation:* Rotating an empty list yields an empty list.

---

### 5.5 rotate_right/2

<a id="55-rotate_right2"></a>

*Purpose:* Rotates a list right by moving the last element to the front.

**Definition:**

```
rotate_right([], []).
rotate_right(List, Rotated) :-
    append(Rest, [Last], List),
    Rotated = [Last|Rest].
```

**Examples:**

1. **Standard Rotation:**

   ```
   ?- rotate_right([1,2,3,4], X).
   
   ```

   *Explanation:* Moves 4 to the front, yielding [4,1,2,3].
2. **Single-Element List:**

   ```
   ?- rotate_right([a], X).
   
   ```

   *Explanation:* A single-element list remains unchanged ([a]).
3. **Empty List:**

   ```
   ?- rotate_right([], X).
   
   ```

   *Explanation:* Rotating an empty list produces [].

---

### 5.6 remove_duplicates/2

<a id="56-remove_duplicates2"></a>

*Purpose:* Removes duplicate elements from a list while preserving the order of their first appearance.

**Definition:**

```
remove_duplicates([], []).
remove_duplicates([H|T], [H|Result]) :-
    \+ member(H, T),
    remove_duplicates(T, Result).
remove_duplicates([H|T], Result) :-
    member(H, T),
    remove_duplicates(T, Result).
```

**Examples:**

1. **Removing Duplicates from Atoms:**

   ```
   ?- remove_duplicates([a,b,a,c,b], X).
   
   ```

   *Explanation:* Returns [a,b,c] by keeping the first occurrences of a, b, and c.
2. **Numeric List Example:**

   ```
   ?- remove_duplicates([1,2,2,3,1], X).
   
   ```

   *Explanation:* Succeeds with X = [1,2,3].
3. **Empty List:**

   ```
   ?- remove_duplicates([], X).
   
   ```

   *Explanation:* Removing duplicates from an empty list yields [].

---

### 5.7 substitute/4

<a id="57-substitute4"></a>

*Purpose:* Replaces every occurrence of a specified element (Old) in a list with a new element (New).

**Definition:**

```
substitute(_, _, [], []).
substitute(Old, New, [Old|T], [New|Result]) :-
    substitute(Old, New, T, Result).
substitute(Old, New, [H|T], [H|Result]) :-
    H \= Old,
    substitute(Old, New, T, Result).
```

**Examples:**

1. **Replacing Atoms:**

   ```
   ?- substitute(a, x, [a,b,a,c], X).
   
   ```

   *Explanation:* Replaces all occurrences of a with x, resulting in [x,b,x,c].
2. **Replacing Numbers:**

   ```
   ?- substitute(2, 9, [1,2,3,2,4], X).
   
   ```

   *Explanation:* Returns [1,9,3,9,4] by replacing 2 with 9.
3. **No Replacement Needed:**

   ```
   ?- substitute(z, q, [p,q,r], X).
   
   ```

   *Explanation:* Since z is not present, the list remains unchanged: [p,q,r].

---

### 5.8 replace_at/4

<a id="58-replace_at4"></a>

*Purpose:* Replaces the element at a given 1‑indexed position with a new element.

**Definition:**

```
replace_at(1, [_|T], New, [New|T]).
replace_at(Pos, [H|T], New, [H|Result]) :-
    Pos > 1,
    Pos1 is Pos - 1,
    replace_at(Pos1, T, New, Result).
```

**Examples:**

1. **Replacing at Position 2:**

   ```
   ?- replace_at(2, [a,b,c,d], x, X).
   
   ```

   *Explanation:* Replaces the second element (b) with x to yield [a,x,c,d].
2. **Replacing at the First Position:**

   ```
   ?- replace_at(1, [1,2,3], 9, X).
   
   ```

   *Explanation:* The first element is replaced, resulting in [9,2,3].
3. **Replacing at the Last Position:**

   ```
   ?- replace_at(4, [w,x,y,z], a, X).
   
   ```

   *Explanation:* The fourth element (z) is replaced with a, yielding [w,x,y,a].

---

### 5.9 nth0/3

<a id="59-nth03"></a>

*Purpose:* Retrieves the element at the given index using 0‑based indexing.

**Definition:**

```
nth0(0, [H|_], H).
nth0(N, [_|T], Element) :-
    N > 0,
    N1 is N - 1,
    nth0(N1, T, Element).
```

**Examples:**

1. **Accessing the Third Element:**

   ```
   ?- nth0(2, [a,b,c,d], X).
   
   ```

   *Explanation:* Index 2 (third element) is c, so X = c.
2. **Accessing the First Element:**

   ```
   ?- nth0(0, [x,y,z], X).
   
   ```

   *Explanation:* Retrieves the element at index 0, X = x.
3. **Another Example:**

   ```
   ?- nth0(3, [1,2,3,4,5], X).
   
   ```

   *Explanation:* The element at index 3 is 4, so X = 4.

---

### 5.10 duplicate/3

<a id="510-duplicate3"></a>

*Purpose:* Creates a list containing a given element duplicated a specified number of times.

**Definition:**

```
duplicate(_, 0, []).
duplicate(Element, Count, [Element|Rest]) :-
    Count > 0,
    Count1 is Count - 1,
    duplicate(Element, Count1, Rest).
```

**Examples:**

1. **Duplicate an Atom Three Times:**

   ```
   ?- duplicate(a, 3, X).
   
   ```

   *Explanation:* Generates X = [a,a,a].
2. **Zero Duplicates:**

   ```
   ?- duplicate(5, 0, X).
   
   ```

   *Explanation:* With a count of 0, X = [].
3. **Duplicate a Number Four Times:**

   ```
   ?- duplicate(2, 4, X).
   
   ```

   *Explanation:* Produces X = [2,2,2,2].

Below is your complete master cheat sheet with a new section at the end—“6. Useful Predicates.” In this section you’ll find the definition of an all_different/1 predicate (to check that all elements in a list are distinct) plus nine other predicates that you might find useful when working with lists in Prolog.

---

## 6. Useful Predicates

This section collects several predicates that you may find useful in your Prolog programs. It includes a predicate to verify that all elements in a list are different and nine additional predicates that address common list‐processing tasks.

---

### 6.1 all_different/1

**Purpose:**  
Succeeds if every element in a list is unique (i.e. no duplicates).

**Definition:**

```
all_different([]).
all_different([X|Xs]) :-
    all(different(X), Xs),
    all_different(Xs).

different(X, Y) :-
    X \= Y.
```

**Example:**

```
?- all_different([a, b, c]).
true.

?- all_different([a, b, a]).
false.
```

---

### 6.2 list_difference/3

**Purpose:**  
Computes the difference between two lists (elements in List1 that are not in List2).

**Definition:**

```
list_difference([], _, []).
list_difference([H|T], L2, Diff) :-
    member(H, L2),
    list_difference(T, L2, Diff).
list_difference([H|T], L2, [H|Diff]) :-
    \+ member(H, L2),
    list_difference(T, L2, Diff).
```

**Example:**

```
?- list_difference([a,b,c,d], [b,d], Diff).
Diff = [a, c].
```

---

### 6.3 cartesian_product/3

**Purpose:**  
Generates the Cartesian product of two lists as a list of pairs.

**Definition:**

```
cartesian_product(L1, L2, Product) :-
    findall([X, Y], (member(X, L1), member(Y, L2)), Product).
```

**Example:**

```
?- cartesian_product([a,b], [1,2], P).
P = [[a,1], [a,2], [b,1], [b,2]].
```

---

### 6.4 find_duplicates/2

**Purpose:**  
Returns a list of elements that appear more than once in the input list.

**Definition:**

```
find_duplicates(List, Duplicates) :-
    findall(X, (member(X, List), count_occurrences(X, List, N), N > 1), DupRaw),
    remove_duplicates(DupRaw, Duplicates).

count_occurrences(_, [], 0).
count_occurrences(X, [X|T], Count) :-
    count_occurrences(X, T, Count1),
    Count is Count1 + 1.
count_occurrences(X, [_|T], Count) :-
    count_occurrences(X, T, Count).

remove_duplicates([], []).
remove_duplicates([H|T], [H|R]) :-
    \+ member(H, T),
    remove_duplicates(T, R).
remove_duplicates([H|T], R) :-
    member(H, T),
    remove_duplicates(T, R).
```

**Example:**

```
?- find_duplicates([a,b,a,c,b,d], Dups).
Dups = [a, b].
```

---

### 6.5 replace_all/4

**Purpose:**  
Replaces all occurrences of an element (Old) with a new element (New) in a list.

**Definition:**

```
replace_all(_, _, [], []).
replace_all(Old, New, [Old|T], [New|R]) :-
    replace_all(Old, New, T, R).
replace_all(Old, New, [H|T], [H|R]) :-
    H \= Old,
    replace_all(Old, New, T, R).
```

**Example:**

```
?- replace_all(a, x, [a,b,a,c], Result).
Result = [x, b, x, c].
```

---

### 6.6 transpose/2

**Purpose:**  
Transposes a matrix represented as a list of lists (all sublists must be of equal length).

**Definition:**

```
transpose([], []).
transpose([[]|_], []).
transpose(Matrix, [Row|Rows]) :-
    transpose_1(Matrix, Row, RestMatrix),
    transpose(RestMatrix, Rows).

transpose_1([], [], []).
transpose_1([[H|T]|Rows], [H|Hs], [T|Ts]) :-
    transpose_1(Rows, Hs, Ts).
```

**Example:**

```
?- transpose([[1,2,3], [4,5,6], [7,8,9]], T).
T = [[1,4,7], [2,5,8], [3,6,9]].
```

---

### 6.7 power_set/2

**Purpose:**  
Generates the power set (set of all subsets) of a given list.

**Definition:**

```
power_set([], [[]]).
power_set([H|T], PSet) :-
    power_set(T, PT),
    findall([H|Subset], member(Subset, PT), WithH),
    append(PT, WithH, PSet).
```

**Example:**

```
?- power_set([a,b], PS).
PS = [[], [b], [a], [a,b]].
```

---

### 6.8 group_by/3

**Purpose:**  
Groups elements of a list into sublists based on a key computed by a given predicate.

**Definition:**  
*(This implementation uses a helper predicate to build key–group pairs.)*

```
group_by(_, [], []).
group_by(KeyFunc, [X|Xs], [Key-Group|Rest]) :-
    call(KeyFunc, X, Key),
    collect_same(KeyFunc, Key, Xs, Group, RestList),
    group_by(KeyFunc, RestList, Rest).

collect_same(_, _, [], [ ], []).
collect_same(KeyFunc, Key, [Y|Ys], [Y|Group], Rest) :-
    call(KeyFunc, Y, Key), !,
    collect_same(KeyFunc, Key, Ys, Group, Rest).
collect_same(KeyFunc, Key, [Y|Ys], Group, [Y|Rest]) :-
    call(KeyFunc, Y, OtherKey),
    Key \= OtherKey,
    collect_same(KeyFunc, Key, Ys, Group, Rest).
```

**Example:**  
Group numbers by even/odd:

```
even_odd(X, even) :- 0 is X mod 2.
even_odd(X, odd)  :- 1 is X mod 2.

?- group_by(even_odd, [1,3,2,4,5,6], Groups).
Groups = [odd-[1,3], even-[2,4,6], odd-[5]].
```

---

### 6.9 split_by/3

**Purpose:**  
Splits a list into two lists: one with elements that satisfy a predicate and one with those that do not.

**Definition:**

```
split_by(_, [], [], []).
split_by(Pred, [H|T], [H|Yes], No) :-
    call(Pred, H), !,
    split_by(Pred, T, Yes, No).
split_by(Pred, [H|T], Yes, [H|No]) :-
    split_by(Pred, T, Yes, No).
```

**Example:**

```
?- split_by(is_even, [1,2,3,4,5,6], Evens, Odds).
Evens = [2,4,6],
Odds = [1,3,5].

is_even(X) :- 0 is X mod 2.
```

---

### 6.10 chunk/3

**Purpose:**  
Divides a list into chunks (sublists) of a specified size. The last chunk may be smaller if there aren’t enough elements.

**Definition:**

```
chunk([], _, []).
chunk(List, N, [Chunk|Rest]) :-
    N > 0,
    take(N, List, Chunk),
    drop(N, List, Remainder),
    chunk(Remainder, N, Rest).
```

*(Here, you can use the previously defined take/3 and drop/3 predicates.)*

**Example:**

```
?- chunk([1,2,3,4,5,6,7], 3, Chunks).
Chunks = [[1,2,3], [4,5,6], [7]].
```

---

---

### 6.11 push_back/3 and push_front/3

**Purpose:**  
`push_back/3` appends an element to the end of a list, while `push_front/3` prepends an element to the beginning of a list.

**Definition:**

```
% push_back(+List, +Elem, -NewList)
% Given a list and an element, NewList is the list with Elem appended.
push_back(List, Elem, NewList) :-
    append(List, [Elem], NewList).

% push_front(+Elem, +List, -NewList)
% Given a list and an element, NewList is the list with Elem added at the front.
push_front(Elem, List, [Elem|List]).
```

**Example:**

```
?- push_back([a,b,c], d, X).
X = [a,b,c,d].

?- push_front(d, [a,b,c], Y).
Y = [d,a,b,c].
```



---

## 7. Generating Infinite and Finite Sequences

In Prolog, you can simulate infinite sequences by writing *generator* predicates that yield one element per backtracking. For practical use you often want to collect just the first N elements into a list. Below are three common patterns—each showing:

1. A generator predicate for an “infinite” sequence
2. A wrapper predicate to collect the first **N** values into a list

---

### 7.1 Fibonacci Sequence

**Purpose:**
Generate the (conceptually infinite) sequence of Fibonacci numbers and collect the first N terms.

```prolog
% infinite_fib(-F)  
%   Generator: on each backtrack, F unifies with the next Fibonacci number.
infinite_fib(0).
infinite_fib(1).
infinite_fib(F) :-
    infinite_fib(F1),
    infinite_fib(F2),
    F is F1 + F2,
    F1 < F2.    % ensure we always move forward

% fib_n(+N, -List)  
%   Collects the first N Fibonacci numbers in List.
fib_n(0, []) :- !.
fib_n(N, [F|Fs]) :-
    N > 0,
    findall(X, infinite_fib(X), All),  % generates all in order
    length(Prefix, N),
    append(Prefix, _, All),             % take first N
    prefix([F|Fs], Prefix).
```

**Example:**

```prolog
?- fib_n(7, L).
%   L = [0, 1, 1, 2, 3, 5, 8].
```

---

### 7.2 Empty Sublists

**Purpose:**
Generate an infinite stream of empty lists `[]` and collect the first N of them.

```prolog
% infinite_empty(-E)  
%   Generator: E = [] on every backtrack.
infinite_empty([]).
infinite_empty(E) :-
    infinite_empty(E).

% empty_sublists(+N, -List)  
%   Collect N copies of [] in List.
empty_sublists(N, Ls) :-
    findall(E, infinite_empty(E), All),
    length(Prefix, N),
    append(Prefix, _, All),
    prefix(Ls, Prefix).
```

**Example:**

```prolog
?- empty_sublists(4, L).
%   L = [[], [], [], []].
```

---

### 7.3 Arithmetic Progression

**Purpose:**
Simulate an infinite arithmetic progression and collect the first N terms.

```prolog
% infinite_ap(+Start, +Step, -X)  
%   Generator: X = Start, Start+Step, Start+2*Step, ...
infinite_ap(Start, _, Start).
infinite_ap(Start, Step, X) :-
    Next is Start + Step,
    infinite_ap(Next, Step, X).

% ap_n(+Start, +Step, +N, -List)  
%   Collects the first N terms of the progression.
ap_n(Start, Step, N, List) :-
    findall(X, infinite_ap(Start, Step, X), All),
    length(Prefix, N),
    append(Prefix, _, All),
    prefix(List, Prefix).
```

**Example:**

```prolog
?- ap_n(2, 3, 5, L).
%   L = [2, 5, 8, 11, 14].
```

---


#### 7.4 Even Numbers

**Purpose:**
Generate the infinite sequence of even integers (0, 2, 4, …) and collect the first N of them.

```prolog
% infinite_even(-X)
%   On each backtrack, X = 0, 2, 4, …
infinite_even(0).
infinite_even(X) :-
    infinite_even(Y),
    X is Y + 2.

% even_n(+N, -List)
%   Collects the first N even numbers into List.
even_n(N, List) :-
    findall(X, infinite_even(X), All),
    length(Prefix, N),
    append(Prefix, _, All),
    prefix(List, Prefix).
```

**Example:**

```prolog
?- even_n(5, L).
% L = [0, 2, 4, 6, 8].
```

---

#### 7.5 Odd Numbers

**Purpose:**
Generate the infinite sequence of odd integers (1, 3, 5, …) and collect the first N of them.

```prolog
% infinite_odd(-X)
%   On each backtrack, X = 1, 3, 5, …
infinite_odd(1).
infinite_odd(X) :-
    infinite_odd(Y),
    X is Y + 2.

% odd_n(+N, -List)
%   Collects the first N odd numbers into List.
odd_n(N, List) :-
    findall(X, infinite_odd(X), All),
    length(Prefix, N),
    append(Prefix, _, All),
    prefix(List, Prefix).
```

**Example:**

```prolog
?- odd_n(5, L).
% L = [1, 3, 5, 7, 9].
```

---

#### 7.6 Powers of Two

**Purpose:**
Generate the infinite sequence 1, 2, 4, 8, … (powers of two) and collect the first N.

```prolog
% infinite_pow2(-X)
%   On each backtrack, X = 1, 2, 4, 8, …
infinite_pow2(1).
infinite_pow2(X) :-
    infinite_pow2(Y),
    X is Y * 2.

% pow2_n(+N, -List)
%   Collects the first N powers of two into List.
pow2_n(N, List) :-
    findall(X, infinite_pow2(X), All),
    length(Prefix, N),
    append(Prefix, _, All),
    prefix(List, Prefix).
```

**Example:**

```prolog
?- pow2_n(6, L).
% L = [1, 2, 4, 8, 16, 32].
```

---

#### 7.7 Squares of Integers

**Purpose:**
Generate the infinite sequence of square numbers (0, 1, 4, 9, …) and collect the first N.

```prolog
% infinite_nat(-N)  % helper: generate 0,1,2,3,…
infinite_nat(0).
infinite_nat(N) :-
    infinite_nat(M),
    N is M + 1.

% infinite_square(-X)
%   On each backtrack, X = N*N for N = 0,1,2,…
infinite_square(X) :-
    infinite_nat(N),
    X is N * N.

% square_n(+N, -List)
%   Collects the first N squares into List.
square_n(N, List) :-
    findall(X, infinite_square(X), All),
    length(Prefix, N),
    append(Prefix, _, All),
    prefix(List, Prefix).
```

**Example:**

```prolog
?- square_n(7, L).
% L = [0, 1, 4, 9, 16, 25, 36].
```

---

#### 7.8 Prime Numbers

**Purpose:**
Generate the infinite sequence of primes (2, 3, 5, 7, …) and collect the first N.

```prolog
% is_prime(+N)
%   Succeeds if N is a prime.
is_prime(2).
is_prime(N) :-
    N > 2,
    Limit is floor(sqrt(N)),
    \+ ( between(2, Limit, M),
         0 is N mod M ).

% infinite_prime(-P)
%   On each backtrack, P = 2,3,5,7,…
infinite_prime(P) :-
    infinite_nat(N),
    N >= 2,
    is_prime(N),
    P = N.

% prime_n(+N, -List)
%   Collects the first N primes into List.
prime_n(N, List) :-
    findall(P, infinite_prime(P), All),
    length(Prefix, N),
    append(Prefix, _, All),
    prefix(List, Prefix).
```

**Example:**

```prolog
?- prime_n(10, L).
% L = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29].
```

---

#### 7.9 Triangular Numbers

**Purpose:**
Generate the infinite sequence of triangular numbers (0, 1, 3, 6, 10, …) where Tₙ = n(n+1)/2, and collect the first N.

```prolog
% infinite_triangular(-T)
%   On each backtrack, T = N*(N+1)//2 for N = 0,1,2,…
infinite_triangular(T) :-
    infinite_nat(N),
    T is N * (N + 1) // 2.

% triangular_n(+N, -List)
%   Collects the first N triangular numbers into List.
triangular_n(N, List) :-
    findall(T, infinite_triangular(T), All),
    length(Prefix, N),
    append(Prefix, _, All),
    prefix(List, Prefix).
```

**Example:**

```prolog
?- triangular_n(8, L).
% L = [0, 1, 3, 6, 10, 15, 21, 28].
```

---

#### 7.10 Pythagorean Triples

**Purpose:**
Generate (X,Y,Z) with X²+Y²=Z² (X≤Y), an “infinite” stream of Pythagorean triples, and collect the first N.

```prolog
% infinite_pythag(-X,-Y,-Z)
%   On each backtrack, yields next triple [X,Y,Z].
infinite_pythag(X, Y, Z) :-
    infinite_nat(X), X > 0,
    infinite_nat(Y), Y >= X,
    Z2 is X*X + Y*Y,
    Zf is sqrt(Z2),
    Z is round(Zf),
    Zf =:= Z.

% pythagorean_n(+N, -Triples)
%   Collects the first N pythagorean triples as [ [X,Y,Z], … ].
pythagorean_n(N, Triples) :-
    findall([X,Y,Z], infinite_pythag(X,Y,Z), All),
    length(Prefix, N),
    append(Prefix, _, All),
    prefix(Triples, Prefix).
```

**Example:**

```prolog
?- pythagorean_n(5, L).
% L = [[3,4,5], [5,12,13], [6,8,10], [7,24,25], [8,15,17]].
```

---

**Tips for Usage:**

* **Backtracking generators** (`infinite_fib/1`, `infinite_empty/1`, `infinite_ap/3`) never terminate on their own; always wrap them with a limiting predicate (`findall/3` + `length/2` or a custom accumulator) to get a finite list.

* If you don’t need the entire intermediate list, you can write a direct tail-recursive collector:

  ```prolog
  fib_n_acc(0, _, _, Acc, Acc) :- !.
  fib_n_acc(N, A, B, Acc0, Acc) :-
      N > 0,
      append(Acc0, [A], Acc1),
      N1 is N - 1,
      Sum is A + B,
      fib_n_acc(N1, B, Sum, Acc1, Acc).

  fib_n(N, L) :-
      fib_n_acc(N, 0, 1, [], L).
  ```

* Always include a **cut** (`!`) in your base case to prevent unwanted backtracking once you’ve collected N elements.




---

## 8. CLP(FD) Sequence Generators

Below are several “generator” predicates written with CLP(FD).  Each follows the pattern:

1. **Variables and Domains** – Declare your FD variables and their domains
2. **Constraints** – Post the arithmetic or global constraints
3. **Label** – Invoke `label/1` (or a labeling strategy) to search
4. **Show the Results** – Print or return the computed sequence

You’ll also see a few **helper predicates**—small CLP(FD) fragments you can reuse when defining new sequences.

---

### 8.1 Arithmetic Progression: `ap_clp/4`

Generates a list of length *N* starting at `Start` and stepping by `Step`.

```prolog
:- use_module(library(clpfd)).

%% ap_clp(+N, +Start, +Step, -V)
ap_clp(N, Start, Step, V) :-
    %% 1-Variables and Domains
    length(V, N),
    V ins 0..sup,

    %% 2-Constraints
    V = [X1|_], X1 #= Start,
    ap_steps(V, Step),

    %% 3-Label
    label(V),

    %% 4-Show the results
    writeln(V).

ap_steps([_], _).
ap_steps([X,Y|Xs], Step) :-
    Y #= X + Step,
    ap_steps([Y|Xs], Step).
```

**Example:**

```prolog
?- ap_clp(5, 2, 3, V).
[2,5,8,11,14]
```

---

### 8.2 Fibonacci Sequence: `fib_clp/2`

Generates the first *N* Fibonacci numbers.

```prolog
:- use_module(library(clpfd)).

%% fib_clp(+N, -V)
fib_clp(N, V) :-
    %% 1-Variables and Domains
    length(V, N),
    V ins 0..sup,

    %% 2-Constraints
    (   N >= 1 -> nth1(1, V, F1), F1 #= 0 ; true ),
    (   N >= 2 -> nth1(2, V, F2), F2 #= 1 ; true ),
    fib_constr(3, V, N),

    %% 3-Label
    label(V),

    %% 4-Show the results
    writeln(V).

fib_constr(I, _V, N) :- I > N, !.
fib_constr(I, V, N) :-
    I1 is I-1, I2 is I-2,
    nth1(I,  V, Fi),
    nth1(I1, V, Fim1),
    nth1(I2, V, Fim2),
    Fi #= Fim1 + Fim2,
    Inext is I+1,
    fib_constr(Inext, V, N).
```

**Example:**

```prolog
?- fib_clp(7, V).
[0,1,1,2,3,5,8]
```

---

### 8.3 Even Numbers: `even_clp/2`

Generates the first *N* even non-negative integers.

```prolog
:- use_module(library(clpfd)).

%% even_clp(+N, -V)
even_clp(N, V) :-
    %% 1-Variables and Domains
    length(V, N),
    V ins 0..sup,

    %% 2-Constraints
    V = [X1|_], X1 #= 0,
    even_steps(V),

    %% 3-Label
    label(V),

    %% 4-Show the results
    writeln(V).

even_steps([_]).
even_steps([X,Y|Xs]) :-
    Y #= X + 2,
    even_steps([Y|Xs]).
```

**Example:**

```prolog
?- even_clp(6, V).
[0,2,4,6,8,10]
```

---

### 8.4 Powers of Two: `pow2_clp/2`

Generates 1, 2, 4, 8, … up to *N* terms.

```prolog
:- use_module(library(clpfd)).

%% pow2_clp(+N, -V)
pow2_clp(N, V) :-
    %% 1-Variables and Domains
    length(V, N),
    V ins 1..sup,

    %% 2-Constraints
    V = [X1|_], X1 #= 1,
    pow2_steps(V),

    %% 3-Label
    label(V),

    %% 4-Show the results
    writeln(V).

pow2_steps([_]).
pow2_steps([X,Y|Ys]) :-
    Y #= X * 2,
    pow2_steps([Y|Ys]).
```

**Example:**

```prolog
?- pow2_clp(6, V).
[1,2,4,8,16,32]
```

---

### 8.5 Squares of Naturals: `square_clp/2`

Generates 0², 1², 2², … for *N* terms.

```prolog
:- use_module(library(clpfd)).

%% square_clp(+N, -V)
square_clp(N, V) :-
    %% 1-Variables and Domains
    length(V, N),
    V ins 0..sup,

    %% 2-Constraints
    square_steps(0, V),

    %% 3-Label
    label(V),

    %% 4-Show the results
    writeln(V).

square_steps(_, []).
square_steps(I, [X|Xs]) :-
    X #= I * I,
    I1 #= I + 1,
    square_steps(I1, Xs).
```

**Example:**

```prolog
?- square_clp(7, V).
[0,1,4,9,16,25,36]
```

---

### 8.6 Triangular Numbers: `triangular_clp/2`

Generates the sequence Tₙ = n(n+1)/2 for *N* terms.

```prolog
:- use_module(library(clpfd)).

%% triangular_clp(+N, -V)
triangular_clp(N, V) :-
    %% 1-Variables and Domains
    length(V, N),
    V ins 0..sup,

    %% 2-Constraints
    triangular_steps(1, V),

    %% 3-Label
    label(V),

    %% 4-Show the results
    writeln(V).

triangular_steps(_, []).
triangular_steps(I, [X|Xs]) :-
    X #= I*(I+1)//2,
    I1 is I + 1,
    triangular_steps(I1, Xs).
```

**Example:**

```prolog
?- triangular_clp(8, V).
[1,3,6,10,15,21,28,36]
```

---

### 8.7 Prime Numbers (Simple Sieve): `prime_clp/2`

Generates the first *N* primes via naïve trial‐division.

```prolog
:- use_module(library(clpfd)).

%% prime_clp(+N, -P)
prime_clp(N, P) :-
    %% 1-Variables and Domains
    length(P, N),
    P ins 2..sup,

    %% 2-Constraints
    P = [P1|_], P1 #= 2,
    prime_steps(P, 2),

    %% 3-Label
    label(P),

    %% 4-Show the results
    writeln(P).

prime_steps([], _).
prime_steps([X|Xs], Prev) :-
    X #> Prev,
    X mod 2 #\= 0,          % odd >2
    primality(3, X),
    prime_steps(Xs, X).

primality(D, N) :- D*D #> N, !.
primality(D, N) :-
    N mod D #\= 0,
    D2 #= D + 2,            % only test odd divisors
    primality(D2, N).
```

**Example:**

```prolog
?- prime_clp(8, P).
[2,3,5,7,11,13,17,19]
```

---

### 8.8 Pythagorean Triples: `pyth_clp/2`

Generates *N* triples \[X,Y,Z] with X²+Y²=Z² and X≤Y.

```prolog
:- use_module(library(clpfd)).

%% pyth_clp(+N, -Triples)
pyth_clp(N, Triples) :-
    %% 1-Variables and Domains
    length(Triples, N),
    append(Triples, Vars),      % flatten for labeling
    Vars ins 1..sup,

    %% 2-Constraints
    maplist(pyth_constraint, Triples),

    %% 3-Label
    label(Vars),

    %% 4-Show the results
    writeln(Triples).

pyth_constraint([X,Y,Z]) :-
    X #=< Y,
    X*X + Y*Y #= Z*Z.
```

**Example:**

```prolog
?- pyth_clp(5, T).
[[3,4,5],[5,12,13],[6,8,10],[7,24,25],[8,15,17]]
```

---

 Below are CLP(FD) adaptations of the **Arithmetic Progression** and **Fibonacci** generators so that, instead of “the first N terms,” they produce **all terms less than a given bound** `K` (here `K=25`).

---

### 8.9 Arithmetic Progression up to a Bound

```prolog
:- use_module(library(clpfd)).

%% ap_clp_upto(+Start, +Step, +K, -V)
%%   V is the list of all values Start, Start+Step, Start+2*Step, … that are < K.

ap_clp_upto(Start, Step, K, V) :-
    % 1-Variables and Domains
    MaxCount is ((K - Start) // Step) + 1,
    length(V, MaxCount),
    V ins Start..K-1,

    % 2-Constraints
    V = [X1|_],            X1 #= Start,
    ap_steps(V, Step),
    last(V, Last),         Last #< K,

    % 3-Label
    label(V),

    % 4-Show the results
    writeln(V).

ap_steps([_], _).
ap_steps([X,Y|Xs], Step) :-
    Y #= X + Step,
    ap_steps([Y|Xs], Step).
```

**Example:**

```prolog
?- ap_clp_upto(2, 3, 25, V).
[2,5,8,11,14,17,20,23]
```

---

### 8.10 Fibonacci up to a Bound

```prolog
:- use_module(library(clpfd)).

%% compute_max_fib_index(+K, -N)
%%   N is the largest index such that Fib(N) < K.
compute_max_fib_index(K, N) :-
    compute_max_fib_index(0, 1, 1, K, 2, N).

compute_max_fib_index(_, F2, F2, _, I, I) :- F2 >= K, !.
compute_max_fib_index(F1, F2, _, K, I0, N) :-
    F2 < K,
    F3 is F1 + F2,
    I1 is I0 + 1,
    compute_max_fib_index(F2, F3, F3, K, I1, N).

%% fib_clp_upto(+K, -V)
%%   V is the list of all Fibonacci numbers < K.
fib_clp_upto(K, V) :-
    % 1-Variables and Domains
    compute_max_fib_index(K, N),
    length(V, N),
    V ins 0..K-1,

    % 2-Constraints
    ( N >= 1 -> nth1(1, V, F1), F1 #= 0 ; true ),
    ( N >= 2 -> nth1(2, V, F2), F2 #= 1 ; true ),
    fib_constr(3, V, N),
    last(V, Last),            Last #< K,

    % 3-Label
    label(V),

    % 4-Show the results
    writeln(V).

fib_constr(I, _V, N) :- I > N, !.
fib_constr(I, V, N) :-
    I1 is I-1, I2 is I-2,
    nth1(I,  V, Fi),
    nth1(I1, V, Fi1),
    nth1(I2, V, Fi2),
    Fi #= Fi1 + Fi2,
    Inext is I + 1,
    fib_constr(Inext, V, N).
```

**Example:**

```prolog
?- fib_clp_upto(25, V).
[0,1,1,2,3,5,8,13,21]
```

---

With these two predicates you no longer fix the **number** of terms in advance, but instead compute how many will fit under the threshold `K`, impose `Last #< K`, and let CLP(FD) fill in the rest.


Below are the same ten undirected‐graph predicates, each with a more detailed description and **three usage examples** illustrating different scenarios.

---

## 9 Graph Predicates

### 9.1 Undirected Graph Predicates

We assume an undirected graph is represented as:

```prolog
graph(Vertices, Edges).
```

where `Vertices` is a list of distinct vertex IDs, and `Edges` is a list of unordered pairs `[X,Y]` (meaning an undirected edge between `X` and `Y`).

#### 9.1.1 `edge/3`

```prolog
%% edge(+X, +Y, +Edges) is semidet.
%% True if there is an undirected edge between X and Y in Edges.
edge(X, Y, Edges) :-
    (   member([X,Y], Edges)
    ;   member([Y,X], Edges)
    ).
```

**Description:**
Checks whether vertices `X` and `Y` are directly connected by an edge in `Edges`. Because the graph is undirected, either `[X,Y]` or `[Y,X]` counts.

**Examples:**

1. **Simple “exists” check (order matches stored edge):**

   ```prolog
   ?- edge(1, 2, [[1,2],[2,3],[3,4]]).
   true.
   ```

   *Explanation:*
   The list of edges contains `[1,2]`, so `edge(1,2,…)` succeeds.

2. **“Reverse” order check (edge stored as `[Y,X]`):**

   ```prolog
   ?- edge(3, 2, [[1,2],[2,3],[3,4]]).
   true.
   ```

   *Explanation:*
   The pair `[2,3]` is in `Edges`; since the graph is undirected, `edge(3,2,…)` also succeeds.

3. **No edge present:**

   ```prolog
   ?- edge(1, 4, [[1,2],[2,3],[3,4]]).
   false.
   ```

   *Explanation:*
   Neither `[1,4]` nor `[4,1]` appears in the edge list, so it fails.

---

#### 9.1.2 `adjacent/3`

```prolog
%% adjacent(+X, -Y, +Edges) is nondet.
%% True for each Y that is a neighbor of X in Edges.
adjacent(X, Y, Edges) :-
    edge(X, Y, Edges).
```

**Description:**
Nondeterministically enumerates every direct neighbor `Y` of vertex `X`. Each time you backtrack, you get another `Y`.

**Examples:**

1. **Enumerate all neighbors:**

   ```prolog
   ?- adjacent(2, N, [[1,2],[2,3],[2,4],[4,5]]).
   N = 1 ;
   N = 3 ;
   N = 4 ;
   false.
   ```

   *Explanation:*
   On backtracking, Prolog reports `N = 1`, then `3`, then `4`.

2. **Use `findall/3` to collect neighbors at once:**

   ```prolog
   ?- findall(N, adjacent(3, N, [[2,3],[3,4],[1,3]]), Ns).
   Ns = [2,4,1].
   ```

   *Explanation:*
   `findall/3` gathers every solution for `N`. Order corresponds to appearance of edges.

3. **No neighbors (fails immediately):**

   ```prolog
   ?- adjacent(5, N, [[1,2],[2,3],[3,4]]).
   false.
   ```

   *Explanation:*
   Vertex `5` does not appear in any edge, so `adjacent(5,…)` has no solutions.

---

#### 9.1.3 `neighbors/3`

```prolog
%% neighbors(+X, +Edges, -List) is det.
%% List is the (sorted, duplicate-free) set of all neighbors of X in Edges.
neighbors(X, Edges, Neighs) :-
    findall(Y, adjacent(X, Y, Edges), Ys),
    sort(Ys, Neighs).
```

**Description:**
Collects every neighbor of `X` into a sorted list without duplicates. Useful for quickly seeing all vertices that share an edge with `X`.

**Examples:**

1. **Typical case (some duplicates in raw edges):**

   ```prolog
   ?- neighbors(3, [[1,3],[3,2],[3,1],[3,4],[2,3]], Neighs).
   Neighs = [1,2,4].
   ```

   *Explanation:*
   Although edges `[1,3]` and `[3,1]` both appear, `sort/2` removes duplicates and orders them.

2. **Isolated vertex returns empty list:**

   ```prolog
   ?- neighbors(5, [[1,2],[2,3],[3,4]], Neighs).
   Neighs = [].
   ```

   *Explanation:*
   No edges involve `5`, so its neighbor list is empty.

3. **Single‐edge neighbor:**

   ```prolog
   ?- neighbors(4, [[3,4]], N).
   N = [3].
   ```

   *Explanation:*
   Vertex `4` has exactly one neighbor, `3`.

---

#### 9.1.4 `degree/3`

```prolog
%% degree(+X, +Edges, -D) is det.
%% D is the number of neighbors (degree) of vertex X in Edges.
degree(X, Edges, D) :-
    neighbors(X, Edges, Neighs),
    length(Neighs, D).
```

**Description:**
Computes the “degree” of vertex `X`—i.e., how many edges incident on `X`. It uses `neighbors/3` internally.

**Examples:**

1. **Vertex with multiple neighbors:**

   ```prolog
   ?- degree(2, [[1,2],[2,3],[2,4],[4,2]], D).
   D = 3.
   ```

   *Explanation:*
   Unique neighbors of `2` are `[1,3,4]` (even though `[4,2]` duplicates `[2,4]`).

2. **Leaf (degree = 1):**

   ```prolog
   ?- degree(5, [[5,6],[1,2]], D).
   D = 1.
   ```

   *Explanation:*
   Only edge touching `5` is `[5,6]`.

3. **Isolated vertex (degree = 0):**

   ```prolog
   ?- degree(7, [[1,2],[2,3],[3,4]], D).
   D = 0.
   ```

   *Explanation:*
   Vertex `7` does not appear in any edge.

---

#### 9.1.5 `simple_path/5`

```prolog
%% simple_path(+Start, +Goal, +Edges, +Visited, -Path) is nondet.
%% True if Path is a simple (acyclic) path [Start, …, Goal] in Edges,
%% avoiding any vertex in Visited.  Visited accumulates as we go.
simple_path(Goal, Goal, _Edges, _Visited, [Goal]) :- !.
simple_path(X, Goal, Edges, Visited, [X|Rest]) :-
    \+ member(X, Visited),
    adjacent(X, Next, Edges),
    \+ member(Next, Visited),
    simple_path(Next, Goal, Edges, [X|Visited], Rest).
```

**Description:**
Finds any path from `Start` to `Goal` that does not revisit a vertex. Each solution (on backtracking) yields a different simple path. Call initially with `Visited = []`.

**Examples:**

1. **Two different simple paths:**

   ```prolog
   ?- simple_path(1,5, [[1,2],[2,3],[3,5],[1,4],[4,5]], [], P).
   P = [1, 2, 3, 5] ;
   P = [1, 4, 5] ;
   false.
   ```

   *Explanation:*
   Both `[1,2,3,5]` and `[1,4,5]` are valid simple paths from `1` to `5`.

2. **No path exists:**

   ```prolog
   ?- simple_path(a, d, [[a,b],[b,c]], [], P).
   false.
   ```

   *Explanation:*
   There is no way to reach `d` from `a`; it fails immediately.

3. **Longer graph, multiple routes:**

   ```prolog
   ?- GraphEdges = [[1,2],[2,3],[3,4],[2,5],[5,4]],
      simple_path(1,4, GraphEdges, [], P).
   P = [1, 2, 3, 4] ;
   P = [1, 2, 5, 4] ;
   false.
   ```

   *Explanation:*
   Two simple routes: `1→2→3→4` and `1→2→5→4`.

---

#### 9.1.6 `path_length/5`

```prolog
%% path_length(+X, +Y, +Edges, +Visited, -Len) is nondet.
%% Len is the number of edges in a simple path from X to Y,
%% each discovered via `simple_path/5`. Call with Visited = [] initially.
path_length(X, Y, Edges, Visited, Len) :-
    simple_path(X, Y, Edges, Visited, Path),
    length(Path, L),     % L is number of vertices
    Len is L - 1.        % convert to number of edges
```

**Description:**
Chaque solution gives a different path, so different lengths on backtracking. If you only want the shortest, you can collect all and take the minimum.

**Examples:**

1. **Enumerate all path lengths:**

   ```prolog
   ?- path_length(1,5, [[1,2],[2,3],[3,5],[1,4],[4,5]], [], L).
   L = 3 ;    % via [1,2,3,5]
   L = 2 ;    % via [1,4,5]
   false.
   ```

   *Explanation:*
   Two lengths: 3 edges and 2 edges.

2. **Single trivial path (start = goal):**

   ```prolog
   ?- path_length(v, v, [[v,w]], [], L).
   L = 0.
   ```

   *Explanation:*
   Calling `simple_path(v,v,…)` yields `[v]`, so `Len = 0`.

3. **No path:**

   ```prolog
   ?- path_length(a, d, [[a,b],[c,d]], [], L).
   false.
   ```

   *Explanation:*
   `simple_path/5` fails, so `path_length/5` fails.

---

#### 9.1.7 `connected/2`

```prolog
%% connected(+Vertices, +Edges) is semidet.
%% True if every vertex in Vertices belongs to a single connected component.
connected([], _).
connected([Start|Rest], Edges) :-
    reachable([Start], Edges, ReachAll),
    sort(ReachAll, Rsorted),
    sort([Start|Rest], AllSorted),
    Rsorted == AllSorted.

%% reachable(+Frontier, +Edges, -AllReached)
%% Gathers all vertices reachable (via any number of edges) from Frontier.
reachable([], _Edges, []).
reachable([V|Vs], Edges, Reachable) :-
    reachable(Vs, Edges, R1),
    (   member(V, R1)
    ->  Reachable = R1
    ;   neighbors(V, Edges, Neighs),
        append(Neighs, R1, Temp),
        Reachable = [V|Temp]
    ).
```

**Description:**
Performs a graph traversal (similar to BFS/DFS) from one starting vertex. If the set of all reachable vertices matches the full `Vertices` list, the graph is connected.

**Examples:**

1. **Connected graph (chain):**

   ```prolog
   ?- connected([1,2,3,4], [[1,2],[2,3],[3,4]]).
   true.
   ```

   *Explanation:*
   All vertices 1–4 are in a single component.

2. **Disconnected graph:**

   ```prolog
   ?- connected([1,2,3,4], [[1,2],[3,4]]).
   false.
   ```

   *Explanation:*
   The component `{1,2}` is separate from `{3,4}`.

3. **Singleton or empty:**

   ```prolog
   ?- connected([x], []).
   true.

   ?- connected([], []).
   true.
   ```

   *Explanation:*
   A single vertex or no vertices is trivially connected.

---

#### 9.1.8 `component/3`

```prolog
%% component(+X, +Edges, -Comp) is det.
%% Comp is the (sorted, duplicate-free) set of vertices reachable from X.
component(X, Edges, Comp) :-
    reachable([X], Edges, R),
    sort(R, Comp).
```

**Description:**
Returns the entire connected component in which `X` lies, by reusing `reachable/3`.

**Examples:**

1. **Component of a vertex in a small graph:**

   ```prolog
   ?- component(2, [[1,2],[2,3],[3,4],[5,6]], C).
   C = [1,2,3,4].
   ```

   *Explanation:*
   From `2` you can reach `1,3,4` but not `5` or `6`.

2. **Isolated vertex yields singleton:**

   ```prolog
   ?- component(z, [[a,b],[b,c]], C).
   C = [z].
   ```

   *Explanation:*
   `z` is not in any edge, so `reachable([z],…)` returns `[z]`.

3. **Another component in the same graph:**

   ```prolog
   ?- component(5, [[1,2],[3,4],[5,6],[6,7]], C).
   C = [5,6,7].
   ```

   *Explanation:*
   From `5` you visit `6`, then `7`.

---

#### 9.1.9 `has_cycle/2`

```prolog
%% has_cycle(+Vertices, +Edges) is semidet.
%% True if there exists at least one simple cycle in the graph.
has_cycle(Vertices, Edges) :-
    member(V, Vertices),
    cycle_from(V, V, Edges, [], _Cycle).

%% cycle_from(+Start, +Curr, +Edges, +Visited, -Cycle)
%% Finds a simple cycle that begins and ends at Start (length ≥ 3).
cycle_from(Start, Curr, Edges, Visited, [Start,Curr]) :-
    adjacent(Curr, Next, Edges),
    Next == Start,
    \+ member(Next, Visited),
    !.  % Found a 2-edge return; minimal cycle length is 2 here, but we want ≥3?
cycle_from(Start, Curr, Edges, Visited, [Curr|Rest]) :-
    \+ member(Curr, Visited),
    adjacent(Curr, Next, Edges),
    Next \= Start,
    cycle_from(Start, Next, Edges, [Curr|Visited], Rest).
```

**Description:**
Searches for any closed loop (cycle) of length ≥ 3 in the undirected graph. It picks each vertex `V` and attempts to walk until it returns to `V` without revisiting other nodes. On success, it produces one example cycle; otherwise fails.

**Examples:**

1. **Graph with a simple triangle:**

   ```prolog
   ?- has_cycle([1,2,3], [[1,2],[2,3],[3,1]]).
   true.
   ```

   *Explanation:*
   Triangle cycle exists: 1→2→3→1.

2. **Graph with no cycles (tree):**

   ```prolog
   ?- has_cycle([a,b,c,d], [[a,b],[b,c],[c,d]]).
   false.
   ```

   *Explanation:*
   This is a path of length 3; no way to return to the start.

3. **More complex graph, cycle of length 4:**

   ```prolog
   ?- has_cycle([1,2,3,4,5], [[1,2],[2,3],[3,4],[4,1],[3,5]]).
   true.
   ```

   *Explanation:*
   One cycle is 1→2→3→4→1. The presence of `[3,5]` doesn’t matter.

---

#### 9.1.10 `shortest_path/5`

```prolog
%% shortest_path(+Start, +Goal, +Edges, -Path, -Dist) is semidet.
%% Finds a shortest simple path (fewest edges) from Start to Goal.
%% Path is a list of vertices [Start,…,Goal]; Dist = number of edges.
shortest_path(Start, Goal, Edges, Path, Dist) :-
    bfs([[Start,[Start]]], Goal, Edges, [Start], PathRev),
    reverse(PathRev, Path),
    length(Path, L), Dist is L - 1.

%% bfs(+Queue, +Goal, +Edges, +Visited, -ResultPath)
bfs([[Goal, Path]|_], Goal, _Edges, _Visited, Path).
bfs([[Curr, PathSoFar]|Queue], Goal, Edges, Visited, Result) :-
    Curr \= Goal,
    findall(
      [Next, [Next|PathSoFar]],
      ( adjacent(Curr, Next, Edges),
        \+ member(Next, Visited)
      ),
      NextPairs
    ),
    extract_vertices(NextPairs, NewVs),
    append(Visited, NewVs, Visited2),
    append(Queue, NextPairs, NewQueue),
    bfs(NewQueue, Goal, Edges, Visited2, Result).

extract_vertices([], []).
extract_vertices([[V,_]|Rest], [V|Vs]) :-
    extract_vertices(Rest, Vs).
```

**Description:**
Uses a breadth‐first search (BFS) approach to guarantee that the first time we reach `Goal`, we have done so by the shortest number of edges. The queue holds pairs `[CurrentVertex, PathSoFar]`. When `Goal` appears, `PathSoFar` (reversed) is the shortest path.

**Examples:**

1. **Unique shortest path:**

   ```prolog
   ?- shortest_path(1, 5, [[1,2],[2,3],[3,5],[1,4],[4,5]], P, D).
   P = [1,4,5],
   D = 2.
   ```

   *Explanation:*
   Although `1→2→3→5` also reaches 5, it has length 3 edges; the BFS finds `1→4→5` (2 edges) first.

2. **Multiple equal‐length choices (reports one):**

   ```prolog
   ?- shortest_path(a, d, [[a,b],[b,d],[a,c],[c,d]], P, D).
   P = [a,b,d],
   D = 2.

   % On backtracking we still get the same, because BFS never leaves a choice point 
   % once the goal is found.
   ```

   *Explanation:*
   There are two paths of length 2 (`a→b→d` and `a→c→d`), but BFS enqueues `[a,b]` before `[a,c]`, so it finds `a→b→d` first and stops.

3. **Goal unreachable:**

   ```prolog
   ?- shortest_path(x, y, [[x,z],[z,w]], P, D).
   false.
   ```

   *Explanation:*
   No sequence of edges leads from `x` to `y`, so it fails.


#### 9.1.11 is\_tree/2

```prolog
%% is_tree(+Vertices, +Edges) is semidet.
%% True if the undirected graph (Vertices,Edges) is a tree:
%%   1. It is connected.
%%   2. It contains no cycles.
is_tree(Vertices, Edges) :-
    connected(Vertices, Edges),
    \+ has_cycle(Vertices, Edges).
```

**Description:**
Succeeds exactly when the graph is connected and acyclic—i.e., it has exactly one connected component and no simple cycles.

**Examples:**

1. **A simple tree (chain of 3):**

   ```prolog
   ?- is_tree([1,2,3], [[1,2],[2,3]]).
   true.
   ```

   *Explanation:* The graph is connected and has no cycle.

2. **Not a tree because of a cycle:**

   ```prolog
   ?- is_tree([1,2,3], [[1,2],[2,3],[3,1]]).
   false.
   ```

   *Explanation:* Although connected, the triangle \[1–2–3–1] is a cycle.

3. **Not a tree because disconnected:**

   ```prolog
   ?- is_tree([1,2,3,4], [[1,2],[2,3]]).
   false.
   ```

   *Explanation:* Vertex 4 is isolated, so the graph isn’t connected.

---

#### 9.1.12 number\_of\_components/3

```prolog
%% number_of_components(+Vertices, +Edges, -N) is det.
%% N is the number of connected components in the graph.
number_of_components(Vertices, Edges, N) :-
    findall(Comp,
            ( member(V, Vertices),
              component(V, Edges, Comp)
            ),
            Comps),
    sort(Comps, UniqueComps),
    length(UniqueComps, N).
```

**Description:**
Computes how many disjoint connected subgraphs (components) the graph has by using `component/3` for every vertex, removing duplicates via `sort/2`, and counting.

**Examples:**

1. **Single component:**

   ```prolog
   ?- number_of_components([1,2,3], [[1,2],[2,3]], N).
   N = 1.
   ```

   *Explanation:* All vertices connect into one component.

2. **Two components:**

   ```prolog
   ?- number_of_components([a,b,c,d], [[a,b],[c,d]], N).
   N = 2.
   ```

   *Explanation:* {a,b} and {c,d} are separate.

3. **All isolated vertices:**

   ```prolog
   ?- number_of_components([1,2,3], [], N).
   N = 3.
   ```

   *Explanation:* No edges, so each vertex is its own component.

---

#### 9.1.13 spanning\_tree/3

```prolog
%% spanning_tree(+Vertices, +Edges, -TreeEdges) is nondet.
%% TreeEdges is a set of |Vertices|-1 edges forming a spanning tree
%% of a connected graph.
spanning_tree(Vertices, Edges, TreeEdges) :-
    length(Vertices, N),
    M is N - 1,
    combination(M, Edges, TreeEdges),
    connected(Vertices, TreeEdges).

%% combination(+K, +List, -Comb)
%% True if Comb is a K-element sublist of List.
combination(0, _List, []) :- !.
combination(K, [H|T], [H|Comb]) :-
    K > 0,
    K1 is K - 1,
    combination(K1, T, Comb).
combination(K, [_|T], Comb) :-
    K > 0,
    combination(K, T, Comb).
```

**Description:**
Nondeterministically picks K = |V|-1 edges and checks if they connect all vertices without leaving any component disconnected. Each solution is one spanning tree.

**Examples:**

1. **Unique spanning tree in a chain:**

   ```prolog
   ?- spanning_tree([1,2,3], [[1,2],[2,3]], T).
   T = [[1,2],[2,3]].
   ```

   *Explanation:* Only one way to pick 2 edges that connect all three vertices.

2. **Two possible spanning trees in a square:**

   ```prolog
   ?- Edges = [[1,2],[2,3],[3,4],[4,1]], spanning_tree([1,2,3,4], Edges, T).
   T = [[1,2],[2,3],[3,4]] ;
   T = [[2,3],[3,4],[4,1]] ;
   T = [[3,4],[4,1],[1,2]] ;
   T = [[4,1],[1,2],[2,3]] ;
   false.
   ```

   *Explanation:* Any three edges of the cycle form a spanning tree.

3. **Fails if graph disconnected:**

   ```prolog
   ?- spanning_tree([a,b,c], [[a,b]], T).
   false.
   ```

   *Explanation:* No way to connect all vertices with just one edge.

---

#### 9.1.14 forest/3

```prolog
%% forest(+Vertices, +Edges, -ForestEdges) is det.
%% ForestEdges is a spanning forest: a union of spanning trees
%% for each connected component.
forest(Vertices, Edges, ForestEdges) :-
    number_of_components(Vertices, Edges, _),
    findall(T,
            ( member(V, Vertices),
              component(V, Edges, Comp),
              spanning_tree(Comp, Edges, T)
            ),
            Trees),
    append(Trees, ForestEdges).
```

**Description:**
Builds a spanning tree for each component and concatenates their edges into a forest. Equivalent to a minimum spanning forest when edge weights are ignored.

**Examples:**

1. **Single component reduces to its tree:**

   ```prolog
   ?- forest([1,2,3], [[1,2],[2,3]], F).
   F = [[1,2],[2,3]].
   ```

   *Explanation:* Only one component ⇒ same as its spanning tree.

2. **Two components each of size 2:**

   ```prolog
   ?- forest([a,b,c,d], [[a,b],[c,d]], F).
   F = [[a,b],[c,d]].
   ```

   *Explanation:* Trees for {a,b} and {c,d}.

3. **Larger component + isolated vertex:**

   ```prolog
   ?- forest([1,2,3,4], [[1,2],[2,3]], F).
   F = [[1,2],[2,3]].
   ```

   *Explanation:* Vertex 4 is its own trivial tree (no edges).

---

#### 9.1.15 bridge/4

```prolog
%% bridge(+X, +Y, +Vertices, +Edges) is semidet.
%% True if edge [X,Y] is a bridge: removing it increases
%% the number of components.
bridge(X, Y, Vertices, Edges) :-
    edge(X, Y, Edges),
    delete_edge(X, Y, Edges, Edges1),
    number_of_components(Vertices, Edges, N0),
    number_of_components(Vertices, Edges1, N1),
    N1 > N0.

%% delete_edge(+X, +Y, +Edges, -Edges1)
%% True if Edges1 is Edges without the undirected edge [X,Y].
delete_edge(X, Y, Edges, Edges1) :-
    ( select([X,Y], Edges, Edges1)
    ; select([Y,X], Edges, Edges1)
    ).
```

**Description:**
An edge is a bridge if its removal disconnects some part of the graph (i.e., increases component count).

**Examples:**

1. **Bridge in a chain:**

   ```prolog
   ?- bridge(2,3,[1,2,3], [[1,2],[2,3]], B).
   B = true.
   ```

   *Explanation:* Removing \[2,3] splits off vertex 3.

2. **Non-bridge in a triangle:**

   ```prolog
   ?- bridge(1,2,[1,2,3], [[1,2],[2,3],[3,1]], B).
   B = false.
   ```

   *Explanation:* Graph remains connected after removing any single edge.

3. **Edge not present:**

   ```prolog
   ?- bridge(a,b,[a,b,c], [[b,c]], B).
   false.
   ```

   *Explanation:* \[a,b] isn’t in Edges, so predicate fails.

---

#### 9.1.16 articulation\_point/3

```prolog
%% articulation_point(+X, +Vertices, +Edges) is semidet.
%% True if removing vertex X (and its incident edges)
%% increases the number of components.
articulation_point(X, Vertices, Edges) :-
    delete(X, Vertices, V1),
    exclude(incident(X), Edges, E1),
    \+ connected(V1, E1).

%% incident(+X, +Edge)
%% True if Edge is incident on X.
incident(X, [X,_]).
incident(X, [_,X]).
```

**Description:**
A vertex is an articulation point (cut-vertex) if removing it and all its edges disconnects the graph.

**Examples:**

1. **Middle of a chain is an articulation point:**

   ```prolog
   ?- articulation_point(2, [1,2,3], [[1,2],[2,3]]).
   true.
   ```

   *Explanation:* Removing 2 splits 1 and 3.

2. **Leaf is not an articulation point:**

   ```prolog
   ?- articulation_point(3, [1,2,3], [[1,2],[2,3]]).
   false.
   ```

   *Explanation:* Removing 3 leaves 1–2 connected.

3. **No articulation in a triangle:**

   ```prolog
   ?- articulation_point(1, [1,2,3], [[1,2],[2,3],[3,1]]).
   false.
   ```

   *Explanation:* Triangle remains connected after any single-vertex removal.

---

#### 9.1.17 is\_bipartite/2

```prolog
%% is_bipartite(+Vertices, +Edges) is semidet.
%% True if the graph is 2-colorable (no odd-length cycles).
is_bipartite(Vertices, Edges) :-
    \+ has_odd_cycle(Vertices, Edges).

%% has_odd_cycle(+Vs, +Es) is nondet.
%% Detects any simple cycle of odd length ≥ 3.
has_odd_cycle(Vs, Es) :-
    member(Start, Vs),
    odd_cycle_from(Start, Start, Es, [], 0).

odd_cycle_from(Start, Curr, Es, Visited, Len) :-
    adjacent(Curr, Next, Es),
    (   Next == Start,
        Len >= 2,
        0 is Len mod 2
    ;   \+ member(Next, Visited),
        Len1 is Len + 1,
        odd_cycle_from(Start, Next, Es, [Curr|Visited], Len1)
    ).
```

**Description:**
A graph is bipartite iff it has no simple cycle of odd length. We search for any odd cycle; if none exists, the graph is 2-colorable.

**Examples:**

1. **Even cycle (square) is bipartite:**

   ```prolog
   ?- is_bipartite([1,2,3,4], [[1,2],[2,3],[3,4],[4,1]]).
   true.
   ```

   *Explanation:* No odd cycles.

2. **Triangle is not bipartite:**

   ```prolog
   ?- is_bipartite([1,2,3], [[1,2],[2,3],[3,1]]).
   false.
   ```

   *Explanation:* 3-cycle is odd.

3. **Disconnected union of two chains:**

   ```prolog
   ?- is_bipartite([a,b,c,d], [[a,b],[b,c],[c,d]]).
   true.
   ```

   *Explanation:* Chains are always bipartite.

---

#### 9.1.18 eccentricity/4

```prolog
%% eccentricity(+X, +Vertices, +Edges, -E) is det.
%% E is the maximum distance (number of edges) from X
%% to any vertex reachable in the graph.
eccentricity(X, Vertices, Edges, E) :-
    findall(L,
            ( member(Y, Vertices),
              path_length(X, Y, Edges, [], L)
            ),
            Lengths),
    max_list(Lengths, E).
```

**Description:**
For each vertex Y reachable from X, compute the simple‐path length L, then take the maximum.

**Examples:**

1. **Chain distance:**

   ```prolog
   ?- eccentricity(1, [1,2,3,4], [[1,2],[2,3],[3,4]], E).
   E = 3.
   ```

   *Explanation:* Farthest from 1 is 4, distance 3.

2. **Single vertex has eccentricity 0:**

   ```prolog
   ?- eccentricity(a, [a], [], E).
   E = 0.
   ```

   *Explanation:* No other vertices.

3. **Isolated vertex among others:**

   ```prolog
   ?- eccentricity(5, [5,6,7], [[6,7]], E).
   E = 0.
   ```

   *Explanation:* 5 can only reach itself.

---

#### 9.1.19 diameter/3

```prolog
%% diameter(+Vertices, +Edges, -D) is det.
%% D is the maximum eccentricity over all vertices,
%% i.e., the longest shortest‐path in the graph.
diameter(Vertices, Edges, D) :-
    findall(E,
            ( member(X, Vertices),
              eccentricity(X, Vertices, Edges, E)
            ),
            Es),
    max_list(Es, D).
```

**Description:**
The diameter is the greatest distance between any pair of vertices in the graph.

**Examples:**

1. **Chain of 4:**

   ```prolog
   ?- diameter([1,2,3,4], [[1,2],[2,3],[3,4]], D).
   D = 3.
   ```

   *Explanation:* Farthest pair is 1–4 at distance 3.

2. **Triangle has diameter 1:**

   ```prolog
   ?- diameter([1,2,3], [[1,2],[2,3],[3,1]], D).
   D = 1.
   ```

   *Explanation:* All vertices are neighbors.

3. **Disconnected graph:**

   ```prolog
   ?- diameter([a,b,c], [[a,b]], D).
   D = 1.
   ```

   *Explanation:* We only measure within components; isolated c has eccentricity 0, so max is 1.

---

#### 9.1.20 radius/3

```prolog
%% radius(+Vertices, +Edges, -R) is det.
%% R is the minimum eccentricity among all vertices,
%% i.e., the distance from a “best” central vertex.
radius(Vertices, Edges, R) :-
    findall(E,
            ( member(X, Vertices),
              eccentricity(X, Vertices, Edges, E)
            ),
            Es),
    min_list(Es, R).
```

**Description:**
The radius is the smallest maximum distance from any vertex to all others—i.e., how far from the “center” of the graph.

**Examples:**

1. **Chain of 5:**

   ```prolog
   ?- radius([1,2,3,4,5], [[1,2],[2,3],[3,4],[4,5]], R).
   R = 2.
   ```

   *Explanation:* Vertices 2 or 3 minimize the farthest distance (2 edges).

2. **Star shape:**

   ```prolog
   ?- radius([c,a,b,d], [[c,a],[c,b],[c,d]], R).
   R = 1.
   ```

   *Explanation:* Center c has eccentricity 1.

3. **Disconnected graph:**

   ```prolog
   ?- radius([x,y,z], [[x,y]], R).
   R = 0.
   ```

   *Explanation:* The isolated z has eccentricity 0, which is the minimum.


---

### 9.2 Directed Graphs

For a directed graph, each edge is an **ordered** pair `[U,V]` meaning there is an arc from `U` to `V`. We will mirror the ten predicates from **9.1** but interpret edges as one‐way.

---

#### 9.2.1 `edge_dir/3`

```prolog
%% edge_dir(+U, +V, +Arcs) is semidet.
%% True if there is a directed edge (arc) from U to V in the list Arcs.
edge_dir(U, V, Arcs) :-
    member([U, V], Arcs).
```

**Description:**
Succeeds exactly when the ordered pair `[U,V]` appears in `Arcs`.  Unlike the undirected `edge/3`, there is no `[V,U]` alternative.

**Examples:**

1. **Arc exists in forward direction:**

   ```prolog
   ?- edge_dir(a, b, [[a,b],[b,c],[c,a]]).
   true.
   ```

   *Explanation:*
   `[a,b]` is present exactly as given.

2. **Reverse does not count:**

   ```prolog
   ?- edge_dir(b, a, [[a,b],[b,c],[c,a]]).
   false.
   ```

   *Explanation:*
   Even though `a→b` is in the graph, there is no `b→a` arc.

3. **No such arc:**

   ```prolog
   ?- edge_dir(x, y, [[a,b],[b,c]]).
   false.
   ```

   *Explanation:*
   That ordered pair does not appear.

---

#### 9.2.2 `adjacent_dir/3`

```prolog
%% adjacent_dir(+U, -V, +Arcs) is nondet.
%% On backtracking, V is each vertex such that there is an arc U→V in Arcs.
adjacent_dir(U, V, Arcs) :-
    edge_dir(U, V, Arcs).
```

**Description:**
Enumerates all *successors* `V` of a given vertex `U`.  Each backtracking step yields a different `V`.

**Examples:**

1. **Multiple successors:**

   ```prolog
   ?- adjacent_dir(p, Q, [[p,q],[p,r],[x,p],[q,p]]).
   Q = q ;
   Q = r ;
   false.
   ```

   *Explanation:*
   Only `[p,q]` and `[p,r]` count because the graph is directed; `[x,p]` and `[q,p]` are incoming arcs, not successors of `p`.

2. **Single successor:**

   ```prolog
   ?- adjacent_dir(3, N, [[1,3],[3,2],[3,4]]).
   N = 2 ;
   N = 4 ;
   false.
   ```

   *Explanation:*
   There are two outgoing arcs from `3`: `[3,2]` and `[3,4]`.

3. **No outgoing arcs:**

   ```prolog
   ?- adjacent_dir(a, V, [[b,c],[c,d]]).
   false.
   ```

   *Explanation:*
   Vertex `a` has no outgoing arcs in the list.

---

#### 9.2.3 `neighbors_out/3`

```prolog
%% neighbors_out(+U, +Arcs, -List) is det.
%% List is the sorted set of all successors of U (no duplicates).
neighbors_out(U, Arcs, Ns) :-
    findall(V, adjacent_dir(U, V, Arcs), Vs),
    sort(Vs, Ns).
```

**Description:**
Collects every vertex `V` for which there is an arc `U→V`.  Results are sorted and duplicate‐free.

**Examples:**

1. **Typical case with duplicates in Arcs:**

   ```prolog
   ?- neighbors_out(x, [[x,y],[x,z],[x,y],[w,x]], Ns).
   Ns = [y,z].
   ```

   *Explanation:*
   Even though `[x,y]` appears twice, `sort/2` removes duplicates.

2. **No outgoing arcs:**

   ```prolog
   ?- neighbors_out(5, [[1,2],[2,3],[3,4]], Ns).
   Ns = [].
   ```

   *Explanation:*
   `5` is not the source of any arc.

3. **Single successor:**

   ```prolog
   ?- neighbors_out(node, [[node,foo],[bar,node]], Ns).
   Ns = [foo].
   ```

   *Explanation:*
   Only `[node,foo]` counts; `[bar,node]` is incoming and ignored.

---

#### 9.2.4 `out_degree/3`

```prolog
%% out_degree(+U, +Arcs, -D) is det.
%% D is the number of outgoing arcs from vertex U.
out_degree(U, Arcs, D) :-
    neighbors_out(U, Arcs, Ns),
    length(Ns, D).
```

**Description:**
Computes how many arcs leave `U`.  Equivalent to the length of the successor list.

**Examples:**

1. **Multiple outgoing:**

   ```prolog
   ?- out_degree(a, [[a,b],[a,c],[c,a],[a,b]], D).
   D = 2.
   ```

   *Explanation:*
   Unique successors of `a` are `[b,c]`; duplicates are removed.

2. **Zero outgoing:**

   ```prolog
   ?- out_degree(z, [[x,y],[y,z]], D).
   D = 0.
   ```

   *Explanation:*
   No `[z, _]` arcs, so out‐degree is 0.

3. **Single outgoing:**

   ```prolog
   ?- out_degree(m, [[m,n],[p,m]], D).
   D = 1.
   ```

   *Explanation:*
   Only `[m,n]` counts; `[p,m]` is incoming.

---

#### 9.2.5 `simple_path_dir/5`

```prolog
%% simple_path_dir(+Start, +Goal, +Arcs, +Visited, -Path) is nondet.
%% Path is a simple directed path from Start to Goal: [Start,…,Goal],
%% never revisiting any vertex (except possibly if Start = Goal, trivial).
simple_path_dir(Goal, Goal, _Arcs, _Visited, [Goal]) :- !.
simple_path_dir(U, Goal, Arcs, Visited, [U|Rest]) :-
    \+ member(U, Visited),
    adjacent_dir(U, Next, Arcs),
    \+ member(Next, Visited),
    simple_path_dir(Next, Goal, Arcs, [U|Visited], Rest).
```

**Description:**
Finds an acyclic (simple) directed path following only the direction of arcs.  Each solution is one such path; on backtracking, Prolog will attempt alternative routes.

**Examples:**

1. **Two distinct directed paths:**

   ```prolog
   ?- Arcs = [[a,b],[b,c],[a,d],[d,c]],
      simple_path_dir(a, c, Arcs, [], P).
   P = [a,b,c] ;
   P = [a,d,c] ;
   false.
   ```

   *Explanation:*
   Both `a→b→c` and `a→d→c` respect arc direction.

2. **No directed path due to breaking direction:**

   ```prolog
   ?- simple_path_dir(1, 4, [[1,2],[3,2],[2,3]], [], P).
   false.
   ```

   *Explanation:*
   Although there is an undirected connection 1–2–3–2–… never reaches `4`, and direction prevents any path to `4`.

3. **Trivial path if Start = Goal:**

   ```prolog
   ?- simple_path_dir(x, x, [[x,y],[y,z]], [], P).
   P = [x].
   ```

   *Explanation:*
   A zero‐length path is allowed: `[x]`.

---

#### 9.2.6 `path_length_dir/5`

```prolog
%% path_length_dir(+U, +V, +Arcs, +Visited, -Len) is nondet.
%% Len is the number of arcs in a simple directed path from U to V.
path_length_dir(U, V, Arcs, Visited, Len) :-
    simple_path_dir(U, V, Arcs, Visited, Path),
    length(Path, L),          % L = number of vertices in the path
    Len is L - 1.             % number of arcs = vertices - 1
```

**Description:**
Every solution yields a different directed path (of possibly different lengths).  You can collect or compare lengths.

**Examples:**

1. **Enumerate two lengths:**

   ```prolog
   ?- Arcs = [[s,a],[a,b],[b,t],[s,c],[c,t]],
      path_length_dir(s, t, Arcs, [], L).
   L = 3 ;
   L = 2 ;
   false.
   ```

   *Explanation:*
   Paths are `s→a→b→t` (3 arcs) and `s→c→t` (2 arcs).

2. **Start = Goal (length 0):**

   ```prolog
   ?- path_length_dir(p, p, [[p,q]], [], L).
   L = 0.
   ```

   *Explanation:*
   Single‐vertex path `[p]` has 0 arcs.

3. **No path:**

   ```prolog
   ?- path_length_dir(a, d, [[a,b],[c,d]], [], L).
   false.
   ```

   *Explanation:*
   `d` is unreachable from `a` under directed arcs.

---

#### 9.2.7 `reachable_dir/3`

```prolog
%% reachable_dir(+Frontier, +Arcs, -AllReached) is det.
%% Starting from Frontier list of vertices, collects every vertex reachable
%% via directed arcs in Arcs. Result may include duplicates; caller can sort.
reachable_dir([], _Arcs, []).
reachable_dir([U|Us], Arcs, Reachable) :-
    reachable_dir(Us, Arcs, R1),
    (   member(U, R1)
    ->  Reachable = R1
    ;   neighbors_out(U, Arcs, Ns),
        append(Ns, R1, Temp),
        Reachable = [U|Temp]
    ).
```

**Description:**
Performs a graph traversal respecting direction (similar to a BFS/DFS).  Returns all vertices reachable from any in the initial `Frontier`.

**Examples:**

1. **Simple reachability:**

   ```prolog
   ?- reachable_dir([1], [[1,2],[2,3],[2,4],[5,1]], Rs),
      sort(Rs, Sorted).
   Rs = [1, 2, 3, 4],   % unsorted but including duplicates
   Sorted = [1,2,3,4].
   ```

   *Explanation:*
   From `1` you can visit `2`, then `3` and `4`.  The arc `[5,1]` is irrelevant since it points into `1`.

2. **Multi‐source frontier:**

   ```prolog
   ?- reachable_dir([2,5], [[1,2],[2,3],[3,4],[5,6],[6,7]], Rs),
      sort(Rs, Sorted).
   Rs = [2,3,4,5,6,7],
   Sorted = [2,3,4,5,6,7].
   ```

   *Explanation:*
   Starting from `2` gives `{2,3,4}`, from `5` gives `{5,6,7}`.  Combined as `[2,3,4,5,6,7]`.

3. **No outgoing arcs:**

   ```prolog
   ?- reachable_dir([x], [[a,b],[b,c]], Rs).
   Rs = [x].
   ```

   *Explanation:*
   `x` has no outgoing arcs, so only `x` is reachable.

---

#### 9.2.8 `connected_dir/2` (Strong Connectivity)

```prolog
%% connected_dir(+Vertices, +Arcs) is semidet.
%% True if for every pair of distinct vertices U and V in Vertices,
%% U can reach V and V can reach U (i.e. strongly connected).
connected_dir(Vertices, Arcs) :-
    forall(
      ( member(U, Vertices), member(V, Vertices), U \= V ),
      ( reachable_dir([U], Arcs, ReachUV),
        member(V, ReachUV),
        reachable_dir([V], Arcs, ReachVU),
        member(U, ReachVU)
      )
    ).
```

**Description:**
Checks strong connectivity: for each pair `(U,V)`, there must be a directed path `U→…→V` and also `V→…→U`.  Uses `reachable_dir/3`.

**Examples:**

1. **Strongly connected cycle:**

   ```prolog
   ?- connected_dir([1,2,3], [[1,2],[2,3],[3,1]], Yes).
   Yes = true.
   ```

   *Explanation:*
   Each can reach each via the cycle.

2. **Not strongly connected (missing back‐edge):**

   ```prolog
   ?- connected_dir([a,b,c], [[a,b],[b,c],[c,a]], true).
   true.
   ?- connected_dir([a,b,c], [[a,b],[b,c]], _).
   false.
   ```

   *Explanation:*
   In the second graph, you cannot go back from `c` to `a`.

3. **Single vertex trivially strongly connected:**

   ```prolog
   ?- connected_dir([x], [], true).
   true.
   ```

   *Explanation:*
   A single vertex is always (vacuously) strongly connected to itself.

---

#### 9.2.9 `component_dir/3` (Strongly Connected Component)

```prolog
%% component_dir(+U, +Arcs, -Comp) is det.
%% Comp is the list of all vertices V such that U can reach V and V can reach U.
component_dir(U, Arcs, CompSorted) :-
    reachable_dir([U], Arcs, Reach1),
    findall(W,
        ( member(W, Reach1),
          reachable_dir([W], Arcs, ReachW),
          member(U, ReachW)
        ),
        Comp),
    sort(Comp, CompSorted).
```

**Description:**
Returns the maximal strongly connected component containing `U`.  First finds all vertices reachable from `U`, then filters to those from which `U` is also reachable.

**Examples:**

1. **Simple three‐node SCC:**

   ```prolog
   ?- component_dir(a, [[a,b],[b,c],[c,a],[c,d]], Comp).
   Comp = [a,b,c].
   ```

   *Explanation:*
   Nodes `a,b,c` form a cycle; `d` is not included because `d` cannot reach `a`.

2. **Isolated vertex yields singleton:**

   ```prolog
   ?- component_dir(x, [[y,z],[z,y]], C).
   C = [x].
   ```

   *Explanation:*
   `x` is not in any arc, so only `x` itself is in its SCC.

3. **Two disjoint strong components:**

   ```prolog
   ?- Arcs = [[1,2],[2,1],[3,4],[4,3],[2,3]],
      component_dir(2, Arcs, C1),
      component_dir(3, Arcs, C2).
   C1 = [1,2],
   C2 = [3,4].
   ```

   *Explanation:*
   There is a linkage `2→3`, but no path back, so `1,2` remain their own SCC and `3,4` form theirs.

---

#### 9.2.10 `has_cycle_dir/2`

```prolog
%% has_cycle_dir(+Vertices, +Arcs) is semidet.
%% True if there is at least one directed cycle in the graph.
has_cycle_dir(Vertices, Arcs) :-
    member(U, Vertices),
    cycle_dir(U, U, Arcs, [], _).

%% cycle_dir(+Start, +Curr, +Arcs, +Visited, -Cycle)
%% Finds a directed cycle starting and ending at Start, length ≥ 1.
cycle_dir(Start, Curr, Arcs, Visited, [Start,Curr]) :-
    edge_dir(Curr, Start, Arcs),
    \+ member(Start, Visited),
    !.
cycle_dir(Start, Curr, Arcs, Visited, [Curr|Rest]) :-
    \+ member(Curr, Visited),
    edge_dir(Curr, Next, Arcs),
    Next \= Start,
    cycle_dir(Start, Next, Arcs, [Curr|Visited], Rest).
```

**Description:**
Searches for any directed cycle in which you can leave `U` and eventually return to `U`.  It avoids revisiting other vertices by tracking `Visited`.  On success, it returns one example cycle.

**Examples:**

1. **Simple 2-node cycle:**

   ```prolog
   ?- has_cycle_dir([u,v], [[u,v],[v,u],[v,w]]).
   true.
   ```

   *Explanation:*
   The cycle `u→v→u` exists.

2. **Longer directed cycle:**

   ```prolog
   ?- has_cycle_dir([1,2,3,4], [[1,2],[2,3],[3,4],[4,1],[2,4]]).
   true.
   ```

   *Explanation:*
   One cycle is `1→2→3→4→1`.

3. **No cycle (acyclic DAG):**

   ```prolog
   ?- has_cycle_dir([a,b,c], [[a,b],[b,c]], false).
   false.
   ```

   *Explanation:*
   A simple directed path: no way to return to the start.

---

#### 9.2.11 `shortest_path_dir/5`

```prolog
%% shortest_path_dir(+Start, +Goal, +Arcs, -Path, -Dist) is semidet.
%% Finds a shortest directed path Path = [Start,…,Goal], Dist = number of arcs.
shortest_path_dir(Start, Goal, Arcs, Path, Dist) :-
    bfs_dir([[Start,[Start]]], Goal, Arcs, [Start], PathRev),
    reverse(PathRev, Path),
    length(Path, L), Dist is L - 1.

%% bfs_dir(+Queue, +Goal, +Arcs, +Visited, -ResultPath)
bfs_dir([[Goal, Path]|_], Goal, _Arcs, _Visited, Path).
bfs_dir([[Curr, PathSoFar]|Queue], Goal, Arcs, Visited, Result) :-
    Curr \= Goal,
    findall(
      [Next, [Next|PathSoFar]],
      ( edge_dir(Curr, Next, Arcs),
        \+ member(Next, Visited)
      ),
      NextPairs
    ),
    extract_vertices(NextPairs, NewVs),
    append(Visited, NewVs, Visited2),
    append(Queue, NextPairs, NewQueue),
    bfs_dir(NewQueue, Goal, Arcs, Visited2, Result).

extract_vertices([], []).
extract_vertices([[V,_]|Rest], [V|Vs]) :-
    extract_vertices(Rest, Vs).
```

**Description:**
A BFS over directed arcs that stops as soon as `Goal` is dequeued.  Because BFS explores by layers of increasing depth, the first time you dequeue `Goal`, you have found a shortest (fewest‐arcs) path.

**Examples:**

1. **Unique shortest directed path:**

   ```prolog
   ?- Arcs = [[s,a],[a,t],[s,b],[b,t]],
      shortest_path_dir(s, t, Arcs, P, D).
   P = [s, a, t],
   D = 2.
   ```

   *Explanation:*
   Both `s→a→t` and `s→b→t` have length 2, but BFS enqueues `[s,a]` before `[s,b]`, so returns `s→a→t`.

2. **Longer graph with detours:**

   ```prolog
   ?- Arcs = [[1,2],[2,4],[1,3],[3,5],[5,4]],
      shortest_path_dir(1, 4, Arcs, P, D).
   P = [1,2,4],
   D = 2.
   ```

   *Explanation:*
   Although `1→3→5→4` is a valid path (3 arcs), `1→2→4` (2 arcs) is shorter.

3. **No path:**

   ```prolog
   ?- shortest_path_dir(a, d, [[a,b],[c,d]], P, D).
   false.
   ```

   *Explanation:*
   `d` cannot be reached from `a` under directed arcs.

---

#### 9.2.11 `in_degree/3`

```prolog
%% in_degree(+U, +Arcs, -D) is det.
%% D is the number of incoming arcs to vertex U in the directed graph.
in_degree(U, Arcs, D) :-
    findall(X, member([X, U], Arcs), Preds),
    length(Preds, D).
```

**Description:**
Counts how many arcs end at `U`.  Each `[X,U]` in `Arcs` contributes one to the in-degree.

**Examples:**

1. **Simple count:**

   ```prolog
   ?- in_degree(a, [[x,a],[b,a],[a,c]], D).
   D = 2.
   ```

   *Explanation:*
   Only `[x,a]` and `[b,a]` point into `a`.

2. **Zero in-degree (a source):**

   ```prolog
   ?- in_degree(s, [[s,t],[t,u]], D).
   D = 0.
   ```

   *Explanation:*
   `s` has no incoming arcs.

3. **Self-loop counts as incoming:**

   ```prolog
   ?- in_degree(v, [[v,v],[u,v]], D).
   D = 2.
   ```

   *Explanation:*
   Both `[v,v]` and `[u,v]` contribute.

---

#### 9.2.12 `in_degree_sequence/3`

```prolog
%% in_degree_sequence(+Vertices, +Arcs, -Seq) is det.
%% Seq is the descending sorted list of in-degrees of all vertices.
in_degree_sequence(Vertices, Arcs, Seq) :-
    findall(D, (member(V, Vertices), in_degree(V, Arcs, D)), Degrees),
    sort(Degrees, SortedAsc),
    reverse(SortedAsc, Seq).
```

**Description:**
Computes each vertex’s in-degree and returns the list of those values sorted in descending order.

**Examples:**

1. **Typical case:**

   ```prolog
   ?- in_degree_sequence([a,b,c], [[a,b],[c,b],[a,c]], Seq).
   Seq = [2,1,0].
   ```

   *Explanation:*
   `b` has in-degree 2, `c` has 1, `a` has 0.

2. **All zero:**

   ```prolog
   ?- in_degree_sequence([x,y,z], [], Seq).
   Seq = [0,0,0].
   ```

   *Explanation:*
   No arcs, so every in-degree is 0.

3. **Duplicates removed?**

   ```prolog
   ?- in_degree_sequence([u,v,w], [[u,v],[u,v],[v,w]], Seq).
   Seq = [2,1,0].
   ```

   *Explanation:*
   Although `[u,v]` appears twice, we count both for `v`’s in-degree (2), then sort.

---

#### 9.2.13 `sources/3`

```prolog
%% sources(+Vertices, +Arcs, -Sources) is det.
%% Sources is the list of all vertices with in-degree 0.
sources(Vertices, Arcs, Sources) :-
    include(\V^( \+ member([_,V], Arcs) ), Vertices, Sources).
```

**Description:**
A “source” is a vertex with no incoming arcs.  This collects all such vertices.

**Examples:**

1. **Multiple sources:**

   ```prolog
   ?- sources([a,b,c,d], [[a,c],[b,c],[c,d]], S).
   S = [a,b].
   ```

   *Explanation:*
   Neither `a` nor `b` has arcs into them.

2. **Single source:**

   ```prolog
   ?- sources([1,2,3], [[1,2],[2,3]], S).
   S = [1].
   ```

   *Explanation:*
   Only `1` has no incoming arc.

3. **No sources (every vertex has in-degree ≥1):**

   ```prolog
   ?- sources([x,y], [[x,y],[y,x]], S).
   S = [].
   ```

   *Explanation:*
   Both have incoming arcs.

---

#### 9.2.14 `sinks/3`

```prolog
%% sinks(+Vertices, +Arcs, -Sinks) is det.
%% Sinks is the list of all vertices with out-degree 0.
sinks(Vertices, Arcs, Sinks) :-
    include(\V^( \+ member([V,_], Arcs) ), Vertices, Sinks).
```

**Description:**
A “sink” is a vertex with no outgoing arcs.  This collects all such vertices.

**Examples:**

1. **Multiple sinks:**

   ```prolog
   ?- sinks([a,b,c,d], [[a,b],[c,d]], S).
   S = [b,d].
   ```

   *Explanation:*
   `b` and `d` have no arcs leaving.

2. **Single sink:**

   ```prolog
   ?- sinks([1,2,3], [[1,2],[2,3]], S).
   S = [3].
   ```

   *Explanation:*
   Only `3` has out-degree 0.

3. **No sinks:**

   ```prolog
   ?- sinks([x,y], [[x,y],[y,x]], S).
   S = [].
   ```

   *Explanation:*
   Both have outgoing arcs.

---

#### 9.2.15 `remove_vertex_dir/5`

```prolog
%% remove_vertex_dir(+V, +Vertices, +Arcs, -NewVertices, -NewArcs) is det.
%% Deletes vertex V and all arcs incident to V.
remove_vertex_dir(V, Vertices, Arcs, NewVs, NewAs) :-
    select(V, Vertices, NewVs),
    exclude(\A^memberchk(V,A), Arcs, NewAs).
```

**Description:**
Removes `V` from the vertex list and filters out any arc `[V,_]` or `[_ ,V]`.

**Examples:**

1. **Basic removal:**

   ```prolog
   ?- remove_vertex_dir(b, [a,b,c], [[a,b],[b,c],[c,a]], Vs, As).
   Vs = [a,c],
   As = [[c,a]].
   ```

   *Explanation:*
   Both arcs involving `b` are gone.

2. **No such vertex:**

   ```prolog
   ?- remove_vertex_dir(x, [a,b], [[a,b]], Vs, As).
   false.
   ```

   *Explanation:*
   `x` is not in the vertex list.

3. **Isolated vertex:**

   ```prolog
   ?- remove_vertex_dir(z, [x,y,z], [[x,y]], Vs, As).
   Vs = [x,y],
   As = [[x,y]].
   ```

   *Explanation:*
   `z` had no arcs, so `As` stays the same except `z` removed.

---

#### 9.2.16 `induced_subgraph_dir/4`

```prolog
%% induced_subgraph_dir(+VertSubset, +Vertices, +Arcs, -SubArcs) is det.
%% SubArcs are those arcs [U,V] with both U and V in VertSubset.
induced_subgraph_dir(Vs, _AllVs, Arcs, SubArcs) :-
    include(\[U,V]^( member(U,Vs), member(V,Vs) ), Arcs, SubArcs).
```

**Description:**
Extracts the subgraph on `VertSubset` by keeping only arcs whose both endpoints lie in that subset.

**Examples:**

1. **Proper subgraph:**

   ```prolog
   ?- induced_subgraph_dir([a,b], [a,b,c], [[a,b],[b,c],[c,a]], SA).
   SA = [[a,b]].
   ```

   *Explanation:*
   Only `[a,b]` lies fully inside `{a,b}`.

2. **Empty result:**

   ```prolog
   ?- induced_subgraph_dir([x], [x,y], [[x,y],[y,x]], SA).
   SA = [].
   ```

   *Explanation:*
   No `[x,x]` exists.

3. **All inside:**

   ```prolog
   ?- induced_subgraph_dir([1,2], [1,2], [[1,2],[2,1]], SA).
   SA = [[1,2],[2,1]].
   ```

   *Explanation:*
   Both arcs stay.

---

#### 9.2.17 `reachable_closure/3`

```prolog
%% reachable_closure(+Vertices, +Arcs, -Closure) is det.
%% Closure is the transitive-closure: all pairs [U,V] where V is reachable from U.
reachable_closure(Vertices, Arcs, Closure) :-
    findall([U,W],
        ( member(U,Vertices),
          reachable_dir([U], Arcs, Rs),
          member(W,Rs),
          U \= W
        ),
        Pairs),
    sort(Pairs, Closure).
```

**Description:**
Builds the relation of reachability: for each `U`, collects every `W` reachable via directed paths.

**Examples:**

1. **Simple chain:**

   ```prolog
   ?- reachable_closure([1,2,3], [[1,2],[2,3]], C).
   C = [[1,2],[1,3],[2,3]].
   ```

   *Explanation:*
   From `1` you reach `2` and then `3`; from `2` you reach `3`.

2. **Cycle yields self-pairs:**

   ```prolog
   ?- reachable_closure([a,b], [[a,b],[b,a]], C).
   C = [[a,b],[a,a],[b,a],[b,b]].
   ```

   *Explanation:*
   Each enters a cycle so can reach itself.

3. **Disconnected:**

   ```prolog
   ?- reachable_closure([x,y], [], C).
   C = [].
   ```

   *Explanation:*
   No arcs, so no reachabilities except trivial, which we omit `U=U`.

---

#### 9.2.18 `is_dag/2`

```prolog
%% is_dag(+Vertices, +Arcs) is semidet.
%% True if the directed graph has no directed cycles.
is_dag(Vertices, Arcs) :-
    \+ has_cycle_dir(Vertices, Arcs).
```

**Description:**
A digraph is acyclic (a DAG) precisely if it has no directed cycle.

**Examples:**

1. **Acyclic:**

   ```prolog
   ?- is_dag([a,b,c], [[a,b],[b,c]]).
   true.
   ```
2. **Self-loop fails:**

   ```prolog
   ?- is_dag([x], [[x,x]]).
   false.
   ```
3. **Multi-node cycle fails:**

   ```prolog
   ?- is_dag([1,2,3], [[1,2],[2,3],[3,1]]).
   false.
   ```

---

#### 9.2.19 `condensation/3`

```prolog
%% condensation(+Vertices, +Arcs, -cond(Comps, CondArcs)) is det.
%% Builds the condensation graph of strongly connected components.
condensation(Vs, Arcs, cond(Comps, CondArcs)) :-
  % 1. Find all SCCs
  strongly_connected_components(Vs, Arcs, Comps),
  % 2. Map each vertex to its component index
  assign_comp_indices(Comps, 1, [], Assigns),
  % 3. Build arcs between different components
  findall([Ci,Cj],
    ( member([U,V], Arcs),
      member(U-Ci, Assigns),
      member(V-Cj, Assigns),
      Ci \= Cj
    ),
    Raw),
  sort(Raw, CondArcs).

%% (Helper predicates `strongly_connected_components/3` and `assign_comp_indices/4`
%%  are as given in 9.2.19 above.)
```

**Description:**
Contracts each strongly connected component into a single node, then creates a DAG of these components by adding an arc from component Ci to Cj whenever there is an original arc from any `U∈Ci` to any `V∈Cj`.

**Examples:**

1. **Two SCCs:**

   ```prolog
   ?- Vs=[1,2,3,4], Arcs=[[1,2],[2,1],[3,4]], 
      condensation(Vs,Arcs, C).
   C = cond([[1,2],[3,4]], [[1,2]]).
   ```

   *Explanation:*
   Components are `[1,2]` and `[3,4]`; there is an arc from comp 1→2.

2. **Single big SCC:**

   ```prolog
   ?- condensation([a,b], [[a,b],[b,a]], C).
   C = cond([[a,b]], []).
   ```

   *Explanation:*
   Only one component; no inter-component arcs.

3. **Chain of SCCs:**

   ```prolog
   ?- condensation([u,v,w], [[u,v],[v,u],[v,w]], C).
   C = cond([[u,v],[w]], [[1,2]]).
   ```

   *Explanation:*
   `[u,v]` is comp 1, `[w]` is comp 2, and there is an arc 1→2.

---

#### 9.2.20 `topological_sort/3`

```prolog
%% topological_sort(+Vertices, +Arcs, -Order) is semidet.
%% If the digraph is acyclic, Order is a list of all vertices in topological order.
topological_sort(Vs, Arcs, Order) :-
  % 1. Compute in-degrees
  maplist({Arcs}/[V-D]>>in_degree(V,Arcs,D), Vs, Pairs),
  dict_create(Dict, indeg, Pairs),
  % 2. Initialize queue of zero in-degree vertices
  findall(V, (member(V,Vs), get_dict(V,Dict,0)), Q0),
  % 3. Process via Kahn’s algorithm
  ts_process(Q0, Arcs, Dict, [], Rev),
  length(Rev,N), length(Vs,N),  % must use all vertices
  reverse(Rev, Order).

ts_process([], _A, _D, Acc, Acc).
ts_process([V|Q], Arcs, D0, Acc0, Acc) :-
  append(Acc0, [V], Acc1),
  findall(W, edge_dir(V,W,Arcs), Succ),
  foldl({}/[W,Di,Do,Zs]>>(
    get_dict(W,Di,D), D1 is D-1, put_dict(W,Di,D1,Do),
    (D1=:=0 -> Zs=[W] ; Zs=[])
  ), Succ, D0, D1, NewQs),
  append(Q,NewQs,Q1),
  ts_process(Q1,Arcs,D1,Acc1,Acc).
```

**Description:**
Implements Kahn’s topological-sort: repeatedly remove a source (in-degree 0), append it to the order, decrement its successors’ in-degrees, enqueue any that become zero.  Fails if the graph has a cycle (i.e. you can’t process all vertices).

**Examples:**

1. **Simple DAG:**

   ```prolog
   ?- topological_sort([a,b,c], [[a,b],[b,c]], O).
   O = [a,b,c].
   ```
2. **Two possible orders, returns one:**

   ```prolog
   ?- topological_sort([1,2,3], [[1,3],[2,3]], O).
   O = [1,2,3] ;
   false.
   ```
3. **Cycle fails:**

   ```prolog
   ?- topological_sort([x,y], [[x,y],[y,x]], _).
   false.
   ```

   *Explanation:*
   The mutual cycle prevents any linear ordering.


---


