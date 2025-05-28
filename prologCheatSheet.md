Prolog Cheat Sheet

A comprehensive reference for Prolog concepts—from basic syntax through advanced list processing and SAT/CLP optimizations. Use this as your quick lookup guide when you need to recall definitions, syntax, or usage tips.

Table of Contents

Prolog Basic Syntax
1.1. Basic Prolog (Facts, Rules, Variables, Predicates, Comments, Control Constructs)
1.2. Debugging with write and writeln 1.3. If-Then-Else in Prolog

Operators
2.1. Arithmetic Operators
2.2. Comparison Operators
2.3. Logical Operators
2.4. Unification and Equality Operators
2.5. Arithmetic Evaluation (is)
2.6. Operator Precedence & Associativity
2.7. Logical Implications (:-)

Lists
3.1. List Basics
3.2. Head and Tail Decomposition
3.3. Common List Predicates
    3.3.1. member/2 and memberchk/2
    3.3.2. append/3
    3.3.3. length/2
    3.3.4. nth1/3
    3.3.5. findall/3
    3.3.6. select/3
    3.3.7. reverse/2
    3.3.8. last/2
    3.3.9. delete/3
    3.3.10. union/3 and intersection/3
3.4. Advanced List Predicates
    3.4.1. sort/2 and msort/2
    3.4.2. Custom predicates: unique/2 and intersect/3
3.5. Haskell-like List Functions
    3.5.1. take/3 and drop/3
    3.5.2. takewhile/3 and dropwhile/3
    3.5.3. split_at/4
    3.5.4. zip/3
3.6. Recursive List Processing
    3.6.1. Sum of Elements
    3.6.2. Maximum Element
    3.6.3. Minimum Element
    3.6.4. Product of Elements
    3.6.5. Average of Elements
3.7. Haskell-like Higher-Order Functions
    3.7.1. map/3
    3.7.2. filter/3
    3.7.3. foldl/4 and foldr/4
    3.7.4. scanl/4 and scanr/4
    3.7.5. all/2 and any/2
3.8. Typical List-Based Examples
    3.8.1. Factorial
    3.8.2. Fibonacci
    3.8.3. Reverse a List
    3.8.4. Palindrome Check
    3.8.5. Merge Sort
    3.8.6. Flatten a Nested List
    3.8.7. Sum of Elements
    3.8.8. Product of Elements
    3.8.9. List Length (Recursive)
    3.8.10. Count Occurrences
    3.8.11. Maximum Element (Recursive)
    3.8.12. Minimum Element (Recursive)
    3.8.13. Check if a List is Sorted
    3.8.14. Quick Sort
    3.8.15. Reverse a List with an Accumulator
    3.8.16. Concatenate a List of Lists
    3.8.17. Interleave Two Lists
    3.8.18. Sublist Check (Contiguous)
    3.8.19. Permutations of a List
    3.8.20. Zip Three Lists
3.9. Creating Infinite and Limited Lists
    3.9.1. Finite Arithmetic Progressions
        3.9.1.A. Fixed Number of Elements
        3.9.1.B. Bounded by a Maximum Value
    3.9.2. Infinite Arithmetic Progressions (Generators)
    3.9.3. Practical Scenarios and Combined Usage

Advanced Predicates
4.1. Using findall/3 for Collecting Solutions
4.2. SAT & Constraint Encodings
    4.2.1. expressOr/2 and expressAnd/2
    4.2.2. Cardinality Constraints: atLeast/2, atMost/2, exactly/2
4.3. Clause Generation with writeOneClause/1
4.4. Higher-Order Predicates: maplist/2, maplist/3, include/3, exclude/3
4.5. Defining Custom Predicates
4.6. Recursion Techniques
4.7. Error Handling and Guards
4.8. Generator Predicates
    4.8.1. between/3
    4.8.2. repeat/0
    4.8.3. bagof/3
    4.8.4. setof/3
    4.8.5. succ/2

Extra Self-Declarated Predicates
5.1. inside/2
5.2. prefix/2
5.3. suffix/2
5.4. rotate_left/2
5.5. rotate_right/2
5.6. remove_duplicates/2
5.7. substitute/4
5.8. replace_at/4
5.9. nth0/3
5.10. duplicate/3

1. Prolog Basic Syntax

<a id="prolog-basic-syntax"></a>

1.1 Basic Prolog (Facts, Rules, Variables, Predicates, Comments, Control Constructs)

<a id="basic-prolog-facts-rules-variables-predicates-comments-control-constructs"></a>

Facts
Definition: Simple assertions about the world.
Syntax:

gangster(g01).
notAvailable(g01, [6,13,14,16,21,35,37,41,59]).


Rules
Definition: Implications that derive new information from facts.
Syntax:

available(G, H) :- hour(H), gangster(G), \+ blocked(G, H).


Variables
Naming: Begin with an uppercase letter or underscore.
Examples:

G, H, Cost, _Temp, X1, Y_variable.


Predicates
Definition: Relations defined by facts and rules.
Example:

task(T).
needed(T, H, N).


Comments
Single-line: Use %

% This is a comment.
gangster(g01).  % Declares gangster g01


Multi-line: Use /* ... */

/*
  This is a multi-line comment.
  It spans several lines.
*/


Control Constructs
Cut (!): Prunes the search tree (prevents backtracking).

max(X, Y, X) :- X >= Y, !.
max(X, Y, Y) :- X < Y.


Failure (fail): Forces a predicate to fail (often used in iterative printing).

print_all_fail(List) :-
    member(X, List),
    write(X), nl,
    fail.
print_all_fail(_).


1.2 Debugging with write and writeln

<a id="debugging-with-write-writeln"></a>

When writing Prolog code, it’s often helpful to display the values of variables during execution to understand what’s going on. Prolog provides built-in predicates such as write/1, writeln/1, and format/2 to print information to the console.

Below are some common scenarios demonstrating how to use write and writeln:

Example 1: Display a Single Variable Value

rotate(L, K, T) :-
    length(L, N),
    Aux is K mod N,
    write('Aux is: '), writeln(Aux),
    split_at(Aux, L, L2, L1),
    append(L1, L2, T).


What happens here?

We calculate Aux as K mod N.

We immediately display the value of Aux using write/1 followed by writeln/1.

write('Aux is: ') prints the text without a newline.

writeln(Aux) prints the actual value of Aux and then moves to a new line.

Example 2: Display Multiple Variables in a Single Line

check_and_print(A, B) :-
    A < B,
    write('A = '), write(A),
    write(', B = '), writeln(B),
    writeln('A is less than B!').


What happens here?

We check if A < B.

We print both variables on one line, then print a message on a new line.

Using multiple write/1 calls in a row allows you to control exactly how you space or format your output.

Example 3: Debugging Recursive Calls

sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, TailSum),
    Sum is H + TailSum,
    write('Current head: '), write(H),
    write(', Tail sum: '), write(TailSum),
    write(', Total so far: '), writeln(Sum).


What happens here?

Each time sum_list/2 processes an element, it prints out:

The current head of the list (H).

The sum of the tail (TailSum).

The total sum so far (Sum).

This is especially useful for understanding how recursion unfolds.

1.3 If-Then-Else in Prolog

<a id="if-then-else-in-prolog"></a>

Prolog’s if-then-else construct lets you execute one branch of code if a condition is met and another branch if it is not. The syntax is:

( Condition -> ThenClause ; ElseClause )


How it works:

Evaluate Condition: If the condition succeeds, Prolog commits to the ThenClause and ignores the ElseClause.

Else Clause: If the condition fails, the ElseClause is executed.

This construct is useful within predicates to decide between alternatives based on certain tests.

Example 1: Basic Numeric Check

Determine whether a number is even or odd:

even_or_odd(Number, Result) :-
    ( 0 is Number mod 2 ->
        Result = even
    ; 
        Result = odd
    ).


Explanation:

The predicate checks if Number mod 2 equals 0.

If true, Result is unified with even.

Otherwise, Result is unified with odd.

Example 2: Grading System

Assign a letter grade based on a score:

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


Explanation:

This predicate uses multiple if-then-else checks.

It assigns a grade by testing the score range in descending order.

The first true condition’s clause is executed, and the rest are ignored.

Example 3: Determine the Maximum of Two Numbers

Find the maximum of two numbers using an if-then-else construct:

max(X, Y, Max) :-
    ( X >= Y ->
        Max = X
    ;
        Max = Y
    ).


Explanation:

The predicate compares X and Y.

If X is greater than or equal to Y, then Max is set to X.

Otherwise, Max is set to Y.

2. Operators

<a id="operators"></a>

2.1 Arithmetic Operators

<a id="arithmetic-operators"></a>

Addition (+):

?- X is 2 + 3.  % X = 5.


Subtraction (-):

?- X is 5 - 2.  % X = 3.


Multiplication (*):

?- X is 4 * 3.  % X = 12.


Division (/):

?- X is 10 / 4.  % X = 2.5.


Integer Division (//):

?- X is 10 // 4.  % X = 2.


Modulus (mod):

?- X is 10 mod 4.  % X = 2.


Exponentiation (^):

?- X is 2 ^ 3.  % X = 8.


2.2 Comparison Operators

<a id="comparison-operators"></a>

Equal (=:=):

?- 2 + 3 =:= 5.  % true.


Not Equal (=\=):

?- 2 + 2 =\= 5.  % true.


Less Than (<), Greater Than (>), Less Than or Equal (=<), Greater Than or Equal (>=):

?- 3 < 5.  % true.
?- 5 >= 3. % true.


2.3 Logical Operators

<a id="logical-operators"></a>

Logical AND (,):

eligible(Person) :- over_18(Person), has_valid_id(Person).


Logical OR (;):

role(Person, student) :- student(Person).
role(Person, teacher) :- teacher(Person).


Logical NOT (\+):

not_student(Person) :- \+ student(Person).


2.4 Unification and Equality Operators

<a id="unification-and-equality-operators"></a>

Unification (=):

?- X = 5.  % X becomes 5.


Strict Equality (==):

?- 5 == 5.  % true.


Strict Inequality (\==):

?- 5 \== 3.  % true.


2.5 Arithmetic Evaluation (is)

<a id="arithmetic-evaluation-is"></a>

Usage: Evaluate the right-hand expression and bind the result.

?- X is 2 + 3.  % X = 5.


2.6 Operator Precedence and Associativity

<a id="operator-precedence--associativity"></a>

Example:

?- X is 2 + 3 * 4.  % X = 14.
?- Y is (2 + 3) * 4.  % Y = 20.


Key Table:

Operator

Precedence

Associativity

is

700

xfx

=:=, =\=

700

xfx

<, >, =<, >=

700

xfx

+, -

500

yfx

*, /, //, mod, ^

400

yfx

\+

200

fy

,

100

xfy

;

100

xfy

2.7 Logical Implications (:-)

<a id="logical-implications"></a>

Usage: Defines a rule where the head is true if the body is true.

happy(Person) :- has_money(Person), healthy(Person).


3. Lists

<a id="lists"></a>

Lists in Prolog are fundamental data structures representing ordered sequences of elements. They can contain numbers, atoms, strings, even other lists, and can be partially instantiated (with variables).

3.1 List Basics

<a id="list-basics-syntax-headtail-decomposition"></a>

Definition:
Lists are written using square brackets and elements are separated by commas.

Examples & Explanations:

Empty List:

[].


Explanation:
This is the simplest list, containing no elements. It is useful as the base case in recursive predicates.

List of Integers:

[1, 2, 3, 4].


Explanation:
This list contains four integer elements. When processing such a list, you might use arithmetic operations on the elements.

List of Atoms:

[apple, banana, cherry].


Explanation:
Atoms (like apple) are constants. This list might represent names or labels.

Mixed List:

[g01, "task", 3, [sublist]].


Explanation:
Here, the list contains an atom (g01), a string ("task"), an integer (3), and even another list ([sublist]). Prolog lists are heterogeneous.

List with Variables:

[X, Y, Z].


Explanation:
When elements are variables, Prolog can later instantiate them based on rules or queries. This is useful when generating or matching lists dynamically.

3.2 Head and Tail Decomposition

<a id="head-tail-decomposition"></a>

Concept:
Prolog uses the notation [Head|Tail] to split a list into its first element (Head) and the remaining list (Tail).

Examples & Explanations:

Decomposing a Non-Empty List:

[Head|Tail] = [a, b, c, d].


Explanation:

Head: Unifies with a (the first element).

Tail: Unifies with [b, c, d] (the remainder).
Why:
This technique is central for recursion; you process the head and then recursively process the tail.

Single-Element List:

[Head|Tail] = [a].


Explanation:

Head: Unifies with a.

Tail: Unifies with [] (an empty list) because there are no further elements.
Why:
This shows that even a single-element list is decomposed into an element and an empty tail.

Using Variables:

[X|Rest] = [1, 2, 3].


Explanation:

X: Will be instantiated to 1.

Rest: Will be instantiated to [2, 3].
Why:
This pattern is frequently used in recursive predicates (e.g., printing each element).

Recursive Predicate Example – Print List:

print_list([]).
print_list([Head|Tail]) :-
    write(Head), nl,
    print_list(Tail).


Explanation:

Base Case: When the list is empty ([]), the predicate succeeds without printing.

Recursive Case: The head is printed, and the predicate recursively processes the tail.
Why:
Demonstrates how head–tail decomposition drives recursion.

3.3 Common List Predicates

<a id="common-list-predicates"></a>

Below are the core predicates for list manipulation. Each predicate can work in different modes depending on which arguments are known (instantiated) and which are variables.

3.3.1 member/2

<a id="member2-and-memberchk2"></a>

Purpose:
Determines if an element is a member of a list or enumerates all elements.

Examples & Explanations:

Check Membership (Element Known, List Known):

?- member(b, [a, b, c]).


Explanation:

The query asks, "Is b in the list [a,b,c]?"

Since b is present, it succeeds (returns true).

Enumerate Members (Element Variable, List Known):

?- member(X, [a, b, c]).


Explanation:

Prolog assigns X the value a first, then upon backtracking b, then c.

Output:

X = a

X = b

X = c
Why:
Useful for generating all elements one by one.

Using with Duplicates:

?- member(a, [a, b, a, c]).


Explanation:

Even though a appears twice, member/2 will succeed immediately once it finds the first occurrence.

Backtracking can reveal the second occurrence if needed.

Generating Lists (List Variable):

?- member(1, L).


Explanation:

Here, L is a variable. Prolog tries to generate lists that contain 1 as some element.

Caution: This can lead to infinite possibilities unless constrained by additional goals.

3.3.2 memberchk/2

Purpose:
A deterministic version of member/2 that stops after the first successful match.

Examples & Explanations:

Check First Occurrence:

?- memberchk(b, [a, b, c, b]).


Explanation:

Finds b and stops, without leaving a choice point for further backtracking.

Output: Simply true.

Enumerating with Variable (Less Useful):

?- memberchk(X, [a, b, c]).


Explanation:

X will be instantiated to a (the first element) and then stops.

Why:
Use memberchk/2 when you only need to check for membership, not to list all possibilities.

3.3.3 append/3

<a id="append3"></a>

Purpose:
Used for concatenating lists or splitting a list into two parts. It works in multiple modes.

Examples & Explanations:

Concatenating Two Lists:

?- append([a, b], [c, d], X).


Explanation:

Input: First list [a,b], second list [c,d].

Output: X becomes [a, b, c, d].

Why:
Directly joins two lists.

Splitting a List into Two Parts:

?- append(X, Y, [1, 2, 3]).


Explanation:

Both X and Y are variables. Prolog finds all ways to split [1,2,3]:

X = [], Y = [1,2,3]

X = [1], Y = [2,3]

X = [1,2], Y = [3]

X = [1,2,3], Y = []

Why:
This is useful for deconstructing a list or generating candidate splits.

Using with a Partially Instantiated List:

?- append([a, b], Y, [a, b, c, d]).


Explanation:

Given that [a, b] is a prefix of [a,b,c,d], Prolog determines Y must be [c, d].

Why:
Constrains the solution by providing known parts of the list.

Reverse-Mode Usage Example:
You can use append/3 to “guess” a list when provided with its parts. For instance, in writing a predicate that splits and then recombines a list in a different order.

3.3.4 length/2

<a id="length2"></a>

Purpose:
Determines the number of elements in a list, or can generate a list of a specified length.

Examples & Explanations:

Finding the Length of a Given List:

?- length([a, b, c], N).


Explanation:

Prolog counts the elements.

Output: N = 3.

Generating a List of a Given Length:

?- length(L, 3).


Explanation:

Here, L is a variable and is instantiated to a list with exactly 3 elements (e.g., [ _G1, _G2, _G3 ]).

Why:
Useful when you need a list of a particular size for further processing.

Empty List Case:

?- length([], N).


Explanation:

Since the list is empty, N = 0.

Using as a Constraint:

?- L = [a, b | Tail], length(L, 5).


Explanation:

The list starts with [a,b] and then an unknown tail. Constraining the length to 5 forces Tail to be a list of 3 elements.

3.3.5 nth1/3

<a id="nth13"></a>

Purpose:
Accesses the N-th element of a list using 1-based indexing.

Examples & Explanations:

Retrieve the Second Element:

?- nth1(2, [a, b, c], X).


Explanation:

Index 2 means the second element, so X = b.

Find the Position of an Element:

?- nth1(Pos, [a, b, c], b).


Explanation:

Prolog finds that b is at position 2 (since indexing starts at 1).

First Element Access:

?- nth1(1, [apple, banana, cherry], X).


Explanation:

X is unified with apple because it is the first element.

Out-of-Bounds Query:

?- nth1(4, [a, b, c], X).


Explanation:

The list has only 3 elements, so the query fails.

Why:
The predicate expects the index to be within the valid range (1 to length of list).

3.3.6 findall/3

<a id="findall3"></a>

Purpose:
Collects all solutions for a given goal and returns them as a list.

Examples & Explanations:

Collect All Elements of a List:

?- findall(X, member(X, [a, b, c]), L).


Explanation:

The goal member(X, [a, b, c]) generates X = a, then b, then c.

Output: L = [a, b, c].

Computing Squares:

?- findall(Square, (between(1, 5, N), Square is N * N), Squares).


Explanation:

between(1,5,N) generates numbers from 1 to 5.

For each N, Square is N*N computes its square.

Output: Squares = [1, 4, 9, 16, 25].

No Solutions Scenario:

?- findall(X, (member(X, [a,b]), X == c), L).


Explanation:

There is no X in [a, b] such that X == c.

Output: L = [].

Using findall with Complex Goals:

?- findall([X, Y], (member(X, [1,2]), member(Y, [a,b])), Pairs).


Explanation:

Generates all pairs [X, Y] where X is from [1,2] and Y is from [a,b].

Output: Pairs = [[1,a], [1,b], [2,a], [2,b]].

3.3.7 select/3

<a id="select3"></a>

Purpose:
Removes one occurrence of an element from a list, returning the remainder.

Examples & Explanations:

Remove a Specific Element:

?- select(b, [a, b, c], L).


Explanation:

Removes the first occurrence of b from the list.

Output: L = [a, c].

Enumerate All Removals (With Duplicates):

?- select(X, [a, b, a, c], L).


Explanation:

Prolog will first choose X = a and L = [b, a, c].

On backtracking, it may choose X = b with L = [a, a, c], then again X = a with a different removal order, etc.

Why:
This illustrates how select/3 can be used to enumerate different ways of removing one occurrence.

Using select to Split a List:

?- select(X, [1,2,3,4], Rest).


Explanation:

This will generate all pairs where one element is selected and the remaining list (Rest) is returned.

Outputs include:

X = 1, Rest = [2,3,4]

X = 2, Rest = [1,3,4]

etc.

3.3.8 reverse/2

<a id="reverse2"></a>

Purpose:
Reverses the order of elements in a list.

Examples & Explanations:

Reversing a Given List:

?- reverse([a, b, c], X).


Explanation:

The predicate produces X = [c, b, a].

Using reverse in the Opposite Mode:

?- reverse(X, [c, b, a]).


Explanation:

Prolog infers that X must be [a, b, c] because reversing [a, b, c] gives [c, b, a].

Both Arguments Variables (Use with Caution):

?- reverse(X, Y).


Explanation:

With both variables free, Prolog may generate many pairs where one list is the reverse of the other. Typically, at least one list is provided.

3.3.9 last/2

<a id="last2"></a>

Purpose:
Retrieves the last element of a list.

Examples & Explanations:

Simple Case:

?- last([a, b, c], X).


Explanation:

The last element of [a,b,c] is c, so X = c.

Using in Verification:

?- last([1, 2, 3, 4], 4).


Explanation:

This succeeds because 4 is indeed the last element of the list.

Failure on Empty List:

?- last([], X).


Explanation:

The predicate fails since there is no last element in an empty list.

3.3.10 delete/3

<a id="delete3"></a>

Purpose:
Removes all occurrences of a specified element from a list.

Examples & Explanations:

Removing a Single Occurrence (All Occurrences):

?- delete([a, b, c, b], b, L).


Explanation:

All b elements are removed from the list.

Output: L = [a, c].

With Multiple Duplicates:

?- delete([a, b, a, c, a], a, L).


Explanation:

Removes every occurrence of a.

Output: L = [b, c].

Using delete in a Goal:

remove_item(Item, List, Result) :-
    delete(List, Item, Result).


Explanation:

This predicate wraps delete/3 to remove all instances of Item from List.

3.3.11 union/3 and intersection/3

<a id="union3-and-intersection3"></a>

Purpose:
Compute the union (merging lists without duplicates) and intersection (common elements) of two lists.

Examples & Explanations:

Union of Two Lists:

?- union([a, b, c], [b, c, d], X).


Explanation:

The union of [a,b,c] and [b,c,d] is [a,b,c,d].

Note: Order is typically preserved from the first list, with new elements appended.

Intersection of Two Lists:

?- intersection([a, b, c], [b, c, d], X).


Explanation:

Only the elements present in both lists are returned.

Output: X = [b, c].

Handling Duplicates:

Union Example with Duplicates:

?- union([a, b, a], [b, c, c], X).


Explanation:

Despite duplicates in the inputs, the output is unique: X = [a, b, c].

Intersection Example with Duplicates:

?- intersection([a, a, b, c], [a, c, d], X).


Explanation:

The intersection returns unique common elements: X = [a, c].

3.4 Advanced List Predicates

<a id="advanced-list-predicates"></a>

3.4.1 sort/2 vs. msort/2

<a id="sort2-and-msort2"></a>

Both predicates sort lists, but with a crucial difference regarding duplicates.

sort/2

Purpose: Sorts a list in ascending order while removing duplicates.

Example 1 – Basic Sorting:

?- sort([3, 1, 2, 3, 4, 2], X).


Explanation:

Input: [3,1,2,3,4,2]

Process: The list is sorted to [1,2,3,4], and duplicate values are removed.

Output: X = [1,2,3,4]

Example 2 – With Atoms and Mixed Types:

?- sort([banana, apple, cherry, apple], Sorted).


Explanation:

Atoms are compared lexicographically.

Output: Sorted = [apple, banana, cherry]

Note: Duplicates (e.g., apple) are removed.

msort/2

Purpose: Sorts a list in ascending order but preserves all duplicates.

Example 1 – Basic Sorting with Duplicates:

?- msort([3, 1, 2, 3, 4, 2], X).


Explanation:

Input: [3,1,2,3,4,2]

Process: The list is sorted, but every instance is kept.

Output: X = [1,2,2,3,3,4]

Example 2 – Mixed Data:

?- msort([banana, apple, cherry, apple], Sorted).


Explanation:

Output: Sorted = [apple, apple, banana, cherry]

Duplicates are preserved in the order given by the sorting algorithm.

3.4.2 Custom Predicates

<a id="custom-predicates-unique2-and-intersect3"></a>

Custom predicates allow you to implement specific behaviors not built into Prolog. Two common examples are unique/2 and intersect/3.

unique/2

Purpose: Removes duplicate elements while preserving the order of the first occurrences.

Implementation:

unique([], []).
unique([H|T], [H|UniqueT]) :-
    \+ member(H, T),
    unique(T, UniqueT).
unique([H|T], UniqueT) :-
    member(H, T),
    unique(T, UniqueT).


Example 1 – Simple Duplicate Removal:

?- unique([1,2,3,2,4,1], X).


Explanation:

The predicate processes the list from left to right.

Output: X = [1,2,3,4]

The first occurrence of 1 is kept; subsequent 1’s are removed.

Example 2 – List of Atoms:

?- unique([apple, banana, apple, cherry, banana], Unique).


Explanation:

Output: Unique = [apple, banana, cherry]

Maintains the order of first appearances.

intersect/3

Purpose: Finds the intersection between two lists, i.e., elements that appear in both lists. Depending on the implementation, duplicates may or may not be handled.

Implementation:

intersect([], _, []).
intersect([H|T], L2, [H|I]) :-
    member(H, L2),
    intersect(T, L2, I).
intersect([H|T], L2, I) :-
    \+ member(H, L2),
    intersect(T, L2, I).


Example 1 – Basic Intersection:

?- intersect([1,2,3,4], [3,4,5,6], X).


Explanation:

Input: First list is [1,2,3,4] and second list is [3,4,5,6].

Process: Only elements present in both lists are retained.

Output: X = [3,4]

Example 2 – With Duplicates in First List:

?- intersect([a, a, b, c], [a, c, d], Y).


Explanation:

Output: Y = [a, c]

The predicate will include a only once if it checks uniqueness or as many times as it appears in the first list if duplicates are not explicitly handled.

3.5 Haskell-like List Functions

<a id="haskell-like-list-functions"></a>

This section discusses a series of list-processing predicates reminiscent of common functions in Haskell. Each predicate is accompanied by a thorough explanation of its inner workings. Where relevant, we also illustrate partial application (fixing some arguments of a predicate while leaving others free) and why it matters in predicates like takewhile/3 and dropwhile/3.

3.5.1 take/3

<a id="take3"></a>

Purpose:
Extracts the first N elements from a list, or fewer if the list is shorter than N.

Implementation:

take(0, _, []).
take(_, [], []).
take(N, [H|T], [H|Taken]) :-
    N > 0,
    N1 is N - 1,
    take(N1, T, Taken).


The base cases:

If N = 0, no elements are taken.

If the list is empty, the result is also empty.

The recursive case:

Take one element H if N > 0.

Decrement N and recurse down the tail of the list.

Example: Select Only a Few Items

?- take(3, [apple, banana, cherry, date, fig], X).


Process:

We start with N=3. The head of the list is apple, so it is included.

Decrement N to 2, move on to banana.

Decrement N to 1, move on to cherry.

Decrement N to 0. At this point, the recursion stops taking new elements.

Outcome: Prolog unifies X with [apple, banana, cherry].

Example: Limiting an Already Short List

?- take(5, [red, green], X).


Process:

The list has only two elements. By the time we run out of elements, we have already taken [red, green].

The predicate stops once the list is empty.

Outcome: X is [red, green].

3.5.2 drop/3

<a id="drop3"></a>

Purpose:
Discards the first N elements of a list, returning whatever remains.

Implementation:

drop(0, List, List).
drop(_, [], []).
drop(N, [_|T], Dropped) :-
    N > 0,
    N1 is N - 1,
    drop(N1, T, Dropped).


If N = 0, none are discarded, so the result is the original list.

If the list is empty, nothing remains.

Otherwise, consume elements while decrementing N.

Example: Skipping a Header

?- drop(2, [header1, header2, data1, data2, data3], Remainder).


Process:

With N = 2, Prolog discards the first two items (header1, header2).

It returns whatever is left, [data1, data2, data3].

Outcome: Remainder = [data1, data2, data3].

Example: Discarding Beyond the List Size

?- drop(10, [a, b, c], Remainder).


Process:

The predicate discards a, b, and c.

There are no more elements to process, so the result is the empty list.

Outcome: Remainder = [].

3.5.3 takewhile/3 and dropwhile/3

<a id="takewhile-dropwhile"></a>

Both predicates rely on call(P, Elem) to test whether an element satisfies a condition. This requires that the predicate P be a unary predicate (i.e. take one argument). If you have a predicate with more arguments—one or more of which must be provided dynamically—you must “partially apply” it (wrap it so that some arguments are fixed by variables) so that it becomes unary.

Below, we define a couple of higher-arity predicates and show examples where the extra arguments are not fixed as constants but are taken from dynamic variables.

Definitions

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


Example Setup: A 2-Argument Predicate with a Dynamic First Argument

Imagine a predicate that compares two numbers:

% less_than/2: Succeeds if Y is less than X.
less_than(X, Y) :- Y < X.


In many cases, you want the first argument (the threshold) to come from the context of your data rather than a fixed number.

Example 1 – Using takewhile/3 with less_than/2

We want to collect elements from a list that are less than the first element. The first element is dynamic and becomes the threshold.

?- List = [10, 8, 6, 12, 4],
   List = [Threshold|Tail],
   takewhile(less_than(Threshold), Tail, Result).


Explanation:

The list is deconstructed so that Threshold = 10 and Tail = [8, 6, 12, 4].

The call less_than(Threshold) becomes a unary predicate equivalent to:

ElemSatisfies :- less_than(10, Elem).


As takewhile/3 processes Tail, it tests each element:

8 < 10 succeeds,

6 < 10 succeeds,

12 < 10 fails (stopping the process).

Outcome: Result = [8, 6].

Example 2 – Using dropwhile/3 with less_than/2

Now we drop elements that are less than the first element.

?- List = [10, 8, 6, 12, 4],
   List = [Threshold|Tail],
   dropwhile(less_than(Threshold), Tail, Result).


Explanation:

Again, Threshold = 10 and Tail = [8, 6, 12, 4].

dropwhile/3 tests each element with less_than(10, Elem):

It drops 8 and 6 because both satisfy the condition.

When it reaches 12 (since 12 < 10 fails), it stops and returns the remainder.

Outcome: Result = [12, 4].

Example Setup: A 3-Argument Predicate with Two Dynamic Arguments

Now consider a predicate that checks if a number is within an interval:

% in_interval/3: Succeeds if X is between Min and Max (inclusive).
in_interval(Min, Max, X) :- X >= Min, X =< Max.


We want both Min and Max to be provided dynamically (for example, as the first two elements of a list).

Example 3 – Using takewhile/3 with in_interval/3

We deconstruct a list so that the first two elements become the dynamic bounds.

?- List = [3, 7, 4, 5, 6, 8, 2],
   List = [Min, Max | Tail],
   takewhile(in_interval(Min, Max), Tail, Result).


Explanation:

Here, Min = 3, Max = 7, and Tail = [4, 5, 6, 8, 2].

The partial application in_interval(Min, Max) acts as a unary predicate testing:

ElemSatisfies :- in_interval(3, 7, Elem).


The predicate collects 4, 5, 6 (all between 3 and 7). It stops at 8 because 8 is not ≤ 7.

Outcome: Result = [4, 5, 6].

Example 4 – Using dropwhile/3 with in_interval/3

Using the same dynamic bounds, now drop the elements that satisfy the interval.

?- List = [3, 7, 4, 5, 6, 8, 2],
   List = [Min, Max | Tail],
   dropwhile(in_interval(Min, Max), Tail, Result).


Explanation:

With Min = 3, Max = 7, and Tail = [4, 5, 6, 8, 2], the predicate checks each element:

It drops 4, 5, 6 because they lie within 3 and 7.

It stops at 8, since 8 is not in the interval.

Outcome: Result = [8, 2].

Example Setup: A 2-Argument Predicate with a Dynamic First Argument (Different Relation)

Consider a predicate that checks if a value is greater than a dynamic value:

% greater_than/2: Succeeds if Y is greater than X.
greater_than(X, Y) :- Y > X.


We use it to collect or drop elements relative to a dynamic reference.

Example 5 – Using takewhile/3 with greater_than/2

Suppose you want to take elements from a list that are greater than the first element.

?- List = [5, 7, 9, 4, 8],
   List = [Threshold|Tail],
   takewhile(greater_than(Threshold), Tail, Result).


Explanation:

Here, Threshold = 5 and Tail = [7, 9, 4, 8].

The call greater_than(Threshold) is interpreted as:

ElemSatisfies :- greater_than(5, Elem).


It collects 7 and 9 because both are greater than 5; it stops at 4 (which is not > 5).

Outcome: Result = [7, 9].

Example 6 – Using dropwhile/3 with greater_than/2

Similarly, dropping the elements that are greater than the dynamic threshold:

?- List = [5, 7, 9, 4, 8],
   List = [Threshold|Tail],
   dropwhile(greater_than(Threshold), Tail, Result).


Explanation:

With Threshold = 5 and Tail = [7, 9, 4, 8], the predicate drops 7 and 9.

It stops when it finds 4, which does not satisfy greater_than(5, 4).

Outcome: Result = [4, 8].

3.5.4 split_at/4

<a id="split_at4"></a>

Purpose:
Divides a list into two parts at a given index N.

Implementation:

split_at(0, List, [], List).
split_at(_, [], [], []).
split_at(N, [H|T], [H|Left], Right) :-
    N > 0,
    N1 is N - 1,
    split_at(N1, T, Left, Right).


If N is 0, the left part is empty, and the right part is the entire list.

If the list is empty, both parts are empty regardless of N.

Otherwise, decrement N and move the head to the left part until N hits 0.

Example: Splitting a Student List

?- split_at(3, [alice, bob, charlie, diana, eric], GroupA, GroupB).


Process:

The first three names move to GroupA: [alice, bob, charlie].

The rest end up in GroupB: [diana, eric].

Outcome: GroupA = [alice, bob, charlie], GroupB = [diana, eric].

Example: Splitting an Inventory Beyond Its Length

?- split_at(10, [pen, pencil, eraser], Part1, Part2).


Process:

We move all items into the left group because we never run out of index steps before the list ends.

When the list is empty, recursion stops.

Outcome: Part1 = [pen, pencil, eraser], Part2 = [].

3.5.5 zip/3

<a id="zip3"></a>

Purpose:
Combines two lists element by element into a new list of pairs. The process ceases when the shortest list is exhausted.

Implementation:

zip([], _, []).
zip(_, [], []).
zip([H1|T1], [H2|T2], [[H1,H2]|Zipped]) :-
    zip(T1, T2, Zipped).


If either list is empty, the zipping ends immediately.

Pairs up the heads of both lists, then recurses on the tails.

Example: Pairing Names and Ages

?- zip([alice, bob, charlie], [20, 21, 22], Paired).


Process:

Combine alice with 20, bob with 21, charlie with 22.

End of both lists reached simultaneously.

Outcome: Paired = [[alice, 20], [bob, 21], [charlie, 22]].

Example: Combining Days of the Week and Weather

?- zip([monday, tuesday, wednesday], [sunny, cloudy], Output).


Process:

monday is paired with sunny, tuesday is paired with cloudy.

The second list is now empty, so the process stops.

Outcome: Output = [[monday, sunny], [tuesday, cloudy]].

Key Takeaways

Recursion and Base Cases: Each of these list predicates relies on defining clear base cases (e.g., when a list is empty or a counter is zero) and then handling the recursive step.

call/2 and Partial Application:

takewhile/3 and dropwhile/3 use call(P, Element) to apply a predicate P to Element.

P must have arity 1. If you have a predicate of higher arity, partially apply it by fixing some arguments in advance.

Early Stopping:

takewhile/3 and dropwhile/3 can terminate the traversal early depending on whether the predicate passes or fails.

zip/3 finishes once at least one of the input lists is exhausted.

Splitting vs. Taking/Dropping:

split_at/4 divides the list at a specific index, while take/3 and drop/3 each cover only one half of that logic.

3.6 Recursive List Processing

<a id="recursive-list-processing"></a>

Recursive predicates are central to list processing in Prolog. Here we illustrate several examples with detailed explanations.

3.6.1 Sum of Elements

<a id="sum-of-elements"></a>

Implementation:

sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Rest),
    Sum is H + Rest.


Example:

?- sum_list([1,2,3,4], Sum).


Explanation:

Base Case: An empty list sums to 0.

Recursive Case: For [1,2,3,4], it computes Sum = 1 + (2 + (3 + (4 + 0))).

Output: Sum = 10.

3.6.2 Maximum Element

<a id="maximum-element"></a>

Implementation:

max_list([X], X).
max_list([H|T], Max) :-
    max_list(T, TempMax),
    Max is max(H, TempMax).


Example:

?- max_list([3,1,4,2], Max).


Explanation:

Compares each element recursively.

Output: Max = 4.

3.6.3 Minimum Element

<a id="minimum-element"></a>

Implementation:

min_list([X], X).
min_list([H|T], Min) :-
    min_list(T, TempMin),
    Min is min(H, TempMin).


Example:

?- min_list([3,1,4,2], Min).


Explanation:

Output: Min = 1.

3.6.4 Product of Elements

<a id="product-of-elements"></a>

Implementation:

product_list([], 1).
product_list([H|T], Product) :-
    product_list(T, Rest),
    Product is H * Rest.


Example:

?- product_list([1,2,3,4], Product).


Explanation:

Multiplies all elements together.

Output: Product = 24.

3.6.5 Average of Elements

<a id="average-of-elements"></a>

Implementation:

average_list(List, Avg) :-
    sum_list(List, Sum),
    length(List, Len),
    Len > 0,
    Avg is Sum / Len.


Example:

?- average_list([1,2,3,4], Avg).


Explanation:

Computes the sum and divides by the length.

Output: Avg = 2.5.

3.7 Haskell-like Higher-Order Functions

<a id="haskell-like-higher-order-functions"></a>

These predicates enable a higher–level functional programming style with lists.

3.7.1 map/3

<a id="map3"></a>

Purpose:
Applies a given predicate to each element of the input list to produce an output list.

Implementation:

map([], [], _F).
map([H|T], [H1|T1], F) :-
    call(F, H, H1),
    map(T, T1, F).


Examples:

Squaring Numbers:

square(X, Y) :- Y is X * X.
?- map([1,2,3,4], Squares, square).


Explanation: Each element is squared.
Output: Squares = [1,4,9,16]

Converting Atoms to Uppercase:
(Assuming you have upcase_atom/2 available)

?- map([apple, banana], Upper, upcase_atom).


Explanation: Converts each atom to its uppercase equivalent.
Output: Possibly Upper = [APPLE, BANANA]

Converting Numbers to Atoms:
Define a helper predicate:

number_to_atom(X, Atom) :- atom_number(Atom, X).
?- map([1,2,3,4], Atoms, number_to_atom).


Explanation: Each number is converted to its atom representation.
Output: Atoms = ['1','2','3','4']

3.7.2 filter/3

<a id="filter3"></a>

Purpose:
Selects only those elements from the list that satisfy a given predicate.

Implementation:

filter([], [], _Pred).
filter([H|T], [H|Filtered], Pred) :-
    call(Pred, H),
    filter(T, Filtered, Pred).
filter([_|T], Filtered, Pred) :-
    filter(T, Filtered, Pred).


Examples:

Filtering Even Numbers:

is_even(X) :- 0 is X mod 2.
?- filter([1,2,3,4,5,6], Evens, is_even).


Output: Evens = [2,4,6]

Filtering Numbers Greater Than 3:

greater_than_3(X) :- X > 3.
?- filter([1,2,3,4,5], Result, greater_than_3).


Output: Result = [4,5]

3.7.3 foldl/4 and foldr/4

<a id="foldl4-and-foldr4"></a>

These predicates process lists while carrying an accumulator.

foldl/4 (Left Fold)

Purpose:
Processes a list from left to right.

Implementation:

foldl(_, [], Acc, Acc).
foldl(P, [H|T], Acc, Result) :-
    call(P, H, Acc, NewAcc),
    foldl(P, T, NewAcc, Result).


Examples:

Summing Elements:

add(X, Acc, NewAcc) :- NewAcc is Acc + X.
?- foldl(add, [1,2,3,4], 0, Sum).


Output: Sum = 10

Concatenating Atoms:
(Assuming all list elements are atoms)

concatenate(H, Acc, NewAcc) :- atom_concat(Acc, H, NewAcc).
?- foldl(concatenate, [hello, world], '', Result).


Explanation: Starting with the empty atom, concatenates each atom in order.
Output: Result = helloworld

foldr/4 (Right Fold)

Purpose:
Processes a list from right to left.

Implementation:

foldr(_, [], Acc, Acc).
foldr(P, [H|T], Acc, Result) :-
    foldr(P, T, Acc, NewAcc),
    call(P, H, NewAcc, Result).


Examples:

Multiplying Elements:

multiply(X, Acc, NewAcc) :- NewAcc is X * Acc.
?- foldr(multiply, [1,2,3,4], 1, Product).


Output: Product = 24

Subtraction Example (Right–Associative):
(This computes 1 - (2 - (3 - 0)))

subtract(X, Acc, Result) :- Result is X - Acc.
?- foldr(subtract, [1,2,3], 0, Result).


Output: Result = 2

3.7.4 scanl/4 and scanr/4

These predicates are similar to folds but return the entire list of intermediate accumulator values.

scanl/4 (Left Scan)

Purpose:
Returns a list of successive accumulator states as the list is processed from left to right.

Implementation:

scanl(_, Acc, [], [Acc]).
scanl(P, [H|T], Acc, [Acc|Rest]) :-
    call(P, H, Acc, NewAcc),
    scanl(P, T, NewAcc, Rest).


Example – Summing Elements:

add(X, Acc, NewAcc) :- NewAcc is Acc + X.
?- scanl(add, [1,2,3,4], 0, Result).


Explanation:

Start with 0

0 + 1 = 1

1 + 2 = 3

3 + 3 = 6

6 + 4 = 10
Output: Result = [0,1,3,6,10]

scanr/4 (Right Scan)

Purpose:
Returns a list of successive accumulator states as the list is processed from right to left.

Implementation:

scanr(_, Acc, [], [Acc]).
scanr(P, [H|T], Acc, [Result|Rest]) :-
    scanr(P, T, Acc, Rest),
    Rest = [NewAcc|_],
    call(P, H, NewAcc, Result).


Example – Multiplying Elements:

multiply(X, Acc, NewAcc) :- NewAcc is X * Acc.
?- scanr(multiply, [1,2,3,4], 1, Result).


Explanation:
Evaluates as in Haskell's scanr (*) 1 [1,2,3,4], producing the intermediate products.
Output: Result = [24,24,12,4,1]

3.7.5 all/2 and any/2

<a id="all2-and-any2"></a>

These predicates check if all or any elements of a list satisfy a given condition.

all/2

Purpose:
Succeeds if every element in the list satisfies the predicate.

Implementation:

all(_Pred, []).
all(Pred, [H|T]) :-
    call(Pred, H),
    all(Pred, T).


Examples:

Checking Even Numbers:

is_even(X) :- 0 is X mod 2.
?- all(is_even, [2,4,6,8]).


Output: Succeeds (all numbers are even).

Checking Positivity:

greater_than_zero(X) :- X > 0.
?- all(greater_than_zero, [1,2,3,4]).


Output: Succeeds.

Failure Case:

?- all(is_even, [2,3,6]).


Output: Fails (since 3 is not even).

any/2

Purpose:
Succeeds if at least one element in the list satisfies the predicate.

Implementation:

any(_Pred, []) :- false.
any(Pred, [H|_T]) :-
    call(Pred, H), !.
any(Pred, [_|T]) :-
    any(Pred, T).


Examples:

Checking for an Even Number:

?- any(is_even, [1,3,5,7]).


Output: Fails (none are even).

?- any(is_even, [1,3,4,7]).


Output: Succeeds (4 is even).

Checking for a Negative Number:

is_negative(X) :- X < 0.
?- any(is_negative, [1,-1,2,3]).


Output: Succeeds (since -1 is negative).

3.8 Typical List-Based Examples

<a id="typical-list-based-examples"></a>

These examples demonstrate common algorithms implemented with list recursion. They showcase how recursive patterns, pattern matching, and backtracking work in Prolog when processing lists.

3.8.1 Factorial

<a id="factorial"></a> What It Does:
The factorial predicate computes the factorial of a non-negative integer using recursion. The base case is defined for 0 (0! = 1), and for any N > 0, it recursively computes factorial(N-1) and multiplies it by N.

Implementation:

factorial(0, 1).
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.


Examples:

Base Case:

?- factorial(0, F).


Explanation: Since 0! is defined as 1, Prolog returns F = 1.

Small Number:

?- factorial(5, F).


Explanation: Computes 5! as 5 * 4 * 3 * 2 * 1 = 120.
Output: F = 120.

Larger Number:

?- factorial(7, F).


Explanation: Computes 7! as 7 * 6 * 5 * 4 * 3 * 2 * 1 = 5040.
Output: F = 5040.

3.8.2 Fibonacci

<a id="fibonacci"></a> What It Does:
The Fibonacci predicate calculates the N-th Fibonacci number recursively. The base cases are defined for N=0 (result 0) and N=1 (result 1). For N > 1, it sums the (N-1)-th and (N-2)-th Fibonacci numbers.

Implementation:

fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, F) :-
    N > 1,
    N1 is N - 1, N2 is N - 2,
    fibonacci(N1, F1), fibonacci(N2, F2),
    F is F1 + F2.


Examples:

Base Case 0:

?- fibonacci(0, F).


Explanation: Returns the 0th Fibonacci number: F = 0.

Base Case 1:

?- fibonacci(1, F).


Explanation: Returns the 1st Fibonacci number: F = 1.

Recursive Case:

?- fibonacci(7, F).


Explanation: Computes the sequence up to the 7th term (0,1,1,2,3,5,8,13).
Output: F = 13.

3.8.3 Reverse a List

<a id="reverse-a-list"></a> What It Does:
The reverse predicate recursively reverses the elements of a list by first reversing the tail and then appending the head to the end of the reversed tail.

Implementation:

reverse_list([], []).
reverse_list([H|T], Rev) :-
    reverse_list(T, RevT),
    append(RevT, [H], Rev).


Examples:

Basic List:

?- reverse_list([a,b,c], X).


Explanation: Reverses [a,b,c] to [c,b,a].
Output: X = [c,b,a].

Numeric List:

?- reverse_list([1,2,3,4], X).


Explanation: Reverses [1,2,3,4] to [4,3,2,1].

Empty List:

?- reverse_list([], X).


Explanation: The reverse of an empty list is also an empty list.
Output: X = [].

3.8.4 Palindrome Check

<a id="palindrome-check"></a> What It Does:
The predicate checks whether a list is a palindrome by comparing the list with its reverse.

Implementation:

is_palindrome(List) :-
    reverse(List, List).


Examples:

Alphabetic Palindrome:

?- is_palindrome([r,a,c,e,c,a,r]).


Explanation: The list is the same forwards and backwards.
Output: Succeeds.

Numeric Palindrome:

?- is_palindrome([1,2,3,2,1]).


Explanation: The list reads the same in reverse.
Output: Succeeds.

Non-Palindrome:

?- is_palindrome([a,b,c]).


Explanation: [a,b,c] ≠ [c,b,a], so the predicate fails.

3.8.5 Merge Sort

<a id="merge-sort"></a> What It Does:
Merge sort is a divide-and-conquer algorithm that splits a list into halves, recursively sorts each half, and then merges the sorted halves.

Implementation:

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


Examples:

Even-Length List:

?- merge_sort([3,1,4,1,5,9], Sorted).


Explanation: Splits and sorts [3,1,4,1,5,9] to get [1,1,3,4,5,9].

Odd-Length List:

?- merge_sort([10,2,7,3,6], Sorted).


Explanation: Returns [2,3,6,7,10] after recursively sorting and merging.

Empty List:

?- merge_sort([], Sorted).


Explanation: The empty list remains empty.
Output: Sorted = [].

3.8.6 Flatten a Nested List

<a id="flatten-a-nested-list"></a> What It Does:
The flatten predicate converts a nested list (a list that contains other lists) into a single-level list by recursively processing each element.

Implementation:

flatten_list([], []).
flatten_list([H|T], Flat) :-
    flatten_list(H, FlatH),
    flatten_list(T, FlatT),
    append(FlatH, FlatT, Flat).
flatten_list(L, [L]) :-
    \+ is_list(L).


Examples:

Simple Nested List:

?- flatten_list([a, [b, c], d], X).


Explanation: Converts [a, [b, c], d] to [a,b,c,d].

Deeply Nested List:

?- flatten_list([[1,2], [3, [4,5]], 6], X).


Explanation: Recursively flattens to [1,2,3,4,5,6].

Nested with Empty List:

?- flatten_list([[[a]], b, []], X).


Explanation: Returns [a,b] as the empty list contributes nothing.

3.8.7 Sum of Elements

<a id="sum-of-elements-1"></a> What It Does:
This predicate recursively computes the sum of all elements in a numeric list.

Implementation:

sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Rest),
    Sum is H + Rest.


Examples:

Basic Sum:

?- sum_list([1,2,3,4,5], Sum).


Explanation: Sums 1+2+3+4+5 to yield 15.

Single Element:

?- sum_list([10], Sum).


Explanation: The sum of a single-element list is the element itself (10).

Empty List:

?- sum_list([], Sum).


Explanation: By definition, the sum of an empty list is 0.

3.8.8 Product of Elements

<a id="product-of-elements-1"></a> What It Does:
This predicate multiplies all elements in a list together recursively.

Implementation:

product_list([], Product) :- Product = 1.
product_list([H|T], Product) :-
    product_list(T, Rest),
    Product is H * Rest.


Examples:

Basic Product:

?- product_list([2,3,4], Product).


Explanation: Computes 2*3*4 = 24.

Single Element:

?- product_list([5], Product).


Explanation: The product of a one-element list is 5.

Empty List:

?- product_list([], Product).


Explanation: By definition, the product of an empty list is 1 (multiplicative identity).

3.8.9 List Length (Recursive)

<a id="list-length-recursive"></a> What It Does:
Calculates the number of elements in a list using recursion.

Implementation:

my_length([], 0).
my_length([_|T], Len) :-
    my_length(T, L1),
    Len is L1 + 1.


Examples:

Standard List:

?- my_length([a,b,c,d], L).


Explanation: There are 4 elements, so L = 4.

Empty List:

?- my_length([], L).


Explanation: An empty list has length 0.

Numeric List:

?- my_length([1,2,3], L).


Explanation: Returns L = 3.

3.8.10 Count Occurrences

<a id="count-occurrences"></a> What It Does:
Counts the number of times a specific element appears in a list.

Implementation:

count_occurrences(_, [], 0).
count_occurrences(X, [X|T], Count) :-
    count_occurrences(X, T, Count1),
    Count is Count1 + 1.
count_occurrences(X, [_|T], Count) :-
    count_occurrences(X, T, Count).


Examples:

Counting Letters:

?- count_occurrences(a, [a,b,a,c,a], Count).


Explanation: a appears 3 times; thus, Count = 3.

Counting Numbers:

?- count_occurrences(1, [1,2,1,3,1,1], Count).


Explanation: 1 appears 4 times.

Element Not Present:

?- count_occurrences(x, [a,b,c], Count).


Explanation: Since x is absent, Count = 0.

3.8.11 Maximum Element (Recursive)

<a id="maximum-element-recursive"></a> What It Does:
Finds the largest element in a numeric list by recursively comparing elements.

Implementation:

max_list([X], X).
max_list([H|T], Max) :-
    max_list(T, TempMax),
    Max is max(H, TempMax).


Examples:

Mixed Numbers:

?- max_list([3,7,2,9,5], Max).


Explanation: Returns Max = 9 after comparing all elements.

Single Element:

?- max_list([10], Max).


Explanation: With only one element, Max = 10.

Duplicate Maximums:

?- max_list([1,3,3,2], Max).


Explanation: The maximum value is 3.

3.8.12 Minimum Element (Recursive)

<a id="minimum-element-recursive"></a> What It Does:
Finds the smallest element in a list by recursively comparing elements.

Implementation:

min_list([X], X).
min_list([H|T], Min) :-
    min_list(T, TempMin),
    Min is min(H, TempMin).


Examples:

Mixed Numbers:

?- min_list([3,7,2,9,5], Min).


Explanation: Returns Min = 2 as the smallest element.

Single Element:

?- min_list([10], Min).


Explanation: With one element, Min = 10.

All Equal Elements:

?- min_list([4,4,4], Min).


Explanation: The minimum is 4.

3.8.13 Check if a List is Sorted

<a id="check-if-a-list-is-sorted"></a> What It Does:
Determines whether a list is sorted in non-decreasing order by comparing adjacent elements recursively.

Implementation:

is_sorted([]).
is_sorted([_]).
is_sorted([X, Y|T]) :-
    X =< Y,
    is_sorted([Y|T]).


Examples:

Sorted List:

?- is_sorted([1,2,2,3,4]).


Explanation: All adjacent pairs satisfy X =< Y. Succeeds.

Unsorted List:

?- is_sorted([3,2,1]).


Explanation: 3 =< 2 fails, so the predicate fails.

Empty List:

?- is_sorted([]).


Explanation: An empty list is considered sorted. Succeeds.

3.8.14 Quick Sort

<a id="quick-sort"></a> What It Does:
Quick sort partitions a list around a pivot, recursively sorts the partitions, and concatenates them. It is an alternative sorting algorithm with different performance characteristics compared to merge sort.

Implementation:

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


Examples:

General Case:

?- quicksort([3,6,2,5,4,1], Sorted).


Explanation: Partitions around a pivot and recursively sorts.
Output: Sorted = [1,2,3,4,5,6].

List with Negative Numbers:

?- quicksort([10, -1, 4, 3], Sorted).


Explanation: Returns Sorted = [-1,3,4,10].

Empty List:

?- quicksort([], Sorted).


Explanation: An empty list remains empty.
Output: Sorted = [].

3.8.15 Reverse a List with an Accumulator

<a id="reverse-a-list-with-an-accumulator"></a> What It Does:
This tail-recursive version of list reversal uses an accumulator to efficiently build the reversed list without using append repeatedly.

Implementation:

reverse_acc(List, Reversed) :-
    reverse_acc(List, [], Reversed).

reverse_acc([], Acc, Acc).
reverse_acc([H|T], Acc, Reversed) :-
    reverse_acc(T, [H|Acc], Reversed).


Examples:

Standard List:

?- reverse_acc([a,b,c,d], X).


Explanation: Builds reversed list using an accumulator.
Output: X = [d,c,b,a].

Empty List:

?- reverse_acc([], X).


Explanation: Reversing an empty list returns an empty list.

Numeric List:

?- reverse_acc([1,2,3], X).


Explanation: Returns X = [3,2,1].

3.8.16 Concatenate a List of Lists

<a id="concatenate-a-list-of-lists"></a> What It Does:
Concatenates multiple lists into a single list by recursively appending each sublist.

Implementation:

concat_lists([], []).
concat_lists([L|Ls], Result) :-
    concat_lists(Ls, Rest),
    append(L, Rest, Result).


Examples:

Multiple Sublists:

?- concat_lists([[a,b], [c], [d,e,f]], X).


Explanation: Concatenates to produce [a,b,c,d,e,f].

Including Empty Sublists:

?- concat_lists([[], [1,2], [3]], X).


Explanation: Returns [1,2,3].

Single Sublist:

?- concat_lists([[x,y,z]], X).


Explanation: Simply returns [x,y,z].

3.8.17 Interleave Two Lists

<a id="interleave-two-lists"></a> What It Does:
Interleaves two lists by pairing corresponding elements into a list of pairs. It stops when the shorter list is exhausted.

Implementation:

interleave([], [], []).
interleave([H1|T1], [H2|T2], [[H1,H2]|Rest]) :-
    interleave(T1, T2, Rest).


Examples:

Equal Length Lists:

?- interleave([a,b,c], [1,2,3], X).


Explanation: Combines to [[a,1],[b,2],[c,3]].

One List Shorter:

?- interleave([x,y], [true,false,true], X).


Explanation: Stops after pairing x and y, output: X = [[x,true],[y,false]].

Empty First List:

?- interleave([], [1,2,3], X).


Explanation: With an empty first list, result is X = [].

3.8.18 Sublist Check (Contiguous)

<a id="sublist-check-contiguous"></a> What It Does:
Verifies that a given list (the sublist) appears contiguously within another list by splitting the larger list around it.

Implementation:

sublist(Sub, List) :-
    append(_, L2, List),
    append(Sub, _, L2).


Examples:

Found Sublist:

?- sublist([b,c], [a,b,c,d]).


Explanation: [b,c] appears contiguously in [a,b,c,d].

Another Valid Case:

?- sublist([c,d], [a,b,c,d,e]).


Explanation: Succeeds since [c,d] is present.

Sublist Not Present:

?- sublist([x,y], [a,b,c]).


Explanation: Fails because [x,y] does not appear.

3.8.19 Permutations of a List

<a id="permutations-of-a-list"></a> What It Does:
Generates all possible orderings (permutations) of a list by recursively selecting an element and permuting the remainder.

Implementation:

permutation([], []).
permutation(List, [H|Perm]) :-
    select(H, List, Rest),
    permutation(Rest, Perm).


Examples:

Small List:

?- permutation([1,2,3], P).


Explanation: Generates permutations like [1,2,3], [1,3,2], etc.

List of Atoms:

?- permutation([a,b], P).


Explanation: Yields [a,b] and [b,a].

Empty List:

?- permutation([], P).


Explanation: The only permutation is [].

3.8.20 Zip Three Lists

<a id="zip-three-lists"></a> What It Does:
Combines three lists element-wise into a list of triples. The process stops when any one of the lists is exhausted.

Implementation:

zip3([], [], [], []).
zip3([H1|T1], [H2|T2], [H3|T3], [[H1,H2,H3]|Rest]) :-
    zip3(T1, T2, T3, Rest).


Examples:

All Lists Non-Empty:

?- zip3([a,b], [1,2], [x,y], X).


Explanation: Combines to produce X = [[a,1,x],[b,2,y]].

Longer Lists:

?- zip3([a,b,c], [1,2,3], [p,q,r], X).


Explanation: Yields X = [[a,1,p],[b,2,q],[c,3,r]].

One Empty List:

?- zip3([], [1,2], [x,y], X).


Explanation: With an empty first list, the result is X = [].

3.9 Creating Infinite and Limited Lists

<a id="creating-infinite-and-limited-lists"></a>

In Prolog, you can simulate infinite sequences using generator predicates that produce one element at a time on backtracking. For practical use, you often want to generate a finite (limited) list, either by specifying the number of elements or by constraining the range.

3.9.1 Finite Arithmetic Progressions

A. Fixed Number of Elements

<a id="fixed-number-of-elements"></a>

This predicate generates a list with exactly N elements, starting from a given value and increasing by a fixed step.

% arithmetic_progression_n(+Start, +Step, +N, -List)
arithmetic_progression_n(_, _, 0, []).
arithmetic_progression_n(Start, Step, N, [Start|Rest]) :-
    N > 0,
    NewN is N - 1,
    Next is Start + Step,
    arithmetic_progression_n(Next, Step, NewN, Rest).


Example 1: Generate 5 numbers starting from 2 with a step of 3.

?- arithmetic_progression_n(2, 3, 5, List).
% List = [2, 5, 8, 11, 14].


B. Bounded by a Maximum Value

<a id="bounded-by-a-maximum-value"></a>

This version builds the list until the next element would exceed a specified maximum.

% arithmetic_progression_between(+Start, +Step, +Max, -List)
arithmetic_progression_between(Start, _Step, Max, []) :-
    Start > Max.
arithmetic_progression_between(Start, Step, Max, [Start|Rest]) :-
    Start =< Max,
    Next is Start + Step,
    arithmetic_progression_between(Next, Step, Max, Rest).


Example 2: Generate all numbers from 2 up to 14 with a step of 3.

?- arithmetic_progression_between(2, 3, 14, List).
% List = [2, 5, 8, 11, 14].


3.9.2 Infinite Arithmetic Progressions (Generators)

<a id="infinite-arithmetic-progressions-generators"></a>

For simulating an infinite list, you can define a generator predicate that produces one element of the arithmetic progression per solution (via backtracking). Use these with care—the query won’t terminate unless you restrict it further.

% infinite_arithmetic(+Start, +Step, -Element)
infinite_arithmetic(Start, _Step, Start).
infinite_arithmetic(Start, Step, Element) :-
    Next is Start + Step,
    infinite_arithmetic(Next, Step, Element).


Example 3: Generate elements of an arithmetic sequence starting at 2 with step 3.

?- infinite_arithmetic(2, 3, X).
% On backtracking: X = 2 ; X = 5 ; X = 8 ; X = 11 ; ...


Tip: Use conditions in your query (e.g., X > 20, X < 30) to limit the results when using an infinite generator.

3.9.3 Practical Scenarios and Combined Usage

<a id="practical-scenarios-and-combined-usage"></a>

Scenario 1: Check Membership in an Arithmetic Sequence

You can generate a finite arithmetic sequence and then use the built-in member/2 predicate to check if a particular value belongs to it.

?- arithmetic_progression_n(2, 3, 10, List), member(11, List).
% List = [2,5,8,11,14,17,20,23,26,29],
% Succeeds since 11 is in the list.


Scenario 2: Filtering Values from a Sequence

Combine a generated arithmetic sequence with your filtering predicates. For example, filter the even numbers from a sequence.

% Define predicate to check even numbers.
is_even(X) :- 0 is X mod 2.

?- arithmetic_progression_between(2, 3, 30, List), filter(List, Evens, is_even).
% List = [2,5,8,11,14,17,20,23,26,29],
% Evens = [2,8,14,20,26].


Scenario 3: Using Sequences for Range Checks

You can represent ranges with arithmetic progressions, especially when the step is not 1.

?- arithmetic_progression_between(10, 5, 50, List).
% List = [10,15,20,25,30,35,40,45,50].


Scenario 4: Constraining an Infinite Generator

Apply additional conditions to the infinite generator to retrieve only the values you need.

?- infinite_arithmetic(2, 3, X), X > 20, X < 30.
% On backtracking, possible values: X = 23 ; X = 26.


Scenario 5: Using between/3 for Unit Steps

For consecutive integers (step = 1), Prolog’s built-in between/3 predicate is very useful:

?- between(2, 10, X).
% Generates X = 2, 3, 4, 5, 6, 7, 8, 9, 10.


Note: When the step is not 1, your arithmetic progression predicates are the way to go.

4. Advanced Predicates

<a id="advanced-predicates"></a>

4.1 findall/3

<a id="using-findall3-for-collecting-solutions"></a>

Purpose: Collect all possible solutions for a given goal.

Syntax:

findall(Template, Goal, List).


Examples:

?- findall(N, between(1,5,N), L).  
% L = [1,2,3,4,5].


 For SAT encodings:

relateDoesVarsWithBusyAtHourVars :-
    available(G,H),
    findall(does(G,T,H), task(T), Lits),
    expressOr(busyAtHour(G,H), Lits),
    fail.
relateDoesVarsWithBusyAtHourVars.


4.2 SAT & Constraint Encodings

<a id="sat--constraint-encodings"></a>

4.2.1 expressOr/2 and expressAnd/2

<a id="expressor2-and-expressand2"></a>

Purpose: Define logical relationships for SAT solving.

Syntax & Example:

expressOr(Var, Lits).
% Means Var <--> (Literal1 ∨ Literal2 ∨ ...)

expressAnd(Var, Lits).
% Means Var <--> (Literal1 ∧ Literal2 ∧ ...)


 You can implement these by writing out the logic (or by interfacing with a SAT solver).

4.2.2 Cardinality Constraints

<a id="cardinality-constraints"></a>

atLeast(K, Lits): At least K literals are true.

atMost(K, Lits): At most K literals are true.

exactly(K, Lits): Exactly K literals are true (often defined as a combination of atLeast/2 and atMost/2).

Example:

atLeast(2, [does(g01,T,H), does(g02,T,H), does(g03,T,H)]).
atMost(1, [does(G,killing,H), does(G,countingMoney,H), does(G,politics,H)]).
exactly(3, [does(g01,T,H), does(g02,T,H), does(g03,T,H), does(g04,T,H)]).


4.3 writeOneClause/1

<a id="clause-generation-with-writeoneclause1"></a>

Purpose: Outputs a single clause (a disjunction of literals) for SAT or CNF generation.

Example Implementation:

writeOneClause([]) :- write('0'), nl.
writeOneClause([Lit|Lits]) :-
    write(Lit), write(' '),
    writeOneClause(Lits).


4.4 Higher-Order Predicates

<a id="higher-order-predicates"></a>

maplist/2 and maplist/3:
Apply a predicate across a list (or multiple lists).

% Example: Doubling each element
double(X, Y) :- Y is X * 2.
?- maplist(double, [1,2,3], Doubled).  % Doubled = [2,4,6].


include/3 and exclude/3:
Filter lists by including or excluding elements that satisfy a predicate.

is_even(X) :- 0 is X mod 2.
?- include(is_even, [1,2,3,4], Evens).  % Evens = [2,4].
?- exclude(is_even, [1,2,3,4], Odds).    % Odds = [1,3].


4.5 Defining Custom Predicates

<a id="defining-custom-predicates"></a>

Example – Checking Availability:

available(G, H) :-
    gangster(G),
    hour(H),
    \+ blocked(G, H).


Example – Assigning Tasks:

assign_task(G, T, H) :-
    available(G, H),
    does(G, T, H).


4.6 Recursion Techniques

<a id="recursion-techniques"></a>

Break down complex operations into helper predicates and ensure base cases are covered (see examples under recursive list processing).

4.7 Error Handling and Guards

<a id="error-handling-and-guards"></a>

Example – Safe Division:

safe_divide(_, 0, _) :-
    write('Error: Division by zero.'), nl, fail.
safe_divide(X, Y, Z) :-
    Z is X / Y.


Usage:

?- safe_divide(10, 2, Z).  % Z = 5.
?- safe_divide(10, 0, Z).  % Error printed, fails.


4.8 Generator Predicates

<a id="generator-predicates"></a>

These predicates help generate values and iterate over ranges. They are particularly useful when you need to create choice points or enumerate all possibilities under given constraints.

4.8.1 between/3

<a id="between3"></a> Purpose: Generates an integer N such that Low =< N =< High. Prolog instantiates N one value at a time as you backtrack.

Example 1 – Generating a Range with a Non-Trivial Lower Bound:

?- between(4, 7, N).


Explanation:

First, Prolog unifies N with 4.

On backtracking, N becomes 5, then 6, and finally 7.

This is useful for iterating over the numbers 4 to 7.

Example 2 – Single-Value Range:

?- between(0, 0, N).


Explanation:

The lower and upper bounds are both 0, so there is only one possible value: N = 0.

Example 3 – Combining with a Computation:

?- between(3, 5, N), M is N * 10.


Explanation:

For N = 3, the computation yields M = 30;

then for N = 4, M = 40;

and for N = 5, M = 50.

This example shows how between/3 can be used to drive calculations in a loop-like fashion.

4.8.2 repeat/0

<a id="repeat0"></a> Purpose: repeat/0 always succeeds and creates an infinite choice point. It’s used to drive loops until a condition is met, at which point a cut (!) stops further backtracking.

Example 1 – Interactive Loop Until a Specific Word Is Entered:

?- repeat, write('Type stop to end: '), read(Input), Input == stop, !.


Explanation:

The loop repeatedly prompts the user to type a word.

It continues until the user enters stop.

When Input == stop succeeds, the cut (!) prevents further iterations.

Example 2 – Simulated Menu Loop:

menu_loop :-
    repeat,
    write('Menu: 1. Continue  2. Quit'), nl,
    read(Choice),
    (Choice = 2 -> ! ; (write('You chose to continue.'), nl, fail)).


Explanation:

This predicate shows a menu that repeats until the user selects option 2.

When Choice = 2 is true, the cut stops the loop; otherwise, it writes a message and fails to loop again.

Example 3 – Retry Until a Random Condition is Met:

?- repeat, random_between(1, 10, N), write('Generated: '), write(N), nl, N = 8, !.


Explanation:

Prolog repeatedly generates a random number between 1 and 10 and prints it.

The loop stops when the generated number is 8 (thanks to the cut).

This illustrates using repeat/0 for retrying a goal until a certain condition is satisfied.

4.8.3 bagof/3

<a id="bagof3"></a> Purpose: Collects all solutions for a goal into a list. It preserves duplicates and groups solutions by any free variables that are not explicitly quantified.

Example 1 – Collecting Numbers from a List:

?- bagof(X, member(X, [10,20,30,20]), L).


Explanation:

This gathers each solution for X that is a member of the list.

Output: L = [10,20,30,20] (notice the duplicate 20 is preserved).

Example 2 – Filtering with a Condition:

?- bagof(N, (member(N, [5,10,15,20]), N > 10), L).


Explanation:

Only numbers greater than 10 are collected.

Output: L = [15,20].

Example 3 – Grouping by Ignoring a Variable:

?- bagof(Y, X^(member((X, Y), [(red, apple), (yellow, banana), (red, cherry)])), L).


Explanation:

The X^ operator tells Prolog to ignore X when grouping results.

This collects all Y values from the pairs regardless of their corresponding color.

Output: L = [apple, banana, cherry].

4.8.4 setof/3

<a id="setof3"></a> Purpose: Like bagof/3, but returns a sorted list of unique solutions. It fails if no solution exists.

Example 1 – Unique Sorted Numbers:

?- setof(X, member(X, [4,2,8,4,6,2]), L).


Explanation:

Prolog collects all members, removes duplicates, and sorts them.

Output: L = [2,4,6,8].

Example 2 – Unique Sorted Words:

?- setof(Word, member(Word, [hello, world, hello, prolog]), L).


Explanation:

Collects the words, removes the duplicate hello, and sorts them alphabetically.

Output: L = [hello, prolog, world].

Example 3 – Grouping Tuples:

?- setof((X, Y), member((X, Y), [(a,1), (b,2), (a,3), (c,1)]), L).


Explanation:

This gathers all unique pairs (tuples) and sorts them according to Prolog’s standard term order.

Output: L = [(a,1), (a,3), (b,2), (c,1)].

4.8.5 succ/2

<a id="succ2"></a> Purpose: Establishes a successor relationship between two integers. The predicate succ(N, M) succeeds if M is exactly one more than N (i.e., M = N + 1).

Example 1 – Direct Successor Calculation:

?- succ(7, N).


Explanation:

Prolog unifies N with 8, because 8 is the immediate successor of 7.

Example 2 – Reverse Query to Find a Predecessor:

?- succ(X, 5).


Explanation:

This query asks for a value X such that X + 1 = 5.

Prolog unifies X with 4.

Example 3 – Chaining Successors in a Calculation:

?- succ(A, B), succ(B, C), A = 10.


Explanation:

First, with A = 10, Prolog sets B = 11 (since succ(10, 11) succeeds).

Then, succ(11, C) unifies C with 12.

This shows how you can chain successor relationships to generate a sequence.

5. Extra Self-Declarated Predicates

<a id="5-extra-self-declarated-predicates"></a>

5.1 inside/2

<a id="51-inside2"></a>

Purpose: Checks whether the first list is a subsequence of the second (i.e. all its elements appear in order, though not necessarily consecutively).

Definition:

inside([], _).
inside([X|S], L) :-
    append(_, [X|R], L),
    inside(S, R).


Examples:

Simple Positive Case:

?- inside([2,4], [1,2,3,4,5]).


Explanation: 2 appears in the list, and after it 4 also appears. The query succeeds.

Non-Consecutive Elements:

?- inside([a,c,e], [a,b,c,d,e]).


Explanation: Even though the elements aren’t adjacent, they occur in order (a then c then e). Succeeds.

Order Violation (Failing Case):

?- inside([3,2], [1,2,3,4]).


Explanation: Although both 3 and 2 are present, they do not occur in the order [3,2]. The query fails.

5.2 prefix/2

<a id="52-prefix2"></a>

Purpose: Determines if the first list is a prefix of the second list.

Definition:

prefix([], _).
prefix([H|T], [H|Rest]) :-
    prefix(T, Rest).


Examples:

Valid Prefix:

?- prefix([a,b], [a,b,c,d]).


Explanation: The list [a,b] exactly appears at the start of [a,b,c,d]. Succeeds.

Empty List as Prefix:

?- prefix([], [1,2,3]).


Explanation: An empty list is a prefix of any list. Succeeds.

Non-Prefix Case:

?- prefix([2,3], [1,2,3]).


Explanation: [2,3] does not appear at the beginning of [1,2,3]. The query fails.

5.3 suffix/2

<a id="53-suffix2"></a>

Purpose: Checks whether the first list is a suffix (ending segment) of the second list.

Definition:

suffix(S, S).
suffix(S, [_|Tail]) :-
    suffix(S, Tail).


Examples:

Valid Suffix:

?- suffix([c,d], [a,b,c,d]).


Explanation: The list [c,d] appears at the end of [a,b,c,d]. Succeeds.

Single-Element Suffix:

?- suffix([d], [a,b,c,d]).


Explanation: [d] is the ending element. Succeeds.

Not a Suffix:

?- suffix([b,d], [a,b,c,d]).


Explanation: Although both b and d occur in the list, they aren’t contiguous at the end. The query fails.

5.4 rotate_left/2

<a id="54-rotate_left2"></a>

Purpose: Rotates a list left by moving the first element to the end.

Definition:

rotate_left([], []).
rotate_left([H|T], Rotated) :-
    append(T, [H], Rotated).


Examples:

Standard Rotation:

?- rotate_left([1,2,3,4], X).


Explanation: Moves 1 to the end resulting in [2,3,4,1].

Single-Element List:

?- rotate_left([a], X).


Explanation: A one-element list remains the same ([a]). Succeeds.

Empty List:

?- rotate_left([], X).


Explanation: Rotating an empty list yields an empty list.

5.5 rotate_right/2

<a id="55-rotate_right2"></a>

Purpose: Rotates a list right by moving the last element to the front.

Definition:

rotate_right([], []).
rotate_right(List, Rotated) :-
    append(Rest, [Last], List),
    Rotated = [Last|Rest].


Examples:

Standard Rotation:

?- rotate_right([1,2,3,4], X).


Explanation: Moves 4 to the front, yielding [4,1,2,3].

Single-Element List:

?- rotate_right([a], X).


Explanation: A single-element list remains unchanged ([a]).

Empty List:

?- rotate_right([], X).


Explanation: Rotating an empty list produces [].

5.6 remove_duplicates/2

<a id="56-remove_duplicates2"></a>

Purpose: Removes duplicate elements from a list while preserving the order of their first appearance.

Definition:

remove_duplicates([], []).
remove_duplicates([H|T], [H|Result]) :-
    \+ member(H, T),
    remove_duplicates(T, Result).
remove_duplicates([H|T], Result) :-
    member(H, T),
    remove_duplicates(T, Result).


Examples:

Removing Duplicates from Atoms:

?- remove_duplicates([a,b,a,c,b], X).


Explanation: Returns [a,b,c] by keeping the first occurrences of a, b, and c.

Numeric List Example:

?- remove_duplicates([1,2,2,3,1], X).


Explanation: Succeeds with X = [1,2,3].

Empty List:

?- remove_duplicates([], X).


Explanation: Removing duplicates from an empty list yields [].

5.7 substitute/4

<a id="57-substitute4"></a>

Purpose: Replaces every occurrence of a specified element (Old) in a list with a new element (New).

Definition:

substitute(_, _, [], []).
substitute(Old, New, [Old|T], [New|Result]) :-
    substitute(Old, New, T, Result).
substitute(Old, New, [H|T], [H|Result]) :-
    H \= Old,
    substitute(Old, New, T, Result).


Examples:

Replacing Atoms:

?- substitute(a, x, [a,b,a,c], X).


Explanation: Replaces all occurrences of a with x, resulting in [x,b,x,c].

Replacing Numbers:

?- substitute(2, 9, [1,2,3,2,4], X).


Explanation: Returns [1,9,3,9,4] by replacing 2 with 9.

No Replacement Needed:

?- substitute(z, q, [p,q,r], X).


Explanation: Since z is not present, the list remains unchanged: [p,q,r].

5.8 replace_at/4

<a id="58-replace_at4"></a>

Purpose: Replaces the element at a given 1‑indexed position with a new element.

Definition:

replace_at(1, [_|T], New, [New|T]).
replace_at(Pos, [H|T], New, [H|Result]) :-
    Pos > 1,
    Pos1 is Pos - 1,
    replace_at(Pos1, T, New, Result).


Examples:

Replacing at Position 2:

?- replace_at(2, [a,b,c,d], x, X).


Explanation: Replaces the second element (b) with x to yield [a,x,c,d].

Replacing at the First Position:

?- replace_at(1, [1,2,3], 9, X).


Explanation: The first element is replaced, resulting in [9,2,3].

Replacing at the Last Position:

?- replace_at(4, [w,x,y,z], a, X).


Explanation: The fourth element (z) is replaced with a, yielding [w,x,y,a].

5.9 nth0/3

<a id="59-nth03"></a>

Purpose: Retrieves the element at the given index using 0‑based indexing.

Definition:

nth0(0, [H|_], H).
nth0(N, [_|T], Element) :-
    N > 0,
    N1 is N - 1,
    nth0(N1, T, Element).


Examples:

Accessing the Third Element:

?- nth0(2, [a,b,c,d], X).


Explanation: Index 2 (third element) is c, so X = c.

Accessing the First Element:

?- nth0(0, [x,y,z], X).


Explanation: Retrieves the element at index 0, X = x.

Another Example:

?- nth0(3, [1,2,3,4,5], X).


Explanation: The element at index 3 is 4, so X = 4.

5.10 duplicate/3

<a id="510-duplicate3"></a>

Purpose: Creates a list containing a given element duplicated a specified number of times.

Definition:

duplicate(_, 0, []).
duplicate(Element, Count, [Element|Rest]) :-
    Count > 0,
    Count1 is Count - 1,
    duplicate(Element, Count1, Rest).


Examples:

Duplicate an Atom Three Times:

?- duplicate(a, 3, X).


Explanation: Generates X = [a,a,a].

Zero Duplicates:

?- duplicate(5, 0, X).


Explanation: With a count of 0, X = [].

Duplicate a Number Four Times:

?- duplicate(2, 4, X).


Explanation: Produces X = [2,2,2,2].

Below is your complete master cheat sheet with a new section at the end—“6. Useful Predicates.” In this section you’ll find the definition of an all_different/1 predicate (to check that all elements in a list are distinct) plus nine other predicates that you might find useful when working with lists in Prolog.

6. Useful Predicates

This section collects several predicates that you may find useful in your Prolog programs. It includes a predicate to verify that all elements in a list are different and nine additional predicates that address common list‐processing tasks.

6.1 all_different/1

Purpose:
Succeeds if every element in a list is unique (i.e. no duplicates).

Definition:

all_different([]).
all_different([X|Xs]) :-
    all(different(X), Xs),
    all_different(Xs).

different(X, Y) :-
    X \= Y.


Example:

?- all_different([a, b, c]).
true.

?- all_different([a, b, a]).
false.


6.2 list_difference/3

Purpose:
Computes the difference between two lists (elements in List1 that are not in List2).

Definition:

list_difference([], _, []).
list_difference([H|T], L2, Diff) :-
    member(H, L2),
    list_difference(T, L2, Diff).
list_difference([H|T], L2, [H|Diff]) :-
    \+ member(H, L2),
    list_difference(T, L2, Diff).


Example:

?- list_difference([a,b,c,d], [b,d], Diff).
Diff = [a, c].


6.3 cartesian_product/3

Purpose:
Generates the Cartesian product of two lists as a list of pairs.

Definition:

cartesian_product(L1, L2, Product) :-
    findall([X, Y], (member(X, L1), member(Y, L2)), Product).


Example:

?- cartesian_product([a,b], [1,2], P).
P = [[a,1], [a,2], [b,1], [b,2]].


6.4 find_duplicates/2

Purpose:
Returns a list of elements that appear more than once in the input list.

Definition:

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


Example:

?- find_duplicates([a,b,a,c,b,d], Dups).
Dups = [a, b].


6.5 replace_all/4

Purpose:
Replaces all occurrences of an element (Old) with a new element (New) in a list.

Definition:

replace_all(_, _, [], []).
replace_all(Old, New, [Old|T], [New|R]) :-
    replace_all(Old, New, T, R).
replace_all(Old, New, [H|T], [H|R]) :-
    H \= Old,
    replace_all(Old, New, T, R).


Example:

?- replace_all(a, x, [a,b,a,c], Result).
Result = [x, b, x, c].


6.6 transpose/2

Purpose:
Transposes a matrix represented as a list of lists (all sublists must be of equal length).

Definition:

transpose([], []).
transpose([[]|_], []).
transpose(Matrix, [Row|Rows]) :-
    transpose_1(Matrix, Row, RestMatrix),
    transpose(RestMatrix, Rows).

transpose_1([], [], []).
transpose_1([[H|T]|Rows], [H|Hs], [T|Ts]) :-
    transpose_1(Rows, Hs, Ts).


Example:

?- transpose([[1,2,3], [4,5,6], [7,8,9]], T).
T = [[1,4,7], [2,5,8], [3,6,9]].


6.7 power_set/2

Purpose:
Generates the power set (set of all subsets) of a given list.

Definition:

power_set([], [[]]).
power_set([H|T], PSet) :-
    power_set(T, PT),
    findall([H|Subset], member(Subset, PT), WithH),
    append(PT, WithH, PSet).


Example:

?- power_set([a,b], PS).
PS = [[], [b], [a], [a,b]].


6.8 group_by/3

Purpose:
Groups elements of a list into sublists based on a key computed by a given predicate.

Definition:
(This implementation uses a helper predicate to build key–group pairs.)

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


Example:
Group numbers by even/odd:

even_odd(X, even) :- 0 is X mod 2.
even_odd(X, odd)  :- 1 is X mod 2.

?- group_by(even_odd, [1,3,2,4,5,6], Groups).
Groups = [odd-[1,3], even-[2,4,6], odd-[5]].


6.9 split_by/3

Purpose:
Splits a list into two lists: one with elements that satisfy a predicate and one with those that do not.

Definition:

split_by(_, [], [], []).
split_by(Pred, [H|T], [H|Yes], No) :-
    call(Pred, H), !,
    split_by(Pred, T, Yes, No).
split_by(Pred, [H|T], Yes, [H|No]) :-
    split_by(Pred, T, Yes, No).


Example:

?- split_by(is_even, [1,2,3,4,5,6], Evens, Odds).
Evens = [2,4,6],
Odds = [1,3,5].

is_even(X) :- 0 is X mod 2.


6.10 chunk/3

Purpose:
Divides a list into chunks (sublists) of a specified size. The last chunk may be smaller if there aren’t enough elements.

Definition:

chunk([], _, []).
chunk(List, N, [Chunk|Rest]) :-
    N > 0,
    take(N, List, Chunk),
    drop(N, List, Remainder),
    chunk(Remainder, N, Rest).


(Here, you can use the previously defined take/3 and drop/3 predicates.)

Example:

?- chunk([1,2,3,4,5,6,7], 3, Chunks).
Chunks = [[1,2,3], [4,5,6], [7]].


6.11 push_back/3 and push_front/3

Purpose:
push_back/3 appends an element to the end of a list, while push_front/3 prepends an element to the beginning of a list.

Definition:

% push_back(+List, +Elem, -NewList)
% Given a list and an element, NewList is the list with Elem appended.
push_back(List, Elem, NewList) :-
    append(List, [Elem], NewList).

% push_front(+Elem, +List, -NewList)
% Given a list and an element, NewList is the list with Elem added at the front.
push_front(Elem, List, [Elem|List]).


Example:

?- push_back([a,b,c], d, X).
X = [a,b,c,d].

?- push_front(d, [a,b,c], Y).
Y = [d,a,b,c].


