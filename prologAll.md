# Prolog Cheat Sheet

A comprehensive cheat sheet covering Prolog concepts from basic syntax to advanced topics. Use this as a quick reference for your exams and future projects.

## Table of Contents

1. [Prolog Basic Syntax](#1-prolog-basic-syntax)
   1. [Basic Prolog](#11-basic-prolog)
   2. [Operators](#12-operators)
   3. [Lists](#13-lists)
   4. [Advanced Predicates](#14-advanced-predicates)
2. [SAT Optimization](#2-sat-optimization)
3. [Advanced Prolog (Graph Manipulation)](#3-advanced-prolog-graph-manipulation)
4. [Constraint Logic Programming (CLP) FD - Pseudo-Boolean Constraints](#4-constraint-logic-programming-clp-fd---pseudo-boolean-constraints)

---

## 1. Prolog Basic Syntax

Understanding the foundational elements of Prolog is crucial for writing effective programs. This section covers the essentials, including facts, rules, variables, predicates, lists, comments, control constructs, and useful built-in predicates.

### 1.1. Basic Prolog

#### **1.1.1. Facts**

- **Definition**: Basic assertions about the world that are unconditionally true.
- **Syntax**: `predicate(argument1, argument2, ...).`
- **Example**:
  ```prolog
  % Declares that g01 is a gangster
  gangster(g01).
  
  % Specifies the hours when g01 is not available
  notAvailable(g01, [6,13,14,16,21,35,37,41,59]).
  ```

#### **1.1.2. Rules**

- **Definition**: Logical implications that define relationships between facts and derive new information.
- **Syntax**: `Head :- Body.`
- **Example**:
  ```prolog
  % A gangster is available at hour H if H is a valid hour,
  % the individual is a gangster, and they are not blocked at H
  available(G, H) :- hour(H), gangster(G), \+ blocked(G, H).
  ```

#### **1.1.3. Variables**

- **Definition**: Placeholders that can be unified with constants, atoms, or other variables.
- **Naming**: Start with uppercase letters or underscores (`_`). Variables are case-sensitive.
- **Examples**:
  - `G`, `H`, `Cost`
  - `_Temp`, `X1`, `Y_variable`

#### **1.1.4. Predicates**

- **Definition**: Relations defined by facts and rules. They are the primary means of computation and querying in Prolog.
- **Usage**: Represent properties, relationships, or actions.
- **Example**:
  ```prolog
  % Defines a task with identifier T
  task(T).
  
  % Specifies that task T is needed at hour H with quantity N
  needed(T, H, N).
  ```

#### **1.1.5. Comments**

- **Single-line Comments**: Use `%` to start a comment. Everything after `%` on the same line is ignored.
  ```prolog
  % This is a single-line comment
  gangster(g01). % Declares g01 as a gangster
  ```
  
- **Multi-line Comments**: Enclosed between `/*` and `*/`.
  ```prolog
  /*
    This is a multi-line comment.
    It can span multiple lines.
  */
  ```

#### **1.1.6. Control Constructs**

- **Cut (`!`)**
  - **Definition**: Prunes Prolog's search tree, preventing backtracking beyond the point where the cut appears.
  - **Usage**:
    ```prolog
    % Prevents Prolog from considering alternative solutions once condition is met
    predicate :- condition, !, action.
    ```
  - **Example**:
    ```prolog
    max(X, Y, X) :- X >= Y, !.
    max(X, Y, Y) :- X < Y.
    ```
  
- **Failure (`fail`)**
  - **Definition**: Forces a predicate to fail, useful for iterative processes or generating multiple solutions.
  - **Usage**:
    ```prolog
    somePredicate :- action, fail.
    somePredicate.
    ```
  - **Example**:
    ```prolog
    % Prints all elements in a list
    print_all([]).
    print_all([H|T]) :- write(H), nl, print_all(T).
    
    % Alternative using fail for backtracking
    print_all_fail(List) :-
        member(X, List),
        write(X), nl,
        fail.
    print_all_fail(_).
    ```


### 1.2 Operators

#### **1.2. Operators**

Operators in Prolog are symbols that represent various operations, such as arithmetic calculations, comparisons, and logical operations. Understanding these operators is essential for writing effective and efficient Prolog programs.

##### **1.2.1. Arithmetic Operators**

Prolog provides a range of arithmetic operators for performing calculations. These operators evaluate arithmetic expressions and are used within arithmetic goals.

- **Addition (`+`)**
  - **Usage**: Adds two numbers.
  - **Example**:
    ```prolog
    ?- X is 2 + 3.
    X = 5.
    ```

- **Subtraction (`-`)**
  - **Usage**: Subtracts one number from another.
  - **Example**:
    ```prolog
    ?- X is 5 - 2.
    X = 3.
    ```

- **Multiplication (`*`)**
  - **Usage**: Multiplies two numbers.
  - **Example**:
    ```prolog
    ?- X is 4 * 3.
    X = 12.
    ```

- **Division (`/`)**
  - **Usage**: Divides one number by another, resulting in a floating-point number.
  - **Example**:
    ```prolog
    ?- X is 10 / 4.
    X = 2.5.
    ```

- **Integer Division (`//`)**
  - **Usage**: Divides one number by another, resulting in an integer by truncating the decimal.
  - **Example**:
    ```prolog
    ?- X is 10 // 4.
    X = 2.
    ```

- **Modulus (`mod`)**
  - **Usage**: Finds the remainder of the division of one number by another.
  - **Example**:
    ```prolog
    ?- X is 10 mod 4.
    X = 2.
    ```

- **Exponentiation (`^`)**
  - **Usage**: Raises a number to the power of another number.
  - **Example**:
    ```prolog
    ?- X is 2 ^ 3.
    X = 8.
    ```

##### **1.2.2. Comparison Operators**

Comparison operators are used to compare two arithmetic expressions. They do not evaluate to a value but instead succeed or fail based on the comparison.

- **Equal (`=:=`)**
  - **Definition**: Succeeds if both arithmetic expressions evaluate to the same number.
  - **Example**:
    ```prolog
    ?- 2 + 3 =:= 5.
    true.
    
    ?- 2 + 2 =:= 5.
    false.
    ```

- **Not Equal (`=\=`)**
  - **Definition**: Succeeds if both arithmetic expressions do not evaluate to the same number.
  - **Example**:
    ```prolog
    ?- 2 + 2 =\= 5.
    true.
    
    ?- 2 + 3 =\= 5.
    false.
    ```

- **Less Than (`<`)**
  - **Definition**: Succeeds if the first arithmetic expression is less than the second.
  - **Example**:
    ```prolog
    ?- 3 < 5.
    true.
    
    ?- 5 < 3.
    false.
    ```

- **Greater Than (`>`)**
  - **Definition**: Succeeds if the first arithmetic expression is greater than the second.
  - **Example**:
    ```prolog
    ?- 5 > 3.
    true.
    
    ?- 3 > 5.
    false.
    ```

- **Less Than or Equal (`=<`)**
  - **Definition**: Succeeds if the first arithmetic expression is less than or equal to the second.
  - **Example**:
    ```prolog
    ?- 3 =< 5.
    true.
    
    ?- 5 =< 5.
    true.
    
    ?- 6 =< 5.
    false.
    ```

- **Greater Than or Equal (`>=`)**
  - **Definition**: Succeeds if the first arithmetic expression is greater than or equal to the second.
  - **Example**:
    ```prolog
    ?- 5 >= 3.
    true.
    
    ?- 5 >= 5.
    true.
    
    ?- 3 >= 5.
    false.
    ```

##### **1.2.3. Logical Operators**

- **Comma (`,`)**: Logical AND
  - **Usage**: Combines multiple goals that must all succeed.
  - **Example**:
    ```prolog
    % A person is eligible if they are over 18 and have a valid ID
    eligible(Person) :- over_18(Person), has_valid_id(Person).
    ```

- **Semicolon (`;`)**: Logical OR
  - **Usage**: Offers alternative goals where at least one must succeed.
  - **Example**:
    ```prolog
    % A person can be a student or a teacher
    role(Person, student) :- student(Person).
    role(Person, teacher) :- teacher(Person).
    ```

- **Negation as Failure (`\+`)**
  - **Definition**: Succeeds if the enclosed goal fails.
  - **Usage**:
    ```prolog
    % A person is not a student
    not_student(Person) :- \+ student(Person).
    ```
    

##### **1.2.4. Unification and Equality Operators**

These operators are used for pattern matching and variable binding.

- **Unification (`=`)**
  - **Definition**: Attempts to make two terms identical by finding a suitable substitution for variables.
  - **Usage**: Binds variables to values to satisfy equality.
  - **Example**:
    ```prolog
    ?- X = 5.
    X = 5.
    
    ?- X = Y.
    X = Y.
    
    ?- X = 5, Y = X.
    X = 5,
    Y = 5.
    ```

- **Strict Equality (`==`)**
  - **Definition**: Succeeds if both terms are identical without performing any variable bindings.
  - **Usage**: Checks if two terms are exactly the same.
  - **Example**:
    ```prolog
    ?- 5 == 5.
    true.
    
    ?- X == 5.
    false.
    
    ?- X = 5, X == 5.
    X = 5.
    ```

- **Strict Inequality (`\==`)**
  - **Definition**: Succeeds if two terms are not identical without performing any variable bindings.
  - **Usage**: Checks if two terms are different.
  - **Example**:
    ```prolog
    ?- 5 \== 3.
    true.
    
    ?- X \== 5.
    true.
    
    ?- X = 5, X \== 5.
    false.
    ```

##### **1.2.5. Arithmetic Evaluation (`is`)**

- **Definition**: Evaluates an arithmetic expression on the right-hand side and unifies it with the variable on the left-hand side.
- **Usage**: Assigns the result of an arithmetic calculation to a variable.
- **Example**:
  ```prolog
  ?- X is 2 + 3.
  X = 5.
  
  ?- Y is X * 2, X is 4.
  Y = 8,
  X = 4.
  
  ?- Z is 10 / 3.
  Z = 3.3333333333333335.
  ```

##### **1.2.6. Operator Precedence and Associativity**

Understanding operator precedence and associativity is crucial to correctly interpret complex expressions.

- **Precedence**: Determines the order in which operators are evaluated.
- **Associativity**: Determines how operators of the same precedence are grouped in the absence of parentheses.

| Operator               | Precedence | Associativity |
|------------------------|------------|---------------|
| `is`                   | 700        | xfx           |
| `=:=`, `=\=`           | 700        | xfx           |
| `<`, `>`, `=<`, `>=`   | 700        | xfx           |
| `+`, `-`               | 500        | yfx           |
| `*`, `/`, `//`, `mod`, `^` | 400  | yfx           |
| `\+`                   | 200        | fy            |
| `,`                    | 100        | xfy           |
| `;`                    | 100        | xfy           |

- **Example of Precedence**:
  ```prolog
  ?- X is 2 + 3 * 4.
  X = 14. % Multiplication has higher precedence than addition
  
  ?- Y is (2 + 3) * 4.
  Y = 20. % Parentheses override precedence
  ```

##### **1.2.7. Logical Implications (`:-`)**

- **Definition**: Represents a rule where the head is true if the body is true.
- **Usage**: Defines relationships and dependencies between predicates.
- **Example**:
  ```prolog
  % A person is happy if they have money and are healthy
  happy(Person) :- has_money(Person), healthy(Person).
  ```

### 1.3. Lists

Lists are fundamental data structures in Prolog, representing ordered collections of elements. They are versatile and widely used in various algorithms and data manipulations. This section provides an organized overview of list operations, predicates, and typical examples to enhance your Prolog programming skills.

---

#### **1.3.1. List Basics**

- **Definition**: Ordered sequences of elements, which can be heterogeneous (different types).
- **Syntax**: `[Element1, Element2, ..., ElementN]`
- **Examples**:
  ```prolog
  % Empty list
  []
  
  % List of integers
  [1, 2, 3, 4]
  
  % Mixed list
  [g01, "task", 3, [sublist]].
  ```

---

#### **1.3.2. Head and Tail**

- **Definition**: The first element of a list is the **head**, and the remainder of the list is the **tail**.
- **Syntax**: `[Head|Tail]`
- **Examples**:
  ```prolog
  % Splitting a list into head and tail
  [H|T] = [a, b, c, d].
  % H = a
  % T = [b, c, d]
  ```

---

#### **1.3.3. Common List Predicates**

These predicates are essential for basic list manipulations and queries. Each predicate is discussed with its different usage modes based on which arguments are instantiated (known) or variables.

##### **1.3.3.1. `member/2`**

- **Purpose**: Checks if an element is part of a list.
- **Syntax**: `member(Element, List).`
- **Usage Modes**:
  
  1. **Element Known, List Known**:
     - **Query**: Is `Element` in `List`?
     - **Example**:
       ```prolog
       ?- member(b, [a, b, c]).
       true.
       
       ?- member(d, [a, b, c]).
       false.
       ```
  
  2. **Element Unknown, List Known**:
     - **Query**: Retrieve elements from `List` one by one.
     - **Example**:
       ```prolog
       ?- member(X, [a, b, c]).
       X = a ;
       X = b ;
       X = c ;
       false.
       ```
  
  3. **List Variable**:
     - **When `List` is a variable**, `member/2` can generate lists containing the `Element` at various positions.
     - **Example**:
       ```prolog
       ?- member(a, L).
       L = [a|_] ;
       L = [_G1, a|_] ;
       L = [_G1, _G2, a|_] ;
       ...
       ```
     - **Note**: This can lead to infinite possibilities; use with care.

- **Common Pitfalls**:
  - **Infinite Lists**: Using `member/2` with infinite lists can cause Prolog to loop indefinitely.
  - **Instantiation Errors**: Ensuring that at least one argument is sufficiently instantiated to avoid excessive backtracking.

---

##### **1.3.3.2. `memberchk/2`**

- **Purpose**: Similar to `member/2` but stops after finding the first occurrence (non-backtracking).
- **Syntax**: `memberchk(Element, List).`
- **Usage Modes**:
  
  1. **Element Known, List Known**:
     - **Example**:
       ```prolog
       ?- memberchk(b, [a, b, c, b]).
       true.
       ```
  
  2. **Element Unknown, List Known**:
     - **Not typically used in this mode**, as `memberchk/2` is designed to find the first occurrence without backtracking.
  
  3. **List Variable**:
     - **Usage**: Less common; behaves similarly to `member/2` but without backtracking.
     - **Example**:
       ```prolog
       ?- memberchk(X, [a, b, c]).
       X = a.
       ```

- **Key Differences from `member/2`**:
  - **Efficiency**: Faster when only one solution is needed.
  - **Deterministic**: Does not produce multiple solutions upon backtracking.

---

##### **1.3.3.3. `append/3`**

- **Purpose**: Concatenates two lists or decomposes a list into parts.
- **Syntax**: `append(List1, List2, Result).`
- **Usage Modes**:

  1. **Concatenation**: Both `List1` and `List2` are known; find `Result`.
     - **Example**:
       ```prolog
       ?- append([a, b], [c, d], X).
       X = [a, b, c, d].
       ```
  
  2. **Decomposition**: `Result` is known; find `List1` and `List2`.
     - **Example**:
       ```prolog
       ?- append(X, Y, [1, 2, 3]).
       X = [],
       Y = [1, 2, 3];
       X = [1],
       Y = [2, 3];
       X = [1, 2],
       Y = [3];
       X = [1, 2, 3],
       Y = [].
       ```
  
  3. **Partial Instantiation**:
     - **One List Known, One Variable**:
       ```prolog
       ?- append([a, b], Y, [a, b, c, d]).
       Y = [c, d].
       
       ?- append(X, [c, d], [a, b, c, d]).
       X = [a, b].
       ```
  
  4. **List Variable**:
     - **When `List1` or `List2` is a variable**, Prolog can generate possible lists that satisfy the `append/3` relationship.
     - **Example**:
       ```prolog
       ?- append(X, [d], [a, b, c, d]).
       X = [a, b, c].
       ```
       ```prolog
       ?- append([a|X], [d], [a, b, c, d]).
       X = [b, c].
       ```

- **Advanced Usage**:
  - **Reversing a List**:
    ```prolog
    reverse_list(List, Reversed) :-
        append(Rest, [Last], List),
        reverse_list(Rest, ReversedTail),
        append(ReversedTail, [Last], Reversed).
    reverse_list([], []).
    ```
  
  - **Splitting a List**:
    ```prolog
    split_list(List, Left, Right) :-
        append(Left, Right, List).
    ```

- **Common Pitfalls**:
  - **Ambiguity**: Using `append/3` with multiple variables can lead to ambiguous or infinite solutions.
    ```prolog
    ?- append(X, Y, [a, b, c]).
    X = [],
    Y = [a, b, c];
    X = [a],
    Y = [b, c];
    ...
    ```
  - **Performance**: Excessive backtracking when deconstructing large lists can be computationally expensive.

- **Best Practices**:
  - **Instantiate as Much as Possible**: Provide as much information as possible to reduce unnecessary backtracking.
  - **Use Deterministic Predicates**: When only one solution is needed, consider using `memberchk/2` instead of `member/2`, or structuring `append/3` queries to be more specific.

---

##### **1.3.3.4. `length/2`**

- **Purpose**: Determines the length of a list or creates a list of a certain length.
- **Syntax**: `length(List, Length).`
- **Usage Modes**:

  1. **Length Known, List Variable**:
     - **Purpose**: Generate a list of a specific length with variables as elements.
     - **Example**:
       ```prolog
       ?- length(X, 3).
       X = [_G1, _G2, _G3].
       ```
  
  2. **List Known, Length Variable**:
     - **Purpose**: Determine the length of a given list.
     - **Example**:
       ```prolog
       ?- length([a, b, c], N).
       N = 3.
       ```
  
  3. **Both List and Length Known**:
     - **Verification**: Check if the list has the specified length.
     - **Example**:
       ```prolog
       ?- length([a, b, c], 3).
       true.
       
       ?- length([a, b, c], 2).
       false.
       ```
  
  4. **List Variable, Length Variable**:
     - **Usage**: Less common; can generate lists and their lengths simultaneously.
     - **Example**:
       ```prolog
       ?- length(X, Y).
       X = [],
       Y = 0;
       X = [_G1],
       Y = 1;
       X = [_G1, _G2],
       Y = 2;
       ...
       ```

- **Common Pitfalls**:
  - **Infinite Lists**: Attempting to determine the length of an infinite list will result in non-termination.
    ```prolog
    ?- length([a|X], N).
    % Prolog will attempt to find a list X such that [a|X] has length N, leading to infinite possibilities.
    ```
  - **Instantiating Both Arguments**: Providing both `List` and `Length` can be redundant and may lead to unnecessary checks.

- **Best Practices**:
  - **Avoid Determining Length of Infinite or Unbounded Lists**.
  - **Use with Proper Instantiation**: Ensure that at least one argument is sufficiently instantiated to prevent infinite loops.

---

##### **1.3.3.5. `nth1/3`**

- **Purpose**: Retrieves the N-th element from a list (1-based indexing).
- **Syntax**: `nth1(Index, List, Element).`
- **Usage Modes**:

  1. **Index Known, List Known, Element Variable**:
     - **Purpose**: Retrieve the element at the specified position.
     - **Example**:
       ```prolog
       ?- nth1(2, [a, b, c], X).
       X = b.
       
       ?- nth1(4, [a, b, c], X).
       false.
       ```
  
  2. **Element Known, List and Index Variable**:
     - **Purpose**: Find all positions of a given element in the list.
     - **Example**:
       ```prolog
       ?- nth1(Pos, [a, b, c, b], b).
       Pos = 2 ;
       Pos = 4 ;
       false.
       ```
  
  3. **List Known, Element and Index Variable**:
     - **Purpose**: Retrieve the element and its position.
     - **Example**:
       ```prolog
       ?- nth1(Pos, [a, b, c], Element).
       Pos = 1,
       Element = a ;
       Pos = 2,
       Element = b ;
       Pos = 3,
       Element = c ;
       false.
       ```
  
  4. **Index Variable, Element Variable, List Known**:
     - **Usage**: Not common; useful for generating all possible (Index, Element) pairs.
     - **Example**:
       ```prolog
       ?- nth1(Pos, [a, b, c], Element).
       Pos = 1,
       Element = a ;
       Pos = 2,
       Element = b ;
       Pos = 3,
       Element = c ;
       false.
       ```

- **Common Pitfalls**:
  - **Out of Bounds Indices**: Requesting an index beyond the list's length results in `false`.
  - **Zero or Negative Indices**: `nth1/3` uses 1-based indexing; indices less than 1 are invalid.

- **Best Practices**:
  - **Ensure Valid Indices**: Validate that the index is within the bounds of the list.
  - **Use with Known Indices**: To retrieve elements efficiently, prefer having the index instantiated.

---

##### **1.3.3.6. `findall/3`**

- **Purpose**: Collects all possible solutions that satisfy a certain condition into a list.
- **Syntax**: `findall(Template, Goal, List).`
- **Usage Modes**:

  1. **Template and Goal Known, List Variable**:
     - **Purpose**: Gather all instances where `Goal` is true, substituting into `Template`.
     - **Example**:
       ```prolog
       % Collect all even numbers from 1 to 5
       ?- findall(X, (between(1, 5, X), X mod 2 =:= 0), L).
       L = [2, 4].
       
       % Collect all members of a list
       ?- findall(X, member(X, [a, b, c]), L).
       L = [a, b, c].
       ```
  
  2. **Partial Instantiation**:
     - **Template Variable**:
       - **Example**:
         ```prolog
         ?- findall(X, member(X, [a, b, c]), L).
         L = [a, b, c].
         ```
     - **Goal Variable**:
       - **Usage**: Rare; typically, `Goal` is a well-defined predicate.
  
  3. **List Variable**:
     - **When `List` is a variable**, `findall/3` will instantiate it with all solutions.
       ```prolog
       ?- findall(X, nth1(2, [a, b, c], X), L).
       L = [b].
       ```

- **Common Pitfalls**:
  - **Duplicate Elements**: `findall/3` does not remove duplicates; use `setof/3` or `bagof/3` if needed.
  - **Infinite Solutions**: If `Goal` can produce infinitely many solutions, `findall/3` will attempt to collect all, leading to non-termination.

- **Best Practices**:
  - **Use `findall/3` When All Solutions Are Needed**: It's suitable for scenarios where a complete list of results is required.
  - **Combine with Constraints**: Ensure that `Goal` has a finite number of solutions to prevent infinite loops.

---

##### **1.3.3.7. `select/3`**

- **Purpose**: Selects an element from a list, returning the element and the remaining list.
- **Syntax**: `select(Element, List, Rest).`
- **Usage Modes**:

  1. **Element Known, List Known**:
     - **Purpose**: Remove `Element` from `List`, producing `Rest`.
     - **Example**:
       ```prolog
       ?- select(b, [a, b, c], X).
       X = [a, c].
       
       ?- select(d, [a, b, c], X).
       false.
       ```
  
  2. **Element Unknown, List Known**:
     - **Purpose**: Enumerate all elements and corresponding `Rest` lists.
     - **Example**:
       ```prolog
       ?- select(X, [a, b, c], Rest).
       X = a,
       Rest = [b, c] ;
       X = b,
       Rest = [a, c] ;
       X = c,
       Rest = [a, b] ;
       false.
       ```
  
  3. **List Known, Element and Rest Variables**:
     - **Usage**: Useful for generating all possible `(Element, Rest)` pairs.
     - **Example**:
       ```prolog
       ?- select(X, [1, 2, 3], [1, 3]).
       X = 2.
       ```

- **Common Pitfalls**:
  - **Multiple Occurrences**: If `List` contains duplicates, `select/3` can select any occurrence.
    ```prolog
    ?- select(X, [a, b, a], [b, a]).
    X = a ;
    X = a ;
    false.
    ```
  
  - **Element Not Present**: If the `Element` is not in the `List`, the predicate fails.

- **Best Practices**:
  - **Handling Duplicates**: Be aware that `select/3` can select any matching element in case of duplicates.
  - **Determinism**: If only one selection is desired, consider using `selectchk/3` (a hypothetical variant similar to `memberchk/2`), though it doesn't exist in standard Prolog.

---

##### **1.3.3.8. `reverse/2`**

- **Purpose**: Reverses a list.
- **Syntax**: `reverse(List, Reversed).`
- **Usage Modes**:

  1. **List Known, Reversed Variable**:
     - **Purpose**: Generate the reversed version of `List`.
     - **Example**:
       ```prolog
       ?- reverse([a, b, c], X).
       X = [c, b, a].
       ```
  
  2. **Reversed Known, List Variable**:
     - **Purpose**: Find a `List` that reverses to `Reversed`.
     - **Example**:
       ```prolog
       ?- reverse(X, [c, b, a]).
       X = [a, b, c].
       ```
  
  3. **Both Variables**:
     - **Usage**: Generate all possible pairs where `List` is the reverse of `Reversed`.
     - **Example**:
       ```prolog
       ?- reverse(X, Y).
       X = [],
       Y = [];
       X = [_G1],
       Y = [_G1];
       X = [_G1, _G2],
       Y = [_G2, _G1];
       ...
       ```

- **Common Pitfalls**:
  - **Infinite Solutions**: When both `List` and `Reversed` are variables, Prolog can generate infinite possibilities.
  
- **Best Practices**:
  - **Use with Sufficient Instantiation**: Typically, reverse one known list to obtain the other.

---

##### **1.3.3.9. `last/2`**

- **Purpose**: Retrieves the last element of a list.
- **Syntax**: `last(List, Element).`
- **Usage Modes**:

  1. **List Known, Element Variable**:
     - **Purpose**: Find the last element of `List`.
     - **Example**:
       ```prolog
       ?- last([a, b, c], X).
       X = c.
       
       ?- last([1, 2, 3], X).
       X = 3.
       ```
  
  2. **Element Known, List Variable**:
     - **Purpose**: Generate lists where `Element` is the last element.
     - **Example**:
       ```prolog
       ?- last([1, 2, 3, 4], 4).
       true.
       
       ?- last(X, c).
       X = [c];
       X = [_G1, c];
       X = [_G1, _G2, c];
       ...
       ```
  
  3. **Both Variables**:
     - **Usage**: Generate all possible `(List, Element)` pairs where `Element` is the last element of `List`.
     - **Example**:
       ```prolog
       ?- last(X, Y).
       X = [Y];
       X = [_G1, Y];
       X = [_G1, _G2, Y];
       ...
       ```

- **Common Pitfalls**:
  - **Empty Lists**: `last/2` fails for empty lists.
    ```prolog
    ?- last([], X).
    false.
    ```
  
- **Best Practices**:
  - **Ensure Non-Empty Lists**: Before using `last/2`, confirm that the list is not empty.

---

##### **1.3.3.10. `delete/3`**

- **Purpose**: Removes **all** occurrences of an element from a list.
- **Syntax**: `delete(List, Element, Result).`
- **Usage Modes**:

  1. **Element Known, List Known**:
     - **Purpose**: Remove `Element` from `List`, producing `Result`.
     - **Example**:
       ```prolog
       ?- delete([a, b, c, b], b, X).
       X = [a, c].
       
       ?- delete([1, 2, 2, 3], 2, X).
       X = [1, 3].
       ```
  
  2. **Element Unknown, List Known**:
     - **Purpose**: Not commonly used; typically, `Element` is provided.
  
  3. **List Known, Element and Result Variables**:
     - **Purpose**: Find elements that can be removed to form `Result`.
     - **Example**:
       ```prolog
       ?- delete([a, b, c], X, [a, c]).
       X = b.
       ```
  
  4. **List Variable**:
     - **When `List` is a variable**, `delete/3` can generate lists by inserting elements.
       ```prolog
       ?- delete(X, b, [a, c]).
       X = [a, b, c].
       X = [a, c, b].
       false.
       ```
  
- **Common Pitfalls**:
  - **Understanding "All Occurrences"**: `delete/3` removes **all** instances of `Element`.
    ```prolog
    ?- delete([a, b, a], a, X).
    X = [b].
    ```
  
  - **Difference from `select/3`**: Unlike `select/3`, which removes a single occurrence, `delete/3` removes all.

- **Best Practices**:
  - **Use `select/3` for Single Occurrence Removal**: If only one instance needs to be removed, prefer `select/3` over `delete/3`.

---

##### **1.3.3.11. `union/3`**

- **Purpose**: Computes the union of two lists, removing duplicates.
- **Syntax**: `union(List1, List2, Union).`
- **Usage Modes**:

  1. **Both Lists Known, Union Variable**:
     - **Purpose**: Combine `List1` and `List2` into `Union`, excluding duplicates.
     - **Example**:
       ```prolog
       ?- union([a, b, c], [b, c, d], X).
       X = [a, b, c, d].
       
       ?- union([1, 2, 3], [3, 4, 5], X).
       X = [1, 2, 3, 4, 5].
       ```
  
  2. **Partial Instantiation**:
     - **List1 or List2 Variable**:
       - **Example**:
         ```prolog
         ?- union([a, b], Y, [a, b, c, d]).
         Y = [c, d].
         ```
  
  3. **List Variable**:
     - **When `Union` is a variable**, `List1` and `List2` can be used to generate possible unions.
       ```prolog
       ?- union(X, Y, [a, b, c]).
       % Possible solutions where X and Y combine to form [a, b, c] without duplicates
       ```

- **Common Pitfalls**:
  - **Order Preservation**: `union/3` preserves the order of elements from `List1` followed by non-duplicate elements from `List2`.
  - **Duplicates within Lists**: If `List1` or `List2` contain duplicates, `union/3` removes them in the `Union`.

- **Best Practices**:
  - **Ensure No Duplicates Within Individual Lists**: To avoid unexpected results, consider removing duplicates from `List1` and `List2` before computing the union.

---

##### **1.3.3.12. `intersection/3`**

- **Purpose**: Computes the intersection of two lists, retaining only common elements.
- **Syntax**: `intersection(List1, List2, Intersection).`
- **Usage Modes**:

  1. **Both Lists Known, Intersection Variable**:
     - **Purpose**: Find elements present in both `List1` and `List2`.
     - **Example**:
       ```prolog
       ?- intersection([a, b, c], [b, c, d], X).
       X = [b, c].
       
       ?- intersection([1, 2, 3], [3, 4, 5], X).
       X = [3].
       ```
  
  2. **Partial Instantiation**:
     - **List1 or List2 Variable**:
       - **Example**:
         ```prolog
         ?- intersection([a, b, c], Y, [b, c]).
         Y = [b, c].
         ```
  
  3. **List Variable**:
     - **When `Intersection` is a variable**, `List1` and `List2` can be used to generate possible intersections.
       ```prolog
       ?- intersection(X, Y, [a, b]).
       % Possible solutions where X and Y share [a, b] as common elements
       ```

- **Common Pitfalls**:
  - **Order Preservation**: The order of elements in `Intersection` follows the order in `List1`.
  - **Duplicates**: `intersection/3` removes duplicates; each common element appears only once in `Intersection`.

- **Best Practices**:
  - **Use `setof/3` or `bagof/3` for Advanced Queries**: For more control over the collection process, especially with variables, consider using `setof/3` or `bagof/3`.

---

#### **1.3.4. Advanced List Predicates**

These predicates offer more specialized operations for list manipulation.

##### **1.3.4.1. `sort/2`**

- **Purpose**: Sorts a list in ascending order, removing duplicate elements.
- **Syntax**: `sort(UnsortedList, SortedList).`
- **Usage Modes**:

  1. **Unsorted List Known, Sorted List Variable**:
     - **Example**:
       ```prolog
       ?- sort([3, 1, 2, 3, 4, 2], X).
       X = [1, 2, 3, 4].
       ```
  
  2. **Sorted List Known, Unsorted List Variable**:
     - **Usage**: Rare; typically, `UnsortedList` is provided.
  
  3. **Both Variables**:
     - **Usage**: Prolog can generate lists that sort to a particular `SortedList`.
       ```prolog
       ?- sort(X, [1, 2, 3]).
       X = [1, 2, 3];
       X = [1, 1, 2, 3];
       X = [1, 2, 2, 3];
       ...
       ```

- **Common Pitfalls**:
  - **Removal of Duplicates**: `sort/2` eliminates duplicate elements, which might not be desired in some contexts.
  
- **Best Practices**:
  - **Use `msort/2` When Preserving Duplicates**: If duplicates need to be retained, prefer `msort/2` over `sort/2`.

---

##### **1.3.4.2. `msort/2`**

- **Purpose**: Sorts a list in ascending order, preserving duplicate elements.
- **Syntax**: `msort(UnsortedList, SortedList).`
- **Usage Modes**:

  1. **Unsorted List Known, Sorted List Variable**:
     - **Example**:
       ```prolog
       ?- msort([3, 1, 2, 3, 4, 2], X).
       X = [1, 2, 2, 3, 3, 4].
       ```
  
  2. **Sorted List Known, Unsorted List Variable**:
     - **Usage**: Similar to `sort/2`, less common.
  
  3. **Both Variables**:
     - **Usage**: Generates lists that sort (with duplicates) to a specific `SortedList`.
       ```prolog
       ?- msort(X, [1, 2, 2, 3]).
       X = [1, 2, 2, 3];
       X = [1, 2, 3, 2];
       X = [2, 1, 2, 3];
       ...
       ```

- **Common Pitfalls**:
  - **Performance**: Sorting with duplicates can be computationally more intensive for large lists.
  
- **Best Practices**:
  - **Choose `msort/2` Over `sort/2` When Duplicates Matter**: Clearly distinguish when duplicates are relevant to your application.

---

##### **1.3.4.3. `unique/2`**

- **Purpose**: Removes duplicate elements from a list while preserving the original order.
- **Syntax**: `unique(List, UniqueList).`
- **Implementation**:
  ```prolog
  unique([], []).
  unique([H|T], [H|UniqueT]) :-
      \+ member(H, T),
      unique(T, UniqueT).
  unique([H|T], UniqueT) :-
      member(H, T),
      unique(T, UniqueT).
  ```
- **Usage Modes**:

  1. **List Known, Unique List Variable**:
     - **Example**:
       ```prolog
       ?- unique([1, 2, 3, 2, 4, 1], X).
       X = [1, 2, 3, 4].
       ```
  
  2. **Unique List Known, List Variable**:
     - **Usage**: Can generate all possible original lists that reduce to `UniqueList` by removing duplicates.
  
  3. **Both Variables**:
     - **Usage**: Generates lists and their unique counterparts.
       ```prolog
       ?- unique(X, [a, b]).
       X = [a, b];
       X = [a, a, b];
       X = [a, b, b];
       X = [a, a, b, b];
       ...
       ```

- **Common Pitfalls**:
  - **Order Preservation**: The first occurrence of each element is preserved; subsequent duplicates are removed.
  
- **Best Practices**:
  - **Use for Eliminating Duplicates with Order Preservation**: When the order of elements is important, `unique/2` is preferable over `sort/2`.

---

##### **1.3.4.4. `intersect/3`**

- **Purpose**: Finds the intersection of two lists.
- **Syntax**: `intersect(List1, List2, Intersection).`
- **Implementation**:
  ```prolog
  intersect([], _, []).
  intersect([H|T], List2, [H|Intersection]) :-
      member(H, List2),
      intersect(T, List2, Intersection).
  intersect([H|T], List2, Intersection) :-
      \+ member(H, List2),
      intersect(T, List2, Intersection).
  ```
- **Usage Modes**:

  1. **Both Lists Known, Intersection Variable**:
     - **Example**:
       ```prolog
       ?- intersect([1, 2, 3, 4], [3, 4, 5, 6], X).
       X = [3, 4].
       ```
  
  2. **Partial Instantiation**:
     - **List1 or List2 Variable**:
       - **Example**:
         ```prolog
         ?- intersect([a, b, c], Y, [b, c]).
         Y = [b, c].
         ```
  
  3. **List Variable**:
     - **When `Intersection` is a variable**, `List1` and `List2` can be used to generate possible intersections.
       ```prolog
       ?- intersect(X, Y, [a, b]).
       X = [a, b],
       Y = [a, b];
       X = [a, b, c],
       Y = [a, b, d];
       ...
       ```

- **Common Pitfalls**:
  - **Order Preservation**: The order of elements in `Intersection` follows the order in `List1`.
  - **Duplicates**: If `List1` contains duplicates, `Intersection` may include duplicates unless handled.

- **Best Practices**:
  - **Remove Duplicates Before Intersection**: To avoid multiple instances of the same element in `Intersection`, consider using `unique/2` on `List1` or `List2` beforehand.

---

#### **1.3.5. Haskell-like List Functions**

Incorporating functional programming paradigms, these predicates mimic Haskell's powerful list operations.

##### **1.3.5.1. `take/3`**

- **Purpose**: Takes the first N elements from a list.
- **Syntax**: `take(N, List, Taken).`
- **Implementation**:
  ```prolog
  take(0, _, []).
  take(_, [], []).
  take(N, [H|T], [H|Taken]) :-
      N > 0,
      N1 is N - 1,
      take(N1, T, Taken).
  ```
- **Usage Modes**:

  1. **N Known, List Known, Taken Variable**:
     - **Example**:
       ```prolog
       ?- take(2, [a, b, c, d], X).
       X = [a, b].
       ```
  
  2. **N Known, Taken Known, List Variable**:
     - **Purpose**: Generate lists where the first N elements match `Taken`.
     - **Example**:
       ```prolog
       ?- take(2, X, [a, b]).
       X = [a, b|_G1].
       ```
  
  3. **N Variable**:
     - **Usage**: Less common; Prolog can generate all possible prefixes with varying lengths.
       ```prolog
       ?- take(N, [a, b, c], X).
       N = 0,
       X = [];
       N = 1,
       X = [a];
       N = 2,
       X = [a, b];
       N = 3,
       X = [a, b, c];
       false.
       ```

- **Common Pitfalls**:
  - **Negative N Values**: Typically, `N` should be a non-negative integer. Negative values can lead to unexpected behaviors.
  - **Exceeding List Length**: When `N` exceeds the list's length, `Taken` becomes the entire list without error.

- **Best Practices**:
  - **Validate N**: Ensure that `N` is a non-negative integer to prevent logical errors.

---

##### **1.3.5.2. `drop/3`**

- **Purpose**: Drops the first N elements from a list.
- **Syntax**: `drop(N, List, Dropped).`
- **Implementation**:
  ```prolog
  drop(0, List, List).
  drop(_, [], []).
  drop(N, [_|T], Dropped) :-
      N > 0,
      N1 is N - 1,
      drop(N1, T, Dropped).
  ```
- **Usage Modes**:

  1. **N Known, List Known, Dropped Variable**:
     - **Example**:
       ```prolog
       ?- drop(2, [a, b, c, d], X).
       X = [c, d].
       
       ?- drop(5, [1, 2, 3], X).
       X = [].
       ```
  
  2. **N Known, Dropped Known, List Variable**:
     - **Purpose**: Generate lists where the last part after dropping N elements matches `Dropped`.
     - **Example**:
       ```prolog
       ?- drop(2, X, [c, d]).
       X = [_, _, c, d].
       ```
  
  3. **N Variable**:
     - **Usage**: Prolog can generate all possible lists with varying numbers of initial elements dropped.
       ```prolog
       ?- drop(N, [a, b, c], X).
       N = 0,
       X = [a, b, c];
       N = 1,
       X = [b, c];
       N = 2,
       X = [c];
       N = 3,
       X = [];
       false.
       ```

- **Common Pitfalls**:
  - **Negative N Values**: As with `take/3`, `N` should be non-negative.
  - **Infinite Recursion**: Dropping from an infinite list with an uninstantiated `N` can cause non-termination.

- **Best Practices**:
  - **Ensure Finite Lists**: When working with potentially large or infinite lists, ensure that operations terminate.

---

##### **1.3.5.3. `takewhile/3`**

- **Purpose**: Takes elements from the list while a condition holds.
- **Syntax**: `takewhile(Predicate, List, Taken).`
- **Implementation**:
  ```prolog
  takewhile(_, [], []).
  takewhile(P, [H|T], [H|Taken]) :-
      call(P, H),
      takewhile(P, T, Taken).
  takewhile(P, [H|_], []) :-
      \+ call(P, H).
  ```
- **Usage Modes**:

  1. **Predicate Known, List Known, Taken Variable**:
     - **Example**:
       ```prolog
       % Predicate to check if a number is less than 4
       less_than_4(X) :- X < 4.
       
       ?- takewhile(less_than_4, [1, 2, 3, 4, 1], X).
       X = [1, 2, 3].
       
       ?- takewhile(less_than_4, [5, 6, 1], X).
       X = [].
       ```
  
  2. **Predicate Known, Taken Known, List Variable**:
     - **Purpose**: Generate lists where the initial segment satisfies `Predicate` and `Taken` is the segment.
     - **Example**:
       ```prolog
       ?- takewhile(less_than_4, X, [1, 2, 3]).
       X = [1, 2, 3|_G1].
       ```
  
  3. **Predicate and List Variable**:
     - **Usage**: Prolog can generate all possible `Taken` lists that satisfy the predicate.
       ```prolog
       ?- takewhile(less_than_4, [H|T], X).
       % Generates lists starting with elements < 4
       ```

- **Common Pitfalls**:
  - **Undefined Predicates**: Ensure that `Predicate` is properly defined.
  - **Infinite Lists**: Using `takewhile/3` with infinite lists can cause non-termination if the condition never fails.

- **Best Practices**:
  - **Use with Finite Lists**: Prefer finite lists or ensure that the predicate will eventually fail.

---

##### **1.3.5.4. `dropwhile/3`**

- **Purpose**: Drops elements from the list while a condition holds and returns the rest.
- **Syntax**: `dropwhile(Predicate, List, Dropped).`
- **Implementation**:
  ```prolog
  dropwhile(_, [], []).
  dropwhile(P, [H|T], Dropped) :-
      call(P, H),
      dropwhile(P, T, Dropped).
  dropwhile(P, List, List) :-
      List \= [],
      \+ ( [H|_] = List, call(P, H) ).
  ```
- **Usage Modes**:

  1. **Predicate Known, List Known, Dropped Variable**:
     - **Example**:
       ```prolog
       % Predicate to check if a number is less than 4
       less_than_4(X) :- X < 4.
       
       ?- dropwhile(less_than_4, [1, 2, 3, 4, 1], X).
       X = [4, 1].
       
       ?- dropwhile(less_than_4, [5, 6, 1], X).
       X = [5, 6, 1].
       ```
  
  2. **Predicate Known, Dropped Known, List Variable**:
     - **Purpose**: Generate lists where the tail after dropping elements satisfies the condition.
     - **Example**:
       ```prolog
       ?- dropwhile(less_than_4, X, [4, 1]).
       X = [1, 2, 3, 4, 1].
       ```
  
  3. **Predicate and List Variable**:
     - **Usage**: Prolog can generate all possible `Dropped` lists where the initial segment satisfies the predicate.
       ```prolog
       ?- dropwhile(less_than_4, [H|T], X).
       % Generates lists where the first few elements satisfy the predicate
       ```

- **Common Pitfalls**:
  - **Undefined Predicates**: Ensure that `Predicate` is properly defined.
  - **Infinite Lists**: Dropping from an infinite list without ever failing the predicate leads to non-termination.

- **Best Practices**:
  - **Ensure Predicate Eventually Fails**: To prevent infinite recursion, ensure that the predicate does not always hold.

---

##### **1.3.5.5. `split_at/4`**

- **Purpose**: Splits a list at the N-th position into two lists.
- **Syntax**: `split_at(N, List, Left, Right).`
- **Implementation**:
  ```prolog
  split_at(0, List, [], List).
  split_at(_, [], [], []).
  split_at(N, [H|T], [H|Left], Right) :-
      N > 0,
      N1 is N - 1,
      split_at(N1, T, Left, Right).
  ```
- **Usage Modes**:

  1. **N Known, List Known, Left and Right Variables**:
     - **Example**:
       ```prolog
       ?- split_at(2, [a, b, c, d], Left, Right).
       Left = [a, b],
       Right = [c, d].
       
       ?- split_at(5, [1, 2, 3], Left, Right).
       Left = [1, 2, 3],
       Right = [].
       ```
  
  2. **N Known, Left and Right Known, List Variable**:
     - **Purpose**: Generate `List` by concatenating `Left` and `Right`.
     - **Example**:
       ```prolog
       ?- split_at(2, X, [a, b], [c, d]).
       X = [a, b, c, d].
       ```
  
  3. **N Variable**:
     - **Usage**: Prolog can generate all possible splits with varying `N`.
       ```prolog
       ?- split_at(N, [a, b, c], Left, Right).
       N = 0,
       Left = [],
       Right = [a, b, c];
       N = 1,
       Left = [a],
       Right = [b, c];
       N = 2,
       Left = [a, b],
       Right = [c];
       N = 3,
       Left = [a, b, c],
       Right = [];
       false.
       ```

- **Common Pitfalls**:
  - **Negative N Values**: Should be non-negative; negative values lead to no solution.
  - **Splitting Beyond List Length**: Results in `Right` being empty without error.

- **Best Practices**:
  - **Validate N**: Ensure `N` is a non-negative integer.
  - **Use Known N When Possible**: For efficiency, provide `N` to avoid unnecessary backtracking.

---

##### **1.3.5.6. `zip/3`**

- **Purpose**: Zips two lists into a list of pairs.
- **Syntax**: `zip(List1, List2, Zipped).`
- **Implementation**:
  ```prolog
  zip([], _, []).
  zip(_, [], []).
  zip([H1|T1], [H2|T2], [[H1, H2]|Zipped]) :-
      zip(T1, T2, Zipped).
  ```
- **Usage Modes**:

  1. **Both Lists Known, Zipped Variable**:
     - **Example**:
       ```prolog
       ?- zip([a, b, c], [1, 2, 3], X).
       X = [[a, 1], [b, 2], [c, 3]].
       
       ?- zip([a, b], [1, 2, 3], X).
       X = [[a, 1], [b, 2]].
       ```
  
  2. **Partial Instantiation**:
     - **List1 Known, List2 Variable**:
       - **Example**:
         ```prolog
         ?- zip([a, b], Y, [[a, 1], [b, 2]]).
         Y = [1, 2].
         ```
  
  3. **List Variables**:
     - **When all arguments are variables**, Prolog can generate all possible zipped lists.
       ```prolog
       ?- zip(X, Y, [[a, 1], [b, 2]]).
       X = [a, b],
       Y = [1, 2].
       ```
  
- **Common Pitfalls**:
  - **Unequal List Lengths**: `zip/3` stops when the shorter list ends without error.
  
- **Best Practices**:
  - **Ensure Lists Have Desired Lengths**: To avoid unexpected truncation, confirm that lists are of intended lengths before zipping.

---

#### **1.3.6. Recursive List Processing**

Many list operations in Prolog are defined recursively. Understanding recursion is key to manipulating lists effectively.

##### **1.3.6.1. Summing Elements in a List**

```prolog
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Rest),
    Sum is H + Rest.

% Usage:
?- sum_list([1, 2, 3, 4], Sum).
Sum = 10.
```

---

##### **1.3.6.2. Finding the Maximum Element**

```prolog
max_list([X], X).
max_list([H|T], Max) :-
    max_list(T, TempMax),
    Max is max(H, TempMax).

% Usage:
?- max_list([3, 1, 4, 2], Max).
Max = 4.
```

---

##### **1.3.6.3. Finding the Minimum Element**

```prolog
min_list([X], X).
min_list([H|T], Min) :-
    min_list(T, TempMin),
    Min is min(H, TempMin).

% Usage:
?- min_list([3, 1, 4, 2], Min).
Min = 1.
```

---

##### **1.3.6.4. Calculating the Product of Elements**

```prolog
product_list([], 1).
product_list([H|T], Product) :-
    product_list(T, Rest),
    Product is H * Rest.

% Usage:
?- product_list([1, 2, 3, 4], Product).
Product = 24.
```

---

##### **1.3.6.5. Calculating the Average of Elements**

```prolog
average_list(List, Avg) :-
    sum_list(List, Sum),
    length(List, Length),
    Length > 0,
    Avg is Sum / Length.

% Usage:
?- average_list([1, 2, 3, 4], Avg).
Avg = 2.5.
```

---

#### **1.3.7. Haskell-like Higher-Order Functions**

Prolog can emulate higher-order functions common in functional programming languages like Haskell. These predicates allow for more expressive and concise list manipulations.

##### **1.3.7.1. `map/3`**

- **Purpose**: Applies a predicate to each element of a list.
- **Syntax**: `map(InputList, OutputList, Predicate).`
- **Implementation**:
  ```prolog
  map([], [], _F).
  map([H|T], [H1|T1], F) :-
      call(F, H, H1),
      map(T, T1, F).
  ```
- **Usage Modes**:

  1. **InputList Known, Predicate Known, OutputList Variable**:
     - **Example**:
       ```prolog
       % Example Predicate:
       square(X, Y) :- Y is X * X.
       
       ?- map([1, 2, 3, 4], Squares, square).
       Squares = [1, 4, 9, 16].
       ```
  
  2. **Other Modes**:
     - **Limited Usage**: Typically, `InputList` and `Predicate` are known to map elements effectively.

- **Common Pitfalls**:
  - **Predicate Definition**: Ensure that `Predicate` correctly transforms elements.
  - **List Lengths**: `InputList` and `OutputList` must correspond element-wise.

- **Best Practices**:
  - **Define Clear Transformation Predicates**: Predicates used with `map/3` should have a clear input-output relationship.

---

##### **1.3.7.2. `filter/3`**

- **Purpose**: Filters elements of a list based on a given predicate.
- **Syntax**: `filter(InputList, FilteredList, Predicate).`
- **Implementation**:
  ```prolog
  filter([], [], _Pred).
  filter([H|T], [H|Filtered], Pred) :-
      call(Pred, H),
      filter(T, Filtered, Pred).
  filter([_|T], Filtered, Pred) :-
      filter(T, Filtered, Pred).
  ```
- **Usage Modes**:

  1. **InputList Known, Predicate Known, FilteredList Variable**:
     - **Example**:
       ```prolog
       % Example Predicate:
       is_even(X) :- 0 is X mod 2.
       
       ?- filter([1, 2, 3, 4, 5, 6], Evens, is_even).
       Evens = [2, 4, 6].
       ```
  
  2. **Other Modes**:
     - **Less Common**: Typically, `InputList` and `Predicate` are provided.

- **Common Pitfalls**:
  - **Predicate Logic**: Ensure that the predicate accurately defines the filtering condition.
  
- **Best Practices**:
  - **Use Well-Defined Predicates**: Clear and correct predicates prevent unintended filtering results.

---

##### **1.3.7.3. `foldl/4`**

- **Purpose**: Folds a list from the left using a binary predicate and an initial accumulator.
- **Syntax**: `foldl(Predicate, List, Initial, Result).`
- **Implementation**:
  ```prolog
  foldl(_, [], Acc, Acc).
  foldl(P, [H|T], Acc, Result) :-
      call(P, H, Acc, Acc1),
      foldl(P, T, Acc1, Result).
  ```
- **Usage Modes**:

  1. **Predicate Known, List Known, Initial Known, Result Variable**:
     - **Example**:
       ```prolog
       % Example Predicate:
       add(X, Acc, Acc1) :- Acc1 is Acc + X.
       
       ?- foldl(add, [1, 2, 3, 4], 0, Sum).
       Sum = 10.
       ```
  
  2. **Other Modes**:
     - **Less Common**: Typically, the `Predicate`, `List`, and `Initial` are provided to compute `Result`.

- **Common Pitfalls**:
  - **Predicate Definition**: The predicate must correctly handle the accumulator to avoid logical errors.
  
- **Best Practices**:
  - **Ensure Correct Accumulator Handling**: The predicate should accurately update the accumulator based on each element.

---

##### **1.3.7.4. `foldr/4`**

- **Purpose**: Folds a list from the right using a binary predicate and an initial accumulator.
- **Syntax**: `foldr(Predicate, List, Initial, Result).`
- **Implementation**:
  ```prolog
  foldr(_, [], Acc, Acc).
  foldr(P, [H|T], Acc, Result) :-
      foldr(P, T, Acc, Acc1),
      call(P, H, Acc1, Result).
  ```
- **Usage Modes**:

  1. **Predicate Known, List Known, Initial Known, Result Variable**:
     - **Example**:
       ```prolog
       % Example Predicate:
       multiply(X, Acc, Acc1) :- Acc1 is X * Acc.
       
       ?- foldr(multiply, [1, 2, 3, 4], 1, Product).
       Product = 24.
       ```
  
  2. **Other Modes**:
     - **Less Common**: Typically used with known predicates, lists, and initial accumulators.

- **Common Pitfalls**:
  - **Recursion Depth**: Folding from the right can lead to deeper recursion stacks for large lists compared to folding from the left.
  
- **Best Practices**:
  - **Use Appropriately**: Choose between `foldl/4` and `foldr/4` based on the desired folding direction and efficiency considerations.

---

#### **1.3.8. Typical Examples**

Prolog is well-suited for defining mathematical and algorithmic concepts using lists. Below are several typical examples that demonstrate practical applications of list manipulation.

##### **1.3.8.1. Factorial**

- **Definition**: Calculates the factorial of a non-negative integer.
- **Implementation**:
  ```prolog
  factorial(0, 1).
  factorial(N, F) :-
      N > 0,
      N1 is N - 1,
      factorial(N1, F1),
      F is N * F1.
  
  % Usage:
  ?- factorial(5, F).
  F = 120.
  ```

---

##### **1.3.8.2. Fibonacci**

- **Definition**: Computes the N-th Fibonacci number.
- **Implementation**:
  ```prolog
  fibonacci(0, 0).
  fibonacci(1, 1).
  fibonacci(N, F) :-
      N > 1,
      N1 is N - 1,
      N2 is N - 2,
      fibonacci(N1, F1),
      fibonacci(N2, F2),
      F is F1 + F2.
  
  % Usage:
  ?- fibonacci(6, F).
  F = 8.
  ```

---

##### **1.3.8.3. Reverse a List**

- **Definition**: Reverses the elements of a list.
- **Implementation**:
  ```prolog
  reverse_list([], []).
  reverse_list([H|T], Rev) :-
      reverse_list(T, RevT),
      append(RevT, [H], Rev).
  
  % Usage:
  ?- reverse_list([a, b, c], X).
  X = [c, b, a].
  ```

---

##### **1.3.8.4. Palindrome Check**

- **Definition**: Determines whether a list is a palindrome.
- **Implementation**:
  ```prolog
  is_palindrome(List) :-
      reverse(List, List).
  
  % Usage:
  ?- is_palindrome([r, a, c, e, c, a, r]).
  true.
  
  ?- is_palindrome([h, e, l, l, o]).
  false.
  ```

---

##### **1.3.8.5. Merge Sort**

- **Definition**: Sorts a list using the merge sort algorithm.
- **Implementation**:
  ```prolog
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
  
  % Usage:
  ?- merge_sort([3, 1, 4, 1, 5, 9, 2, 6], Sorted).
  Sorted = [1, 1, 2, 3, 4, 5, 6, 9].
  ```

---

##### **1.3.8.6. Map Function**

- **Definition**: Applies a given function to each element of a list.
- **Implementation**:
  ```prolog
  map([], [], _F).
  map([H|T], [H1|T1], F) :-
      call(F, H, H1),
      map(T, T1, F).
  
  % Example Predicate:
  square(X, Y) :- Y is X * X.
  
  % Usage:
  ?- map([1, 2, 3, 4], Squares, square).
  Squares = [1, 4, 9, 16].
  ```

---

##### **1.3.8.7. Filter Function**

- **Definition**: Filters elements of a list based on a given predicate.
- **Implementation**:
  ```prolog
  filter([], [], _Pred).
  filter([H|T], [H|Filtered], Pred) :-
      call(Pred, H),
      filter(T, Filtered, Pred).
  filter([_|T], Filtered, Pred) :-
      filter(T, Filtered, Pred).
  
  % Example Predicate:
  is_even(X) :- 0 is X mod 2.
  
  % Usage:
  ?- filter([1, 2, 3, 4, 5, 6], Evens, is_even).
  Evens = [2, 4, 6].
  ```

---

##### **1.3.8.8. Flatten a Nested List**

- **Definition**: Flattens a nested list structure into a single-level list.
- **Implementation**:
  ```prolog
  flatten_list([], []).
  flatten_list([H|T], FlatList) :-
      flatten_list(H, FlatHead),
      flatten_list(T, FlatTail),
      append(FlatHead, FlatTail, FlatList).
  flatten_list(L, [L]) :-
      \+ is_list(L).
  
  % Usage:
  ?- flatten_list([a, [b, [c, d], e], f], X).
  X = [a, b, c, d, e, f].
  ```

---

##### **1.3.8.9. Maximum Element Using `max_list/2`**

- **Definition**: Finds the maximum element in a list.
- **Implementation**:
  ```prolog
  max_list([X], X).
  max_list([H|T], Max) :-
      max_list(T, TempMax),
      Max is max(H, TempMax).
  
  % Usage:
  ?- max_list([3, 1, 4, 2], Max).
  Max = 4.
  ```

---

#### **1.3.9. Practical Tips for Using List Predicates**

1. **Understand Predicate Modes**:
   - Prolog predicates like `append/3` and `length/2` can operate in multiple modes (e.g., list known vs. variable).
   - **Example**:
     ```prolog
     % Mode 1: append(List1, List2, Result)
     ?- append([a, b], [c, d], X).
     X = [a, b, c, d].
     
     % Mode 2: append(List1, List2, [a, b, c]).
     ?- append(X, Y, [a, b, c]).
     X = [],
     Y = [a, b, c];
     X = [a],
     Y = [b, c];
     ...
     ```

2. **Leverage Backtracking**:
   - Prolog uses backtracking to explore all possible solutions. Be mindful of this to prevent infinite loops.
   - **Example**:
     ```prolog
     % Potentially infinite solutions
     ?- append(X, Y, [a|Y]).
     X = [],
     Y = [a|Y].
     % Prolog will loop indefinitely trying to satisfy Y = [a|Y]
     ```

3. **Use Deterministic Predicates When Possible**:
   - Predicates like `memberchk/2` can prevent unnecessary backtracking by stopping after the first solution.
   - **Example**:
     ```prolog
     ?- memberchk(b, [a, b, c, b]).
     true.
     % No further backtracking for additional 'b's
     ```

4. **Avoid Mixing Deterministic and Non-Deterministic Predicates**:
   - Combining predicates that can generate multiple solutions with those that are deterministic can lead to unexpected behaviors.
   - **Example**:
     ```prolog
     % Using member/2 within a loop can generate multiple possibilities
     find_duplicates(List, Duplicates) :-
         findall(X, (member(X, List), select(X, List, _)), Duplicates).
     ```
   
5. **Optimize Predicate Order for Efficiency**:
   - Place more restrictive predicates early to reduce the search space.
   - **Example**:
     ```prolog
     % Inefficient: generating all possible lists first
     find_path(Initial, Final, Path, Cost) :-
         findall(Path, some_expensive_condition(Initial, Final, Path), Paths),
         find_min_cost(Paths, Cost).
     
     % Efficient: filtering paths as they are generated
     find_path(Initial, Final, Path, Cost) :-
         some_expensive_condition(Initial, Final, Path),
         calculate_cost(Path, Cost).
     ```

6. **Handle Special Cases Explicitly**:
   - Define base cases for recursive predicates to ensure termination.
   - **Example**:
     ```prolog
     % Base case for reversing a list
     reverse_list([], []).
     ```

7. **Use Helper Predicates for Complex Operations**:
   - Break down complex list manipulations into smaller, manageable predicates.
   - **Example**:
     ```prolog
     % Splitting a list into two halves
     split_list(List, Left, Right) :-
         length(List, Len),
         HalfLen is Len // 2,
         split_at(HalfLen, List, Left, Right).
     ```

---

#### **1.3.10. Summary and Best Practices**

- **Predicate Modes**: Always consider which arguments are known and which are variables when using list predicates.
- **Avoid Infinite Loops**: Ensure that predicates terminate by providing sufficient instantiation or handling special cases.
- **Leverage Built-in Predicates**: Utilize Prolog's powerful built-in predicates like `memberchk/2` and `append/3` for efficient list manipulations.
- **Modular Design**: Create modular predicates for reusable and readable code.
- **Testing and Debugging**: Use trace tools and test various usage modes to understand predicate behaviors fully.

---

### Additional Examples to Illustrate `append/3` and `length/2` Usages

To further clarify how `append/3` and `length/2` work in different scenarios, let's explore some practical examples.

#### **Example 1: Using `append/3` for List Splitting**

**Goal**: Split a list into two parts at a specific index.

```prolog
% Define a predicate to split a list at N
split_at(N, List, Left, Right) :-
    append(Left, Right, List),
    length(Left, N).

% Usage:
?- split_at(2, [a, b, c, d, e], Left, Right).
Left = [a, b],
Right = [c, d, e].
```

**Explanation**:
1. `append(Left, Right, List)` decomposes `List` into `Left` and `Right`.
2. `length(Left, N)` ensures that `Left` has exactly `N` elements.
3. Prolog uses these constraints to find appropriate `Left` and `Right` segments.

#### **Example 2: Generating Lists of a Certain Length with `length/2`**

**Goal**: Create a list of variables with a specific length.

```prolog
% Generate a list of 3 variables
?- length(X, 3).
X = [_G1, _G2, _G3].
```

**Explanation**:
- When `List` is a variable and `Length` is known, Prolog instantiates `List` with a list containing `Length` number of anonymous variables.

#### **Example 3: Combining `append/3` and `length/2` for Controlled List Generation**

**Goal**: Generate a list where the first two elements are `[a, b]` and the total length is 4.

```prolog
% Define a predicate to create a list with first two elements [a, b] and total length 4
create_list(List) :-
    append([a, b], Rest, List),
    length(List, 4),
    append([a, b], Rest, List).

% Usage:
?- create_list(X).
X = [a, b, _G1, _G2].
```

**Explanation**:
1. `append([a, b], Rest, List)` ensures that `List` starts with `[a, b]` followed by `Rest`.
2. `length(List, 4)` constrains `List` to have exactly 4 elements.
3. Prolog combines these constraints to generate `List` as `[a, b, _G1, _G2]`, where `_G1` and `_G2` are variables.

---

### Understanding `oneStep/3` with `append/3` and `length/2`

Let's revisit your original `oneStep/3` predicate and understand how `append/3` and `length/2` work within it.

```prolog
oneStep(CostStep, State, NextState) :-    
    append(U1, [Y|U0], State),
    append(U3, [X|U2], U1),
    length(U2, L),
    CostStep is L + 1,
    V0 = U0, V2 = U2, V3 = U3,
    append(V3, [Y|V2], V1),
    append(V1, [X|V0], NextState).
```

**Step-by-Step Breakdown**:

1. **Decompose `State` into `U1` and `[Y|U0]`**:
   - `append(U1, [Y|U0], State)`:
     - Splits `State` into two parts:
       - `U1`: The prefix before element `Y`.
       - `[Y|U0]`: Element `Y` followed by the suffix `U0`.
     - **Example**:
       - If `State = [a, b, c, d]`, possible splits include:
         - `U1 = []`, `Y = a`, `U0 = [b, c, d]`
         - `U1 = [a]`, `Y = b`, `U0 = [c, d]`
         - And so on.

2. **Further Decompose `U1` into `U3` and `[X|U2]`**:
   - `append(U3, [X|U2], U1)`:
     - Splits `U1` into:
       - `U3`: The prefix before element `X`.
       - `[X|U2]`: Element `X` followed by the suffix `U2`.
     - **Example**:
       - If `U1 = [a, b]`, possible splits:
         - `U3 = []`, `X = a`, `U2 = [b]`
         - `U3 = [a]`, `X = b`, `U2 = []`

3. **Calculate the Cost of Swapping `X` and `Y`**:
   - `length(U2, L)`:
     - `U2` represents the elements between `X` and `Y`.
     - `L` is the number of elements between them.
   - `CostStep is L + 1`:
     - The cost is the distance between `X` and `Y`, calculated as the number of elements between them plus one.

4. **Perform the Swap to Generate `NextState`**:
   - `V0 = U0, V2 = U2, V3 = U3`:
     - Assigns the segments for clarity.
   - `append(V3, [Y|V2], V1)`:
     - Places `Y` in the position where `X` was.
   - `append(V1, [X|V0], NextState)`:
     - Appends `X` after `Y`, resulting in `NextState` with `X` and `Y` swapped.

**Illustrative Example**:

- **Initial State**: `[5, 1, 3, 4, 2, 6]`
- **Goal**: Swap `5` (position 1) with `2` (position 5).

**Execution**:

1. **First `append/3`**:
   - `U1 = []`, `Y = 5`, `U0 = [1, 3, 4, 2, 6]`
  
2. **Second `append/3`**:
   - `U3 = []`, `X = 5`, `U2 = [1, 3, 4, 2, 6]`
  
3. **Calculate Cost**:
   - `L = length([1, 3, 4, 2, 6]) = 5`
   - `CostStep = 5 + 1 = 6`
  
4. **Generate `NextState`**:
   - `V1 = [Y|V2] = [5|[1, 3, 4, 2, 6]] = [5, 1, 3, 4, 2, 6]`
   - `NextState = [Y|V2] appended with [X|V0] = [5, 1, 3, 4, 2, 6]`
   - **Note**: In this particular case, swapping `5` with itself results in the same `State`. To perform a meaningful swap, different splits of `State` are needed.

**Alternative Split for Meaningful Swap**:

1. **First `append/3`**:
   - `U1 = [5, 1, 3, 4]`, `Y = 2`, `U0 = [6]`
  
2. **Second `append/3`**:
   - `U3 = [5, 1], X = 3`, `U2 = [4]`
  
3. **Calculate Cost**:
   - `L = length([4]) = 1`
   - `CostStep = 1 + 1 = 2`
  
4. **Generate `NextState`**:
   - `V1 = [5, 1 | [2]] = [5, 1, 2]`
   - `NextState = [5, 1, 2 | [3 | [6]]] = [5, 1, 2, 3, 6]`
  
**Final Swap Result**:
- **From**: `[5, 1, 3, 4, 2, 6]`
- **To**: `[5, 1, 2, 3, 4, 6]`
- **Cost**: `2`

**Key Insights**:
- **Multiple Splits**: `append/3` allows multiple ways to split the list, enabling different swap operations.
- **Cost Calculation**: Determining `L` correctly ensures accurate cost computation based on the distance between swapped elements.
- **Avoid Self-Swaps**: Ensure that `X` and `Y` are distinct to perform meaningful transpositions.

---


### 1.4. Advanced Predicates

Beyond the basics, Prolog offers a suite of advanced predicates that facilitate complex operations, especially when dealing with logic programming and constraint solving.

#### **1.4.1. `findall/3`**


**Understanding `findall/3`**

The `findall/3` predicate in Prolog is used to gather all possible solutions to a query into a list. Its general syntax is:

```prolog
findall(Template, Goal, List)
```

- **Template**: Specifies **what** to collect from each solution.
- **Goal**: The Prolog query or condition to satisfy.
- **List**: The resulting list containing all instances of the **Template** that satisfy the **Goal**.

**Key Point:**

- **Template** determines **what** part of the **Goal's** solution you want to collect.
- The **Goal** defines **how** to find the solutions.


### **Example 1: Collecting Numbers Within a Range**

**Goal:** Collect all integers from 1 to 5.

#### **Prolog Code:**

```prolog
% Query to collect numbers from 1 to 5
?- findall(N, between(1, 5, N), Numbers).
```

#### **Explanation:**

- **Template (`N`):** Represents each number in the range.
- **Goal (`between(1, 5, N)`):** Generates numbers `N` starting from 1 up to 5.
- **List (`Numbers`):** Collects all generated numbers into the list `Numbers`.

#### **Result:**

```prolog
Numbers = [1, 2, 3, 4, 5].
```

---

### **Example 2: Collecting Squares of Numbers**

**Goal:** Collect the squares of numbers from 1 to 5.

#### **Prolog Code:**

```prolog
% Query to collect squares of numbers from 1 to 5
?- findall(Square, (between(1, 5, N), Square is N * N), Squares).
```

#### **Explanation:**

- **Template (`Square`):** Represents the square of each number.
- **Goal (`between(1, 5, N), Square is N * N`):**
  - `between(1, 5, N)`: Generates numbers from 1 to 5.
  - `Square is N * N`: Calculates the square of `N`.
- **List (`Squares`):** Collects all squared values into `Squares`.

#### **Result:**

```prolog
Squares = [1, 4, 9, 16, 25].
```

---

### **Example 3:Collecting Sat Literals**

#### **Prolog Code:**

```prolog
relateDoesVarsWithBusyAtHourVars :- 
    available(G,H), 
    findall( does(G,T,H), task(T), Lits ), 
    expressOr( busyAtHour(G,H), Lits ), 
    fail.
relateDoesVarsWithBusyAtHourVars.
```

 **`findall/3` Usage:**
   - **Template:** `does(G, T, H)`
     - For each available gangster `G` at hour `H`, we want to collect all possible `does(G, T, H)` predicates, where `T` represents a task.
   - **Goal:** `task(T)`
     - For each gangster `G` and hour `H`, iterate over all tasks `T` that can be assigned.
   - **List:** `Lits`
     - Collects all `does(G, T, H)` instances into the list `Lits`.

**What `findall/3` Does Here:**

For each available gangster `G` at hour `H`, `findall/3` gathers all possible task assignments (`does(G, T, H)`) into the list `Lits`. This list is then used to express that if any of these assignments occur, the gangster is busy at that hour.


---

### **Example 4: Collecting SAT literals 2**

**`findall/3` Usage:**
   - **Template:** `-busyAtHour(G, H)`
     - For each hour `H` between `HIni` and `HFin`, collect the negation of `busyAtHour(G, H)`.
   - **Goal:** `between(HIni, HFin, H)`
     - Generates all hours `H` from `HIni` to `HFin` inclusive.
   - **List:** `Clause`
     - Collects all `-busyAtHour(G, H)` instances into the list `Clause`.


**What `findall/3` Does Here:**

For each gangster `G` and starting hour `HIni`, `findall/3` gathers all negated busy hour literals (`-busyAtHour(G, H)`) within the range `HIni` to `HFin`. This list `Clause` is then used to create a clause that enforces the constraint that gangster `G` cannot be busy for more than `K` consecutive hours.

#### **1.4.2. `expressOr/2` and `expressAnd/2`**

- **Purpose**: Define logical relationships between variables and lists of literals, often used in SAT encoding.
  
- **`expressOr(Var, Lits)`**
  - **Definition**: Specifies that `Var` is true if at least one literal in `Lits` is true.
  - **Usage**:
    ```prolog
    expressOr(a, [x, y]).
    ```
    Translates to: `a <--> (x ∨ y)`
  
- **`expressAnd(Var, Lits)`**
  - **Definition**: Specifies that `Var` is true only if all literals in `Lits` are true.
  - **Usage**:
    ```prolog
    expressAnd(b, [x, y, z]).
    ```
    Translates to: `b <--> (x ∧ y ∧ z)`

- **Example Implementation**:
  ```prolog
  expressOr(Var, Lits) :-
      % Var is true if any of Lits is true
      write(Var), write(' is true if any of '), write(Lits), nl,
      % Additional logic to encode this relationship
      ...
  
  expressAnd(Var, Lits) :-
      % Var is true only if all of Lits are true
      write(Var), write(' is true only if all of '), write(Lits), nl,
      % Additional logic to encode this relationship
      ...
  ```

#### **1.4.3. Cardinality Constraints (`atLeast/2`, `atMost/2`, `exactly/2`)**

- **Purpose**: Enforce constraints on the number of literals that can be true within a list.
  
- **`atLeast(K, Lits)`**
  - **Definition**: At least `K` literals in `Lits` must be true.
  - **Example**:
    ```prolog
    % At least 2 gangsters must be available at hour H for task T
    atLeast(2, [does(g01, T, H), does(g02, T, H), does(g03, T, H)]).
    ```
  
- **`atMost(K, Lits)`**
  - **Definition**: At most `K` literals in `Lits` can be true.
  - **Example**:
    ```prolog
    % At most 1 task can be assigned to gangster G at hour H
    atMost(1, [does(G, killing, H), does(G, countingMoney, H), does(G, politics, H)]).
    ```
  
- **`exactly(K, Lits)`**
  - **Definition**: Exactly `K` literals in `Lits` must be true.
  - **Example**:
    ```prolog
    % Exactly 3 gangsters must be available for task T at hour H
    exactly(3, [does(g01, T, H), does(g02, T, H), does(g03, T, H), does(g04, T, H)]).
    ```
  
- **Example Implementation**:
  ```prolog
  atLeast(K, Lits) :-
      % Ensure at least K literals in Lits are true
      ...
  
  atMost(K, Lits) :-
      % Ensure at most K literals in Lits are true
      ...
  
  exactly(K, Lits) :-
      atLeast(K, Lits),
      atMost(K, Lits).
  ```

#### **1.4.4. `writeOneClause/1`**

- **Purpose**: Outputs a clause (a disjunction of literals) to a file or standard output, often used in SAT encoding.
- **Syntax**: `writeOneClause([Literal1, Literal2, ..., LiteralN]).`
- **Example**:
  ```prolog
  % Writes the clause (¬does(g01, killing, 1) ∨ ¬does(g02, countingMoney, 2))
  writeOneClause([-does(g01, killing, 1), -does(g02, countingMoney, 2)]).
  ```

- **Example Implementation**:
  ```prolog
  writeOneClause([]) :- 
      % Represents an empty clause, which is always false
      write('0'), nl.
  
  writeOneClause([Lit|Lits]) :- 
      write(Lit), write(' '),
      writeOneClause(Lits).
  writeOneClause([]) :- write('0'), nl.
  ```

#### **1.4.5. Higher-Order Predicates**

- **`maplist/2` and `maplist/3`**
  - **Purpose**: Apply a predicate to each element (or corresponding elements) of a list.
  - **Syntax**:
    - `maplist(Predicate, List).`
    - `maplist(Predicate, List1, List2).`
  - **Example**:
    ```prolog
    % Define a predicate to double a number
    double(X, Y) :- Y is X * 2.
    
    % Apply 'double' to each element in the list
    ?- maplist(double, [1, 2, 3], Doubled).
    Doubled = [2, 4, 6].
    ```

- **`include/3` and `exclude/3`**
  - **Purpose**: Filter elements of a list based on a predicate.
  - **Syntax**:
    - `include(Predicate, List, Included).`
    - `exclude(Predicate, List, Excluded).`
  - **Example**:
    ```prolog
    % Define a predicate to check if a number is even
    is_even(X) :- 0 is X mod 2.
    
    % Include only even numbers
    ?- include(is_even, [1, 2, 3, 4], Evens).
    Evens = [2, 4].
    
    % Exclude even numbers
    ?- exclude(is_even, [1, 2, 3, 4], Odds).
    Odds = [1, 3].
    ```

#### **1.4.6. Defining Custom Predicates**

Creating custom predicates allows you to encapsulate logic and reuse code efficiently.

- **Example: Checking Availability**
  ```prolog
  % A gangster is available at hour H if they are a gangster and H is a valid hour
  % and they are not blocked at H.
  available(G, H) :- 
      gangster(G), 
      hour(H), 
      \+ blocked(G, H).
  ```

- **Example: Assigning Tasks**
  ```prolog
  % Assign task T to gangster G at hour H if G is available
  assign_task(G, T, H) :- 
      available(G, H), 
      does(G, T, H).
  ```

#### **1.4.7. Recursion**

Recursion allows Prolog to perform iterative computations by having predicates call themselves with modified arguments.

- **Example: Calculating Factorial**
  ```prolog
  factorial(0, 1).
  factorial(N, F) :-
      N > 0,
      N1 is N - 1,
      factorial(N1, F1),
      F is N * F1.
  
  % Usage:
  ?- factorial(5, F).
  F = 120.
  ```

- **Example: Traversing a List**
  ```prolog
  % Prints each element of a list
  print_list([]).
  print_list([H|T]) :-
      write(H), nl,
      print_list(T).
  
  % Usage:
  ?- print_list([a, b, c]).
  a
  b
  c
  ```

#### **1.4.8. Error Handling and Guards**

Using guards and conditions to handle different cases and prevent errors.

- **Example: Safe Division**
  ```prolog
  safe_divide(_, 0, _) :- 
      write('Error: Division by zero.'), nl, 
      fail.
  safe_divide(X, Y, Z) :- 
      Z is X / Y.
  
  % Usage:
  ?- safe_divide(10, 2, Z).
  Z = 5.
  
  ?- safe_divide(10, 0, Z).
  Error: Division by zero.
  false.
  ```
---

## 2. SAT Optimization (Gangsters Problem)

### **2.1. Problem Overview**

- **Objective**: Assign tasks to gangsters while minimizing constraints like maximum consecutive working hours.
- **Components**:
  - **Tasks**: Different activities requiring gangsters.
  - **Gangsters**: Individuals with availability constraints.
  - **Hours**: Time slots for task assignments.

### **2.2. Key Concepts**

#### **2.2.1. SAT Variables**

- **Definition**: Boolean variables representing task assignments and states.
- **Examples**:
  ```prolog
  does(G, T, H)        % "gangster G does task T at hour H"
  busyAtHour(G, H)    % "gangster G is busy at hour H"
  doesTask(G, T)      % "gangster G does task T"
  ```

#### **2.2.2. Clause Generation**

- **Purpose**: Translate problem constraints into SAT clauses.
- **Examples**:
  ```prolog
  writeClauses(MaxConsecutiveHours) :-
      enoughGangstersAtEachHour,
      eachHourEachGangsterAtMostOneTask,
      noGangsterDoesTwoDifferentTasksConsecutively,
      ...
  ```

#### **2.2.3. Constraints Implementation**

- **At Least / At Most Constraints**:
  ```prolog
  atLeast(K, Lits).
  atMost(K, Lits).
  ```
- **Expressing Logical Relations**:
  ```prolog
  expressOr(Var, Lits).
  expressAnd(Var, Lits).
  ```

### **2.3. Example Predicates**

#### **2.3.1. `available/2`**

- **Description**: Determines if a gangster is available at a specific hour.
- **Usage**:
  ```prolog
  available(G, H) :- hour(H), gangster(G), \+ blocked(G, H).
  ```

#### **2.3.2. `writeClauses/1`**

- **Description**: Generates all necessary clauses for the SAT solver based on constraints.
- **Usage**:
  ```prolog
  writeClauses(MaxConsecutiveHours) :-
      enoughGangstersAtEachHour,
      ...
  ```

#### **2.3.3. `displaySol/1`**

- **Description**: Displays the solution in a readable format.
- **Usage**:
  ```prolog
  displaySol(M) :- 
      write('gangster '), write(G), write(': '), ... 
  ```

### **2.4. Useful Built-in Predicates**

- **`findall/3`**: Collects all relevant SAT variables for constraints.
- **`member/2`**: Checks membership within lists of gangsters or tasks.
- **`nth1/3`**: Accesses elements in lists representing task needs per hour.
- **`between/3`**: Iterates over hour ranges for constraint applications.

### **2.5. Advanced Techniques**

- **Optimization with Binary Search**: Iteratively finding the minimal `K` by reducing the search space.
- **Transformation of Logical Relations**: Using Tseitin transformations to handle equivalences.

---

## 3. Advanced Prolog (Graph Manipulation)

### **3.1. Problem Overview**

- **Objective**: Determine the minimal number of vertex exchanges to transform one graph into another.
- **Components**:
  - **Graphs**: Represented as lists of edges in normal form.
  - **Exchanges**: Swapping vertices to achieve the target graph.

### **3.2. Key Concepts**

#### **3.2.1. Graph Representation**

- **Normal Form (NF)**:
  - Edges are ordered with the first vertex less than the second.
  - Edge list is sorted lexicographically.
- **Example**:
  ```prolog
  [[1,2], [1,4], [1,5], [2,3], [2,4], [3,5]]
  ```

#### **3.2.2. Vertex Exchange (`subs/3`)**

- **Definition**: Swaps two vertices across all edges in the graph.
- **Usage**:
  ```prolog
  subs([U, V], [[U, W]|T1], [[V, W]|T2]).
  ```

#### **3.2.3. Recursion and Backtracking**

- **Usage**: Explore all possible exchange sequences to reach the target graph.
- **Example**:
  ```prolog
  computePath(Cost, State, FinalState, PathSoFar, TotalPath) :-
      ...
  ```

### **3.3. Example Predicates**

#### **3.3.1. `oneStep/3`**

- **Description**: Applies a single vertex exchange to transform one graph into another.
- **Usage**:
  ```prolog
  oneStep(1, G, H) :-
      order(G, N),
      between(1, N, U), between(1, N, V), U \= V,
      subs([U, V], G, I),
      normalForm(I, H).
  ```

#### **3.3.2. `normalForm/2`**

- **Description**: Ensures the graph is in normal form after transformations.
- **Usage**:
  ```prolog
  normalForm(X, Y) :-
      edgeSort(X, Z),
      sort(Z, Y).
  ```

#### **3.3.3. `computePath/5`**

- **Description**: Recursively computes the sequence of exchanges to reach the target graph.
- **Usage**:
  ```prolog
  computePath(0, S, S, C, C).
  computePath(Cost, State, FinalState, PathSoFar, TotalPath) :-
      ...
  ```

### **3.4. Useful Built-in Predicates**

- **`flatten/2`**: Flattens nested lists, useful for processing edge lists.
- **`max_list/2`**: Finds the maximum element in a list.
- **`sort/2`**: Sorts lists, removing duplicates.
- **`length/2`**: Determines the length of a list.

### **3.5. Optimization Techniques**

- **Pruning with `\+ member/2`**: Avoids revisiting already explored states.
- **Cost-Based Iteration**: Incrementally increasing the allowed number of steps to find the minimal solution.

---

## 4. Constraint Logic Programming (CLP) FD - Pseudo-Boolean Constraints

### **4.1. Problem Overview**

- **Objective**: Determine logical implications between pseudo-boolean constraints.
- **Components**:
  - **Constraints**: Linear inequalities involving Boolean variables.
  - **Implications**: Logical relationships where one constraint enforces another.

### **4.2. Key Concepts**

#### **4.2.1. Pseudo-Boolean Constraints**

- **Definition**: Linear inequalities with Boolean variables.
- **Form**: `a1*x1 + a2*x2 + ... + an*xn >= a0`
- **Representation in Prolog**:
  ```prolog
  [1, 3, 3] >= 4  % Represents 1*x1 + 3*x2 + 3*x3 >= 4
  ```

#### **4.2.2. Logical Consequence**

- **Definition**: Constraint B is a consequence of Constraint A if every model of A satisfies B.
- **Prolog Implementation**: Using SAT solving techniques to verify implications.

#### **4.2.3. Counterexamples**

- **Definition**: Assignments that satisfy the premise but not the consequence.
- **Usage**: Demonstrates that an implication does not hold.

### **4.3. Example Predicates**

#### **4.3.1. `implies/2`**

- **Description**: Checks if one constraint logically implies another.
- **Implementation**:
  ```prolog
  implies(A >= R, B >= S) :-
      does_not_imply(A >= R, B >= S, _), !,
      fail.
  implies(_, _).
  ```

#### **4.3.2. `does_not_imply/3`**

- **Description**: Determines if a constraint does not imply another and provides a counterexample.
- **Implementation**:
  ```prolog
  does_not_imply(A >= R, B >= S, X) :-
      length(A, N),
      length(X, N),
      X ins 0..1,
      scalar_product(A, X, #>=, R),
      scalar_product(B, X, #<, S),
      label(X),
      true.
  ```

#### **4.3.3. `scalar_product/4`**

- **Description**: Computes the scalar product of a list of coefficients and variables.
- **Usage**:
  ```prolog
  scalar_product([1,3,3], [X1, X2, X3], #>=, 4)
  ```

### **4.4. Constraint Logic Programming (CLP) FD**

#### **4.4.1. `ins/2`**

- **Description**: Defines the domain of variables.
- **Usage**:
  ```prolog
  X ins 0..1  % X is a Boolean variable
  ```

#### **4.4.2. `label/1`**

- **Description**: Assigns values to variables within their domains to satisfy constraints.
- **Usage**:
  ```prolog
  label(X)
  ```

### **4.5. Example Usage**

#### **4.5.1. Implication Holds**

```prolog
?- implies([1, 3, 3] >= 4, [1, 1, 1] >= 2).
true.
```

#### **4.5.2. Implication Does Not Hold**

```prolog
?- implies([1, 3, 3] >= 4, [1, 1, 1] >= 3).
false.
```

#### **4.5.3. Finding Counterexamples**

```prolog
?- does_not_imply([1, 3, 3] >= 4, [1, 1, 1] >= 3, X).
X = [0, 1, 1].
```

### **4.6. Useful Built-in Predicates**

- **`length/2`**: Determines the length of a list.
- **`scalar_product/4`**: Computes linear combinations.
- **`label/1`**: Assigns values to variables.
- **`ins/2`**: Sets variable domains.

### **4.7. Advanced Techniques**

- **Logical Equivalence Transformation**: Using negation and conjunction to test implications.
- **Model Generation**: Leveraging CLP(FD) solvers to find satisfying assignments.

---
