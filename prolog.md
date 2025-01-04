# Prolog Cheat Sheet

A comprehensive cheat sheet covering Prolog concepts from basic syntax to advanced topics. Use this as a quick reference for your exams and future projects.

## Table of Contents

1. [Prolog Basic Syntax](#1-prolog-basic-syntax)
   1. [Basic Prolog](#11-basic-prolog)
   2. [Lists](#12-lists)
   3. [Advanced Predicates](#13-advanced-predicates)
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

### 1.2. Lists

Lists are fundamental data structures in Prolog, representing ordered collections of elements. They are versatile and widely used in various algorithms and data manipulations.

#### **1.2.1. List Basics**

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

#### **1.2.2. Head and Tail**

- **Definition**: The first element of a list is the head, and the remainder of the list is the tail.
- **Syntax**: `[Head|Tail]`
- **Examples**:
  ```prolog
  % Splitting a list into head and tail
  [H|T] = [a, b, c, d].
  % H = a
  % T = [b, c, d]
  ```

#### **1.2.3. Common List Predicates**

1. **`member/2`**
   - **Purpose**: Checks if an element is part of a list.
   - **Syntax**: `member(Element, List).`
   - **Example**:
     ```prolog
     ?- member(b, [a, b, c]).
     true.
     
     ?- member(d, [a, b, c]).
     false.
     ```
   - **Usage in Definitions**:
     ```prolog
     % Define blocked hours
     blocked(G, H) :- notAvailable(G, L), member(H, L).
     ```

2. **`append/3`**
   - **Purpose**: Concatenates two lists.
   - **Syntax**: `append(List1, List2, Result).`
   - **Example**:
     ```prolog
     ?- append([a, b], [c, d], X).
     X = [a, b, c, d].
     
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

3. **`length/2`**
   - **Purpose**: Determines the length of a list or creates a list of a certain length.
   - **Syntax**: `length(List, Length).`
   - **Example**:
     ```prolog
     ?- length([a, b, c], N).
     N = 3.
     
     ?- length(X, 2).
     X = [_G1, _G2].
     ```

4. **`nth1/3`**
   - **Purpose**: Retrieves the N-th element from a list (1-based indexing).
   - **Syntax**: `nth1(Index, List, Element).`
   - **Example**:
     ```prolog
     ?- nth1(2, [a, b, c], X).
     X = b.
     
     ?- nth1(4, [a, b, c], X).
     false.
     ```

5. **`findall/3`**
   - **Purpose**: Collects all possible solutions that satisfy a certain condition into a list.
   - **Syntax**: `findall(Template, Goal, List).`
   - **Example**:
     ```prolog
     % Collect all even numbers from 1 to 5
     ?- findall(X, (between(1, 5, X), X mod 2 =:= 0), L).
     L = [2, 4].
     ```

6. **`maplist/2` and `maplist/3`**
   - **Purpose**: Applies a predicate to each element of a list.
   - **Syntax**:
     - `maplist(Predicate, List).`
     - `maplist(Predicate, List1, List2).`
   - **Example**:
     ```prolog
     % Increment each number in the list
     increment(X, Y) :- Y is X + 1.
     
     ?- maplist(increment, [1, 2, 3], L).
     L = [2, 3, 4].
     ```

7. **`select/3`**
   - **Purpose**: Selects an element from a list, returning the element and the remaining list.
   - **Syntax**: `select(Element, List, Rest).`
   - **Example**:
     ```prolog
     ?- select(b, [a, b, c], X).
     X = [a, c].
     
     ?- select(X, [1, 2, 3], [1, 3]).
     X = 2.
     ```

8. **`reverse/2`**
   - **Purpose**: Reverses a list.
   - **Syntax**: `reverse(List, Reversed).`
   - **Example**:
     ```prolog
     ?- reverse([a, b, c], X).
     X = [c, b, a].
     ```

#### **1.2.4. Recursive List Processing**

Many list operations in Prolog are defined recursively. Understanding recursion is key to manipulating lists effectively.

- **Example: Summing Elements in a List**
  ```prolog
  sum_list([], 0).
  sum_list([H|T], Sum) :-
      sum_list(T, Rest),
      Sum is H + Rest.
  
  % Usage:
  ?- sum_list([1, 2, 3, 4], Sum).
  Sum = 10.
  ```

- **Example: Finding the Maximum Element**
  ```prolog
  max_list([X], X).
  max_list([H|T], Max) :-
      max_list(T, TempMax),
      Max is max(H, TempMax).
  
  % Usage:
  ?- max_list([3, 1, 4, 2], Max).
  Max = 4.
  ```

### 1.3. Advanced Predicates

Beyond the basics, Prolog offers a suite of advanced predicates that facilitate complex operations, especially when dealing with logic programming and constraint solving.

#### **1.3.1. `findall/3`**

- **Purpose**: Gathers all possible instances that satisfy a particular goal into a list.
- **Syntax**: `findall(Template, Goal, List).`
- **Example**:
  ```prolog
  % Find all tasks that gangster G is available to perform at hour H
  available_tasks(G, H, Tasks) :-
      findall(T, (task(T), available(G, H)), Tasks).
  
  % Usage:
  ?- available_tasks(g01, 10, Tasks).
  Tasks = [killing, countingMoney, politics].
  ```

#### **1.3.2. `expressOr/2` and `expressAnd/2`**

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

#### **1.3.3. Cardinality Constraints (`atLeast/2`, `atMost/2`, `exactly/2`)**

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

#### **1.3.4. `writeOneClause/1`**

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

#### **1.3.5. Higher-Order Predicates**

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

#### **1.3.6. Defining Custom Predicates**

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

#### **1.3.7. Recursion**

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

#### **1.3.8. Error Handling and Guards**

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
