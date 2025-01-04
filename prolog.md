```markdown
# Prolog Cheat Sheet

A comprehensive cheat sheet covering Prolog concepts from basic syntax to advanced topics such as SAT optimization, graph algorithms, and Constraint Logic Programming (CLP). Use this as a quick reference for your exam.

## Table of Contents

1. [Prolog Basic Syntax](#1-prolog-basic-syntax)
2. [SAT Optimization (Gangsters Problem)](#2-sat-optimization-gangsters-problem)
3. [Advanced Prolog (Graph Manipulation)](#3-advanced-prolog-graph-manipulation)
4. [Constraint Logic Programming (CLP) FD - Pseudo-Boolean Constraints](#4-constraint-logic-programming-clp-fd---pseudo-boolean-constraints)

---

## 1. Prolog Basic Syntax

### **1.1. Facts**

- **Definition**: Basic assertions about the world.
- **Syntax**: `predicate(argument1, argument2, ...).`
- **Example**:
  ```prolog
  gangster(g01).
  notAvailable(g01, [6,13,14,16,21,35,37,41,59]).
  ```

### **1.2. Rules**

- **Definition**: Logical implications that define relationships between facts.
- **Syntax**: `Head :- Body.`
- **Example**:
  ```prolog
  available(G, H) :- hour(H), gangster(G), \+ blocked(G, H).
  ```

### **1.3. Variables**

- **Definition**: Placeholders that can be unified with terms.
- **Naming**: Start with uppercase letters or underscores.
- **Example**: `G`, `H`, `Cost`

### **1.4. Predicates**

- **Definition**: Relations defined by facts and rules.
- **Usage**: Serve as the primary means of computation and querying.
- **Example**:
  ```prolog
  task(T).
  needed(T, H, N).
  ```

### **1.5. Lists**

- **Definition**: Ordered collections of elements.
- **Syntax**: `[Element1, Element2, ..., ElementN]`
- **Example**:
  ```prolog
  gangsters([g01, g02, g03, g04, g05, g06, g07, g08, g09, g10, g11, g12]).
  ```

### **1.6. Comments**

- **Single-line**: `% This is a comment`
- **Multi-line**: `/* This is a multi-line comment */`

### **1.7. Built-in Predicates**

- **`member/2`**: Checks if an element is in a list.
  ```prolog
  member(X, [X|_]).
  member(X, [_|T]) :- member(X, T).
  ```
- **`nth1/3`**: Retrieves the N-th element from a list (1-based indexing).
  ```prolog
  nth1(2, [a, b, c], Element). % Element = b
  ```
- **`findall/3`**: Collects all possible solutions.
  ```prolog
  findall(X, member(X, [1,2,3]), List). % List = [1,2,3]
  ```
- **`between/3`**: Generates numbers within a range.
  ```prolog
  between(1, 5, X). % X = 1; X = 2; ...; X = 5
  ```

### **1.8. Control Constructs**

- **Cut (`!`)**: Prevents backtracking beyond the cut.
  ```prolog
  predicate :- condition, !, action.
  ```
- **Failure (`fail`)**: Forces a predicate to fail, useful in iterative constructs.
  ```prolog
  somePredicate :- action, fail.
  somePredicate.
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

**Good luck on your exam!**

```
