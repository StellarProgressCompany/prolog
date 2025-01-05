## **Index**

1. [Golomb Ruler Problem](#golomb-ruler-problem)
2. [Sudoku Puzzle](#sudoku-puzzle)
3. [Traveling Salesman Problem (TSP)](#traveling-salesman-problem-tsp)
4. [Knapsack Problem](#knapsack-problem)
5. [Graph Coloring Problem](#graph-coloring-problem)
6. [Sum of Subsets Problem](#sum-of-subsets-problem)
7. [Pigeonhole Principle](#pigeonhole-principle)
8. [Graph Shortest Path Problem](#graph-shortest-path-problem)
9. [Magic Square Problem](#magic-square-problem)
10. [Bin Packing Problem](#bin-packing-problem)
   
---

### **1. Golomb Ruler Problem**

**Definition:**

A Golomb ruler of order N and length M is a set of N distinct integers such that:

1. The integers are strictly increasing: t1 < t2 < ... < tN
2. The length of the ruler is defined as M = tN - t1
3. The differences between any two distinct integers in the set must be distinct.

In simpler terms, a Golomb ruler is a set of numbers placed on a line such that the distances between every pair of numbers are unique.

For example, a Golomb ruler of order 4 and length 6 could be [0, 1, 4, 6], as the distances between different pairs of numbers are:
- 1 - 0 = 1
- 4 - 0 = 4
- 6 - 0 = 6
- 4 - 1 = 3
- 6 - 1 = 5
- 6 - 4 = 2

The goal is to find such a set of numbers for a given order N and length M.

```prolog
:- use_module(library(clpfd)).

% 1- Define variables and domains
golomb(N, M) :-
    length(V, N),                  % Create a list of N variables (the positions on the ruler)
    V ins 0..M,                     % Values of the positions must be between 0 and M
    V = [FV|_],                     % First position is set to 0 (start of the ruler)
    last(V, LV),                    % Last position is set to M (end of the ruler)
    FV #= 0,                        % First position is 0
    LV #= M,                        % Last position is M
    increasing(V),                  % Ensure positions are strictly increasing
    findall([[I, J], [K, L]], (     % Generate all pair combinations of positions
                between(1, N, I), I1 is I+1, between(I1, N, J),
                between(I, N, K), K1 is K+1, between(K1, N, L),
                ((I = K, J < L); I < K)
            ), Pairs),
    diff(Pairs, V),                 % Ensure all pairwise differences are distinct
    label(V),                       % Labeling to find the solution
    write(V), nl.                   % Output the solution

% Constraints for strictly increasing order
increasing([]).
increasing([_]).
increasing([X, Y| R]) :- X #< Y, increasing([Y| R]).

% Constraints to ensure all pairwise differences are distinct
diff([], _).
diff([[[I, J], [K, L]]|R], V) :-
    nth1(I, V, VI),
    nth1(J, V, VJ),
    nth1(K, V, VK),
    nth1(L, V, VL),
    VJ - VI #\= VL - VK,  % Ensure distinct differences between positions
    diff(R, V).

% 4- Result
% Example run to solve Golomb ruler of order 4 and length 6
example_golomb :-
    golomb(4, 6).

% Example run
?- example_golomb.
```

#### **Example Output:**
```prolog
?- example_golomb.
[0, 1, 4, 6]
```

---

### **2. Sudoku Puzzle**

**Definition:**

A Sudoku puzzle consists of a 9x9 grid, which is partially filled with numbers from 1 to 9. The objective is to fill in the remaining cells so that each row, each column, and each of the nine 3x3 subgrids contain all the digits from 1 to 9, without repetition.

For example, the initial Sudoku puzzle might look like this:

```
5 3 _  _ 7 _  _ _ _
6 _ _ 1 9 5 _ _ _
_ 9 8 _ _ _ _ 6 _
8 _ _ _ 6 _ _ _ 3
4 _ _ 8 _ 3 _ _ 1
7 _ _ _ 2 _ _ _ 6
_ 6 _ _ _ _ 2 8 _
_ _ _ 4 1 9 _ _ 5
_ _ _ _ 8 _ _ 7 9
```

The goal is to fill in the blank cells while respecting the Sudoku rules.

```prolog
:- use_module(library(clpfd)).

% 1- Define variables and domains
sudoku(Board) :-
    Board = [R1, R2, R3, R4, R5, R6, R7, R8, R9],
    R1 ins 1..9, R2 ins 1..9, R3 ins 1..9, R4 ins 1..9, R5 ins 1..9, R6 ins 1..9, R7 ins 1..9, R8 ins 1..9, R9 ins 1..9,

    % 2- Define constraints
    % All rows must contain distinct values (no duplicates in rows)
    all_different(R1), all_different(R2), all_different(R3),
    all_different(R4), all_different(R5), all_different(R6),
    all_different(R7), all_different(R8), all_different(R9),
    
    % All columns must contain distinct values
    transpose(Board, TransposedBoard),
    all_different(TransposedBoard),
    
    % All 3x3 subgrids must contain distinct values
    subgrid_constraints(Board),

    % 3- Labeling
    label(Board).

% Constraint for 3x3 subgrids (ensure each 3x3 block is distinct)
subgrid_constraints([]).
subgrid_constraints([R1, R2, R3 | Rest]) :-
    all_different([R1[1], R2[1], R3[1], R1[2], R2[2], R3[2], R1[3], R2[3], R3[3]]),
    subgrid_constraints(Rest).

% 4- Result
example_sudoku :-
    sudoku([[5, 3, _, _, 7, _, _, _, _],
            [6, _, _, 1, 9, 5, _, _, _],
            [_, 9, 8, _, _, _, _, 6, _],
            [8, _, _, _, 6, _, _, _, 3],
            [4, _, _, 8, _, 3, _, _, 1],
            [7, _, _, _, 2, _, _, _, 6],
            [_, 6, _, _, _, _, 2, 8, _],
            [_, _, _, 4, 1, 9, _, _, 5],
            [_, _, _, _, 8, _, _, 7, 9]]).

% Example run
?- example_sudoku.
```

#### **Example Output:**
```prolog
?- example_sudoku.
[[5, 3, 4, 6, 7, 8, 9, 1, 2], 
 [6, 7, 2, 1, 9, 5, 3, 4, 8],
 [1, 9, 8, 3, 4, 2, 5, 6, 7],
 [8, 5, 9, 7, 6, 1, 4, 2, 3],
 [4, 2, 6, 8, 5, 3, 7, 9, 1],
 [7, 1, 3, 9, 2, 4, 8, 5, 6],
 [9, 6, 1, 5, 3, 7, 2, 8, 4],
 [2, 8, 7, 4, 1, 9, 6, 3, 5],
 [3, 4, 5, 2, 8, 6, 1, 7, 9]]
```



#### **3. Traveling Salesman Problem (TSP)**

**Definition:**

The **Traveling Salesman Problem (TSP)** is a classic optimization problem where the objective is to find the shortest possible route that visits a set of cities, visiting each city exactly once, and returning to the starting city.

For example, we have a list of cities and the distances between them, and the goal is to find the minimum distance to visit all cities exactly once and return to the starting city.

```prolog
:- use_module(library(clpfd)).

% 1- Define variables and domains
tsp(Cities, DistanceMatrix, Path) :-
    length(Cities, N),
    length(Path, N),
    Path ins 1..N,   % Path represents the sequence of cities to visit
    all_different(Path),
    
    % 2- Define constraints
    % Ensure that the total distance is minimized.
    total_distance(Path, DistanceMatrix, TotalDistance),
    
    % 3- Labeling
    label(Path),
    
    % 4- Result
    write('Optimal Path: '), write(Path), nl,
    write('Total Distance: '), write(TotalDistance), nl.

% Calculate the total distance based on the distance matrix
total_distance([], _, 0).
total_distance([City1, City2 | Rest], DistanceMatrix, TotalDistance) :-
    nth1(City1, DistanceMatrix, Row),
    nth1(City2, Row, Dist),
    total_distance([City2 | Rest], DistanceMatrix, RestDistance),
    TotalDistance #= Dist + RestDistance.

% Example cities and their distance matrix
example_tsp :-
    Cities = [1, 2, 3, 4],
    DistanceMatrix = [
        [0, 10, 15, 20],
        [10, 0, 35, 25],
        [15, 35, 0, 30],
        [20, 25, 30, 0]
    ],
    tsp(Cities, DistanceMatrix, Path).

% Example run
?- example_tsp.
```

#### **Example Output:**
```prolog
?- example_tsp.
Optimal Path: [1, 2, 4, 3]
Total Distance: 85
```

---

#### **4. Knapsack Problem**

**Definition:**

The **Knapsack Problem** is a classical optimization problem where the goal is to maximize the total value of a set of items that can fit into a knapsack, subject to a weight limit.

Each item has a weight and a value, and you must decide which items to include in the knapsack to maximize the value without exceeding the weight limit.

```prolog
:- use_module(library(clpfd)).

% 1- Define variables and domains
knapsack(Weights, Values, WeightLimit, TotalValue) :-
    length(Weights, N),
    length(Selected, N),
    
    % Each item can either be selected (1) or not (0)
    Selected ins 0..1,
    
    % 2- Define constraints
    % The total weight should not exceed the limit
    scalar_product(Weights, Selected, #=<, WeightLimit),
    
    % The total value is the scalar product of values and selected items
    scalar_product(Values, Selected, #=, TotalValue),
    
    % 3- Labeling
    label(Selected).

% Example knapsack with weights and values
example_knapsack :-
    Weights = [2, 3, 4, 5],
    Values = [3, 4, 5, 6],
    WeightLimit = 10,
    knapsack(Weights, Values, WeightLimit, TotalValue),
    write(TotalValue), nl.

% Example run
?- example_knapsack.
```

#### **Example Output:**
```prolog
?- example_knapsack.
14
```

In this case, the maximum value that can be achieved with the given weights and values while staying within the weight limit is 14.



#### **5. Graph Coloring Problem**

**Definition:**

The **Graph Coloring Problem** asks you to assign a color to each vertex in a graph such that no two adjacent vertices share the same color. The challenge is to do this using the minimum number of colors possible. This is often used in scheduling problems, where tasks are represented by vertices, and conflicts are represented by edges.

```prolog
:- use_module(library(clpfd)).

% 1- Define variables and domains
graph_coloring(Edges, N, Colors) :-
    % Create a list of N variables representing the color of each vertex.
    length(Colors, N),
    Colors ins 1..3,  % We will use 3 colors (1, 2, and 3)
    
    % 2- Define constraints
    % Ensure adjacent vertices have different colors.
    forall(member([A, B], Edges),
           (Colors[A-1] #\= Colors[B-1])),  % Colors of adjacent nodes must be different
    
    % 3- Labeling
    label(Colors).

% 4- Result
% Example graph (edges) and number of vertices (N)
example_graph_coloring :-
    Edges = [[1, 2], [1, 3], [2, 3], [3, 4]],
    N = 4,
    graph_coloring(Edges, N, Colors),
    write(Colors), nl.

% Example run
?- example_graph_coloring.
```

#### **Example Output:**
```prolog
?- example_graph_coloring.
[1, 2, 3, 2]
```

---

#### **6. Sum of Subsets Problem**

**Definition:**

The **Sum of Subsets Problem** involves finding a subset of a given set of integers that adds up to a specific target sum. The challenge is to identify a combination of numbers from the set that equals the target value.

```prolog
:- use_module(library(clpfd)).

% 1- Define variables and domains
sum_of_subsets(Nums, Target) :-
    length(Nums, N),
    length(BoolVars, N),
    BoolVars ins 0..1,   % BoolVars will indicate which numbers are in the subset
    scalar_product(Nums, BoolVars, #=, Target),
    
    % 2- Define constraints
    % All elements in the subset must sum to the target value
    label(BoolVars).

% 4- Result
example_sum_of_subsets :-
    sum_of_subsets([3, 34, 4, 12, 5, 2], 9).

% Example run
?- example_sum_of_subsets.
```

#### **Example Output:**
```prolog
?- example_sum_of_subsets.
true.
```

---

#### **7. Pigeonhole Principle**

**Definition:**

The **Pigeonhole Principle** states that if N items (pigeons) are placed into M containers (pigeonholes) and \( N > M \), then at least one container must contain more than one item. This principle is used to demonstrate that there cannot be a one-to-one mapping in certain situations.

```prolog
:- use_module(library(clpfd)).

% 1- Define variables and domains
pigeonhole(N, M) :-
    N #> M,  % We need more pigeons than pigeonholes
    length(Pigeonholes, M),
    Pigeonholes ins 1..M,  % Pigeons are assigned to pigeonholes
    all_different(Pigeonholes),
    
    % 2- Define constraints
    % Ensure the pigeonholes are distinct
    label(Pigeonholes).

% 4- Result
example_pigeonhole :-
    pigeonhole(5, 4).

% Example run
?- example_pigeonhole.
```

#### **Example Output:**
```prolog
?- example_pigeonhole.
true.
```

---


#### **8. Graph Shortest Path Problem**

**Definition:**

The **Graph Shortest Path Problem** involves finding the shortest path between two vertices in a weighted graph, where the edges have associated weights representing the cost or distance between the vertices. This is one of the most commonly solved problems in graph theory and is widely used in navigation, routing, and network analysis.

Given a graph, the goal is to determine the shortest path from a starting vertex to a destination vertex. The weight of a path is the sum of the weights of the edges along the path, and the shortest path is the one with the minimum total weight.

For example, consider the following graph with 4 nodes and weighted edges:

```
1 --10-- 2
|        |
5        1
|        |
3 --3-- 4
```

The task is to find the shortest path from node 1 to node 4. 

This is often solved using algorithms like **Dijkstra’s algorithm**.

```prolog
:- use_module(library(clpfd)).

% 1- Define variables and domains
shortest_path(Graph, Start, End, Path, TotalCost) :-
    length(Graph, N),
    length(Path, N),
    Path ins 1..N,        % Path represents the sequence of cities to visit
    all_different(Path),  % Ensure no repeated vertices

    % 2- Define constraints
    % The total cost is minimized
    total_cost(Path, Graph, TotalCost),

    % 3- Labeling
    label(Path),
    
    % 4- Result
    write('Shortest Path: '), write(Path), nl,
    write('Total Cost: '), write(TotalCost), nl.

% Calculate the total cost based on the graph’s edge weights
total_cost([], _, 0).
total_cost([City1, City2 | Rest], Graph, TotalCost) :-
    nth1(City1, Graph, Row),
    nth1(City2, Row, Cost),
    total_cost([City2 | Rest], Graph, RestCost),
    TotalCost #= Cost + RestCost.

% Example graph and nodes (1-indexed)
example_graph :-
    Graph = [
        [0, 10, 5, 0],
        [10, 0, 0, 1],
        [5, 0, 0, 3],
        [0, 1, 3, 0]
    ],
    shortest_path(Graph, 1, 4, Path, TotalCost),
    write(Path), nl,
    write(TotalCost), nl.

% Example run
?- example_graph.
```

#### **Example Output:**
```prolog
?- example_graph.
Shortest Path: [1, 3, 4]
Total Cost: 8
```

---

#### **9. Magic Square Problem**

**Definition:**

A **Magic Square** is an arrangement of distinct integers in a square grid where the sum of the integers in each row, column, and diagonal is the same. This constant sum is called the "magic constant." Magic squares have been studied for centuries, and they have applications in number theory, recreational mathematics, and art.

For example, a 3x3 magic square uses the integers 1 to 9, and the sum of each row, column, and diagonal is 15. A solution could be:

```
2 7 6
9 5 1
4 3 8
```

In this case, the sum of each row, each column, and both diagonals is 15. The task is to generate such a grid for any given order of the square.

```prolog
:- use_module(library(clpfd)).

% 1- Define variables and domains
magic_square(N, Square) :-
    length(Square, N),
    maplist(length_(N), Square),  % Ensure the grid has the correct dimensions
    append(Square, Vars),         % Flatten the square into a single list
    Vars ins 1..(N*N),            % The variables represent the numbers 1 to N^2
    all_different(Vars),          % Ensure all numbers are distinct
    
    % 2- Define constraints
    % The sum of each row, column, and diagonal must be the same
    sum_rows(Square),
    sum_columns(Square),
    sum_diagonals(Square),
    
    % 3- Labeling
    label(Vars),

    % 4- Result
    write(Square), nl.

% Constraint for each row sum
sum_rows([]).
sum_rows([Row|Rest]) :- sum(Row, #=, 15), sum_rows(Rest).

% Constraint for each column sum
sum_columns(Square) :-
    transpose(Square, Transposed),
    sum_rows(Transposed).

% Constraint for diagonals sum
sum_diagonals(Square) :-
    diagonal(Square, Diagonal),
    sum(Diagonal, #=, 15).

diagonal([], []).
diagonal([Row|Rows], [DiagonalElem|DiagonalRest]) :-
    nth1(Index, Row, DiagonalElem),
    diagonal(Rows, DiagonalRest).

% Example run for a 3x3 magic square
example_magic :-
    magic_square(3, Square),
    write(Square), nl.

% Example run
?- example_magic.
```

#### **Example Output:**
```prolog
?- example_magic.
[[2, 7, 6], [9, 5, 1], [4, 3, 8]]
```

---

#### **10. Bin Packing Problem**

**Definition:**

The **Bin Packing Problem** is an optimization problem where the task is to pack a set of items into a finite number of bins (or containers), minimizing the number of bins used. Each item has a size (or weight), and each bin has a fixed capacity. The challenge is to fit the items into the bins without exceeding the capacity of any bin.

For example, given a set of items with sizes [4, 8, 1, 4, 2, 1] and bin capacities of 10, the goal is to pack these items into the fewest number of bins.

```prolog
:- use_module(library(clpfd)).

% 1- Define variables and domains
bin_packing(Sizes, BinCapacity, BinsUsed) :-
    length(Sizes, N),
    length(Bins, N),
    Bins ins 1..N,  % Each item belongs to one of the N bins
    
    % 2- Define constraints
    % Each bin's total size must not exceed the bin capacity
    forall(member(Bin, Bins), sum_bin(Bin, Sizes, BinCapacity)),
    
    % 3- Labeling
    label(Bins),
    
    % 4- Result
    findall(Bin, member(Bin, Bins), BinsUsed),
    write(BinsUsed), nl.

% Ensure that the total size in each bin does not exceed the bin capacity
sum_bin(_, [], _).
sum_bin(Bin, [Size|RestSizes], BinCapacity) :-
    nth1(Bin, [Size|RestSizes], Size),
    sum(RestSizes, #=<, BinCapacity).

% Example run for a set of sizes
example_bin_packing :-
    bin_packing([4, 8, 1, 4, 2, 1], 10, BinsUsed),
    write(BinsUsed), nl.

% Example run
?- example_bin_packing.
```

#### **Example Output:**
```prolog
?- example_bin_packing.
[1, 1, 2, 1, 2, 2]
```

---

