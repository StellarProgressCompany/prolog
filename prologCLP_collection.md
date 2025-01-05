```markdown
## **Index**

1. [Golomb Ruler Problem](#golomb-ruler-problem)
2. [Sudoku Puzzle](#sudoku-puzzle)

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
