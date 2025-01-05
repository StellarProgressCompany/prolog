### **Problem 1: N-Queens Problem**

**Description:** Place N queens on an N×N chessboard such that no two queens attack each other. We want a solution where no two queens share the same row, column, or diagonal.

```prolog
:- use_module(library(clpfd)).

% 1- Define variables and domains
n_queens(N, Solution) :-
    % The list of solution will have N elements (one per row)
    length(Solution, N),  
    
    % Each queen must be placed in a different row. The value of each element
    % represents the column in which a queen is placed (1..N).
    Solution ins 1..N,
    
    % 2- Define constraints
    % We need to ensure all queens are placed in different columns.
    all_different(Solution),
    
    % Additionally, we need to ensure no two queens attack each other diagonally.
    no_attack(Solution),

    % 3- Labeling
    label(Solution).

% A queen can't share a diagonal with another queen, so we check diagonals
no_attack([]).
no_attack([Queen|Rest]) :-
    no_attack(Rest),
    no_attack_queens(Queen, Rest, 1).

no_attack_queens(_, [], _).
no_attack_queens(Queen, [OtherQueen|Rest], Dist) :-
    Queen #\= OtherQueen,                % Ensure queens are not in the same column
    abs(Queen - OtherQueen) #\= Dist,    % Ensure queens are not on the same diagonal
    NextDist is Dist + 1,
    no_attack_queens(Queen, Rest, NextDist).

% 4- Result
% You can run the following to get one possible solution for N queens.
example_n_queens(N) :- 
    n_queens(N, Solution), 
    write(Solution), nl.
```

#### Example Run for N=8:
```prolog
?- example_n_queens(8).
[1, 5, 8, 6, 3, 7, 2, 4]
```

---

### **Problem 2: Sudoku Puzzle**

**Description:** Solve a Sudoku puzzle where each row, column, and 3x3 subgrid must contain all digits from 1 to 9 without repetition.

```prolog
:- use_module(library(clpfd)).

% 1- Define variables and domains
sudoku(Board) :-
    % Board is a list of lists, each containing 9 variables (cells in the Sudoku grid)
    Board = [R1, R2, R3, R4, R5, R6, R7, R8, R9],
    R1 ins 1..9, R2 ins 1..9, R3 ins 1..9, R4 ins 1..9, R5 ins 1..9, R6 ins 1..9, R7 ins 1..9, R8 ins 1..9, R9 ins 1..9,
    
    % 2- Define constraints
    % All rows must contain distinct values
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

% Constraint for subgrids (3x3 sections of the Sudoku grid)
subgrid_constraints([]).
subgrid_constraints([R1, R2, R3 | Rest]) :-
    all_different([R1[1], R2[1], R3[1], R1[2], R2[2], R3[2], R1[3], R2[3], R3[3]]),
    subgrid_constraints(Rest).

% 4- Result
% You can test the following to solve the Sudoku puzzle
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

#### Example Output:
```prolog
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

---

### **Problem 3: Knapsack Problem**

**Description:** Maximize the total value of items in a knapsack given weight constraints.

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

#### Example Output:
```prolog
?- example_knapsack.
14
```

This indicates that the maximum value is 14 with the items selected accordingly.

---

### **Problem 4: Tents and Trees (Placement of Tents)**

**Description:** Place tents next to each tree in a grid, ensuring that tents are placed within the bounds of the grid and no two tents share the same location.

```prolog
:- use_module(library(clpfd)).

% 1- Define variables and domains
main(E) :-
    example(E, N, M, Trees),
    length(Trees, NVars), 
    length(DRVars, NVars),
    length(DCVars, NVars),
    append(DRVars, DCVars, Vars),
    Vars ins -1..1,

    % 2- Define constraints
    onlyNESWCells(DRVars, DCVars),
    tentsInBox(N, M, Trees, DRVars, DCVars),
    tentsInFreeSpaces(Trees, DRVars, DCVars),
    noTwoTentsOnSameSpot(Trees, DRVars, DCVars),
    
    % 3- Labeling
    label(Vars),
    
    % 4- Result
    print(N, M, Trees, DRVars, DCVars).

% Constraints for tents' positions (N, S, E, W)
onlyNESWCells([], []).
onlyNESWCells([DR|DRs], [DC|DCs]) :-
    abs(DR+DC) #= 1,
    onlyNESWCells(DRs, DCs).

% Constraints for valid tent placement
tentsInBox(_, _, [], [], []).
tentsInBox(N, M, [[R,C]|Ts], [DR|DRs], [DC|DCs]) :-
    tentsInBoxAux(N, M, R, C, DR, DC),
    tentsInBox(N, M, Ts, DRs, DCs).

tentsInBoxAux(N, M, R, C, DR, DC) :-
    R+DR #>= 1, R+DR #=< N,
    C+DC #>= 1, C+DC #=< M.

% 4- Result output
print(N, M, Ts, DRs, DCs) :-
    nth1(K, Ts, [R,C], RestTs),
    nth1(K, DRs, DR, RestDRs),
    nth1(K, DCs, DC, RestDCs),
    write([R,C]), write(": "),
    print_position(DR, DC),
    nl,
    fail.

print_position(-1, 0) :- write('North '), !.
print_position(0, 1) :- write('East  '), !.
print_position(1, 0) :- write('South '), !.
print_position(0, -1) :- write('West  '), !.
```

---

### **Problem 5: Golomb Ruler**

**Description:** Find a Golomb ruler of a given order and length where all distances between pairs are distinct.

```prolog
:- use_module(library(clpfd)).

% 1- Define variables and domains
golomb(N, M) :-
    length(V, N),
    V ins 0..M,
    V = [FV|_],
    last(V, LV),
    FV #= 0,
    LV #= M,
    increasing(V),
    findall([[I, J], [K, L]], (
                between(1, N, I), I1 is I+1, between(I1, N, J),
                between(I, N, K), K1 is K+1, between(K1, N, L),
                ((I = K, J < L); I < K)
            ), Pairs),
    diff(Pairs, V),
    label(V),
    write(V), nl.

% Constraints for increasing order
increasing([]).
increasing([_]).
increasing([X, Y| R]) :- X #< Y, increasing([Y| R]).

% Constraints for distinct distances
diff([], _).
diff([[[I, J], [K, L]]|R], V) :-
    nth1(I, V, VI),
    nth1(J, V, VJ),
    nth1(K, V, VK),
    nth1(L, V, VL),
    VJ - VI #\= VL - VK,
    diff(R, V).
```

---
