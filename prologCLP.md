## Prolog CLP Cheat Sheet (using `ins`)

### 1. **Define Variables and Domains**
Variables are defined using `ins` for constraints over integer domains.

#### 1.1 **Defining Single Variables**
You can define single variables with their domain using the `ins` predicate. This ensures that the variable will take values within a specific range.

##### Examples:
- Defining a variable `X` with values between 1 and 5:
  ```prolog
  X ins 1..5.
  ```

- Defining a variable `Y` with values between 10 and 20:
  ```prolog
  Y ins 10..20.
  ```

- Defining a variable `Z` that takes values from 0 to 100, inclusive:
  ```prolog
  Z ins 0..100.
  ```

#### 1.2 **Defining Lists of Variables**
You can define a list of variables, where each variable in the list will have a domain.

##### Examples:
- Defining a list of three variables with domains between 0 and 10:
  ```prolog
  X = [X1, X2, X3], X ins 0..10.
  ```

- Defining a list of variables with specific constraints:
  ```prolog
  L = [L1, L2, L3, L4], L ins 1..100, L1 #= 10, L4 #= 50.
  ```

#### 1.3 **List Length and Variable Domain**
The `length/2` predicate can be used to define the length of a list, while the variables in the list are constrained by `ins`.

##### Example:
- Defining a list of 4 variables with domains between 0 and 50:
  ```prolog
  length(L, 4), L ins 0..50.
  ```

---

### 2. **Define Constraints**
Constraints define the relationships between variables. They can be arithmetic, logical, or domain-specific.

#### 2.1 **Basic Arithmetic Constraints**
Arithmetic constraints are used to impose conditions like equality, greater-than, and less-than.

##### Examples:
- Sum of two variables:
  ```prolog
  X + Y #= 10.
  ```

- Difference between two variables:
  ```prolog
  X - Y #= 5.
  ```

- Greater-than constraint:
  ```prolog
  X #> Y.
  ```

- Less-than constraint:
  ```prolog
  X #< Y.
  ```

#### 2.2 **Scalar Product Constraints**
The scalar product is commonly used in problems involving weighted sums or pseudo-boolean constraints.

##### Example:
- Scalar product of two lists should be greater than or equal to a value:
  ```prolog
  scalar_product([1, 3, 3], [X1, X2, X3], #>=, 4).
  ```

#### 2.3 **All Different Constraint**
The `all_different/1` predicate ensures that all elements in a list are distinct.

##### Example:
- Ensuring all variables in a list are different:
  ```prolog
  all_different([X1, X2, X3, X4]).
  ```

#### 2.4 **Custom Constraints**
You can define custom constraints to impose more specific conditions.

##### Example:
- Imposing that two variables should not be equal:
  ```prolog
  X #\= Y.
  ```

- Imposing that if `X` is greater than 0, then `Y` should be 1:
  ```prolog
  (X #> 0) #=> (Y #= 1).
  ```

#### 2.5 **Combination of Constraints**
You can combine multiple constraints to form more complex conditions.

##### Example:
- Sum of two variables must equal a third, and the first variable should be less than 10:
  ```prolog
  X + Y #= Z, X #< 10.
  ```

#### 2.6 **Recursive Constraints**
Here are some recursive constraints where the size of the list is not known beforehand:

##### 2.6.1 **Increasing Order Constraint**
This constraint ensures that the elements of a list are in increasing order, regardless of its size:

- A list is in increasing order if:
  - An empty list is trivially in increasing order.
  - A list with one element is trivially in increasing order.
  - For a list with two or more elements, the first element must be less than the second element, and the rest of the list must also be in increasing order.
  
  ```prolog
  increasing([]).
  increasing([_]).
  increasing([X, Y | Rest]) :- X #< Y, increasing([Y | Rest]).
  ```

##### 2.6.2 **Decreasing Order Constraint**
This constraint ensures that the elements of a list are in decreasing order, recursively:

- A list is in decreasing order if:
  - An empty list is trivially in decreasing order.
  - A list with one element is trivially in decreasing order.
  - For a list with two or more elements, the first element must be greater than the second element, and the rest of the list must also be in decreasing order.
  
  ```prolog
  decreasing([]).
  decreasing([_]).
  decreasing([X, Y | Rest]) :- X #> Y, decreasing([Y | Rest]).
  ```

##### 2.6.3 **Sum of List Constraint**
This constraint ensures that the sum of a list of numbers is equal to a certain value:

- The sum of an empty list is zero.
- For a non-empty list, the sum is the head element plus the sum of the rest of the list.
  
  ```prolog
  sum_list([], 0).
  sum_list([X | Rest], Sum) :- sum_list(Rest, RestSum), Sum #= X + RestSum.
  ```

##### 2.6.4 **Non-negative List Constraint**
This constraint ensures that all elements of a list are non-negative numbers:

- A list is non-negative if each element is greater than or equal to zero.
  
  ```prolog
  non_negative([]).
  non_negative([X | Rest]) :- X #>= 0, non_negative(Rest).
  ```

##### 2.6.5 **Max of List Constraint**
This constraint ensures that a list has a maximum element, where the maximum is greater than or equal to every other element in the list:

- A list is valid if it contains one element that is greater than or equal to all the other elements.
  
  ```prolog
  max_list([X], X).
  max_list([X | Rest], Max) :- max_list(Rest, RestMax), Max #= max(X, RestMax).
  ```

##### 2.6.6 **Min of List Constraint**
This constraint ensures that a list has a minimum element, where the minimum is less than or equal to every other element in the list:

- A list is valid if it contains one element that is less than or equal to all the other elements.
  
  ```prolog
  min_list([X], X).
  min_list([X | Rest], Min) :- min_list(Rest, RestMin), Min #= min(X, RestMin).
  ```

##### 2.6.7 **Element Constraints (Custom)**
This constraint ensures that a specific element in the list matches a condition based on recursive traversal of the list.

- For example, ensuring that an element `X` appears exactly once in a list:
  
  ```prolog
  contains_one([X], X).
  contains_one([Y | Rest], X) :- contains_one(Rest, X).
  ```

##### 2.6.8 **All Positive Integers Constraint**
This ensures that all elements in a list are positive integers.

- A list is valid if all elements are greater than zero:
  
  ```prolog
  all_positive([]).
  all_positive([X | Rest]) :- X #> 0, all_positive(Rest).
  ```

##### 2.6.9 **Equal Distribution Constraint**
This constraint ensures that a list of variables is evenly distributed across a given range.

- A list of variables should be equally distributed in a range from `1` to `N`:
  
  ```prolog
  equal_distribution(List, N) :- 
      length(List, L), 
      L #= N, 
      all_different(List), 
      List ins 1..N.
  ```

##### 2.6.10 **Distance Between Different Pairs (diff/2)**
This constraint ensures that the distance between the values assigned to pairs of indices in a list of variables is different for each pair.

##### Example:
- Given a list `LP` of pairs of indices `[[I, J], [K, L]]`, and a list `LV` of variables, ensure the difference between the values assigned to the `I`-th and `J`-th variables is different from the difference between the values assigned to the `K`-th and `L`-th variables.
  
  ```prolog
  diff([], _).
  diff([[[I, J], [K, L]] | R], V) :-
      nth1(I, V, VI),
      nth1(J, V, VJ),
      nth1(K, V, VK),
      nth1(L, V, VL),
      VJ - VI #\= VL - VK,
      diff(R, V).
  ```

##### 2.6.11 **Distinct Elements in List (distinct_elements/1)**
This constraint ensures that all elements in a list are distinct, and it applies recursively.

##### Example:
- Ensure all elements in a list are distinct by comparing each element to all others.
  
  ```prolog
  distinct_elements([]).
  distinct_elements([X | Rest]) :- 
      \+ member(X, Rest),
      distinct_elements(Rest).
  ```

##### 2.6.12 **Sum of Differences (sum_diff/2)**
This constraint ensures that the sum of the absolute differences between consecutive elements in a list is equal to a given value.

##### Example:
- The sum of the absolute differences between consecutive elements in the list must equal `Sum`.
  
  ```prolog
  sum_diff([X], 0).
  sum_diff([X, Y | Rest], Sum) :-
      sum_diff([Y | Rest], RestSum),
      Diff #= abs(X - Y),
      Sum #= Diff + RestSum.
  ```

##### 2.6.13 **Count Occurrences (count_occurrences/3)**
This constraint counts how many times a specific value appears in a list.

##### Example:
- Count how many times `X` appears in list `L` and ensure it matches the required count `Count`.
  
  ```prolog
  count_occurrences([], _, 0).
  count_occurrences([X | Rest], X, Count) :-
      count_occurrences(Rest, X, RestCount),
      Count #= RestCount + 1.
  count_occurrences([Y | Rest], X, Count) :-
      Y \= X,
      count_occurrences(Rest, X, Count).
  ```

##### 2.6.14 **Length of Sublist (sublist_length/2)**
This constraint ensures that a sublist of a list has a specific length.

##### Example:
- The sublist of `List` from index `I` to index `J` must have length `Length`.
  
  ```prolog
  sublist_length(List, Length) :- 
      length(List, Length).
  ```

##### 2.6.15 **Pairs with Sum Constraint (pair_sum/3)**
This constraint ensures that the sum of pairs of elements from a list satisfies a condition.

##### Example:
- For each pair of adjacent elements in the list, ensure the sum is less than or equal to a value `Max`.
  
  ```prolog
  pair_sum([], _).
  pair_sum([X, Y | Rest], Max) :-
      X + Y #=< Max,
      pair_sum(Rest, Max).
  ```

##### 2.6.16 **Sorted Sublist Constraint (sorted_sublist/1)**
This constraint ensures that a sublist is sorted in increasing order.

##### Example:
- Ensure the sublist of `List` starting from index `I` to index `J` is sorted in increasing order.
  
  ```prolog
  sorted_sublist([]).
  sorted_sublist([_]).
  sorted_sublist([X, Y | Rest]) :-
      X #< Y,
      sorted_sublist([Y | Rest]).
  ```

##### 2.6.17 **Consecutive Positive Elements Constraint (consecutive_positive/1)**
This constraint ensures that all consecutive elements in a list are positive integers.

##### Example:
- Ensure that for each pair of consecutive elements, both must be positive integers.
  
  ```prolog
  consecutive_positive([]).
  consecutive_positive([X, Y | Rest]) :-
      X #> 0,
      Y #> 0,
      consecutive_positive([Y | Rest]).
  ```

##### 2.6.18 **Balanced Partition (balanced_partition/2)**
This constraint ensures that a list can be partitioned into two subsets with approximately equal sums.

##### Example:
- The sum of elements in each subset must be close to half of the total sum.
  
  ```prolog
  balanced_partition(List, Partition) :-
      sum_list(List, Total),
      Half #= Total // 2,
      partition(Half, List, Partition).
  ```

##### 2.6.19 **Subset Sum Constraint (subset_sum/2)**
This constraint ensures that the sum of a subset of a list of numbers equals a target sum.

##### Example:
- Ensure that the sum of any subset of `List` equals `Target`.
  
  ```prolog
  subset_sum([], 0).
  subset_sum([X | Rest], Target) :-
      subset_sum(Rest, RestSum),
      Target #= RestSum + X.
  ```

##### 2.6.20 **Unique Pair Sums (unique_pair_sums/1)**
This constraint ensures that the sums of all pairs of elements in a list are distinct.

##### Example:
- For each pair of elements `(X, Y)`, ensure the sum `X + Y` is unique across all pairs.
  
  ```prolog
  unique_pair_sums([]).
  unique_pair_sums([X | Rest]) :-
      unique_pair_sums(Rest),
      \+ (member(Y, Rest), X + Y #= X + Y).
  ```

---


### 3. **Labeling**
Labeling is the process of assigning concrete values to the variables, respecting the domain and constraints.

#### 3.1 **Basic Labeling**
The basic `label/1` predicate is used to find solutions by instantiating variables.

##### Example:
- Labeling a list of variables:
  ```prolog
  label([X1, X2, X3]).
  ```

#### 3.2 **Labeling with Search Strategies**
You can specify search strategies for labeling, such as `first_fail`, `min`, `max`, and others.

##### Example:
- Labeling with the `first_fail` strategy:
  ```prolog
  label([X1, X2, X3], [ffc]).
  ```

- Labeling with the `min` and `max` strategies:
  ```prolog
  label([X1, X2, X3], [min, max]).
  ```

- Labeling with the `bisect` strategy:
  ```prolog
  label([X1, X2], [bisect]).
  ```

#### 3.3 **Labeling with Specific Variables**
You can also label specific variables by listing them explicitly.

##### Example:
- Labeling just two variables:
  ```prolog
  label([X1, X2]).
  ```

#### 3.4 **Using `indomain/1` for Labeling**
`indomain/1` can be used for single variable labeling, often used in recursive constraints.

##### Example:
- Labeling a single variable:
  ```prolog
  indomain(X).
  ```

---

### 4. **Result**
Once the labeling process is complete, you can retrieve and display the results.

#### 4.1 **Displaying Results**
To show the values of variables, you can use `write/1` followed by `nl` for a newline.

##### Example:
- Displaying the result of a list of variables:
  ```prolog
  write([X1, X2, X3]), nl.
  ```

#### 4.2 **Using `findall/3` to Collect Results**
`findall/3` is a predicate that collects all solutions to a query into a list.

##### Example:
- Collecting solutions into a list:
  ```prolog
  findall([X1, X2], (X1 ins 1..5, X2 ins 1..5, X1 + X2 #= 10), Solutions).
  ```

#### 4.3 **Using `setof/3` to Collect Results with Ordering**
`setof/3` is similar to `findall/3`, but it ensures that the results are unique and ordered.

##### Example:
- Collecting unique and ordered solutions:
  ```prolog
  setof([X1, X2], (X1 ins 1..5, X2 ins 1..5, X1 + X2 #= 10), Solutions).
  ```

#### 4.4 **Collecting Results and Displaying Them**
You can use `findall/3` to gather results and then display them.

##### Example:
- Gathering and displaying solutions:
  ```prolog
  findall(X, (X ins 1..10, X #> 5), Solutions), write(Solutions), nl.
  ```

#### 4.5 **Printing Multiple Solutions**
In case you want to display multiple solutions one by one:

##### Example:
- Printing multiple solutions:
  ```prolog
  label([X1, X2]), write([X1, X2]), nl, fail.
  ```

---
