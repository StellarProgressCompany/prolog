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
