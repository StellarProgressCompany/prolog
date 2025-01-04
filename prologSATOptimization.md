# SAT Optimization Cheat Sheet

## Index

1. [Theory](#theory)
   - [atMost](#atmost)
   - [atLeast](#atleast)
   - [expressOr](#expressor)
   - [expressAnd](#expressand)
   - [writeOneClause](#writeoneclause)
   - [exactly](#exactly)
2. [Examples of Clauses in the Gangsters Problem](#examples-of-clauses-in-the-gangsters-problem)

---

## Theory

##  Table of Contents
![image](https://github.com/user-attachments/assets/84624c19-c624-4ecb-bf63-9cf276553bb2)

### **atMost**

**Definition:**
Ensures that the sum of certain boolean variables does not exceed a specified value.

**Mathematical Expression:**

![image](https://github.com/user-attachments/assets/722446a1-03fe-464d-962a-c18e7cd8938d)


**Prolog Encoding:**
```prolog
atMost(K, Lits)
```

**Explanation:**
In Prolog, `atMost(K, Lits)` generates clauses to ensure that no more than `K` literals in the list `Lits` are true.

---

### b. **atLeast**

**Definition:**
Ensures that the sum of certain boolean variables is at least a specified value.

**Mathematical Expression:**

![image](https://github.com/user-attachments/assets/13695c92-7a39-4883-947b-323daef9c67c)

**Prolog Encoding:**
```prolog
atLeast(K, Lits)
```

**Explanation:**
In Prolog, `atLeast(K, Lits)` generates clauses to ensure that at least `K` literals in the list `Lits` are true.

---

### c. **expressOr**

**Definition:**
Establishes a logical equivalence where a variable is true if and only if at least one variable in a list is true.

**Mathematical Expression:**

![image](https://github.com/user-attachments/assets/ca23d933-c3e6-4896-94ed-441b90b9f7da)


**Prolog Encoding:**
```prolog
expressOr(Var, [A, B, ..., N])
```

**Explanation:**
In Prolog, `expressOr(Var, Lits)` creates clauses that ensure `Var` is true exactly when at least one literal in `Lits` is true.

---

### d. **expressAnd**

**Definition:**
Establishes a logical equivalence where a variable is true if and only if all variables in a list are true.

**Mathematical Expression:**

![image](https://github.com/user-attachments/assets/a90f98c6-4ea0-4052-a1df-dc15be74fcea)


**Prolog Encoding:**
```prolog
expressAnd(Var, [A, B, ..., N])
```

**Explanation:**
In Prolog, `expressAnd(Var, Lits)` creates clauses that ensure `Var` is true exactly when all literals in `Lits` are true.

---

### e. **writeOneClause**

**Definition:**
Writes a single clause to the SAT solver's input, representing a disjunction of literals.

![image](https://github.com/user-attachments/assets/805637a1-19b1-4677-a490-b23f07335500)


**Prolog Encoding:**
```prolog
writeOneClause([Lit1, Lit2, ..., LitN])
```

**Explanation:**
In Prolog, `writeOneClause(Lits)` writes a clause where at least one literal in `Lits` must be true.

---

### f. **exactly**

**Definition:**
Ensures that exactly a specified number of boolean variables are true.

**Mathematical Expression:**

![image](https://github.com/user-attachments/assets/a317d790-3200-4067-bcbe-1394fc78b942)


**Prolog Encoding:**
```prolog
exactly(K, Lits)
```

**Explanation:**
In Prolog, `exactly(K, Lits)` combines `atLeast(K, Lits)` and `atMost(K, Lits)` to ensure exactly `K` literals in `Lits` are true.
---

## Examples of Clauses in the Gangsters Problem

### Definitions and SAT Variables

To effectively model the Gangsters Problem, several definitions and SAT variables are established. These form the foundation for translating the problem constraints into SAT clauses.

#### **Definitions**

- **Tasks and Requirements:**
  - `task(T)`: Defines `T` as a task that needs to be performed.
  - `needed(T, H, N)`: Specifies that for task `T` at hour `H`, `N` gangsters are required.
  
- **Gangsters and Availability:**
  - `gangster(G)`: Defines `G` as a gangster.
  - `hour(H)`: Defines `H` as an hour within the planning horizon (1 to 72).
  - `blocked(G, H)`: Indicates that gangster `G` is unavailable at hour `H`.
  - `available(G, H)`: Indicates that gangster `G` is available at hour `H` (i.e., not blocked).

- **Consecutive Hours:**
  - `threeConsecutiveHours(H1, H2, H3)`: Defines a sequence of three consecutive hours starting from `H1`.

#### **SAT Variables**

- **Primary Variables:**
  - `does(G, T, H)`: Represents that gangster `G` is assigned to task `T` at hour `H`.
    - **Meaning:** "Gangster `G` does task `T` at hour `H`."
    - **Usage:** Mandatory for modeling task assignments.
  
  - `busyAtHour(G, H)`: Indicates that gangster `G` is busy (assigned to any task) at hour `H`.
    - **Meaning:** "Gangster `G` is busy at hour `H`."
    - **Usage:** Helps in enforcing constraints related to gangster availability and task assignments.

- **Additional Variables for the Exam:**
  - `doesTask(G, T)`: Represents that gangster `G` performs task `T` at least once during the planning period.
    - **Meaning:** "Gangster `G` does task `T`."
    - **Usage:** Facilitates constraints on the number of different tasks a gangster can perform.

---

### Example Clauses

Below are specific Prolog clauses used to model the constraints of the Gangsters Problem. Each clause corresponds to a particular requirement that must be satisfied in the solution.

#### **1. enoughGangstersAtEachHour**

**Objective:**
Ensure that for every task `T` and every hour `H`, at least `N` available gangsters are assigned to perform task `T`.

**Prolog Implementation:**
```prolog
enoughGangstersAtEachHour :-  
    needed(T, H, N), 
    findall(does(G, T, H), available(G, H), Lits), 
    atLeast(N, Lits), 
    fail.
enoughGangstersAtEachHour.
```

**Explanation:**
- **Step 1:** For each task `T` and hour `H`, retrieve the number of required gangsters `N` using `needed(T, H, N)`.
- **Step 2:** Collect all possible `does(G, T, H)` literals where gangster `G` is available at hour `H`.
- **Step 3:** Apply the `atLeast(N, Lits)` constraint to ensure that at least `N` gangsters are assigned to task `T` at hour `H`.
- **Step 4:** The `fail` predicate forces Prolog to backtrack and apply the clause to all possible combinations of `T` and `H`. The base case `enoughGangstersAtEachHour.` ensures success after all combinations are processed.

---

#### **2. eachHourEachGangsterAtMostOneTask**

**Objective:**
Ensure that each gangster is assigned to at most one task per hour.

**Prolog Implementation:**
```prolog
eachHourEachGangsterAtMostOneTask :- 
    available(G, H), 
    findall(does(G, T, H), task(T), Lits), 
    atMost(1, Lits), 
    fail.
eachHourEachGangsterAtMostOneTask.
```

**Explanation:**
- **Step 1:** For each gangster `G` and hour `H` where `G` is available, gather all possible `does(G, T, H)` literals for every task `T`.
- **Step 2:** Apply the `atMost(1, Lits)` constraint to ensure that `G` is assigned to at most one task during hour `H`.
- **Step 3:** The `fail` predicate ensures that the constraint is applied to all relevant combinations by forcing Prolog to backtrack. The base case `eachHourEachGangsterAtMostOneTask.` ensures overall success.

---

#### **3. noGangsterDoesTwoDifferentTasksConsecutively**

**Objective:**
Prevent any gangster from performing two different tasks in consecutive hours.

**Prolog Implementation:**
```prolog
noGangsterDoesTwoDifferentTasksConsecutively :-
    gangster(G), 
    task(T1), task(T2), T1 \= T2, 
    hour(H1), H2 is H1 + 1, 
    hour(H2), 
    available(G, H1), available(G, H2),
    writeOneClause([ -does(G, T1, H1), -does(G, T2, H2) ]),
    fail. 
noGangsterDoesTwoDifferentTasksConsecutively.
```

**Explanation:**
- **Step 1:** For each gangster `G`, consider all pairs of different tasks `T1` and `T2`.
- **Step 2:** For each consecutive pair of hours `H1` and `H2 = H1 + 1`, check if `G` is available at both hours.
- **Step 3:** Add a clause `[-does(G, T1, H1), -does(G, T2, H2)]` which enforces that if `G` performs task `T1` at hour `H1`, they cannot perform task `T2` at hour `H2`, and vice versa.
- **Step 4:** The `fail` predicate ensures that this constraint is applied to all relevant combinations. The base case ensures success after processing all combinations.

---

#### **4. relateDoesVarsWithBusyAtHourVars**

**Objective:**
Link the `does(G, T, H)` variables with the `busyAtHour(G, H)` variables, ensuring consistency between task assignments and gangster availability.

**Prolog Implementation:**
```prolog
relateDoesVarsWithBusyAtHourVars :- 
    available(G, H), 
    findall(does(G, T, H), task(T), Lits), 
    expressOr(busyAtHour(G, H), Lits), 
    fail.
relateDoesVarsWithBusyAtHourVars.
```

**Explanation:**
- **Step 1:** For each available gangster `G` and hour `H`, collect all `does(G, T, H)` literals corresponding to every task `T`.
- **Step 2:** Use `expressOr(busyAtHour(G, H), Lits)` to create clauses that ensure `busyAtHour(G, H)` is true if and only if at least one `does(G, T, H)` is true.
  - **Forward Direction:** If `does(G, T, H)` is true for any `T`, then `busyAtHour(G, H)` must be true.
  - **Backward Direction:** If `busyAtHour(G, H)` is true, then at least one `does(G, T, H)` must be true.
- **Step 3:** The `fail` predicate forces Prolog to apply this relationship to all relevant combinations. The base case ensures success after all combinations are processed.

---



