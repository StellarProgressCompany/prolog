# SAT Optimization Cheat Sheet

## Index

1. [Theory](#theory)
   - [atMost](#a-atmost)
   - [atLeast](#b-atleast)
   - [expressOr](#c-expressor)
   - [expressAnd](#d-expressand)
   - [writeOneClause](#e-writeoneclause)
   - [exactly](#f-exactly)
2. [Examples of Clauses in the Gangsters Problem](#examples-of-clauses-in-the-gangsters-problem)

---

## Theory

##  Table of Contents
![image](https://github.com/user-attachments/assets/84624c19-c624-4ecb-bf63-9cf276553bb2)

### **atMost**

**Definition:**
Ensures that the sum of certain boolean variables does not exceed a specified value.

**Mathematical Expression:**
\[
\sum_{i=1}^{n} x_i \leq K
\]

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
\[
\sum_{i=1}^{n} x_i \geq K
\]

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
\[
Var \leftrightarrow (A \lor B \lor \dots \lor N)
\]

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
\[
Var \leftrightarrow (A \land B \land \dots \land N)
\]

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
\[
\sum_{i=1}^{n} x_i = K
\]

**Prolog Encoding:**
```prolog
exactly(K, Lits)
```

**Explanation:**
In Prolog, `exactly(K, Lits)` combines `atLeast(K, Lits)` and `atMost(K, Lits)` to ensure exactly `K` literals in `Lits` are true.





---
