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
   - [a. atMost Example](#a-atmost-example)
   - [b. atLeast Example](#b-atleast-example)
   - [c. expressOr Example](#c-expressor-example)
   - [d. expressAnd Example](#d-expressand-example)
   - [e. writeOneClause Example](#e-writeoneclause-example)
   - [f. exactly Example](#f-exactly-example)

---

## Theory

### a. **atMost**

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

## 📝 Examples of Clauses in the Gangsters Problem

### a. **atMost Example**

**Mathematical Expression:**
\[
\sum_{D=1}^{M} \text{coincide2}(G_1, G_2, D) \leq C
\]

**Prolog Encoding:**
```prolog
maxCoincidences(C) :-
    handmaid(G1), handmaid(G2), G1 < G2,
    findall(coincide2(G1, G2, D), day(D), L),
    atMost(C, L),
    fail.
maxCoincidences(_).
```

**Explanation:**
This clause ensures that any two gangsters `G1` and `G2` coincide in the same row on no more than `C` days across all `M` days.

---

### b. **atLeast Example**

**Mathematical Expression:**
\[
\sum_{D=1}^{M} \text{coincide3}(G_1, G_2, G_3, D) \geq 1
\]

**Prolog Encoding:**
```prolog
mandatoryGroups :-
    mandatory(L1), member([G1, G2, G3], L1),
    findall(coincide3(G1, G2, G3, D), day(D), L2),
    atLeast(1, L2),
    fail.
mandatoryGroups.
```

**Explanation:**
This clause ensures that each mandatory group of three gangsters shares a row on at least one day.

---

### c. **expressOr Example**

**Mathematical Expression:**
\[
\text{coincide2}(G_1, G_2, D) \leftrightarrow \bigvee_{R} \text{together}(G_1, G_2, D, R)
\]

**Prolog Encoding:**
```prolog
relate_together_coincide2 :-
    day(D),
    handmaid(G1), handmaid(G2), G1 < G2,
    findall(together(G1, G2, D, R), row(R), L),
    expressOr(coincide2(G1, G2, D), L),
    fail.
relate_together_coincide2.
```

**Explanation:**
This clause links the `coincide2` variable to the logical OR of all `together` variables across rows, ensuring `coincide2(G1, G2, D)` is true if `G1` and `G2` share any row on day `D`.

---

### d. **expressAnd Example**

**Mathematical Expression:**
\[
\text{together}(G_1, G_2, D, R) \leftrightarrow (\text{hdr}(G_1, D, R) \land \text{hdr}(G_2, D, R))
\]

**Prolog Encoding:**
```prolog
relate_hdr_together :-
    day(D), row(R),
    handmaid(G1), handmaid(G2), G1 < G2,
    expressAnd(together(G1, G2, D, R), [hdr(G1, D, R), hdr(G2, D, R)]),
    fail.
relate_hdr_together.
```

**Explanation:**
This clause ensures that `together(G1, G2, D, R)` is true exactly when both `hdr(G1, D, R)` and `hdr(G2, D, R)` are true.

---

### e. **writeOneClause Example**

**Mathematical Expression:**
\[
\neg \text{hdr}(H_1, D, R) \lor \neg \text{hdr}(H_2, D, R) \lor \neg \text{hdr}(H_3, D, R)
\]

**Prolog Encoding:**
```prolog
forbiddenGroups :-
    forbidden(L), member([H1, H2, H3], L),
    day(D), row(R),
    writeOneClause([-hdr(H1, D, R), -hdr(H2, D, R), -hdr(H3, D, R)]),
    fail.
forbiddenGroups.
```

**Explanation:**
This clause prevents any forbidden group of three handmaids from being assigned to the same row on the same day by writing a clause that negates all three `hdr` variables.

---

### f. **exactly Example**

**Mathematical Expression:**
\[
\sum_{H} \text{hdr}(H, D, R) = K
\]

**Prolog Encoding:**
```prolog
forEachDRexactlyKH :-
    rowSize(K),
    day(D), row(R),
    findall(hdr(H, D, R), handmaid(H), L),
    exactly(K, L),
    fail.
forEachDRexactlyKH.
```

**Explanation:**
This clause ensures that exactly `K` handmaids are assigned to each row `R` on each day `D` by applying the `exactly` predicate to the list of `hdr` variables.

---

## 🔍 Quick Reference
![image](https://github.com/user-attachments/assets/84624c19-c624-4ecb-bf63-9cf276553bb2)


---

## 📌 Best Practices

- **Intermediate Variables:** For complex equivalences involving both AND and OR operations, introduce intermediate variables to simplify the logical expressions.
  
- **Avoid Redundancy:** Ensure that each logical equivalence is represented accurately without unnecessary duplication of clauses.
  
- **Consistent Variable Naming:** Use clear and consistent naming conventions for SAT variables to enhance readability and maintainability.

- **Modular Encoding:** Break down complex constraints into smaller, manageable Prolog predicates to simplify debugging and comprehension.

---

## 📖 Further Reading

- [Tseitin Transformation](https://en.wikipedia.org/wiki/Tseitin_transformation): Learn how to convert arbitrary logical formulas into CNF.
  
- [SAT Solvers](https://en.wikipedia.org/wiki/Satisfiability_problem#SAT_solvers): Understand the algorithms and strategies used by SAT solvers like Kissat.

- [Prolog Programming](https://www.swi-prolog.org/pldoc/doc_for?object=manual): Dive deeper into Prolog syntax and advanced features.

---
