include "disjunctive_strict.mzn";

predicate disjunctive(array[int] of var int: s,
                      array[int] of var int: d) = cumulative(s,d,[1 | dummy in s], 1);
