%-----------------------------------------------------------------------------%
% Constrains 'c' to be strictly less than the number of occurrences of 'y'
% in 'x'.
%-----------------------------------------------------------------------------%

predicate count_lt(array[int] of var int: x, var int: y, var int: c) =
    let {var int :c2} in
      count_eq(x,y,c2) /\ c < c2;

predicate count_lt_decomp(array[int] of var int: x, var int: y, var int: c) =
    c < sum(i in index_set(x)) ( bool2int(x[i] == y) );

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
