%-----------------------------------------------------------------------------%
% Requires that the number of distinct values in 'x' is 'n'.
%-----------------------------------------------------------------------------%
predicate nvalue(var int: n, array[int] of var int: x) =
    nvalue_int(n, x);

predicate nvalue_int(var int: n, array[int] of var int: x);