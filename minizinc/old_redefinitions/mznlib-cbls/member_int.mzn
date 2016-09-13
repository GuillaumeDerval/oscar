%-----------------------------------------------------------------------------%
% Requires that 'y' occurs in the array or set 'x'.
%-----------------------------------------------------------------------------%

predicate member_int(array[int] of var int: x, var int: y) = member_int_decomp(x,y);

predicate member_int_decomp(array[int] of var int: x, var int: y) =
    exists(i in index_set(x)) ( x[i] == y );
