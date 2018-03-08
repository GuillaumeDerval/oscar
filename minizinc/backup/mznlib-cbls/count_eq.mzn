%-----------------------------------------------------------------------------%
% Constrains 'c' to be the number of occurrences of 'y' in 'x'. 
%-----------------------------------------------------------------------------%

predicate count_eq(array[int] of var int: x, var int: y, var int: c);

predicate count_eq_reif(array[int] of var int: x, var int: y, var int: c, var bool: r);

 %=
 %   c = sum(i in index_set(x)) ( bool2int(x[i] == y) );

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
