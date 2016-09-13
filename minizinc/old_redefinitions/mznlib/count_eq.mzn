%predicate count_eq(array[int] of var int: x, var int: y, var int: c) =
%    c = sum(i in index_set(x)) ( bool2int(x[i] == y) );
    

predicate count_eq(array[int] of var int: x, var int: y, var int: c) = oscar_count_eq(x, y, c);

predicate oscar_count_eq(array[int] of var int: x, var int: y, var int: c);