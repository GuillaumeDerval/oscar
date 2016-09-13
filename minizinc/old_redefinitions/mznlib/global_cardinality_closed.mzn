predicate global_cardinality_closed(array[int] of var int: x, array[int] of int: cover, array[int] of var int: counts) = 
		oscar_global_cardinality_closed(x, cover, counts);

predicate oscar_global_cardinality_closed(array[int] of var int: x, array[int] of int: cover, array[int] of var int: counts);