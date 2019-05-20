predicate global_cardinality(array[int] of var int: x, array[int] of int: cover, array[int] of var int: counts) = 
		oscar_global_cardinality(x, cover, counts);

predicate oscar_global_cardinality(array[int] of var int: x, array[int] of int: cover, array[int] of var int: counts);