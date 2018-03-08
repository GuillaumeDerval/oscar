predicate table_int(array[int] of var int:  x, array[int, int] of int:  t) = 
		let {
	  		array[1..card(index_set_1of2(t))*card(index_set_2of2(t))] of int : t2 = 
	  		[ t[i, j] | i in index_set_1of2(t), j in index_set_2of2(t)]
		}
		in 
	  	oscar_table_int(x, t2);
		
predicate oscar_table_int(array[int] of var int: x, array[int] of int: t2);