predicate strict_lex2(array[int, int] of var int: x) = 
		let {
	  		array[1..card(index_set_1of2(x))*card(index_set_2of2(x))] of var int : x2 = 
	  		[ x[i, j] | i in index_set_1of2(x), j in index_set_2of2(x)]
		}
		in 
	  	oscar_strict_lex2(x2, card(index_set_1of2(x)), card(index_set_2of2(x)));   % use length(x) or length(index_set_1of2(x)) ?
		
predicate oscar_strict_lex2(array[int] of var int: x2, int: i, int: j);