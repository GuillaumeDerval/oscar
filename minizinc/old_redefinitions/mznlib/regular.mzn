%code is mostly coming from jacop

predicate regular(array[int] of var int: x, int: Q, int: S,
                  array[int,int] of int: d, int: q0, set of int: F) =
    assert(Q > 0,
        "regular: 'Q' must be greater than zero",
    
    assert(S > 0,
        "regular: 'S' must be greater than zero",
    
    assert(index_set_1of2(d) = 1..Q /\ index_set_2of2(d) == 1..S,
        "regular: the transition function 'd' must be [1..Q,1..S]",

    assert(forall([d[i, j] in 0..Q | i in 1..Q, j in 1..S]),
        "regular: transition function 'd' points to states outside 0..Q",

        % Nb: we need the parentheses around the expression otherwise the
        % parser thinks it's a generator call!
    assert((q0 in 1..Q),
        "regular: start state 'q0' not in 1..Q",
    
    assert(F subset 1..Q,
        "regular: final states in 'F' contain states outside 1..Q",

	let {
	  array[1..card(index_set_1of2(d))*card(index_set_2of2(d))] of int : d2 = 
	  	[ d[i, j] | i in index_set_1of2(d), j in index_set_2of2(d)]
	}
	in 
	  oscar_regular(x, Q, S, d2, q0, F)

    ))))));

predicate oscar_regular(array[int] of var int: x, int: Q, int: S,
                        array[int] of int: d, int: q0, set of int: F);