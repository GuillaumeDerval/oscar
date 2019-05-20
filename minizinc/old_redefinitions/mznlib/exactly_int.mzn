include "among.mzn";

predicate exactly_int(int: n, array[int] of var int: x, int: v) = 
	let {
	      set of int: set_v={v}
	  }
	  in 
	     among(n, x, set_v);
