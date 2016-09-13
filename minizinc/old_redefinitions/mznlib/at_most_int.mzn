%include "among.mzn";
%
%predicate at_most_int(int: n, array[int] of var int: x, int: v) = 
%	let {
%	      var 0..n: var_n,
%	      set of int: set_v={v}
%	  }
%	  in 
%	     among(var_n, x, set_v);

predicate at_most_int(int: n, array[int] of var int: x, int: v) = 
		oscar_at_most_int(n, x, v);

predicate oscar_at_most_int(int: n, array[int] of var int: x, int: v);