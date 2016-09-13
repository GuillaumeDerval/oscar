%include "among.mzn";

%predicate at_least_int(int: n, array[int] of var int: x, int: v) = 
%	let {
%	      int: max_n = max(index_set(x)),
%	      var n..max_n: var_n,
%	      set of int: set_v={v}
%	  }
%	  in 
%	     among(var_n, x, set_v);

predicate at_least_int(int: n, array[int] of var int: x, int: v) = 
		oscar_at_least_int(n, x, v);

predicate oscar_at_least_int(int: n, array[int] of var int: x, int: v);