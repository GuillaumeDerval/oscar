% This file contains redefinitions of standard builtins for version 2.0.2
% that can be overridden by solvers.

%Ignore symmetry breaking constraints.
predicate symmetry_breaking_constraint(var bool: b) = true;
%Ignore redundant constraints. The effect of redundant constraints is not clear. Ignoring them will at least slightly reduce the number of objects to be maintained by the solver.
predicate redundant_constraint(var bool: b) = true;

%Did not change the following definitions

predicate array_var_bool_element_nonshifted(var int: idx, array[int] of var bool: x, var bool: c) =
  array_var_bool_element((idx-(min(index_set(x))-1))::domain,array1d(x),c);

predicate array_var_int_element_nonshifted(var int: idx, array[int] of var int: x, var int: c) =
  array_var_int_element((idx-(min(index_set(x))-1))::domain,array1d(x),c);

predicate array_var_float_element_nonshifted(var int: idx, array[int] of var float: x, var float: c) =
  array_var_float_element((idx-(min(index_set(x))-1))::domain,array1d(x),c);

predicate array_var_set_element_nonshifted(var int: idx, array[int] of var set of int: x, var set of int: c) =
  array_var_set_element((idx-(min(index_set(x))-1))::domain,array1d(x),c);
