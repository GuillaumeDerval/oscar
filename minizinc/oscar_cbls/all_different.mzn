%-----------------------------------------------------------------------------%
% Constrains the array of objects 'x' to be all different.
%-----------------------------------------------------------------------------%

include "all_different_int.mzn";
include "all_different_set.mzn";

predicate all_different(array[int] of var int: x) = 
  if card(index_set(x)) > 0 then all_different_int(x) else true endif;

predicate all_different(array[int] of var set of int: x) =
    if card(index_set(x)) > 0 then all_different_set(x) else true endif;

% Synonyms for the above.

predicate alldifferent(array[int] of var int: x) =
    all_different(x);

predicate alldifferent(array[int] of var set of int: x) =
    all_different(x);

