%-----------------------------------------------------------------------------%
% Constrains two arrays of int variables, 'f' and 'invf', to represent
% inverse functions.  All the values in each array must be within the index
% set of the other array.
%-----------------------------------------------------------------------------%

predicate inverse_decomp(array[int] of var int: f,
                  array[int] of var int: invf) =
    forall(i in index_set(f), j in index_set(invf)) (
           f[i] in index_set(invf) /\
        invf[j] in index_set(f   ) /\
        (j == f[i] <-> i == invf[j])
    );

predicate inverse(array[int] of var int: f, array[int] of var int: invf) = 
    let{
       int: offsetf = min(index_set(f))-1,
       int: offsetinvf = min(index_set(invf))-1
    } in inverse_no_offset([f[i]-offsetinvf | i in index_set(f)],[invf[i]-offsetf | i in index_set(invf)]);

predicate inverse_no_offset(array[int] of var int: f, array[int] of var int: invf);