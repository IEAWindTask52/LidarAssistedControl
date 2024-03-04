import numpy as np


def CreatePermutationMatrix(pre_processing_variation):
    n_variation = len(pre_processing_variation)
    variation_depth = np.full(n_variation, np.nan)

    for i_variation in range(n_variation):
        variation_depth[i_variation] = len(pre_processing_variation[i_variation][1])

    n_permutation = int(np.prod(variation_depth))

    permutation = np.empty((0, 0), dtype=int)

    for i_variation in range(n_variation):
        permutation_sequence = reshape_rows(permutation,variation_depth[i_variation])
        if i_variation == 0:
            repetitions = 1
            repeated_sequence = np.tile(np.arange(1, int(variation_depth[i_variation]) + 1),
                                        repetitions)
            permutation = repeated_sequence
        else:
            repetitions = np.prod(int(variation_depth[i_variation-1]))
            repeated_sequence = np.tile(np.arange(1, int(variation_depth[i_variation]) + 1),
                                        repetitions)
            permutation = [list(row) for row in zip(permutation_sequence, repeated_sequence)]

    return n_variation, n_permutation, permutation


def reshape_rows(M, a):
    M = np.array(M)
    size_M = M.shape
    if size_M == (0,):  # Handle case when matrix is empty
        M_out = np.empty((0, 1), dtype=int)
    else:
        M_out = np.repeat(M, a)
    return M_out
