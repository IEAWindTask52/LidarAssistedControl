import numpy as np

def CreatePermutationMatrix(pre_processing_variation):
    n_variation = len(pre_processing_variation)
    variation_depth = np.full(n_variation, np.nan)
    for i_variation in range(n_variation):
        variation_depth[i_variation] = len(pre_processing_variation[1])
    n_permutation = np.prod(variation_depth)

    permutation = []
    for i_variation in range(n_variation):
        permutation = np.hstack((reshape_rows(permutation, variation_depth[i_variation]),
                                 np.tile(np.arange(1, variation_depth[i_variation] + 1), int(np.prod(variation_depth[:i_variation])))))

    return n_variation, n_permutation, permutation

def reshape_rows(M, a):
    M = np.array(M)
    size_M = M.shape
    M_out = np.reshape(np.tile(M, (1, a)).T, (size_M[1], size_M[0]*a)).T
    return M_out
