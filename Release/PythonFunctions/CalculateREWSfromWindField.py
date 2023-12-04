import numpy as np
from scipy.io import loadmat

def CalulateREWSfromWindField(file_name, loop):
    # Load .mat file
    file = '../IEA15MW_03/CaluculateRWESfromWindField.mat'
    mat = loadmat(file)
    # Get 't_all' and 'v_0_all' variables
    t_all = mat['t_all'][0]
    v_0_all = mat['v_0_all'][0]


    # Return 't_all' and 'v_0_all' for the seed vector
    return v_0_all[loop],t_all[loop]
