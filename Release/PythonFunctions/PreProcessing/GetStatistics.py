import pandas as pd
import numpy as np

def GetStatistics(statistics_file, variable, u_ref):
    statistics = pd.read_csv(statistics_file)
    u_ref_vec = statistics['mean_Wind1VelX']
    value = np.interp(u_ref, u_ref_vec, statistics[variable], left=None, right=None, period=None)
    return value
