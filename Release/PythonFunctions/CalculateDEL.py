import numpy as np
import rainflow

def CalculateDEL(Data, Time, WoehlerExponent, N_REF=2e6/(20*8760*3600/600)):
    """
    Calculates Damage Equivalent Loads.
    Function for CalculateStatistics.
    """
    # calculate DEL
    dt = np.diff(Time[:2])
    c = rainflow.count_cycles(Data, 1/dt)
    Count = c[:, 0]
    Range = c[:, 1]
    DEL = (np.sum(Range**WoehlerExponent * Count) / N_REF)**(1/WoehlerExponent)

    return DEL
