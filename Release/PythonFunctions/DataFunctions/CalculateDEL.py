import numpy as np
from rainflow import count_cycles


def CalculateDEL(Data, Time, WoehlerExponent, N_REF=2e6/(20*8760*3600/600)):

    # Perform rainflow counting
    cycles = count_cycles(Data)

    # Extract counts and ranges
    counts = [cycle[1] for cycle in cycles]
    ranges = [cycle[0] for cycle in cycles]

    # Calculate DEL
    DEL = (np.sum(np.array(ranges) ** WoehlerExponent * np.array(counts)) / N_REF) ** (1 / WoehlerExponent)

    return DEL
