import numpy as np
import re

def GetSimulationName(pre_processing_variation, variation_values):
    n_variation = len(pre_processing_variation)
    simulation_name_temp = ""
    for i_variation in range(n_variation):
        simulation_name_temp += pre_processing_variation[i_variation][0] + "_"
        format_str = pre_processing_variation[i_variation][2]
        if format_str:
            simulation_name_temp += f'{int(variation_values[i_variation]):{format_str}}' + "_"
        else:
            simulation_name_temp += str(variation_values[i_variation]) + "_"

    # remove last '_'
    simulation_name_temp = simulation_name_temp[:-1]

    # replace special characters
    simulation_name = re.sub(r'[.\-+]', lambda x: {'.' : 'd', '-' : 'm', '+' : 'p'}[x.group()], simulation_name_temp)

    return simulation_name
