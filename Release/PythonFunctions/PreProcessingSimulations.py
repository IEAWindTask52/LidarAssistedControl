import numpy as np
import os
import shutil
from GetSimulationName import GetSimulationName
from CreatePermutationMatrix import CreatePermutationMatrix
def PreProcessingsimulations(simulation_folder, pre_processing_variation, input_files, modifications, tool='OpenFAST'):
    n_variation, n_permutation, permutation = CreatePermutationMatrix(pre_processing_variation)
    simulation_names = []
    data_files = [None] * n_permutation
    i_simulation = 0  # init counter

    for i_permutation in range(n_permutation):
        # get VariationValues for this permutation
        variation_values = np.full(n_variation, np.nan)
        for i_variation in range(n_variation):
            variation_values[i_variation] = pre_processing_variation[i_variation][1][permutation[i_permutation, i_variation]]

        # Get SimulationName, ResultFile and store in DataFiles
        simulation_name = GetSimulationName(pre_processing_variation, variation_values)
        if tool == 'OpenFAST':
            result_file = os.path.join(simulation_folder, f'{simulation_name}.outb')
        elif tool == 'Flex5':
            result_file = os.path.join(simulation_folder, f'{simulation_name}.res')
        data_files[i_permutation] = result_file

        # generate files only if simulation does not exist already
        if not os.path.exists(result_file):
            # make a copy of Modifications
            this_modifications = modifications.copy()
            # replace function handles with string
            n_modification = len(modifications)
            for i_modification in range(n_modification):
                if callable(modifications[i_modification][3]):
                    this_function = modifications[i_modification][3]
                    this_modifications[i_modification][3] = this_function(variation_values)

            # replace <SimulationName>
            this_modifications = [[item.replace('<SimulationName>', simulation_name) for item in modification] for modification in this_modifications]
            this_input_files = [[item.replace('<SimulationName>', simulation_name) for item in input_file] for input_file in input_files]

            # adjust InputFiles
            adjust_input_files(this_input_files, this_modifications, simulation_folder)

            # update counter and store SimulationName
            i_simulation += 1
            simulation_names.append(simulation_name)

            # display
            print(f'Writing files for simulation {simulation_names[i_simulation]} ({i_simulation})')

    return simulation_names, data_files
