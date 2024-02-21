from joblib import Parallel, delayed
import os
import subprocess

def ProcessingSimulations(simulation_folder, simulation_names, exe_file, tool='OpenFAST', n_core=None):
    if n_core is None:
        n_core = os.cpu_count()

    n_simulation = len(simulation_names)
    result = [None] * n_simulation
    status = [None] * n_simulation

    # move to SimulationFolder
    main_folder = os.getcwd()
    os.chdir(simulation_folder)

    # run simulations
    if tool == 'Flex5':
        for i_simulation in range(n_simulation):
            simulation_name = simulation_names[i_simulation]
            print(f'Running simulation {simulation_name} ({i_simulation+1}/{n_simulation})')
            status[i_simulation], result[i_simulation] = subprocess.getstatusoutput(
                f'{exe_file} {simulation_name}.inf {simulation_name}.log {simulation_name}.res')
    elif tool == 'OpenFAST':
        results = Parallel(n_jobs=n_core)(delayed(run_simulation)(i_simulation, simulation_name, exe_file) for i_simulation, simulation_name in enumerate(simulation_names))
        status, result = zip(*results)

    # back to main folder
    os.chdir(main_folder)

    return status, result

def run_simulation(i_simulation, simulation_name, exe_file):
    print(f'Running simulation {simulation_name} ({i_simulation+1}/{n_simulation})')
    status, result = subprocess.getstatusoutput(f'{exe_file} {simulation_name}.fst')
    return status, result
