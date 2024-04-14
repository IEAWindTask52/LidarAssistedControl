# Setup
import os
from joblib import Parallel, delayed
from subprocess import getstatusoutput
from multiprocessing import cpu_count


def ProcessingSimulations(SimulationFolder, SimulationNames, ExeFile, Tool='OpenFAST', nCore=None):
    if nCore is None:
        nCore = cpu_count()

    # get dimensions and allocation
    n_Simulation = len(SimulationNames)
    result = [None] * n_Simulation
    status = [float('nan')] * n_Simulation

    # move to SimulationFolder
    MainFolder = os.getcwd()
    os.chdir(SimulationFolder)

    def run_simulation(i_Simulation):
        SimulationName = SimulationNames[i_Simulation]
        print(f"Running simulation {SimulationName} ({i_Simulation + 1}/{n_Simulation})")
        if Tool == 'Flex5':
            command = f"{ExeFile} {SimulationName}.inf {SimulationName}.log {SimulationName}.res"
        elif Tool == 'OpenFAST':
            command = f"{ExeFile} {SimulationName}.fst"
        return getstatusoutput(command)

    # run simulations
    results = Parallel(n_jobs=nCore)(delayed(run_simulation)(i) for i in range(n_Simulation))

    for i, (status_code, output) in enumerate(results):
        status[i] = status_code
        result[i] = output

    # back to main folder
    os.chdir(MainFolder)

    return status, result
