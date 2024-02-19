import os
import shutil
import numpy as np
import sys
# setup
sys.path.append('..\PythonFunctions')
from GetParametersforDLC1p2 import GetParametersforDLC1p2

# get configuration for DLC 1.2
SimulationMode = 'FeedbackOnly'
PostProcessingConfig, PreProcessingVariation, InputFiles, Modifications = GetParametersforDLC1p2(SimulationMode)

'''
# generate simulation folder and copy all files from OpenFAST and TemplateFolder to SimulationFolder
ExeFile = 'openfast_x64.exe'
SimulationFolder = 'SimulationResults_' + SimulationMode
TemplateFolder = '../IEA-15-240-RWT-Monopile'
if not os.path.exists(SimulationFolder):
    os.mkdir(SimulationFolder)
shutil.copytree(TemplateFolder, SimulationFolder)
shutil.copy2('..\\OpenFAST\\' + ExeFile, os.path.join(SimulationFolder, ExeFile))

# get SimulationNames and DataFiles
SimulationNames, DataFiles = PreProcessingSimulations(SimulationFolder, PreProcessingVariation, InputFiles, Modifications)

# run simulations
status, result = ProcessingSimulations(SimulationFolder, SimulationNames, ExeFile)

# TimeResults
TimeResults = CollectTimeResults(DataFiles, PostProcessingConfig)
PlotTimeResults(TimeResults, PostProcessingConfig)

# Statistics
Statistics = CalculateStatistics(TimeResults, PostProcessingConfig)

# Evaluate only, if you want to overwrite the results: 
# np.save('Statistics_' + SimulationMode, Statistics)
'''