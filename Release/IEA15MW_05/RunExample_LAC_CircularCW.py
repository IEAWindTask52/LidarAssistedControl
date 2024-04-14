# Script to run DLC 1.2 for "LAC_CircularCW".
import os
import shutil
import sys

# setup
sys.path.append('..\\PythonFunctions')
from GetParametersforDLC1p2 import GetParametersforDLC1p2
from PreProcessing.PreProcessingSimulations import PreProcessingSimulations
from Processing.ProcessingSimulations import ProcessingSimulations
from PostProcessing.CollectTimeResults import CollectTimeResults
from PostProcessing.PlotTimeResults import PlotTimeResults
from PostProcessing.CalculateStatistics import CalculateStatistics

# get configuration for DLC 1.2
SimulationMode = 'LAC_CircularCW'
PostProcessingConfig, PreProcessingVariation, InputFiles, Modifications = GetParametersforDLC1p2(SimulationMode)

# generate simulation folder and copy all files from OpenFAST and TemplateFolder to SimulationFolder
ExeFile = 'openfast_x64.exe'
SimulationFolder = 'SimulationResults_' + SimulationMode
TemplateFolder = '..\\IEA-15-240-RWT-Monopile'

if not os.path.exists(SimulationFolder):
    os.makedirs(SimulationFolder)

# Copy only the contents of the TemplateFolder to SimulationFolder
for item in os.listdir(TemplateFolder):
    s = os.path.join(TemplateFolder, item)
    d = os.path.join(SimulationFolder, item)
    if os.path.isdir(s):
        if not os.path.exists(d):
            shutil.copytree(s, d, symlinks=True, ignore=None)
    else:
        if not os.path.exists(d):
            shutil.copy2(s, d)

# Copy the executable file to SimulationFolder
exe_destination = os.path.join(SimulationFolder, ExeFile)
if not os.path.exists(exe_destination):
    shutil.copy(os.path.join('..', 'OpenFAST', ExeFile), exe_destination)

# get SimulationNames and DataFiles
SimulationNames, DataFiles = PreProcessingSimulations(SimulationFolder, PreProcessingVariation, InputFiles, Modifications)

# run simulations
status, result = ProcessingSimulations(SimulationFolder, SimulationNames, ExeFile)

# TimeResults
TimeResults = CollectTimeResults(DataFiles, PostProcessingConfig)
PlotTimeResults(TimeResults, PostProcessingConfig)

# Statistics
Statistics = CalculateStatistics(TimeResults, PostProcessingConfig, DataFiles)

# Evaluate only, if you want to overwrite the results:
#Statistics.to_csv('Statistics_' + SimulationMode + '.csv')
