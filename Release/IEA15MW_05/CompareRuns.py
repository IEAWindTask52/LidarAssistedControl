# Script to compare DLC 1.2 for "FeedbackOnly", "LAC_CircularCW", and "LAC_4BeamPulsed".
# Result:
# Improvements by LAC_CircularCW over FeedbackOnly :
# Cost for Summer Games 2024 ("DLC 1.2 Marathon"): 1.433506
# Improvements by LAC_4BeamPulsed over FeedbackOnly :
# Cost for Summer Games 2024 ("DLC 1.2 Marathon"): 1.323144

# Setup
import pandas as pd
import matplotlib.pyplot as plt
import sys

sys.path.append('..\PythonFunctions')

from CalculateCost import CalculateCost
from DataFunctions.DataFilterWindSpeedBins import DataFilterWindSpeedBins
from DataFunctions.CalculateLifeTimeWeightedMean import CalculateLifeTimeWeightedMean
from DataFunctions.CalculateLifeTimeWeightedDEL import CalculateLifeTimeWeightedDEL
from DataFunctions.CalculateMaxValuesPerBin import CalculateMaxValuesPerBin
from PostProcessing.CalculateProcessResults import CalculateProcessResults

# Calculate life-time weighted DEL and mean
SimulationModes = ['FeedbackOnly', 'LAC_CircularCW', 'LAC_4BeamPulsed']
nSimulationModes = len(SimulationModes)

# Config
WindSpeedBins = list(range(4, 25, 2))
WindSpeedChannel = 'mean_Wind1VelX'
FrequencyResults = []  # dummy, because FrequencyResults are not used here
PostProcessingConfig = {}
PostProcessingConfig['CalculateProcessResults'] = [lambda ProcessResults, FrequencyResults, Statistics: DataFilterWindSpeedBins(Statistics, WindSpeedBins, WindSpeedChannel),
                                                   lambda ProcessResults, FrequencyResults, Statistics: CalculateLifeTimeWeightedDEL(ProcessResults.copy(), Statistics, WindSpeedBins, 'DEL_4_TwrBsMyt', options={'WoehlerExponent': 4}),
                                                   lambda ProcessResults, FrequencyResults, Statistics: CalculateLifeTimeWeightedDEL(ProcessResults.copy(), Statistics, WindSpeedBins, 'DEL_4_RotTorq', options={'WoehlerExponent': 4}),
                                                   lambda ProcessResults, FrequencyResults, Statistics: CalculateLifeTimeWeightedDEL(ProcessResults.copy(), Statistics, WindSpeedBins, 'DEL_10_RootMyb1', options={'WoehlerExponent': 10}),
                                                   lambda ProcessResults, FrequencyResults, Statistics: CalculateLifeTimeWeightedMean(ProcessResults.copy(), Statistics, WindSpeedBins, 'mean_GenPwr'),
                                                   lambda ProcessResults, FrequencyResults, Statistics: CalculateLifeTimeWeightedMean(ProcessResults.copy(), Statistics, WindSpeedBins, 'Travel_BldPitch1'),
                                                   lambda ProcessResults, FrequencyResults, Statistics: CalculateMaxValuesPerBin(ProcessResults.copy(), Statistics, 'Overshoot_GenSpeed'),
                                                   lambda ProcessResults, FrequencyResults, Statistics: CalculateMaxValuesPerBin(ProcessResults.copy(), Statistics, 'max_GenTq')
                                                   ]

ProcessResults = []
for iSimulationMode in range(0, nSimulationModes):
    Statistics = pd.read_csv('statistics_' + str(SimulationModes[iSimulationMode]) + '.csv')
    ProcessResults.append(CalculateProcessResults(FrequencyResults, Statistics.copy(), PostProcessingConfig))

# Plots
VS_MaxTq = 21765444.21450  # Maximum generator torque in Region 3 [Nm]

plt.figure('Life-time weighted DEL')
plt.subplot(311)
plt.title('Life-time weighted tower DEL')
plt.grid(True)
plt.box(True)
plt.plot(WindSpeedBins, ProcessResults[0]['LTW_DEL_4_TwrBsMyt_PerBin'] / 1e3, 'o-')
plt.plot(WindSpeedBins, ProcessResults[1]['LTW_DEL_4_TwrBsMyt_PerBin'] / 1e3, '.-')
plt.plot(WindSpeedBins, ProcessResults[2]['LTW_DEL_4_TwrBsMyt_PerBin'] / 1e3, '.-')
plt.ylabel('Tower DEL [MNm]')

plt.subplot(312)
plt.title('Life-time weighted shaft DEL')
plt.grid(True)
plt.box(True)
plt.plot(WindSpeedBins, ProcessResults[0]['LTW_DEL_4_RotTorq_PerBin'] / 1e3, 'o-')
plt.plot(WindSpeedBins, ProcessResults[1]['LTW_DEL_4_RotTorq_PerBin'] / 1e3, '.-')
plt.plot(WindSpeedBins, ProcessResults[2]['LTW_DEL_4_RotTorq_PerBin'] / 1e3, '.-')
plt.ylabel('Shaft DEL [MNm]')

plt.subplot(313)
plt.title('Life-time weighted blade DEL')
plt.grid(True)
plt.box(True)
plt.plot(WindSpeedBins, ProcessResults[0]['LTW_DEL_10_RootMyb1_PerBin'] / 1e3, 'o-')
plt.plot(WindSpeedBins, ProcessResults[1]['LTW_DEL_10_RootMyb1_PerBin'] / 1e3, '.-')
plt.plot(WindSpeedBins, ProcessResults[2]['LTW_DEL_10_RootMyb1_PerBin'] / 1e3, '.-')
plt.legend(SimulationModes, loc='best', fontsize='small')
plt.ylabel('Blade DEL [MNm]')
plt.xlabel('wind speed [m/s]')

plt.figure('Other values')
plt.subplot(411)
plt.title('Life-time weighted electrical power')
plt.grid(True)
plt.box(True)
plt.plot(WindSpeedBins, ProcessResults[0]['LTW_mean_GenPwr_PerBin'] / 1e3, 'o-')
plt.plot(WindSpeedBins, ProcessResults[1]['LTW_mean_GenPwr_PerBin'] / 1e3, '.-')
plt.plot(WindSpeedBins, ProcessResults[2]['LTW_mean_GenPwr_PerBin'] / 1e3, '.-')
plt.ylabel('Electrical power [MW]')

plt.subplot(412)
plt.title('Over-speed per bin')
plt.grid(True)
plt.box(True)
plt.plot(WindSpeedBins, ProcessResults[0]['Overshoot_GenSpeed_PerBin'] * 100, 'o-')
plt.plot(WindSpeedBins, ProcessResults[1]['Overshoot_GenSpeed_PerBin'] * 100, '.-')
plt.plot(WindSpeedBins, ProcessResults[2]['Overshoot_GenSpeed_PerBin'] * 100, '.-')
plt.ylabel('Over-speed [%]')

plt.subplot(413)
plt.title('Life-time weighted pitch travel')
plt.grid(True)
plt.box(True)
plt.plot(WindSpeedBins, ProcessResults[0]['LTW_Travel_BldPitch1_PerBin'], 'o-')
plt.plot(WindSpeedBins, ProcessResults[1]['LTW_Travel_BldPitch1_PerBin'], '.-')
plt.plot(WindSpeedBins, ProcessResults[2]['LTW_Travel_BldPitch1_PerBin'], '.-')
plt.ylabel('Pitch travel [deg]')

plt.subplot(414)
plt.title('Maximum generator torque')
plt.grid(True)
plt.box(True)
plt.plot(WindSpeedBins, ProcessResults[0]['max_GenTq_PerBin'] / 1e3, 'o-')
plt.plot(WindSpeedBins, ProcessResults[1]['max_GenTq_PerBin'] / 1e3, '.-')
plt.plot(WindSpeedBins, ProcessResults[2]['max_GenTq_PerBin'] / 1e3, '.-')
plt.plot([WindSpeedBins[0], WindSpeedBins[-1]], [VS_MaxTq / 1e6, VS_MaxTq / 1e6], 'k-')
plt.ylabel('Genrator torque [MW]')
plt.legend(SimulationModes + ['maximum'], loc='best')
plt.xlabel('wind speed [m/s]')

plt.show()

# Display results
for iSimulationMode in range(1, nSimulationModes):
    Cost = CalculateCost(ProcessResults[iSimulationMode], ProcessResults[0])
    print('-------------------------------------------------')
    print('Improvements by', SimulationModes[iSimulationMode], 'over', SimulationModes[0], ':')
    print('Energy increase: %4.2f %%' % ((ProcessResults[iSimulationMode]['LTW_mean_GenPwr'] / ProcessResults[0]['LTW_mean_GenPwr'] - 1) * 100))
    print('Tower load reduction: %4.1f %%' % ((ProcessResults[iSimulationMode]['LTW_DEL_4_TwrBsMyt'] / ProcessResults[0]['LTW_DEL_4_TwrBsMyt'] - 1) * 100))
    print('Shaft load reduction: %4.1f %%' % ((ProcessResults[iSimulationMode]['LTW_DEL_4_RotTorq'] / ProcessResults[0]['LTW_DEL_4_RotTorq'] - 1) * 100))
    print('Blade load reduction: %4.1f %%' % ((ProcessResults[iSimulationMode]['LTW_DEL_10_RootMyb1'] / ProcessResults[0]['LTW_DEL_10_RootMyb1'] - 1) * 100))
    print('Over-speed reduction: %4.2f %%' % ((ProcessResults[iSimulationMode]['Overshoot_GenSpeed'] - ProcessResults[0]['Overshoot_GenSpeed']) * 100))
    print('Pitch travel reduction: %4.2f %%' % ((ProcessResults[iSimulationMode]['LTW_Travel_BldPitch1'] / ProcessResults[0]['LTW_Travel_BldPitch1'] - 1) * 100))
    print('-----------------------')
    print('Cost for Summer Games 2024 ("DLC 1.2 Marathon"): {:.6f}'.format(Cost))
    print('-------------------------------------------------')

