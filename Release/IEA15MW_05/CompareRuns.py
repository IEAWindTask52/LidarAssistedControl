import pandas as pd
import matplotlib.pyplot as plt
import sys
import numpy as np

sys.path.append('../PythonFunctions')

from DataFunctions.DataFilterWindSpeedBins import DataFilterWindSpeedBins
from DataFunctions.CalculateLifeTimeWeightedMean import CalculateLifeTimeWeightedMean
from DataFunctions.CalculateLifeTimeWeightedDEL import CalculateLifeTimeWeightedDEL
from DataFunctions.CalculateMaxValuesPerBin import CalculateMaxValuesPerBin
from PostProcessing.CalculateProcessResults import CalculateProcessResults

# Config
WindSpeedBins = list(range(4, 25, 2))
WindSpeedChannel = 'mean_Wind1VelX'
FrequencyResults = {}
PostProcessingConfig = {'CalculateProcessResults': [
    lambda ProcessResults,FrequencyResults,Statistics: DataFilterWindSpeedBins(Statistics,WindSpeedBins,WindSpeedChannel),
    lambda ProcessResults,FrequencyResults,Statistics: CalculateLifeTimeWeightedDEL(ProcessResults.copy(),Statistics,WindSpeedBins,'DEL_4_TwrBsMyt',options={'WoehlerExponent': 4}),
    lambda ProcessResults,FrequencyResults,Statistics: CalculateLifeTimeWeightedDEL(ProcessResults.copy(),Statistics,WindSpeedBins,'DEL_4_RotTorq',options={'WoehlerExponent': 4}),
    lambda ProcessResults,FrequencyResults,Statistics: CalculateLifeTimeWeightedDEL(ProcessResults.copy(), Statistics, WindSpeedBins, 'DEL_10_RootMyb1',options={'WoehlerExponent': 10}),
    lambda ProcessResults,FrequencyResults,Statistics: CalculateLifeTimeWeightedMean(ProcessResults.copy(), Statistics, WindSpeedBins, 'mean_GenPwr'),
    lambda ProcessResults,FrequencyResults,Statistics: CalculateLifeTimeWeightedMean(ProcessResults.copy(),Statistics,WindSpeedBins,'Travel_BldPitch1'),
    lambda ProcessResults,FrequencyResults,Statistics: CalculateMaxValuesPerBin(ProcessResults.copy(),Statistics,'Overshoot_GenSpeed')
]}

# FeedbackOnly
Statistics_FeedbackOnly = pd.read_csv('Statistics_FeedbackOnly.csv')
ProcessResults_FeedbackOnly = CalculateProcessResults(FrequencyResults,Statistics_FeedbackOnly.copy(),PostProcessingConfig)

# LAC_CircularCW
Statistics_LAC_CircularCW = pd.read_csv('Statistics_LAC_CircularCW.csv')
ProcessResults_LAC_CircularCW = CalculateProcessResults(FrequencyResults, Statistics_LAC_CircularCW.copy(),PostProcessingConfig)

# LAC_4BeamPulsed
Statistics_LAC_4BeamPulsed = pd.read_csv('Statistics_LAC_4BeamPulsed.csv')
ProcessResults_LAC_4BeamPulsed = CalculateProcessResults(FrequencyResults,Statistics_LAC_4BeamPulsed.copy(),PostProcessingConfig)

# Plots
plt.figure('Life-time weighted DEL')
plt.subplot(311)
plt.title('Life-time weighted tower DEL')
plt.plot(WindSpeedBins, ProcessResults_FeedbackOnly['LTW_DEL_4_TwrBsMyt_PerBin']/1e3, 'o-')
plt.plot(WindSpeedBins, ProcessResults_LAC_CircularCW['LTW_DEL_4_TwrBsMyt_PerBin']/1e3, '.-')
plt.plot(WindSpeedBins, ProcessResults_LAC_4BeamPulsed['LTW_DEL_4_TwrBsMyt_PerBin']/1e3, '.-')
plt.legend(['FeedbackOnly', 'LAC CircularCW', 'LAC 4BeamPulsed'])
plt.ylabel('Tower DEL [MNm]')
plt.grid(True)
plt.box(True)

plt.subplot(312)
plt.title('Life-time weighted shaft DEL')
plt.plot(WindSpeedBins, ProcessResults_FeedbackOnly['LTW_DEL_4_RotTorq_PerBin']/1e3, 'o-')
plt.plot(WindSpeedBins, ProcessResults_LAC_CircularCW['LTW_DEL_4_RotTorq_PerBin']/1e3, '.-')
plt.plot(WindSpeedBins, ProcessResults_LAC_4BeamPulsed['LTW_DEL_4_RotTorq_PerBin']/1e3, '.-')
plt.legend(['FeedbackOnly', 'LAC CircularCW', 'LAC 4BeamPulsed'])
plt.ylabel('Shaft DEL [MNm]')
plt.grid(True)
plt.box(True)

plt.subplot(313)
plt.title('Life-time weighted blade DEL')
plt.plot(WindSpeedBins, ProcessResults_FeedbackOnly['LTW_DEL_10_RootMyb1_PerBin']/1e3, 'o-')
plt.plot(WindSpeedBins, ProcessResults_LAC_CircularCW['LTW_DEL_10_RootMyb1_PerBin']/1e3, '.-')
plt.plot(WindSpeedBins, ProcessResults_LAC_4BeamPulsed['LTW_DEL_10_RootMyb1_PerBin']/1e3, '.-')
plt.legend(['FeedbackOnly', 'LAC CircularCW', 'LAC 4BeamPulsed'])
plt.ylabel('Blade DEL [MNm]')
plt.xlabel('wind speed [m/s]')
plt.grid(True)
plt.box(True)

plt.figure('Other values')
plt.subplot(311)
plt.title('Life-time weighted electrical power')
plt.plot(WindSpeedBins, ProcessResults_FeedbackOnly['LTW_mean_GenPwr_PerBin']/1e3, 'o-')
plt.plot(WindSpeedBins, ProcessResults_LAC_CircularCW['LTW_mean_GenPwr_PerBin']/1e3, '.-')
plt.plot(WindSpeedBins, ProcessResults_LAC_4BeamPulsed['LTW_mean_GenPwr_PerBin']/1e3, '.-')
plt.legend(['FeedbackOnly', 'LAC CircularCW', 'LAC 4BeamPulsed'])
plt.ylabel('Electrical power [MW]')
plt.grid(True)
plt.box(True)

plt.subplot(312)
plt.title('Over-speed per bin')
plt.plot(WindSpeedBins, ProcessResults_FeedbackOnly['Overshoot_GenSpeed_PerBin']*100, 'o-')
plt.plot(WindSpeedBins, ProcessResults_LAC_CircularCW['Overshoot_GenSpeed_PerBin']*100, '.-')
plt.plot(WindSpeedBins, ProcessResults_LAC_4BeamPulsed['Overshoot_GenSpeed_PerBin']*100, '.-')
plt.legend(['FeedbackOnly', 'LAC CircularCW', 'LAC 4BeamPulsed'])
plt.ylabel('Over-speed [%]')
plt.grid(True)
plt.box(True)

plt.subplot(313)
plt.title('Life-time weighted pitch travel')
plt.plot(WindSpeedBins, ProcessResults_FeedbackOnly['LTW_Travel_BldPitch1_PerBin'], 'o-')
plt.plot(WindSpeedBins, ProcessResults_LAC_CircularCW['LTW_Travel_BldPitch1_PerBin'], '.-')
plt.plot(WindSpeedBins, ProcessResults_LAC_4BeamPulsed['LTW_Travel_BldPitch1_PerBin'], '.-')
plt.legend(['FeedbackOnly', 'LAC CircularCW', 'LAC 4BeamPulsed'])
plt.ylabel('Pitch travel [deg]')
plt.xlabel('wind speed [m/s]')
plt.grid(True)
plt.box(True)
plt.tight_layout()
#plt.show()

# Display results
print('-------------------------------------------------')
print('Improvements by LAC_CircularCW over FeedbackOnly:')
print('Tower load reduction: %4.1f %%' % ((ProcessResults_LAC_CircularCW['LTW_DEL_4_TwrBsMyt']/ProcessResults_FeedbackOnly['LTW_DEL_4_TwrBsMyt']-1)*100))
print('Shaft load reduction: %4.1f %%' % ((np.mean(ProcessResults_LAC_CircularCW['LTW_DEL_4_RotTorq_PerBin'])/np.mean(ProcessResults_FeedbackOnly['LTW_DEL_4_RotTorq_PerBin'])-1)*100).mean())
print('Blade load reduction: %4.1f %%' % ((np.mean(ProcessResults_LAC_CircularCW['LTW_DEL_10_RootMyb1_PerBin'])/np.mean(ProcessResults_FeedbackOnly['LTW_DEL_10_RootMyb1_PerBin'])-1)*100))
print('Energy increase: %4.2f %%' % ((ProcessResults_LAC_CircularCW['LTW_mean_GenPwr']/ProcessResults_FeedbackOnly['LTW_mean_GenPwr']-1)*100))
print('Over-speed reduction: %4.2f %%' % ((ProcessResults_LAC_CircularCW['Overshoot_GenSpeed']-ProcessResults_FeedbackOnly['Overshoot_GenSpeed'])*100))
print('Pitch Travel reduction: %4.2f %%' % ((ProcessResults_LAC_CircularCW['LTW_Travel_BldPitch1']/ProcessResults_FeedbackOnly['LTW_Travel_BldPitch1']-1)*100))
print('-------------------------------------------------')
print('Improvements by 4BeamPulsed over FeedbackOnly:')
print('Tower load reduction: %4.1f %%' % ((ProcessResults_LAC_4BeamPulsed['LTW_DEL_4_TwrBsMyt']/ProcessResults_FeedbackOnly['LTW_DEL_4_TwrBsMyt']-1)*100))
print('Shaft load reduction: %4.1f %%' % ((np.mean(ProcessResults_LAC_4BeamPulsed['LTW_DEL_4_RotTorq_PerBin'])/np.mean(ProcessResults_FeedbackOnly['LTW_DEL_4_RotTorq_PerBin'])-1)*100))
print('Blade load reduction: %4.1f %%' % ((np.mean(ProcessResults_LAC_4BeamPulsed['LTW_DEL_10_RootMyb1_PerBin'])/np.mean(ProcessResults_FeedbackOnly['LTW_DEL_10_RootMyb1_PerBin'])-1)*100))
print('Energy increase: %4.2f %%' % ((ProcessResults_LAC_4BeamPulsed['LTW_mean_GenPwr']/ProcessResults_FeedbackOnly['LTW_mean_GenPwr']-1)*100))
print('Over-speed reduction: %4.2f %%' % ((ProcessResults_LAC_4BeamPulsed['Overshoot_GenSpeed']-ProcessResults_FeedbackOnly['Overshoot_GenSpeed'])*100))
print('Pitch Travel reduction: %4.2f %%' % ((ProcessResults_LAC_4BeamPulsed['LTW_Travel_BldPitch1']/ProcessResults_FeedbackOnly['LTW_Travel_BldPitch1']-1)*100))

