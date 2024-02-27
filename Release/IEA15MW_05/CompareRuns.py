import pandas as pd
import matplotlib.pyplot as plt
import sys

sys.path.append('..\PythonFunctions')

from DataFunctions.DataFilterWindSpeedBins import DataFilterWindSpeedBins
from DataFunctions.CalculateLifeTimeWeightedMean import CalculateLifeTimeWeightedMean
from DataFunctions.CalculateLifeTimeWeightedDEL import CalculateLifeTimeWeightedDEL

# Config
WindSpeedBins = list(range(4, 25, 2))
WindSpeedChannel = 'mean_Wind1VelX'

# FeedbackOnly
Statistics_FeedbackOnly = pd.read_csv('Statistics_FeedbackOnly.csv')
ProcessResults_FeedbackOnly = DataFilterWindSpeedBins(Statistics_FeedbackOnly.copy(), WindSpeedBins, WindSpeedChannel)
ProcessResults_FeedbackOnly = CalculateLifeTimeWeightedDEL(ProcessResults_FeedbackOnly.copy(), Statistics_FeedbackOnly, WindSpeedBins, 'DEL_TwrBsMyt')
ProcessResults_FeedbackOnly = CalculateLifeTimeWeightedMean(ProcessResults_FeedbackOnly.copy(), Statistics_FeedbackOnly, WindSpeedBins, 'mean_GenPwr')

# LAC_CircularCW
Statistics_LAC_CircularCW = pd.read_csv('Statistics_LAC_CircularCW.csv')
ProcessResults_LAC_CircularCW = DataFilterWindSpeedBins(Statistics_LAC_CircularCW.copy(), WindSpeedBins, WindSpeedChannel)
ProcessResults_LAC_CircularCW = CalculateLifeTimeWeightedDEL(ProcessResults_LAC_CircularCW.copy(), Statistics_LAC_CircularCW, WindSpeedBins, 'DEL_TwrBsMyt')
ProcessResults_LAC_CircularCW = CalculateLifeTimeWeightedMean(ProcessResults_LAC_CircularCW.copy(), Statistics_LAC_CircularCW, WindSpeedBins, 'mean_GenPwr')

# LAC_4BeamPulsed
Statistics_LAC_4BeamPulsed = pd.read_csv('Statistics_LAC_4BeamPulsed.csv')
ProcessResults_LAC_4BeamPulsed = DataFilterWindSpeedBins(Statistics_LAC_4BeamPulsed.copy(), WindSpeedBins, WindSpeedChannel)
ProcessResults_LAC_4BeamPulsed = CalculateLifeTimeWeightedDEL(ProcessResults_LAC_4BeamPulsed.copy(), Statistics_LAC_4BeamPulsed, WindSpeedBins, 'DEL_TwrBsMyt')
ProcessResults_LAC_4BeamPulsed = CalculateLifeTimeWeightedMean(ProcessResults_LAC_4BeamPulsed.copy(), Statistics_LAC_4BeamPulsed, WindSpeedBins, 'mean_GenPwr')

# Plots
# Life-time weighted tower DEL
plt.figure(figsize=(8, 6))
plt.title('Life-time weighted tower DEL')
plt.plot(ProcessResults_FeedbackOnly['WindSpeedBins'], ProcessResults_FeedbackOnly['LTW_DEL_TwrBsMyt_PerBin'] / 1e3, 'o-')
plt.plot(ProcessResults_LAC_CircularCW['WindSpeedBins'], ProcessResults_LAC_CircularCW['LTW_DEL_TwrBsMyt_PerBin'] / 1e3, '.-')
plt.plot(ProcessResults_LAC_4BeamPulsed['WindSpeedBins'], ProcessResults_LAC_4BeamPulsed['LTW_DEL_TwrBsMyt_PerBin'] / 1e3, '.-')
plt.legend(['FeedbackOnly', 'LAC CircularCW', 'LAC 4BeamPulsed'])
plt.xlabel('wind speed [m/s]')
plt.ylabel('Tower DEL [MNm]')
plt.grid(True)

# Life-time weighted electrical power
plt.figure(figsize=(8, 6))
plt.title('Life-time weighted electrical power')
plt.plot(ProcessResults_FeedbackOnly['WindSpeedBins'], ProcessResults_FeedbackOnly['LTW_mean_GenPwr_PerBin'] / 1e3, 'o-')
plt.plot(ProcessResults_LAC_CircularCW['WindSpeedBins'], ProcessResults_LAC_CircularCW['LTW_mean_GenPwr_PerBin'] / 1e3, '.-')
plt.plot(ProcessResults_LAC_4BeamPulsed['WindSpeedBins'], ProcessResults_LAC_4BeamPulsed['LTW_mean_GenPwr_PerBin'] / 1e3, '.-')
plt.legend(['FeedbackOnly', 'LAC CircularCW', 'LAC 4BeamPulsed'])
plt.xlabel('wind speed [m/s]')
plt.ylabel('Electrical power [MW]')
plt.grid(True)
plt.show()

# Display results
print('-------------------------------------------------')
print('Improvements by LAC_CircularCW over FeedbackOnly:')
print(f'Tower load reduction: {((ProcessResults_LAC_CircularCW["LTW_DEL_TwrBsMyt"] / ProcessResults_FeedbackOnly["LTW_DEL_TwrBsMyt"] - 1) * 100):.1f} %')
print(f'Energy increase: {((ProcessResults_LAC_CircularCW["LTW_mean_GenPwr"] / ProcessResults_FeedbackOnly["LTW_mean_GenPwr"] - 1) * 100):.2f} %')
print('-------------------------------------------------')
print('Improvements by 4BeamPulsed over FeedbackOnly:')
print(f'Tower load reduction: {((ProcessResults_LAC_4BeamPulsed["LTW_DEL_TwrBsMyt"] / ProcessResults_FeedbackOnly["LTW_DEL_TwrBsMyt"] - 1) * 100):.1f} %')
print(f'Energy increase: {((ProcessResults_LAC_4BeamPulsed["LTW_mean_GenPwr"] / ProcessResults_FeedbackOnly["LTW_mean_GenPwr"] - 1) * 100):.2f} %')
print('-------------------------------------------------')
