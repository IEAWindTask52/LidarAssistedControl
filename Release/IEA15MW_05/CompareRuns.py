import pandas as pd
import numpy as np
import sys

sys.path.append('..\PythonFunctions')

from DataFilterWindSpeedBins import DataFilterWindSpeedBins
from CalculateLifeTimeWeightedMean import CalculateLifeTimeWeightedMean
from CalculateLifeTimeWeightedDEL import CalculateLifeTimeWeightedDEL
# setup
WindSpeedBins = list(range(4, 25, 2))
WindSpeedChannel = 'mean_Wind1VelX'

# FeedbackOnly
statistics_feedback_only = pd.read_csv('Statistics_FeedbackOnly.csv')
process_results_feedback_only = DataFilterWindSpeedBins(statistics_feedback_only, WindSpeedBins, WindSpeedChannel)
process_results_feedback_only = CalculateLifeTimeWeightedDEL(process_results_feedback_only, statistics_feedback_only, WindSpeedBins, 'DEL_TwrBsMyt')
process_results_feedback_only = CalculateLifeTimeWeightedMean(process_results_feedback_only, statistics_feedback_only, WindSpeedBins, 'mean_GenPwr')

# LAC_CircularCW
statistics_lac_circular_cw = pd.read_csv('Statistics_LAC_CircularCW.csv')
process_results_lac_circular_cw = DataFilterWindSpeedBins(statistics_lac_circular_cw, WindSpeedBins, WindSpeedChannel)
process_results_lac_circular_cw = CalculateLifeTimeWeightedDEL(process_results_lac_circular_cw, statistics_lac_circular_cw, WindSpeedBins, 'DEL_TwrBsMyt')
process_results_lac_circular_cw = CalculateLifeTimeWeightedMean(process_results_lac_circular_cw, statistics_lac_circular_cw, WindSpeedBins, 'mean_GenPwr')

# LAC_4BeamPulsed
statistics_lac_4_beam_pulsed = pd.read_csv('Statistics_LAC_4BeamPulsed.csv')
process_results_lac_4_beam_pulsed = DataFilterWindSpeedBins(statistics_lac_4_beam_pulsed, WindSpeedBins, WindSpeedChannel)
process_results_lac_4_beam_pulsed = CalculateLifeTimeWeightedDEL(process_results_lac_4_beam_pulsed, statistics_lac_4_beam_pulsed, WindSpeedBins, 'DEL_TwrBsMyt')
process_results_lac_4_beam_pulsed = CalculateLifeTimeWeightedMean(process_results_lac_4_beam_pulsed, statistics_lac_4_beam_pulsed, WindSpeedBins, 'mean_GenPwr')




