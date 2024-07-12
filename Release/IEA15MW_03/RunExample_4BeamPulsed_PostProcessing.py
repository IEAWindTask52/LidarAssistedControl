# IEA15MW_03: IEA 15 MW monopile + realistic wind preview from a
# 4-beam pulsed lidar system measuring at 160 m.
# This script needs to be run after RunExample_4BeamPulsed.py.
# Purpose:
# A postprocessing version without the need to compile DLLs for lidar data
# processing to be used in the LAC Summer Games 2024.
# To implement your own solution, replace line 28
# R_FBFF = CalculateREWSfromLidarData_LDP_v1(FBFF, DT, TMax, LDP)
# with your own function with the same inputs and outputs.
# Result:
# Cost for Summer Games 2024 ("18 m/s hurdles"):  0.444770 m/s

import numpy as np
import matplotlib.pyplot as plt
import sys
from scipy.interpolate import interp1d
from CalculateREWSfromLidarData_LDP_v1 import CalculateREWSfromLidarData_LDP_v1

sys.path.append('../PythonFunctions')
from PreProcessing.CalculateREWSfromWindField import CalulateREWSfromWindField
from FileOperations.ReadFASTbinaryIntoStruct import ReadFASTbinaryIntoStruct

# Parameters
nSeed = 6
Seed_vec = np.arange(1, nSeed + 1) + 18 * 100
t_start = 60
TMax = 660
DT = 0.0125
R = 120
tau = 2

# LDP configuration
LDP = {
    "NumberOfBeams": 4,
    "AngleToCenterline": 19.176,
    "IndexGate": 6,
    "FlagLPF": 1,
    "f_cutoff": 0.1232,
    "T_buffer": 1.3889,
}

# Simulation folder
SimulationFolderLAC = "SimulationResults_4BeamPulsed"

# Allocate
MAE = np.empty(nSeed)

# Loop over seeds
for iSeed in range(nSeed):
    Seed = Seed_vec[iSeed]
    WindFileName = f"URef_18_Seed_{Seed:02d}"
    FASTresultFile = f"{SimulationFolderLAC}/{WindFileName}_FlagLAC_1.outb"
    # Load data (you need to implement ReadFASTbinaryIntoStruct)
    FBFF = ReadFASTbinaryIntoStruct(FASTresultFile)

    # Calculate REWS
    # Clear persistent variables from previous call
    R_FBFF = CalculateREWSfromLidarData_LDP_v1(FBFF, DT, TMax, LDP)

    # Get REWS from wind field and interpolate
    TurbSimResultFile = f"TurbulentWind/URef_18_Seed_{Seed:02d}.wnd"
    REWS_WindField, Time_WindField = CalulateREWSfromWindField(TurbSimResultFile, R, 2)
    REWS_WindField_Fs = interp1d(Time_WindField.ravel(),REWS_WindField.ravel())(R_FBFF['Time'])

    # Calculate mean absolute error
    REWS_WindField_Fs_shifted = interp1d(Time_WindField.ravel() - tau, REWS_WindField.ravel())(R_FBFF['Time'])
    Error = REWS_WindField_Fs_shifted - R_FBFF["REWS_b"]
    MAE[iSeed] = np.mean(np.abs(Error[R_FBFF["Time"] >= t_start]))

    # Plot REWS for absolute error
    plt.figure('REWS seed {}'.format(Seed))
    plt.subplot(311)
    plt.plot(R_FBFF['Time'], REWS_WindField_Fs, label='wind field')
    plt.plot(R_FBFF['Time'], R_FBFF['REWS'], label='lidar estimate')
    plt.ylabel('REWS [m/s]')
    plt.legend()
    plt.grid(True)
    plt.subplot(312)
    plt.plot(R_FBFF['Time'], REWS_WindField_Fs_shifted, label='wind field shifted')
    plt.plot(R_FBFF['Time'], R_FBFF['REWS_b'], label='lidar estimate filtered and buffered')
    plt.ylabel('REWS [m/s]')
    plt.legend()
    plt.grid(True)
    plt.subplot(313)
    plt.plot(R_FBFF['Time'], Error)
    plt.ylabel('error [m/s]')
    plt.xlabel('time [s]')
    plt.grid(True)
    plt.show()

# Calculation of Cost for Summer Games 2024
Cost = np.mean(MAE)
print(f"Cost for Summer Games 2024 (\"18 m/s hurdles\"): {Cost:.6f}")

