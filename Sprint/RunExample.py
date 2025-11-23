# Sprint: DLC 1.4 for IEA 15 MW monopile with and without LAC.
# Purpose:
# We want to learn how to simulate a DLC 1.4 with an "Extreme coherent gust
# with direction change (ECD)" with lidar-assisted control (LAC) and how
# LAC can reduce the ultimate tower loads.
# Here, only the rotor motion and tower motion (GenDOF, TwFADOF1, TwSSDOF1)
# are enabled for simplicity.
# Result:
# Cost for Summer Games 2025 ("30 s sprint"):  0.739948 (4BeamPulsed)
# Cost for Summer Games 2025 ("30 s sprint"):  1.219835 (CircularCW)

# Setup
import os
import matplotlib.pyplot as plt
from PythonFunctions.FileOperations.ReadFASTbinaryIntoStruct import ReadFASTbinaryIntoStruct
from PythonFunctions.FileOperations.ReadROSCOtextIntoStruct import ReadROSCOtextIntoDataframe

# Select LidarType
LidarType = '4BeamPulsed'       # [4BeamPulsed/CircularCW]

# Define FAST input file
SimulationName  = 'IEA-15-240-RWT-Monopile'

# Run FB and FF simulation
os.system(f"openfast_x64.exe {SimulationName}_FB.fst")
os.system(f"openfast_x64.exe {SimulationName}_FBFF_{LidarType}.fst")

# Comparison
# Read in data
FB      = ReadFASTbinaryIntoStruct(SimulationName + '_FB.outb')
FBFF    = ReadFASTbinaryIntoStruct(SimulationName + '_FBFF_' + LidarType + '.outb')
FBFF_R  = ReadROSCOtextIntoDataframe(SimulationName + '_FBFF_' + LidarType + '.RO.dbg')

# Plot results
fig, axs    = plt.subplots(4, 1, sharex=True)
fig.suptitle("Simulation results")

axs[0].plot(FB['Time'],    FB['Wind1VelX'],    label='Wind1VelX')
axs[0].plot(FBFF['Time'],  FBFF_R['REWS_b'],   label='REWS_b')
axs[0].grid()
axs[0].legend()
axs[0].set_ylabel("Windspeed\n[m/s]")

axs[1].plot(FB['Time'],    FB['BldPitch1'],    label='feedback only')
axs[1].plot(FBFF['Time'],  FBFF['BldPitch1'],  label='feedback-feedforward')
axs[1].grid()
axs[1].legend()
axs[1].set_ylabel("BldPitch1\n[deg]")

axs[2].plot(FB['Time'],    FB['RotSpeed'])
axs[2].plot(FBFF['Time'],  FBFF['RotSpeed'])
axs[2].grid()
axs[2].set_ylabel("RotSpeed\n[rpm]")

axs[3].plot(FB['Time'],    FB['TwrBsMyt']/1e3)
axs[3].plot(FBFF['Time'],  FBFF['TwrBsMyt']/1e3)
axs[3].grid()
axs[3].set_ylabel("TwrBsMyt\n[MNm]")
axs[3].set_xlabel("Time [s]")

axs[-1].set_xlim(20, 50)
plt.tight_layout()
#plt.show()

# Display results
RotSpeed_0  = 7.56
TwrBsMyt_0  = 158.3e3
t_Start     = 0

Cost = ((max(abs(FBFF['RotSpeed'][FBFF['Time'] > t_Start] - RotSpeed_0))) / RotSpeed_0
        + (max(abs(FBFF['TwrBsMyt'][FBFF['Time'] > t_Start] - TwrBsMyt_0))) / TwrBsMyt_0)

print(f'Cost for Summer Games 2025 (\"30 s sprint\"): {Cost:.6f}')
