# IEA15MW_01: IEA 15 MW monopile + perfect wind preview from a single point 
# lidar system.
# Origin and changes in files: see ChangeLog.txt.
# Purpose:
# Here, we use a perfect wind preview to demonstrate that the collective
# pitch feedforward controller (designed with SLOW) is able to reduce
# significantly the rotor speed variation when OpenFAST is disturbed by an
# Extreme Operating Gust. Here, only the rotational GenDOF is enabled.  
# Result:       
# Change in rotor over speed:  -96.9 %
# Authors: 		
# Alan Wai Hou Lio, David Schlipf, Feng Guo, Simon Weich, Aravind Venkatachalapathy

# Setup
import shutil
import os
import sys
import matplotlib.pyplot as plot
sys.path.append(os.path.abspath('../PythonFunctions'))
from ManipulateTXTFile import ManipulateTXTFile
from ReadFASTbinaryIntoStruct import ReadFASTbinaryIntoStruct

# Copy of the OpenFast Version in the current directory
FASTexeFile = "openfast_x64.exe"
FASTmapFile = "MAP_x64.dll"
SimulationName = "IEA-15-240-RWT-Monopile_DTUWEC"
FASTexeFile_path = os.path.join(r'..\OpenFAST', FASTexeFile)
FASTmapFile_path = os.path.join(r'..\OpenFAST', FASTmapFile)
CurrentDirectory = os.path.dirname(os.path.abspath(__file__))
shutil.copy(FASTexeFile_path, os.path.join(CurrentDirectory, FASTexeFile))
shutil.copy(FASTmapFile_path, os.path.join(CurrentDirectory, FASTmapFile))

# Run FB
ManipulateTXTFile(os.path.join(os.getcwd(), "DTUWEC.IN"), 'constant    96 1.0;', 'constant    96 0.0;')
os.system(f"{FASTexeFile} {SimulationName}.fst")
shutil.move(SimulationName + '.outb', SimulationName + '_FB.outb')

# Run FBFF
ManipulateTXTFile(os.path.join(os.getcwd(), "DTUWEC.IN"), 'constant    96 0.0;', 'constant    96 1.0;')
os.system(f"{FASTexeFile} {SimulationName}.fst")
shutil.move(SimulationName + '.outb', SimulationName + '_FBFF.outb')

# Clean up
os.remove(os.path.join(os.getcwd(), FASTexeFile))
os.remove(os.path.join(os.getcwd(), FASTmapFile))

# read in data
FB = ReadFASTbinaryIntoStruct(SimulationName + '_FB.outb')
FBFF = ReadFASTbinaryIntoStruct(SimulationName + '_FBFF.outb')

# plot
fig, axes = plot.subplots(4, 1, figsize=(10, 12))

axes[0].plot(FB['Time'], FB['Wind1VelX'])
axes[0].plot(FBFF['Time'], FBFF['VLOS01LI'])
axes[0].legend(['Hub height wind speed', 'Vlos'])
axes[0].set_ylabel('[m/s]')
axes[0].grid(True)
axes[0].set_xlim(100, 130)

axes[1].plot(FB['Time'], FB['BldPitch1'])
axes[1].plot(FBFF['Time'], FBFF['BldPitch1'])
axes[1].legend(['feedback only', 'feedback-feedforward'])
axes[1].set_ylabel('BldPitch1 [deg]')
axes[1].grid(True)
axes[1].set_xlim(100, 130)

axes[2].plot(FB['Time'], FB['RotSpeed'])
axes[2].plot(FBFF['Time'], FBFF['RotSpeed'])
axes[2].set_ylabel('RotSpeed [rpm]')
axes[2].grid(True)
axes[2].set_xlim(100, 130)

axes[3].plot(FB['Time'], FB['TwrBsMyt'] / 1e3)
axes[3].plot(FBFF['Time'], FBFF['TwrBsMyt'] / 1e3)
axes[3].set_ylabel('TwrBsMyt [MNm]')
axes[3].grid(True)
axes[3].set_xlim(100, 130)
axes[3].set_xlabel('time [s]')

plot.show()

# display results
RatedRotorSpeed = 7.56 # [rpm]
t_Start         = 100  # [s]
change_in_rotor_speed = (max(abs(FBFF['RotSpeed'][FBFF['Time'] >= t_Start] - RatedRotorSpeed)) /
                         max(abs(FB['RotSpeed'][FB['Time'] >= t_Start] - RatedRotorSpeed)) - 1) * 100
print(f'Change in rotor over speed: {change_in_rotor_speed:.1f}%')