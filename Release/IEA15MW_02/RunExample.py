# IEA15MW_02: IEA 15 MW floating + perfect wind preview from a single point
# lidar system.
# Origin and changes in files: see ChangeLog.txt.
# Purpose:
# Here, we use a perfect wind preview to demonstrate that the collective
# pitch feedforward controller (designed with SLOW) together with a motion
# compensation (MC) is able to reduce significantly the rotor speed
# variation when OpenFAST is disturbed by an Extreme Operating Gust.
# Here, all DOFs are enabled. If no MC is applied, the system is instable.
# Result:
# Change in platform pitch amplitude (max-min) from FB to FBFF:  128.5 %
# Change in platform pitch amplitude (max-min) from FB to FBFFMC:  -71.5 %
# Authors:
# David Schlipf, Feng Guo, Frank Lemmer, Simon Weich, Aravind Venkatachalapathy

# Setup
import shutil
import os
import sys
import matplotlib.pyplot as plot
sys.path.append(os.path.abspath('../PythonFunctions'))
from ManipulateTXTFile import ManipulateTXTFile
from ReadFASTbinaryIntoStruct import ReadFASTbinaryIntoStruct
from ReadROSCOtextIntoStruct import ReadROSCOtextIntoDataframe

# Copy the adequate OpenFAST version to the example folder
FASTexeFile = "openfast_x64.exe"
FASTmapFile = "MAP_x64.dll"
SimulationName = "IEA-15-240-RWT-UMaineSemi"
FASTexeFile_path = os.path.join(r'..\OpenFAST', FASTexeFile)
FASTmapFile_path = os.path.join(r'..\OpenFAST', FASTmapFile)
CurrentDirectory = os.path.dirname(os.path.abspath(__file__))
shutil.copy(FASTexeFile_path, os.path.join(CurrentDirectory, FASTexeFile))
shutil.copy(FASTmapFile_path, os.path.join(CurrentDirectory, FASTmapFile))

# Run FB
ManipulateTXTFile(os.path.join(os.getcwd(), "ROSCO_v2d6.IN"), '1 ! FlagLAC', '0 ! FlagLAC')         # disable LAC
os.system(f"{FASTexeFile} {SimulationName}.fst")                                                    # run OpenFast
shutil.move(SimulationName + '.outb', SimulationName + '_FB.outb')                                  # store results

# Run FBFF without motion compensation
ManipulateTXTFile(os.path.join(os.getcwd(), "ROSCO_v2d6.IN"), '0 ! FlagLAC', '1 ! FlagLAC')         # enable LAC
ManipulateTXTFile(os.path.join(os.getcwd(), "LDP_v2.IN"), '1 ! MC_Mode', '0 ! MC_Mode')             # disable MC
os.system(f"{FASTexeFile} {SimulationName}.fst")                                                    # run OpenFAST
shutil.move(SimulationName + '.outb', SimulationName + '_FBFF.outb')                                # store results
shutil.move(SimulationName+'.RO.dbg', SimulationName + '_FBFF.dbg')                                 # store rosco output file

# Run FBFF with motion compensation
ManipulateTXTFile(os.path.join(os.getcwd(), "LDP_v2.IN"), '0 ! MC_Mode', '1 ! MC_Mode')             # enable MC
os.system(f"{FASTexeFile} {SimulationName}.fst")                                                    # run OpenFAST
shutil.move(SimulationName + '.outb', SimulationName + '_FBFFMC.outb')                              # store results
shutil.move(SimulationName+'.RO.dbg', SimulationName + '_FBFFMC.dbg')                               # store rosco output file

# Clean up
os.remove(os.path.join(os.getcwd(), FASTexeFile))
os.remove(os.path.join(os.getcwd(), FASTmapFile))

# Comparison
# read in data
FB = ReadFASTbinaryIntoStruct(SimulationName + '_FB.outb')
FBFF = ReadFASTbinaryIntoStruct(SimulationName + '_FBFF.outb')
FBFFMC = ReadFASTbinaryIntoStruct(SimulationName + '_FBFFMC.outb')
R_FBFF = ReadROSCOtextIntoDataframe(SimulationName + '_FBFF.dbg')
R_FBFFMC = ReadROSCOtextIntoDataframe(SimulationName + '_FBFFMC.dbg')

# Plot
fig, axes = plot.subplots(4, 1, figsize=(10, 12))

axes[0].plot(FB['Time'], FB['Wind1VelX'])
axes[0].plot(R_FBFF.iloc[:, 0], R_FBFF.iloc[:, 27])
axes[0].plot(R_FBFFMC.iloc[:, 0], R_FBFFMC.iloc[:, 27])
axes[0].legend(['Hub height wind speed', 'REWS feedback-feedforward', 'REWS feedback-feedforward with MC'])
axes[0].set_ylabel('[m/s]')
axes[0].grid(True)
axes[0].set_xlim(10, 150)

axes[1].plot(FB['Time'], FB['BldPitch1'])
axes[1].plot(FBFF['Time'], FBFF['BldPitch1'])
axes[1].plot(FBFFMC['Time'], FBFFMC['BldPitch1'])
axes[1].legend(['feedback only', 'feedback-feedforward', 'feedback-feedforward with MC'])
axes[1].set_ylabel('BldPitch1 [deg]')
axes[1].grid(True)
axes[1].set_xlim(10, 150)

axes[2].plot(FB['Time'], FB['RotSpeed'])
axes[2].plot(FBFF['Time'], FBFF['RotSpeed'])
axes[2].plot(FBFFMC['Time'], FBFFMC['RotSpeed'])
axes[2].set_ylabel('RotSpeed [rpm]')
axes[2].grid(True)
axes[2].set_xlim(10, 150)

axes[3].plot(FB['Time'], FB['PtfmPitch'])
axes[3].plot(FBFF['Time'], FBFF['PtfmPitch'])
axes[3].plot(FBFFMC['Time'], FBFFMC['PtfmPitch'])
axes[3].set_ylabel('PtfmPitch [deg]')
axes[3].grid(True)
axes[3].set_xlim(10, 150)
axes[3].set_xlabel('time [s]')

plot.show()

# Display results
ChangePlatformNoMC =   (max(abs(FBFF['PtfmPitch']   - min(abs(FBFF['PtfmPitch'])))) /
                        max(abs(FB['PtfmPitch']     - min(abs(FB['PtfmPitch'])))) - 1) * 100
ChangePlatformWithMC = (max(abs(FBFFMC['PtfmPitch'] - min(abs(FBFFMC['PtfmPitch'])))) /
                        max(abs(FB['PtfmPitch']     - min(abs(FB['PtfmPitch'])))) - 1) * 100
print(f'Change in platform pitch amplitude (max-min) from FB to FBFF: {ChangePlatformNoMC:.1f}%')
print(f'Change in platform pitch amplitude (max-min) from FB to FBFFMC: {ChangePlatformWithMC:.1f}%')
