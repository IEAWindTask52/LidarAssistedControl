#
# Script to generate an Extreme Coherent Gust with direction change (ECD)
# according to IEC 61400-1.
#

# Setup
import numpy as np
import matplotlib.pyplot as plt
from WriteBLgrid import WriteBLgrid

# Define file name
FileName = 'ECD_VrPlus2mps'

# PreProcessing
# Time discretization
T       = 70                    # [s]   total simulation time
dt      = 1/100                 # [s]   total simulation time
t       = np.arange(0, T, dt)   # [s]   simulation time vector>
Nt      = t.size

# Wind conditions at hub height
V_hub   = 12.5                  # [m/s] mean wind speed at hub height: v_rated + 2/m/s
t_start = 30                    # [s]   start time of gust event

# Extreme coherent gust with direction change (ECD - IEC 6.3.2.5)
V_cg    = 15                    # [m/s] coherent gust amplitude
T_gust  = 10                    # [s]   rise time of coherent gust

# Gird definitions
dy      = 10                    # [m]   lateral spacing
dz      = 10                    # [m]   vertical spacing
Ny      = 29                    # [-]   number of grid points in lateral direction
Nz      = 29                    # [-]   number of grid points in vertical direction

# Some variables required in the Type 4 wind: Bladed style
HubHeight   = 150               # [m]   hub height
URef        = V_hub+V_cg/2      # [m/s] reference mean wind speed
zOffset     = HubHeight         # [m]   reference height of the grid
z0          = 0.1               # [m]   the rougthness length, not really used

# The normal wind profile model (NWP - IEC 6.3.1.2)
alpha       = 0.2               # [-]   shear exponent for NWP
z_hub       = HubHeight
z           = np.linspace(-(Nz - 1) / 2, (Nz - 1) / 2, Nz) * dz + HubHeight     # [m]   vertical coordinates of the grid
V_z         = V_hub * (z / z_hub) ** alpha                                      # [m/s] mean wind profile at each vertical grid point>

# Magnitude of ECD
u_W         = np.empty((Nz, Nt), dtype=float)

pre         = t < t_start
dur         = (t >= t_start) & (t <= t_start + T_gust)
post        = t > (t_start + T_gust)
t_gust      = t[dur] - t_start

u_W[:,pre]  = V_z[:, None]
u_W[:,dur]  = V_z[:, None] + 0.5 * V_cg * (1.0 - np.cos(np.pi * t_gust / T_gust))
u_W[:,post] = V_z[:, None] + V_cg

# Direction change of ECD
theta       = np.empty(Nt, dtype=float)
theta_cg    = np.deg2rad(720 / V_hub)       # [rad]

theta[pre]  = 0
theta[dur]  = 0.5 * theta_cg * (1.0 - np.cos(np.pi * t_gust / T_gust))
theta[post] = theta_cg

# Transformation into inertial coordinate system
u_I         = u_W * np.cos(theta)       # longitudinal
v_I         = u_W * np.sin(theta)       # lateral

# Plot and validate results
Idx         = np.isin(z, np.arange(50, 251, 50))

fig, axs    = plt.subplots(3, 1, sharex=True)
axs[0].plot(t, u_W[Idx,:].T)
axs[0].grid()
axs[0].set_ylabel("u_W [m/s]")
axs[0].set_title("Wind speed magnitude")

axs[1].plot(t, v_I[Idx,:].T)
axs[1].grid()
axs[1].set_ylabel("v_I [m/s]")
axs[1].set_title("Lateral component")
axs[1].legend([f"{zi:.0f} m" for zi in z[Idx]])

axs[2].plot(t, u_I[Idx,:].T)
axs[2].grid()
axs[2].set_ylabel("u_I [m/s]")
axs[2].set_xlabel("time [s]")
axs[2].set_title("Longitudinal component")

plt.tight_layout()
#plt.show()

# Prepare output arrays for .wnd-file
SummVars        = np.zeros(6, dtype=float)
SummVars[0]     = HubHeight     # HubHeight
SummVars[2]     = URef          # Mean wind speed at hub-height

velocity        = np.empty((Nt, 3, Ny, Nz), dtype=float)        # [time, (u,v,w), y, z]

for iy in range(Ny):
    for iz in range(Nz):
        velocity[:, 0, iy, iz]  = u_I[iz, :]        # longitudinal component
        velocity[:, 1, iy, iz]  = v_I[iz, :]        # lateral component
        velocity[:, 2, iy, iz]  = 0                 # no vertical component

# Get turbulence intensity for .sum-file
# here we use std u for w component to avoid dividing by zero
SummVars[3]     = np.std(velocity[:, 0, 0, 0], ddof=1) / URef * 100
SummVars[4]     = np.std(velocity[:, 1, 0, 0], ddof=1) / URef * 100
SummVars[5]     = np.std(velocity[:, 0, 0, 0], ddof=1) / URef * 100

# Export rotor-plane wind field (.wnd + .sum) for OpenFAST
print("Exporting rotor plane wind field as '.wnd' binary files...")
WriteBLgrid(FileName, velocity, dy, dz, dt, zOffset, z0, SummVars)

with open(FileName + ".sum", "w") as f:
    f.write("This summary file is not complete it only contains required information for the OpenFAST\n")
    f.write("F        Clockwise rotation when looking downwind?\n")
    f.write(f"{SummVars[0]:g}  Hub height [m] \n")
    f.write(f"UBar   =  {SummVars[2]:g} m/s \n")
    f.write(f"TI(u)  =  {SummVars[3]:.4f} %\n")
    f.write(f"TI(v)  =  {SummVars[4]:.4f} %\n")
    f.write(f"TI(w)  =  {SummVars[5]:.4f} %\n")
    f.write("Height Offset =  0 m\n")
    f.write("Creating a PERIODIC output file.\n")
