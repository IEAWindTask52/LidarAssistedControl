# -----------------------------
# Function: Provides parameter for NREL5MW SLOW wind turbine model.
# ------------
# Input:
# none
# ------------
# Output:
# - Parameter   struct of Parameters
# ----------------------------------

import numpy as np
from scipy.io import loadmat
from types import SimpleNamespace


def NREL5MWDefaultParameter_SLOW():

    Parameter               = SimpleNamespace()
    Parameter.General       = SimpleNamespace()
    Parameter.Turbine       = SimpleNamespace()
    Parameter.PitchActuator = SimpleNamespace()
    Parameter.Generator     = SimpleNamespace()

    # General
    Parameter.General.rho = 1.225                         # [kg/m^3]  air density

    # Turbine
    Parameter.Turbine.r_GB  = 97                          # [-]       gearbox ratio
    Parameter.Turbine.R     = 126 / 2                     # [m]       Rotor radius

    Parameter.Turbine.SS = loadmat(
        'functions/PowerAndThrustCoefficientsNREL5MW.mat',
        variable_names  = ['c_P', 'c_T', 'theta', 'lambda'],
        squeeze_me      = True
    )
    # theta/lambda as plain lists: QuickInterp2 (in SLOW.py) does a bisect
    # lookup on these every call, which is much faster on a list than on a
    # numpy array
    Parameter.Turbine.SS['theta']   = Parameter.Turbine.SS['theta'].tolist()
    Parameter.Turbine.SS['lambda']  = Parameter.Turbine.SS['lambda'].tolist()

    # Drive-train dynamics
    Parameter.Turbine.J                     = 4.3784e+07        # [kgm^2]   sum of moments of inertia about low-speed shaft

    # Fore-aft tower dynamics
    Parameter.Turbine.x_0T                  = -0.0656145        # [m]       tower top deflection without wind
    Parameter.Turbine.k_eT                  = 1.80987e+06       # [kg/s^2]  tower equivalent bending stiffness
    Parameter.Turbine.m_eT                  = 4.3671e+05        # [kg]      tower equivalent modal mass
    Parameter.Turbine.c_eT                  = 1.7781e+04        # [kg/s]    tower equivalent structural damping
    Parameter.Turbine.HubHeight             = 90                # [m]       hub height

    # Pitch Actuator
    Parameter.PitchActuator.omega           = 2 * np.pi * 1     # [rad/s]   undamped natural frequency
    Parameter.PitchActuator.xi              = 0.7               # [-]       damping factor
    Parameter.PitchActuator.theta_dot_max   = np.deg2rad(8)     # [rad/s]   pitch rate limit

    # Generator
    Parameter.Generator.eta_el              = 0.944             # [-]       Generator efficiency

    return Parameter