# -----------------------------
# Function:         Provides outputs of a very basic feedback controller.
# ------------
# Input:
# - y_ThisStep      measurements from current time step
# - u_FF_ThisStep   feedforward inputs from current time step
# - dt              time step size
# - Parameter       struct of Parameters
# ------------
# Output:
# - u_ThisStep      controller outputs from this time step
# ----------------------------------

import math
import numpy as np


def FBController(y_ThisStep, u_FF_ThisStep, dt, Parameter):

    # Local variables
    theta_K         = Parameter.CPC.theta_K
    kp              = Parameter.CPC.kp
    Ti              = Parameter.CPC.Ti
    Omega_g_rated   = Parameter.CPC.Omega_g_rated
    theta_min       = Parameter.CPC.theta_min
    theta_max       = Parameter.CPC.theta_max
    M_g_rated       = Parameter.VSC.M_g_rated
    f_cutoff        = Parameter.Filter.f_cutoff

    # Measurements
    Omega_g         = y_ThisStep[0]                 # generator speed
    theta           = y_ThisStep[1]                 # blade pitch angle

    # Feedforward inputs
    if np.isscalar(u_FF_ThisStep):
        theta_dot_FF = u_FF_ThisStep
    else:
        theta_dot_FF = u_FF_ThisStep[0]

    # Persistent variables
    if not hasattr(FBController, 'Omega_g_f'):
        FBController.Omega_g_f = Omega_g

    if not hasattr(FBController, 'integrator'):
        FBController.integrator = theta

    # Low-pass filter
    alpha = math.exp(-2 * math.pi * dt * f_cutoff)       # low-pass filter coefficient, equation (7-2)
    FBController.Omega_g_f = ((1 - alpha) * Omega_g +
                              alpha * FBController.Omega_g_f)  # filtered generator speed, equation (7-1)

    # Pitch controller
    e = FBController.Omega_g_f - Omega_g_rated           # speed error
    g = 1 / (1 + theta / theta_K)                        # gain-correction factor, equation (7-20)

    integrator_dot = g * kp * e / Ti + theta_dot_FF      # integrator input
    FBController.integrator = (FBController.integrator +
                               dt * integrator_dot)       # integrator output

    FBController.integrator = max(FBController.integrator, 0)  # anti-windup

    theta_c_unc = g * kp * e + FBController.integrator   # unconstrained commanded pitch
    theta_c = max(min(theta_c_unc, theta_max), theta_min) # constrained commanded pitch

    # Torque controller
    M_g_c = M_g_rated * Omega_g_rated / FBController.Omega_g_f  # for constant power

    # Final controller outputs
    u_ThisStep = np.array([theta_c, M_g_c])

    return u_ThisStep


def reset_FBController():

    if hasattr(FBController, 'Omega_g_f'):
        del FBController.Omega_g_f

    if hasattr(FBController, 'integrator'):
        del FBController.integrator
