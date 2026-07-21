# Wind turbine update

import numpy as np


def SLOW(x_ThisStep, u_ThisStep, d_ThisStep, dt, Parameter):

    # RK4 (assuming d_NextStep = d_ThisStep)
    k1 = state_eqs(x_ThisStep,                 u_ThisStep, d_ThisStep, Parameter)
    k2 = state_eqs(x_ThisStep + 1/2 * k1 * dt, u_ThisStep, d_ThisStep, Parameter)
    k3 = state_eqs(x_ThisStep + 1/2 * k2 * dt, u_ThisStep, d_ThisStep, Parameter)
    k4 = state_eqs(x_ThisStep +       k3 * dt, u_ThisStep, d_ThisStep, Parameter)

    dx = 1/6 * (k1 + 2*k2 + 2*k3 + k4)
    x_NextStep = x_ThisStep + dt * dx

    return x_NextStep


# Right side of ODE
def state_eqs(x, u, d, Parameter):

    # Local variables
    r_GB = Parameter.Turbine.r_GB
    J = Parameter.Turbine.J
    x_0T = Parameter.Turbine.x_0T
    m_eT = Parameter.Turbine.m_eT
    c_eT = Parameter.Turbine.c_eT
    k_eT = Parameter.Turbine.k_eT
    xi = Parameter.PitchActuator.xi
    omega = Parameter.PitchActuator.omega
    theta_dot_max = Parameter.PitchActuator.theta_dot_max

    v_0 = d
    theta_c = u[0]                     # commanded pitch angle
    M_g_c = u[1]                       # commanded generator torque
    Omega = x[0]                       # rotor speed
    x_T = x[1]                         # tower top displacement
    x_T_dot = x[2]                     # tower top speed
    theta = x[3]                       # pitch angle
    theta_dot = x[4]                   # pitch rate

    # Allocation
    nx = 5
    dx = np.zeros(nx)

    # Aerodynamics
    M_a = CalculateAerodynamicTorque(
        x_T_dot,
        Omega,
        theta_c,
        v_0,
        Parameter
    )

    F_a = CalculateAerodynamicThrust(
        x_T_dot,
        Omega,
        theta_c,
        v_0,
        Parameter
    )

    # ODEs
    dx[0] = 1/J * (M_a - M_g_c * r_GB)
    dx[1] = x_T_dot
    dx[2] = 1/m_eT * (
        F_a
        - c_eT * x_T_dot
        - k_eT * (x_T - x_0T)
    )
    dx[3] = min(max(theta_dot, -theta_dot_max), theta_dot_max)
    dx[4] = omega**2 * (theta_c - theta) - 2 * xi * omega * theta_dot

    return dx


# Aerodynamic Torque
def CalculateAerodynamicTorque(x_T_dot, Omega, theta, v_0, Parameter):

    # Local variables
    R = Parameter.Turbine.R
    rho = Parameter.General.rho

    v_rel = v_0 - x_T_dot                # relative speed of tower and wind
    lambda_ = Omega * R / v_rel

    c_P = QuickInterp2(
        Parameter.Turbine.SS['theta'],
        Parameter.Turbine.SS['lambda'],
        Parameter.Turbine.SS['c_P'],
        theta,
        lambda_
    )

    M_a = 1/2 * rho * np.pi * R**3 * c_P / lambda_ * v_rel**2

    return M_a


# Aerodynamic Thrust
def CalculateAerodynamicThrust(x_T_dot, Omega, theta, v_0, Parameter):

    # Local variables
    R = Parameter.Turbine.R
    rho = Parameter.General.rho

    v_rel = v_0 - x_T_dot                # relative speed of tower and wind
    lambda_ = Omega * R / v_rel

    c_T = QuickInterp2(
        Parameter.Turbine.SS['theta'],
        Parameter.Turbine.SS['lambda'],
        Parameter.Turbine.SS['c_T'],
        theta,
        lambda_
    )

    F_a = 1/2 * rho * np.pi * R**2 * c_T * v_rel**2

    return F_a


# QuickInterp2
def QuickInterp2(X, Y, Z, XI, YI):

    # Keep XI and YI within the limits
    XIc = min(max(X), XI)
    XIc = max(min(X), XIc)

    YIc = min(max(Y), YI)
    YIc = max(min(Y), YIc)

    # Find X and Y values
    nX = len(X)
    nY = len(Y)

    IndexX = np.searchsorted(X, XIc, side='right') - 1
    IndexY = np.searchsorted(Y, YIc, side='right') - 1

    IndexX = max(IndexX, 0)
    IndexX = min(IndexX, nX - 2)

    IndexY = max(IndexY, 0)
    IndexY = min(IndexY, nY - 2)

    X1 = X[IndexX]
    X2 = X[IndexX + 1]
    Y1 = Y[IndexY]
    Y2 = Y[IndexY + 1]

    # Z values
    Z11 = Z[IndexY,     IndexX]
    Z12 = Z[IndexY,     IndexX + 1]
    Z21 = Z[IndexY + 1, IndexX]
    Z22 = Z[IndexY + 1, IndexX + 1]

    # Interpolation
    ZI = (
        Z11 * (X2 - XI) * (Y2 - YI)
        + Z12 * (XI - X1) * (Y2 - YI)
        + Z21 * (X2 - XI) * (YI - Y1)
        + Z22 * (XI - X1) * (YI - Y1)
    ) / (X2 - X1) / (Y2 - Y1)

    return ZI