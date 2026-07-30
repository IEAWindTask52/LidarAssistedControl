# Wind turbine update

from bisect import bisect_right

import numpy as np


def SLOW(x_ThisStep, u_ThisStep, d_ThisStep, dt, Parameter):

    # RK4 (assuming d_NextStep = d_ThisStep)
    k1 = state_eqs(x_ThisStep,                 u_ThisStep, d_ThisStep, Parameter)
    k2 = state_eqs(x_ThisStep + 1/2 * k1 * dt, u_ThisStep, d_ThisStep, Parameter)
    k3 = state_eqs(x_ThisStep + 1/2 * k2 * dt, u_ThisStep, d_ThisStep, Parameter)
    k4 = state_eqs(x_ThisStep +       k3 * dt, u_ThisStep, d_ThisStep, Parameter)

    dx = 1/6 * (k1 + 2*k2 + 2*k3 + k4)
    x_NextStep = x_ThisStep + dt * dx

    # outputs: only [generator speed, pitch angle, tower top acceleration, electrical power] are considered measurable
    y_NextStep = np.array([
        x_NextStep[0] * Parameter.Turbine.r_GB,
        x_NextStep[3],
        dx[2],
        x_NextStep[0] * Parameter.Turbine.r_GB * u_ThisStep[1] * Parameter.Generator.eta_el
    ])

    return x_NextStep, y_NextStep


# Right side of ODE
def state_eqs(x, u, d, Parameter):

    # Local variables
    r_GB            = Parameter.Turbine.r_GB
    J               = Parameter.Turbine.J
    x_0T            = Parameter.Turbine.x_0T
    m_eT            = Parameter.Turbine.m_eT
    c_eT            = Parameter.Turbine.c_eT
    k_eT            = Parameter.Turbine.k_eT
    xi              = Parameter.PitchActuator.xi
    omega           = Parameter.PitchActuator.omega
    theta_dot_max   = Parameter.PitchActuator.theta_dot_max
    R               = Parameter.Turbine.R
    rho             = Parameter.General.rho
    SS              = Parameter.Turbine.SS

    v_0         = d
    theta_c     = u[0]                   # commanded pitch angle
    M_g_c       = u[1]                   # commanded generator torque
    Omega       = x[0]                   # rotor speed
    x_T         = x[1]                   # tower top displacement
    x_T_dot     = x[2]                   # tower top speed
    theta       = x[3]                   # pitch angle
    theta_dot   = x[4]                   # pitch rate

    # Allocation
    nx = 5
    dx = np.zeros(nx)

    # Aerodynamics
    v_rel   = v_0 - x_T_dot              # relative speed of tower and wind
    lambda_ = Omega * R / v_rel
    c_P, c_T = QuickInterp2(SS['theta'], SS['lambda'], SS['c_P'], SS['c_T'], theta, lambda_)
    M_a = 1/2 * rho * np.pi * R**3 * c_P / lambda_ * v_rel**2
    F_a = 1/2 * rho * np.pi * R**2 * c_T * v_rel**2

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


# QuickInterp2
def QuickInterp2(X, Y, Z1, Z2, XI, YI):

    # Bilinear interpolation of two tables sharing one grid. The bracketing
    # indices and the weights depend only on (XI, YI), so they are found once
    # and reused for both tables.

    # Keep XI and YI within the limits
    # (X, Y are the sorted theta/lambda grids, so their min/max are just
    # their first/last element - no need to scan the whole list)
    XIc = min(X[-1], max(X[0], XI))
    YIc = min(Y[-1], max(Y[0], YI))

    # Find X and Y intervals
    nX = len(X)
    nY = len(Y)

    IndexX = bisect_right(X, XIc) - 1
    IndexY = bisect_right(Y, YIc) - 1

    IndexX = max(IndexX, 0)
    IndexX = min(IndexX, nX - 2)

    IndexY = max(IndexY, 0)
    IndexY = min(IndexY, nY - 2)

    X1 = X[IndexX]
    X2 = X[IndexX + 1]
    Y1 = Y[IndexY]
    Y2 = Y[IndexY + 1]

    # weights, shared by both tables (XI and YI unclamped, as before, so a query
    # outside the grid extrapolates linearly from the edge cell)
    wX1 = XI - X1
    wX2 = X2 - XI
    wY1 = YI - Y1
    wY2 = Y2 - YI

    # Interpolation
    ZI1 = (
        Z1[IndexY,     IndexX]     * wX2 * wY2
        + Z1[IndexY,     IndexX + 1] * wX1 * wY2
        + Z1[IndexY + 1, IndexX]     * wX2 * wY1
        + Z1[IndexY + 1, IndexX + 1] * wX1 * wY1
    ) / (X2 - X1) / (Y2 - Y1)
    ZI2 = (
        Z2[IndexY,     IndexX]     * wX2 * wY2
        + Z2[IndexY,     IndexX + 1] * wX1 * wY2
        + Z2[IndexY + 1, IndexX]     * wX2 * wY1
        + Z2[IndexY + 1, IndexX + 1] * wX1 * wY1
    ) / (X2 - X1) / (Y2 - Y1)

    return ZI1, ZI2
