%% Wind turbine update
function [x_NextStep,y_NextStep] = SLOW(x_ThisStep,u_ThisStep,d_ThisStep,dt,Parameter)

% RK4 (assuming d_NextStep = d_ThisStep)
k1         	    = state_eqs(x_ThisStep,            u_ThisStep,d_ThisStep,Parameter);
k2       	    = state_eqs(x_ThisStep + 1/2*k1*dt,u_ThisStep,d_ThisStep,Parameter);
k3       	    = state_eqs(x_ThisStep + 1/2*k2*dt,u_ThisStep,d_ThisStep,Parameter);
k4        	    = state_eqs(x_ThisStep +     k3*dt,u_ThisStep,d_ThisStep,Parameter);
dx              = 1/6*(k1 + 2*k2 + 2*k3 + k4);
x_NextStep      = x_ThisStep + dt*dx;

% outputs: only [generator speed, pitch angle, tower top acceleration, electrical power] are considered measurable
y_NextStep      = [x_NextStep(1)*Parameter.Turbine.r_GB,...
                   x_NextStep(4),...
                   dx(3),...
                   x_NextStep(1)*Parameter.Turbine.r_GB*u_ThisStep(2)*Parameter.Generator.eta_el];

end

%% right side of ODE
function dx = state_eqs(x,u,d,Parameter)
% local variables ---------------------------------------------------------
r_GB            = Parameter.Turbine.r_GB;
J               = Parameter.Turbine.J;
x_0T            = Parameter.Turbine.x_0T;
m_eT            = Parameter.Turbine.m_eT;
c_eT            = Parameter.Turbine.c_eT;
k_eT            = Parameter.Turbine.k_eT;
xi              = Parameter.PitchActuator.xi;
omega           = Parameter.PitchActuator.omega;
theta_dot_max   = Parameter.PitchActuator.theta_dot_max;
R               = Parameter.Turbine.R;
rho             = Parameter.General.rho;
SS              = Parameter.Turbine.SS;
% -------------------------------------------------------------------------
v_0             = d;
theta_c         = u(1);             % commanded pitch angle
M_g_c           = u(2);             % commanded generator torque
Omega           = x(1);          	% rotor speed
x_T             = x(2);          	% tower top displacement
x_T_dot         = x(3);            	% tower top speed
theta           = x(4);             % pitch angle
theta_dot       = x(5);             % pitch rate
% -------------------------------------------------------------------------

% Allocation
nx              = 5;
dx              = zeros(1,nx);

% Aerodynamics
v_rel           = v_0 - x_T_dot;    % relative speed of tower and wind
lambda          = Omega * R / v_rel;
[c_P,c_T]       = QuickInterp2(SS.theta,SS.lambda,SS.c_P,SS.c_T,theta,lambda);
M_a             = 1/2 * rho * pi * R^3 * c_P / lambda * v_rel^2;
F_a             = 1/2 * rho * pi * R^2 * c_T * v_rel^2;

% ODEs
dx(1)           = 1/J     * ( M_a - M_g_c*r_GB);
dx(2)           = x_T_dot;
dx(3)           = 1/m_eT  * ( F_a - c_eT*x_T_dot - k_eT*(x_T-x_0T));
dx(4)           = min(max(theta_dot,-theta_dot_max),theta_dot_max);
dx(5)           = omega^2*(theta_c - theta) - 2*xi*omega*theta_dot;

end

%% QuickInterp2
function [ZI1,ZI2] = QuickInterp2(X,Y,Z1,Z2,XI,YI)
%#codegen

% Bilinear interpolation of two tables sharing one grid. The bracketing indices
% and the weights depend only on (XI,YI), so they are found once and reused for
% both tables.

nX          = length(X);
nY          = length(Y);
X1v         = X(1);
XEnd        = X(nX);
Y1v         = Y(1);
YEnd        = Y(nY);

% keep XI and YI within the limits (X and Y are sorted ascending, so the
% end points are the extrema - no need to scan the arrays)
XIc         = min(XEnd,XI);
XIc         = max(X1v,XIc);
YIc         = min(YEnd,YI);
YIc         = max(Y1v,YIc);

% Find X and Y intervals. theta is on a uniform grid and lambda is uniform in
% 1/lambda (the tables were built from a uniform wind speed grid), so the
% interval follows from arithmetic instead of a search. The while-guards cost
% one comparison each on such a grid and keep the result correct for any
% monotonic grid, should the tables ever be regenerated differently.
IndexX      = floor((XIc-X1v)/((XEnd-X1v)/(nX-1))) + 1;
IndexX      = max(IndexX,1);
IndexX      = min(IndexX,nX-1);
while IndexX > 1    && XIc < X(IndexX  ), IndexX = IndexX - 1; end
while IndexX < nX-1 && XIc > X(IndexX+1), IndexX = IndexX + 1; end

U1          = 1/Y1v;
IndexY      = floor((1/YIc-U1)/((1/YEnd-U1)/(nY-1))) + 1;
IndexY      = max(IndexY,1);
IndexY      = min(IndexY,nY-1);
while IndexY > 1    && YIc < Y(IndexY  ), IndexY = IndexY - 1; end
while IndexY < nY-1 && YIc > Y(IndexY+1), IndexY = IndexY + 1; end

X1          = X(IndexX  );
X2          = X(IndexX+1);
Y1          = Y(IndexY  );
Y2          = Y(IndexY+1);

% weights, shared by both tables (XI and YI unclamped, as before, so a query
% outside the grid extrapolates linearly from the edge cell)
wX1         = XI-X1;
wX2         = X2-XI;
wY1         = YI-Y1;
wY2         = Y2-YI;

% Interpolation
ZI1         =   (Z1(IndexY  ,IndexX  )*wX2*wY2...
                +Z1(IndexY  ,IndexX+1)*wX1*wY2...
                +Z1(IndexY+1,IndexX  )*wX2*wY1...
                +Z1(IndexY+1,IndexX+1)*wX1*wY1)/(X2-X1)/(Y2-Y1);
ZI2         =   (Z2(IndexY  ,IndexX  )*wX2*wY2...
                +Z2(IndexY  ,IndexX+1)*wX1*wY2...
                +Z2(IndexY+1,IndexX  )*wX2*wY1...
                +Z2(IndexY+1,IndexX+1)*wX1*wY1)/(X2-X1)/(Y2-Y1);
end
