%% Wind turbine update
function [x_NextStep,y_NextStep] = SLOW(x_ThisStep,u_ThisStep,d_ThisStep,dt,Parameter)

% RK4 (assuming d_NextStep = d_ThisStep)
k1         	    = state_eqs(x_ThisStep,            u_ThisStep,d_ThisStep,Parameter);
k2       	    = state_eqs(x_ThisStep + 1/2*k1*dt,u_ThisStep,d_ThisStep,Parameter);
k3       	    = state_eqs(x_ThisStep + 1/2*k2*dt,u_ThisStep,d_ThisStep,Parameter);
k4        	    = state_eqs(x_ThisStep +     k3*dt,u_ThisStep,d_ThisStep,Parameter);
dx              = 1/6*(k1 + 2*k2 + 2*k3 + k4);
x_NextStep      = x_ThisStep + dt*dx; 

% outputs: only [generator speed, pitch angel, tower top acceleration, electrical power] are considered measureable
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
M_a             = CalculateAerodynamicTorque(x_T_dot,Omega,theta,v_0,Parameter);
F_a             = CalculateAerodynamicThrust(x_T_dot,Omega,theta,v_0,Parameter);

% ODEs
dx(1)           = 1/J     * ( M_a - M_g_c*r_GB);
dx(2)           = x_T_dot;
dx(3)           = 1/m_eT  * ( F_a - c_eT*x_T_dot - k_eT*(x_T-x_0T));
dx(4)           = min(max(theta_dot,-theta_dot_max),theta_dot_max);
dx(5)           = omega^2*(theta_c - theta) - 2*xi*omega*theta_dot;

end

%% Aerodynamic Torque
function M_a = CalculateAerodynamicTorque(x_T_dot,Omega,theta,v_0,Parameter)

% local variables ---------------------------------------------------------
R       = Parameter.Turbine.R;
rho     = Parameter.General.rho;
% -------------------------------------------------------------------------

v_rel   = v_0 - x_T_dot;   % relative speed of tower and wind
lambda  = Omega * R / v_rel;
c_P     = QuickInterp2(Parameter.Turbine.SS.theta,Parameter.Turbine.SS.lambda,Parameter.Turbine.SS.c_P,theta,lambda);

M_a     = 1/2 * rho * pi * R^3 * c_P / lambda * v_rel^2;

end

%% Aerodynamic Thrust
function F_a = CalculateAerodynamicThrust(x_T_dot,Omega,theta,v_0,Parameter)

% local variables ---------------------------------------------------------
R       = Parameter.Turbine.R;
rho     = Parameter.General.rho;
% -------------------------------------------------------------------------

v_rel   = v_0 - x_T_dot;   % relative speed of tower and wind
lambda  = Omega * R / v_rel;
c_T     = QuickInterp2(Parameter.Turbine.SS.theta,Parameter.Turbine.SS.lambda,Parameter.Turbine.SS.c_T,theta,lambda);

F_a     = 1/2 * rho * pi * R^2 * c_T * v_rel^2;

end

%% QuickInterp2
function ZI = QuickInterp2(X,Y,Z,XI,YI)
%#codegen   

% keep XI and YI within the limits
XIc         = min(max(X),XI);
XIc         = max(min(X),XIc);
YIc         = min(max(Y),YI);
YIc         = max(min(Y),YIc);

% Find X and Y values
nX          = length(X);
nY          = length(Y);
[~,IndexX]  = histc(XIc,X);
[~,IndexY]  = histc(YIc,Y);
% Code to replace histc (but slower) -->
% DiffX           = (XI-X);
% DiffX(DiffX<0)  = inf;   
% [~,IndexX]      = min(DiffX);
% DiffY           = (YI-Y);
% DiffY(DiffY<0)  = inf;   
% [~,IndexY]      = min(DiffY);
% <--
IndexX      = max(IndexX,1);
IndexX      = min(IndexX,nX-1);
IndexY      = max(IndexY,1);
IndexY      = min(IndexY,nY-1);
X1          = X(IndexX  );
X2          = X(IndexX+1);
Y1          = Y(IndexY  );
Y2          = Y(IndexY+1);

% Z values
Z11         = Z(IndexY  ,IndexX  );
Z12         = Z(IndexY  ,IndexX+1);
Z21         = Z(IndexY+1,IndexX  );
Z22         = Z(IndexY+1,IndexX+1);

% Interpolation        
ZI          =   (Z11*(X2-XI)*(Y2-YI)...
                +Z12*(XI-X1)*(Y2-YI)...
                +Z21*(X2-XI)*(YI-Y1)...
                +Z22*(XI-X1)*(YI-Y1))/(X2-X1)/(Y2-Y1);
end
        
