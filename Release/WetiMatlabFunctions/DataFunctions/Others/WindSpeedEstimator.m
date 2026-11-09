function [Omega_est,v_0_est] = WindSpeedEstimator(n,dt,Solver,Omega_est_0,v_0_est_0,Omega_g,theta,M_g,Parameter)

% local variables ---------------------------------------------------------
kp              = Parameter.Estimator.kp;
Ti              = Parameter.Estimator.Ti;
u_max           = Parameter.Estimator.u_max;
u_min           = Parameter.Estimator.u_min;
r_GB            = Parameter.Turbine.r_GB;
Omega_max       = Parameter.Estimator.Omega_max;
Omega_min       = Parameter.Estimator.Omega_min;
% -------------------------------------------------------------------------

% Servos
Omega           = Omega_g/r_GB;
P_LossAux       = CalculateAuxiliaryLosses(M_g,Omega_g,Parameter);
P_LossEl        = CalculateElectricalLosses(M_g,Omega_g,P_LossAux,Parameter);
[M_LossMe,M_Gen]= CalculateMechanicalLosses(P_LossEl,P_LossAux,M_g,Omega_g,Parameter);
M_HSS           = M_LossMe + M_Gen;

% initial values and allocation
x               = NaN(n,2);
x_dot           = NaN(n,2);
x(1,:)          = [Omega_est_0 v_0_est_0];

switch Solver
    case 'ODE1'
        for k = 1:1:n-1    
            x_dot(k,:)  = RightSideODE(x(k,:),Omega(k),theta(k),M_HSS(k),Parameter);
            x(k+1,:)    = x(k,:) + dt * x_dot(k,:);     
            % limit omega
            x(k+1,1)    = min(max(x(k+1,1),Omega_min),Omega_max);
        end

    case 'ODE4'
        for k = 1:1:n-1   
            k1          = RightSideODE(x(k,:),             Omega(k),                  theta(k),                  M_HSS(k),                  Parameter);
            k2          = RightSideODE(x(k,:) + 1/2*k1*dt, 1/2*(Omega(k)+Omega(k+1)), 1/2*(theta(k)+theta(k+1)), 1/2*(M_HSS(k)+M_HSS(k+1)), Parameter);
            k3          = RightSideODE(x(k,:) + 1/2*k2*dt, 1/2*(Omega(k)+Omega(k+1)), 1/2*(theta(k)+theta(k+1)), 1/2*(M_HSS(k)+M_HSS(k+1)), Parameter);
            k4          = RightSideODE(x(k,:) +     k3*dt, Omega(k+1),                theta(k+1),                M_HSS(k+1),                Parameter);
            x_dot(k,:)  = 1/6*(k1 + 2*k2 + 2*k3 + k4);
            x(k+1,:)    = x(k,:) + dt * x_dot(k,:);
            % limit omega
            x(k+1,1)    = min(max(x(k+1,1),Omega_min),Omega_max);            
        end

end

% states
Omega_est       = x(:,1);
v_0_est_I       = x(:,2);

% wind speed estimation
v_0_est_unc     = (Omega-Omega_est)*kp+v_0_est_I;
v_0_est         = max(min(v_0_est_unc,u_max),u_min);

end

function x_dot = RightSideODE(x,Omega,theta,M_HSS,Parameter)

% local variables ---------------------------------------------------------
J               = Parameter.Turbine.J;
r_GB            = Parameter.Turbine.r_GB;
kp              = Parameter.Estimator.kp;
Ti              = Parameter.Estimator.Ti;
u_max           = Parameter.Estimator.u_max;
u_min           = Parameter.Estimator.u_min;
% -------------------------------------------------------------------------

% states
Omega_est       = x(1);
v_0_est_I       = x(2);

% wind speed estimation
v_0_est_unc     = (Omega-Omega_est)*kp+v_0_est_I;
v_0_est         = max(min(v_0_est_unc,u_max),u_min);

% aerodynamics
M_a_est         = CalculateAerodynamicTorque(Omega,theta,v_0_est,Parameter);

% output
Omega_dot       = (M_a_est-M_HSS*r_GB)/J;
v_0_est_I_dot   = ((Omega-Omega_est)*kp+(v_0_est-v_0_est_unc))/Ti;
x_dot           = [Omega_dot v_0_est_I_dot];
end

function M_a = CalculateAerodynamicTorque(Omega,theta,v_0,Parameter)

% local variables ---------------------------------------------------------
R           = Parameter.Turbine.R;
rho         = Parameter.General.rho;
% -------------------------------------------------------------------------

% TSR and power coefficient
lambda      = (Omega*R)/v_0;
c_M         = QuickInterp2(Parameter.Turbine.SS.theta,Parameter.Turbine.SS.lambda,Parameter.Turbine.SS.c_M,theta,lambda);

% Equation (3.9) in [4]
M_a         = 1/2*rho*pi*R^3*c_M*v_0^2;

end

function P_LossAux = CalculateAuxiliaryLosses(M_g,Omega_g,Parameter)

% local variables ---------------------------------------------------------
Pref            = Parameter.Generator.Pref;
AuxLossTableX   = Parameter.Generator.AuxLossTable.x;
AuxLossTableY   = Parameter.Generator.AuxLossTable.y;
% -------------------------------------------------------------------------

% linear interpolation
xi              = ((M_g.*Omega_g) / (Pref*1e3)) * 1e2;
xi_con          = min(max(xi,AuxLossTableX(1)),AuxLossTableX(end)); % clip end values
yi              = interp1(AuxLossTableX, AuxLossTableY, xi_con); 
P_LossAux       = yi / 1e2 * (Pref*1e3) ;

end

function P_LossEl = CalculateElectricalLosses(M_g,Omega_g,P_LossAux,Parameter)

% local variables ---------------------------------------------------------
Pref        = Parameter.Generator.Pref;
ELoss       = Parameter.Generator.ELoss;
% -------------------------------------------------------------------------

% Newton interpolation
y           = ELoss;
xi        	= (M_g.*Omega_g - P_LossAux)/(Pref*1000);
yi          = NewtonQuadraticInterpolation(y,xi);
P_LossEl    = yi/100*(Pref*1000);

end

function [M_LossMe,M_Gen]= CalculateMechanicalLosses(P_LossEl,P_LossAux,M_g,Omega_g,Parameter)

% local variables ---------------------------------------------------------
Pref        = Parameter.Generator.Pref;
Nref        = Parameter.Generator.Nref;
Mloss       = Parameter.Generator.Mloss;
% -------------------------------------------------------------------------

M_LossEl    = RobustTorque(P_LossEl,Omega_g);
M_LossAux   = RobustTorque(P_LossAux,Omega_g);
M_Gen       = M_g + M_LossEl;

% Newton interpolation
Mref        = (Pref*1000) / (Nref/60*2*pi);
y           = Mloss;
xi          = (M_g - M_LossAux)/Mref;
yi          = NewtonQuadraticInterpolation(y,xi);
M_LossMe    = yi/100*Mref;

end

function yi = NewtonQuadraticInterpolation(y,xi)
% https://pythonnumericalmethods.berkeley.edu/notebooks/chapter17.05-Newtons-Polynomial-Interpolation.html
% https://www.lernhelfer.de/schuelerlexikon/mathematik-abitur/artikel/newtonsches-und-lagrangesches-interpolationsverfahren#
% with x0=0; x1=1/2; x2=1;

% coefficients
y0 = y(1);
y1 = y(2);
y2 = y(3);
a0 =                 y0;    % y0 = a0
a1 =        2*y1 - 2*y0;    % y1 = a0 + a1*(x1-x0)                      = a0 + a1*1/2
a2 = 2*y2 - 4*y1 + 2*y0;    % y2 = a0 + a1*(x2-x0) + a2*(x2-x0)*(x2-x1) = a0 + a1     + a2*1/2

% polynomial
% p(x) = a0 + a1*(x-x0) + a2*(x-x0)*(x-x1)
% p(x) = a0 + (a1-1/2*a2)*x + a2*x^2
p0 = a0;                    
p1 = a1-1/2*a2;
p2 = a2;

% interpolation
yi = p0 + p1 * xi + p2 * xi.^2;

end

function M = RobustTorque(P,Omega)

epsilon = 1e-10;
M = P./Omega;

M(abs(Omega)<epsilon)=0;

end