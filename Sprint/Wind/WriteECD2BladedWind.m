%
%   [Description]
%   Script to generate an Extreme Coherent Gust with direction change (ECD)
%   according to IEC 61400-1.
%
%
%% Setup
clear all; close all; clc;

FileName = 'ECD_VrPlus2mps';

%% Preprocessing
% Time discretization
T           = 70;                   % [s]   total simulation time
dt          = 1/100;               	% [s]   simulation time step
t           = 0:dt:T-dt;            % [s]   simulation time vector

% Wind conditions at hub height
V_hub       = 12.5;                 % [m/s] mean wind speed at hub height: v_rated + 2/m/s 
t_start     = 30;                   % [s]   start time of gust event

% Extreme coherent gust with direction change (ECD - IEC 6.3.2.5)
V_cg        = 15;                   % [m/s] coherent gust amplitude
T_gust      = 10;                   % [s]   rise time of coherent gust

% Grid definition
dy          = 10;                   % [m]   lateral spacing
dz          = 10;                   % [m]   vertical spacing
Ny          = 29;                   % [-]   number of grid points in lateral direction
Nz          = 29;                   % [-]   number of grid points in vertical direction

% Some variables required in the Type 4 wind: Bladed style
HubHeight   = 150;                  % [m]   hub height      
URef        = V_hub+V_cg/2;         % [m/s] reference mean wind speed
zOffset     = HubHeight;            % [m]   reference height of the grid
z0          = 0.1;                  % [m]   the rougthness length, not really used

% The normal wind profile model (NWP - IEC 6.3.1.2)
alpha       = 0.2;                                  % [-]   shear exponent for NWP
z_hub       = HubHeight;
z           = [-(Nz-1)/2:1:(Nz-1)/2]*dz+HubHeight;  % [m]   vertical coordinates of the grid
V_z         = V_hub*(z/z_hub).^alpha;               % [m/s] mean wind profile at each vertical grid point

% Magnitude of ECD
u_W         = NaN(Nz, length(t));
for iTimeStep = 1:length(t)
    if (t(iTimeStep) < t_start)
        u_W(:,iTimeStep)    = V_z;
    elseif (t(iTimeStep) >= t_start) && (t(iTimeStep) <= t_start+T_gust)
        t_gust              = t(iTimeStep) - t_start;
        u_W(:,iTimeStep)    = V_z+0.5*V_cg*(1-cos(pi*t_gust/T_gust));
    else
        u_W(:,iTimeStep)    = V_z+V_cg;
    end
end

% Direction change of ECD
theta       = NaN(1, length(t));   
theta_cg    = ones(1, length(t)).*deg2rad((720/V_hub));     % [rad]

for iTimeStep   = 1:length(t)
    if (t(iTimeStep) < t_start)
        theta(iTimeStep)    = 0;
    elseif (t(iTimeStep) >= t_start) && (t(iTimeStep) <= t_start+T_gust)
        t_gust              = t(iTimeStep) - t_start;
        theta(iTimeStep)    = 0.5 * theta_cg(iTimeStep) * (1-cos(pi*t_gust/T_gust));
    else
        theta(iTimeStep)    = theta_cg(iTimeStep);
    end
end

% Transformation into inertial coordinate system
u_I = u_W.*repmat(cos(theta),Nz,1);     % longitudinal
v_I = u_W.*repmat(sin(theta),Nz,1);     % lateral

%% Plot and validate results
Idx = ismember(z,[50:50:250]);

figure

subplot(311)
hold on; grid on; box on
plot(t,u_W(Idx,:)')
ylabel('u_W [m/s]')
title('Wind speed magnitude')

subplot(312)
hold on; grid on; box on
plot(t,v_I(Idx,:)')
ylabel('v_I [m/s]')
title('Lateral component')

subplot(313) 
hold on; grid on; box on
plot(t,u_I(Idx,:)')
ylabel('u_I [m/s]')
title('Longitudinal component')

xlabel('time [s]')
legend(z(Idx)+" m")

%% Prepare output arrays for .wnd-file
SummVars(1) = HubHeight;    % HubHeight
SummVars(3) = URef;         % Mean wind speed at hub-height

velocity    = NaN(max(size(t)),3,Ny,Nz);  % [time, (u,v,w), y, z]
for iy = 1:Ny
    for iz = 1:Nz
        velocity(:,1,iy,iz) = u_I(iz,:);    % longitudinal component
        velocity(:,2,iy,iz) = v_I(iz,:);    % lateral component
        velocity(:,3,iy,iz) = 0;            % no vertical component
    end
end

% Get turbulence intensity for .sum-file
% here we use std u for w component to avoid dividing by zero
SummVars(4) = std(squeeze(velocity(:,1,1,1)))/URef*100;
SummVars(5) = std(squeeze(velocity(:,2,1,1)))/URef*100;     
SummVars(6) = std(squeeze(velocity(:,1,1,1)))/URef*100;

% Export rotor-plane wind field (.wnd + .sum) for OpenFAST
disp('Exporting rotor plane wind field as ".wnd" binary files...')
WriteBLgrid([FileName '.wnd'], velocity, dy, dz, dt, zOffset, z0, SummVars)

fileID    = fopen([FileName '.sum'],'w');
fprintf(fileID,'This summary file is not complete it only contains required information for the OpenFAST');
fprintf(fileID,'\n');
fprintf(fileID,'F        Clockwise rotation when looking downwind?\n');
fprintf(fileID,[num2str(SummVars(1)) '  Hub height [m] \n']);
fprintf(fileID,['UBar   =  ' num2str(SummVars(3)) ' m/s \n']);
fprintf(fileID,['TI(u)  =  ' num2str(SummVars(4)) ' %%\n']);
fprintf(fileID,['TI(v)  =  ' num2str(SummVars(5)) ' %%\n']);
fprintf(fileID,['TI(w)  =  ' num2str(SummVars(6)) ' %%\n']);
fprintf(fileID,['Height Offset =  ' num2str(0) ' m\n']);
fprintf(fileID,'Creating a PERIODIC output file.');
fclose(fileID);
