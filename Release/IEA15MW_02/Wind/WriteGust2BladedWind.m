clear all;
close all;
clc;

%%
FileName = 'EOG_URef_18';

%% Preprocessing
% time
T           = 160;                  % [s]   simulation length
dt          = 1/80;               	% [s]   simulation time step

% wind
V_hub       = 18;                   % [m/s] mean wind speed at hub height 
T_gust      = 10.5;                 % [s]   length of EOG
t_start     = 15;                   % [s]   time when EOG should start
V_ref       = 50;                   % [m/s] reference wind speed average over 10 min
I_ref       = 0.14;                 % [-]   expected value of the turbulence intensity at 15 m/s
D           = 240;                  % [m]   rotor diameter

% calculation of wind signals
t           = 0:dt:T-dt;

% Longitudinal scale parameter at hub height (assuming hub height >60 m)
lambda1     = 42;

% Extreme Operational Gust (EOG) definition
sigma1      = I_ref * (0.75*V_hub + 5.6);
V_e50     	= 1.4*V_ref;
V_e1      	= 0.8*V_e50;
V_gust      = min([1.35*(V_e1-V_hub), 3.3*(sigma1/(1+0.1*D/lambda1))]);

% EOG wind speed vector
u           = zeros(1, length(t));
for iTimeStep = 1:length(t)
    if (t(iTimeStep) >= t_start) && (t(iTimeStep) <= t_start+T_gust)
        t_EOG           = t(iTimeStep) - t_start;
        u(iTimeStep)    = V_hub-0.37*V_gust*sin(3*pi*t_EOG/T_gust)*(1-cos(2*pi*t_EOG/T_gust));
    else
        u(iTimeStep)    = V_hub;
    end
end


figure()
plot(t,u)


% Some variables required in the Type 4 wind: Bladed style
HubHeight   = 150;
dy          = 150;
dz          = 150;
Ny          = 3;
Nz          = 3;
URef        = V_hub;
zOffset     = HubHeight;
z0          = 0.1;          % the rougthness length, not really used


SummVars(1) = HubHeight;    % HubHeight
SummVars(3) = URef;

velocity    = zeros(max(size(t)),3,Ny,Nz);
% prepare to write out
for iy = 1:Ny
    for iz = 1:Nz
        velocity(:,1,iy,iz) = u;
    end
end

% here we use std u for v and w components, to avoid dividing by zero
SummVars(4) = std(squeeze(velocity(:,1,1,1)))/URef*100;
SummVars(5) = std(squeeze(velocity(:,1,1,1)))/URef*100;
SummVars(6) = std(squeeze(velocity(:,1,1,1)))/URef*100;

disp('Exporting rotor plane wind field as ".wnd" binary files...')
% write the .wnd
WriteBLgrid([FileName '.wnd'], velocity, dy, dz, dt, zOffset, z0, SummVars)

% Now write a sum file
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

