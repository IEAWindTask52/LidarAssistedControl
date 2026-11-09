%% Theta_K comparison: 3 deg vs 6 deg
clearvars; close all; clc

%% Load results
load('Statistics_BruteForceRegion3_FeedbackOnly.mat','Statistics');

%% Wind speeds used in the screening simulations
WindSpeedBins = [12 16 20 24];

%% Separate ThetaK = 3 deg and ThetaK = 6 deg
% Data order:
% URef 12: Seed1 03, Seed1 06, Seed2 03, Seed2 06, Seed3 03, Seed3 06
% URef 16: same
% URef 20: same
% URef 24: same

Statistics03 = Statistics(1:2:end,:);
Statistics06 = Statistics(2:2:end,:);

%% Pre-allocate results
nWind = length(WindSpeedBins);

TowerDEL03   = zeros(1,nWind);
TowerDEL06   = zeros(1,nWind);

ShaftDEL03   = zeros(1,nWind);
ShaftDEL06   = zeros(1,nWind);

BladeDEL03   = zeros(1,nWind);
BladeDEL06   = zeros(1,nWind);

Power03      = zeros(1,nWind);
Power06      = zeros(1,nWind);

Overspeed03  = zeros(1,nWind);
Overspeed06  = zeros(1,nWind);

PitchTravel03 = zeros(1,nWind);
PitchTravel06 = zeros(1,nWind);

MaxGenTq03   = zeros(1,nWind);
MaxGenTq06   = zeros(1,nWind);

%% Calculate mean/max over the 3 seeds
for i = 1:nWind

    % Rows belonging to this wind speed
    rows = (i-1)*3 + (1:3);

    %% DELs: mean of 3 seeds
    TowerDEL03(i) = mean(Statistics03.DEL_4_TwrBsMyt(rows));
    TowerDEL06(i) = mean(Statistics06.DEL_4_TwrBsMyt(rows));

    ShaftDEL03(i) = mean(Statistics03.DEL_4_RotTorq(rows));
    ShaftDEL06(i) = mean(Statistics06.DEL_4_RotTorq(rows));

    BladeDEL03(i) = mean(Statistics03.DEL_10_RootMyb1(rows));
    BladeDEL06(i) = mean(Statistics06.DEL_10_RootMyb1(rows));

    %% Electrical power: mean of 3 seeds
    Power03(i) = mean(Statistics03.mean_GenPwr(rows));
    Power06(i) = mean(Statistics06.mean_GenPwr(rows));

    %% Overspeed
    % Maximum value among the 3 seeds
    Overspeed03(i) = max(Statistics03.Overshoot_GenSpeed(rows));
    Overspeed06(i) = max(Statistics06.Overshoot_GenSpeed(rows));

    %% Pitch travel: mean of 3 seeds
    PitchTravel03(i) = mean(Statistics03.Travel_BldPitch1(rows));
    PitchTravel06(i) = mean(Statistics06.Travel_BldPitch1(rows));

    %% Maximum generator torque: maximum of 3 seeds
    MaxGenTq03(i) = max(Statistics03.max_GenTq(rows));
    MaxGenTq06(i) = max(Statistics06.max_GenTq(rows));

end


%% ============================================================
%  PLOT 1: TOWER DEL
% =============================================================

figure('Name','Tower DEL');
hold on; grid on; box on

plot(WindSpeedBins,TowerDEL03/1e3,'o-','LineWidth',1.5)
plot(WindSpeedBins,TowerDEL06/1e3,'s-','LineWidth',1.5)

xlabel('Wind speed [m/s]')
ylabel('Tower DEL')
title('Tower DEL: \theta_K comparison')

legend('\theta_K = 3 deg','\theta_K = 6 deg',...
    'Location','best')


%% ============================================================
%  PLOT 2: SHAFT DEL
% =============================================================

figure('Name','Shaft DEL');
hold on; grid on; box on

plot(WindSpeedBins,ShaftDEL03/1e3,'o-','LineWidth',1.5)
plot(WindSpeedBins,ShaftDEL06/1e3,'s-','LineWidth',1.5)

xlabel('Wind speed [m/s]')
ylabel('Shaft DEL')
title('Shaft DEL: \theta_K comparison')

legend('\theta_K = 3 deg','\theta_K = 6 deg',...
    'Location','best')


%% ============================================================
%  PLOT 3: BLADE DEL
% =============================================================

figure('Name','Blade DEL');
hold on; grid on; box on

plot(WindSpeedBins,BladeDEL03/1e3,'o-','LineWidth',1.5)
plot(WindSpeedBins,BladeDEL06/1e3,'s-','LineWidth',1.5)

xlabel('Wind speed [m/s]')
ylabel('Blade DEL')
title('Blade DEL: \theta_K comparison')

legend('\theta_K = 3 deg','\theta_K = 6 deg',...
    'Location','best')


%% ============================================================
%  PLOT 4: ELECTRICAL POWER
% =============================================================

figure('Name','Electrical Power');
hold on; grid on; box on

plot(WindSpeedBins,Power03/1e3,'o-','LineWidth',1.5)
plot(WindSpeedBins,Power06/1e3,'s-','LineWidth',1.5)

xlabel('Wind speed [m/s]')
ylabel('Electrical power [MW]')
title('Electrical Power: \theta_K comparison')

legend('\theta_K = 3 deg','\theta_K = 6 deg',...
    'Location','best')


%% ============================================================
%  PLOT 5: GENERATOR OVERSPEED
% =============================================================

figure('Name','Generator Overspeed');
hold on; grid on; box on

plot(WindSpeedBins,Overspeed03*100,'o-','LineWidth',1.5)
plot(WindSpeedBins,Overspeed06*100,'s-','LineWidth',1.5)

% Professor's target: overspeed < 20 %
yline(20,'k--','20% limit','LineWidth',1.2)

xlabel('Wind speed [m/s]')
ylabel('Generator overspeed [%]')
title('Generator Overspeed: \theta_K comparison')

legend('\theta_K = 3 deg','\theta_K = 6 deg','20% limit',...
    'Location','best')


%% ============================================================
%  PLOT 6: PITCH TRAVEL
% =============================================================

figure('Name','Pitch Travel');
hold on; grid on; box on

plot(WindSpeedBins,PitchTravel03,'o-','LineWidth',1.5)
plot(WindSpeedBins,PitchTravel06,'s-','LineWidth',1.5)

xlabel('Wind speed [m/s]')
ylabel('Pitch travel')
title('Pitch Travel: \theta_K comparison')

legend('\theta_K = 3 deg','\theta_K = 6 deg',...
    'Location','best')


%% ============================================================
%  PLOT 7: MAXIMUM GENERATOR TORQUE
% =============================================================

figure('Name','Maximum Generator Torque');
hold on; grid on; box on

plot(WindSpeedBins,MaxGenTq03,'o-','LineWidth',1.5)
plot(WindSpeedBins,MaxGenTq06,'s-','LineWidth',1.5)

% Maximum torque limit
VS_MaxTq = 15600000;     % Nm

yline(VS_MaxTq,'k--','Maximum torque',...
    'LineWidth',1.2)

xlabel('Wind speed [m/s]')
ylabel('Generator torque [kNm]')
title('Maximum Generator Torque: \theta_K comparison')

legend('\theta_K = 3 deg','\theta_K = 6 deg',...
    'Maximum torque',...
    'Location','best')


%% ============================================================
%  PLOT 8: ALL-IN-ONE OVERSPEED FOCUS
% =============================================================

figure('Name','Theta_K Optimization');
hold on; grid on; box on

plot(WindSpeedBins,Overspeed03*100,'o-','LineWidth',2)
plot(WindSpeedBins,Overspeed06*100,'s-','LineWidth',2)

yline(20,'k--','20% target','LineWidth',1.2)

xlabel('Wind speed [m/s]')
ylabel('Generator overspeed [%]')
title('\theta_K optimization: Generator overspeed')

legend('\theta_K = 3 deg','\theta_K = 6 deg',...
    '20% target',...
    'Location','best')


%% ============================================================
%  DISPLAY NUMERICAL COMPARISON
% =============================================================

ResultsTable = table(WindSpeedBins', TowerDEL03', TowerDEL06', ShaftDEL03', ShaftDEL06', BladeDEL03', BladeDEL06', Power03', Power06', Overspeed03'*100, Overspeed06'*100,PitchTravel03', PitchTravel06',MaxGenTq03'/1e3, MaxGenTq06'/1e3, ...
    'VariableNames',{'WindSpeed','TowerDEL_3deg','TowerDEL_6deg','ShaftDEL_3deg','ShaftDEL_6deg','BladeDEL_3deg','BladeDEL_6deg','PowerMW_3deg','PowerMW_6deg','OverspeedPct_3deg','OverspeedPct_6deg', 'PitchTravel_3deg','PitchTravel_6deg','MaxGenTq_kNm_3deg','MaxGenTq_kNm_6deg'});
disp(' ')
disp('===== Theta_K COMPARISON =====')
disp(ResultsTable)
%% ============================================================
%  COMBINED FIGURE: ALL THETA_K COMPARISON RESULTS
% =============================================================

figure('Name','Complete Theta_K Comparison',...
    'Position',[100 100 1200 850]);

tiledlayout(4,2,'TileSpacing','compact','Padding','compact');

% 1. Tower DEL
nexttile
plot(WindSpeedBins,TowerDEL03/1e3,'o-','LineWidth',1.5)
hold on
plot(WindSpeedBins,TowerDEL06/1e3,'s-','LineWidth',1.5)
grid on; box on
title('Tower DEL')
xlabel('Wind speed [m/s]')
ylabel('DEL / 1000')
legend('3 deg','6 deg','Location','best')

% 2. Shaft DEL
nexttile
plot(WindSpeedBins,ShaftDEL03/1e3,'o-','LineWidth',1.5)
hold on
plot(WindSpeedBins,ShaftDEL06/1e3,'s-','LineWidth',1.5)
grid on; box on
title('Shaft DEL')
xlabel('Wind speed [m/s]')
ylabel('DEL / 1000')

% 3. Blade DEL
nexttile
plot(WindSpeedBins,BladeDEL03/1e3,'o-','LineWidth',1.5)
hold on
plot(WindSpeedBins,BladeDEL06/1e3,'s-','LineWidth',1.5)
grid on; box on
title('Blade DEL')
xlabel('Wind speed [m/s]')
ylabel('DEL / 1000')

% 4. Electrical power
nexttile
plot(WindSpeedBins,Power03/1e3,'o-','LineWidth',1.5)
hold on
plot(WindSpeedBins,Power06/1e3,'s-','LineWidth',1.5)
grid on; box on
title('Electrical Power')
xlabel('Wind speed [m/s]')
ylabel('Power [MW]')

% 5. Generator overspeed
nexttile
plot(WindSpeedBins,Overspeed03*100,'o-','LineWidth',1.5)
hold on
plot(WindSpeedBins,Overspeed06*100,'s-','LineWidth',1.5)
yline(20,'k--','20% target')
grid on; box on
title('Generator Overspeed')
xlabel('Wind speed [m/s]')
ylabel('Overspeed [%]')

% 6. Pitch travel
nexttile
plot(WindSpeedBins,PitchTravel03,'o-','LineWidth',1.5)
hold on
plot(WindSpeedBins,PitchTravel06,'s-','LineWidth',1.5)
grid on; box on
title('Pitch Travel')
xlabel('Wind speed [m/s]')
ylabel('Pitch travel')

% % 7. Maximum generator torque
% nexttile
% plot(WindSpeedBins,MaxGenTq03,'o-','LineWidth',1.5)
% hold on
% plot(WindSpeedBins,MaxGenTq06,'s-','LineWidth',1.5)
% yline(15600,'k--','Maximum torque')
% grid on; box on
% title('Maximum Generator Torque')
% xlabel('Wind speed [m/s]')
% ylabel('Torque [kNm]')

sgtitle('Region 3 Controller Comparison: \theta_K = 3 deg vs 6 deg')