% Script to generate different frequency signals for the use of the PFC
% DLL.
clear;close all;clc;
%% Preprocessing

% params
Tmax = 30; % [s]
dt = 0.01; % [s]
f0 = 50; % [Hz] grid frequency

% noise parameters
P_noise     = 1e-3;
% triangle parameters
shape = 1/2; % 1/2 = triangle
amplitude = 0.2; 

% generate base signal
t = 0:dt:Tmax;

% select signal type
SignalType = "noisy"; % possible: triangle, noisy
AvgWindow  = 50;

switch SignalType
    case "noisy"
        f_constant = ones(1,length(t))*f0;
        % add noise
        rng(1); % initialize random number generator to get the same results every time
        noise = sqrt(P_noise) * randn(size(t));
        f_noisy = f_constant + noise;
        f = f_noisy;
        RoCoF = diff(movmean(f,AvgWindow))./dt;
    case "triangle"
        f = sawtooth(t,shape)*amplitude + f0;
        RoCoF = diff(f)./dt;
    otherwise
        error("No case with this name")
end

% calculate RoCoF



% Plot signal
figure("Name","FrequencySignalGeneration")
subplot(211)
hold on; grid on; box on
plot(t,f);
ylabel("f [Hz]")

subplot(212)
hold on; grid on; box on
plot(t(1:end-1),RoCoF);
ylabel("RoCoF [Hz/s]")

% Frequency Statistics
fprintf("min. f: %.2f Hz\n",min(f));
fprintf("max. f: %.2f Hz\n",max(f));
fprintf("max. abs. RoCoF: %.2f Hz/s\n",max(abs(RoCoF)));

%% Generate csv file
% frequency
m = [t;f]';
writematrix(m,"FrequencySignal.csv");
