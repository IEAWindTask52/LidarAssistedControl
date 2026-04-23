% Script to generate different frequency signals for the use of the PFC
% DLL.
clear;close all;clc;
%% Preprocessing

% params
Tmax = 40; % [s]
dt = 0.01; % [s]
f0 = 50; % [Hz] grid frequency

% noise parameters
SNR         = 70; % [-] signal to noise ratio
AvgWindow   = 100; % [-] window size for noise averaging
% triangle parameters
shape = 1/2; % 1/2 = triangle
amplitude = 0.2; 

% generate base signal
t = 0:dt:Tmax-dt;

% select signal type
SignalType = "noisy"; % possible: triangle, noisy

switch SignalType
    case "noisy"
        f_constant = ones(1,length(t))*f0;
        % add noise
        rng(1); % initialize random number generator to get the same results every time
        P_signal = mean(f_constant.^2);
        P_noise = P_signal / 10^(SNR/10);
        noise = sqrt(P_noise) * randn(size(t));
        NoiseSmooth = movmean(noise,AvgWindow);
        f_noisy = f_constant + NoiseSmooth;
        f = f_noisy;
    case "triangle"
        f = sawtooth(t,shape)*amplitude + f0;
    otherwise
        error("No case with this name")
end

% calculate RoCoF
RoCoF = diff(f)./dt;

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

%% Generate csv file
% frequency
m = [t(1:end-1);f(1:end-1)]';
writematrix(m,"FrequencySignal.csv");
