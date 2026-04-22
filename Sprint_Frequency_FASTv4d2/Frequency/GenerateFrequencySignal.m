clear;close all;

%% Generate frequency signal with noise

% params
Tmax = 40; % [s]
dt = 0.01; % [s]
f0 = 50; % [Hz] grid frequency

snr = 105; % [] signal to noise ratio

% params for delta torque calculation
H = 1; % [s] system equivalent inertia
S = 15e6; % [VA] rated power
T = -2 * S * H / f0; % 
Omega_rated = rpm2radPs(7.56); % [rpm]

% generate base signal
t = 0:dt:Tmax-dt;
f_constant = ones(1,length(t))*f0;

% add noise
% f = awgn(f_constant,snr); 
rng(1); % initialize random number generator to get the same results every time
P_signal = mean(f_constant.^2);
P_noise = P_signal / 10^(snr/10);
noise = sqrt(P_noise) * randn(size(t));
NoiseSmooth = movmean(noise,51);
f_noisy = f_constant + NoiseSmooth;
f = f_noisy;
% calculate RoCoF
RoCoF = diff(f)./dt;
% determine torque update
delta_P = T * RoCoF;
delta_MgFF = delta_P * (1/Omega_rated);

% plot overview
figure("Name","FrequencySignalGeneration")
subplot(411)
hold on; grid on; box on
plot(t,f);
ylabel("f [Hz]")

subplot(412)
hold on; grid on; box on
plot(t(1:end-1),RoCoF);
ylabel("RoCoF [Hz/s]")

subplot(413)
hold on; grid on; box on
plot(t(1:end-1),delta_MgFF/1e3);
ylabel("delta MgFF [MNm]")
xlabel("time [s]")

subplot(414)
hold on; grid on; box on
plot(t(1:end-1),delta_P/1e3);
ylabel("delta P [MW]")
xlabel("time [s]")

% print some statistics
fprintf("max f = %f\n", max(f));
fprintf("min f = %f\n", min(f));
fprintf("max RoCoF = %f\n", max(RoCoF));
fprintf("min RoCoF = %f\n", min(RoCoF));
fprintf("max delta P = %f\n", max(delta_P/1e3));
fprintf("min delta P = %f\n", min(delta_P/1e3));
fprintf("max delta MgFF = %f\n", max(delta_MgFF/1e3));
fprintf("min delta MgFF = %f\n", min(delta_MgFF/1e3));
%% write signal to csv file
% frequency
m = [t(1:end-1);f(1:end-1)]';
writematrix(m,"FrequencySignalTest.csv");
% delta torque
n = [t(1:end-1);delta_MgFF]';
% writematrix(n,"DeltaTorqueFFSignalTest.csv");

fid = fopen("DeltaTorqueFFSignalTest.csv",'w+');
fprintf(fid,['%f,%f\r\n'],[t(1:end-1)',delta_MgFF']');
fclose(fid);