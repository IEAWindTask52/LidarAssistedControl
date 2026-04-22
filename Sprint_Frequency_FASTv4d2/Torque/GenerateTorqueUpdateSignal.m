clear;
%% Generating test signal for torque update

t = 0:0.01:40;
v = ones(1,length(t))*1e3;

figure("Name","TorqueUpdateSignalGeneration")
plot(t,v);

% write to csv file
m = [t;v]';
writematrix(m,"TorqueTestConstant.csv");