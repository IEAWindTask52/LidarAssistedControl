function [theta,lambda,c_P,c_T] = GetPowerAndThrustCoefficients(RotorPerformanceFile)

fid = fopen(RotorPerformanceFile);
for iLinesSkipped = 1:4
    fgetl(fid); % skip line
end
CurrentLine     = fgetl(fid);
theta           = deg2rad(cell2mat(textscan(CurrentLine,'%f')));
fgetl(fid); % skip line
CurrentLine     = fgetl(fid);
lambda          = cell2mat(textscan(CurrentLine,'%f'))';
for iLinesSkipped = 1:5
    fgetl(fid); % skip line
end
n_theta         = length(theta);
n_lambda        = length(lambda);
c_P = NaN(n_lambda,n_theta);
% read in power coefficient
for i_lambda = 1:n_lambda
    CurrentLine     = fgetl(fid);
    c_P(i_lambda,:)   = cell2mat(textscan(CurrentLine,'%f'));
end
for iLinesSkipped = 1:4
    fgetl(fid); % skip line
end
% read in thrust coefficient
c_T = NaN(n_lambda,n_theta);
for i_lambda = 1:n_lambda
    CurrentLine     = fgetl(fid);
    c_T(i_lambda,:)   = cell2mat(textscan(CurrentLine,'%f'))';
end
fclose(fid);

end

