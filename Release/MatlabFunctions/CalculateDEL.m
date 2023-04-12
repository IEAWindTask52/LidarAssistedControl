function DEL = CalculateDEL(TimeSeries,dt,N_REF,WoehlerExponent)

% TimeSeries    can be a matrix, time by seed
% N_REF         fraction of 2e6 in 20 years for the chosen time length
% WoehlerExponent Woehler coefficient for the material

c               = rainflow(TimeSeries,1/dt);   %the default matlab code
cyclesAmplitude = c(:,2);
cyclesNumber    = c(:,1);
DEL             = (sum(cyclesAmplitude.^WoehlerExponent.*cyclesNumber)/N_REF).^(1/WoehlerExponent);

end