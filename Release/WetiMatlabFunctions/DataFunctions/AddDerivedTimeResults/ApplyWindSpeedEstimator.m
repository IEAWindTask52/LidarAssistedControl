function [TimeResults] = ApplyWindSpeedEstimator(TimeResults,Parameter,InputIDs,InputScaling,v_0_est_0,options)
% Applies the I&I wind speed estimator.
% Function for AddDerivedTimeResults.
% inputs
arguments
    TimeResults                 cell
    Parameter                   struct
    InputIDs                    (3,1) cell              % channel IDs with (Omega_g,theta,M_g) signals
    InputScaling                (3,1) double = [1 1 1]  % scaling for (Omega_g,theta,M_g) signals
    v_0_est_0                   (1,1) double = 4        % [m/s] initial value
    options.Solver              char {mustBeMember(options.Solver,{'ODE1','ODE4'})} = 'ODE1'
    options.Mode                char {mustBeMember(options.Mode,{'Matlab','Mex'})} = 'Mex'
    options.n_MEX               (1,1) double = 12000    % assuming 20 Hz and 600 s
    options.warning             (1,1) {mustBeNumericOrLogical}      = false
    options.ErrorCodeOut        (1,1) {mustBeNumeric}               = -999
    options.Display             char {mustBeMember(options.Display,{'off','on'})} = 'off'
    options.AirDensityMode      char {mustBeMember(options.AirDensityMode,{'Parameter','MeanFromChannel'})} = 'Parameter'
    options.AirDensityChannel   char 
    options.AddOmega_est        (1,1) {mustBeNumericOrLogical}      = false
    options.DeltaOmega_max      (1,1) {mustBeNumeric}               = 0.1047 % [rad/s] maximum deviation of rotor speed to reset Omega_est_0_last (= 1 rpm)
end

% get dimensions
nDataFiles           = size(TimeResults,1);

% init display text
MyText              = "";

% start initial values 
Omega_g_0           = InputScaling(1)*TimeResults{1}.(InputIDs{1}).Data(1);
Omega_est_0_last    = Omega_g_0/Parameter.Turbine.r_GB;
v_0_est_0_last      = v_0_est_0;

% loop over files
for iDataFile = 1:nDataFiles % parfor not possible because of initial value  

    % display if requested
    if strcmp(options.Display,'on')                
        fprintf(repmat('\b',1,strlength(MyText))); % remove previous text
        MyText      = "Estimating REWS: " + iDataFile + "/" + nDataFiles;
        fprintf(MyText);
        if iDataFile==nDataFiles % new line
            fprintf('\n');
        end
    end    
    
    % get data
    Omega_g     = InputScaling(1)*TimeResults{iDataFile}.(InputIDs{1}).Data;
    theta       = InputScaling(2)*TimeResults{iDataFile}.(InputIDs{2}).Data;    
    M_g         = InputScaling(3)*TimeResults{iDataFile}.(InputIDs{3}).Data;
    Time        = TimeResults{iDataFile}.Time;

    % reset Omega_est_0_last if necessary (e.g. after data gap)
    Omega_0     = Omega_g(1)/Parameter.Turbine.r_GB;
    if abs(Omega_0-Omega_est_0_last) > options.DeltaOmega_max
        Omega_est_0_last =  Omega_0;
    end

    % apply
    n               = length(Time);
    dt              = Time(2)-Time(1);
    
    % air density
    switch options.AirDensityMode
        case 'MeanFromChannel'
            Parameter.General.rho = mean(TimeResults{iDataFile}.(options.AirDensityChannel));
    end

    % apply Mex or Matlab
    switch options.Mode
        case 'Matlab'
            [Omega_est,v_0_est] = WindSpeedEstimator(n,dt,options.Solver,Omega_est_0_last,v_0_est_0_last,Omega_g,theta,M_g,Parameter);
        case 'Mex'
            if n==options.n_MEX
                [Omega_est,v_0_est] = WindSpeedEstimator_mex(n,dt,options.Solver,Omega_est_0_last,v_0_est_0_last,Omega_g,theta,M_g,Parameter);
            else
                [Omega_est,v_0_est] = WindSpeedEstimator(n,dt,options.Solver,Omega_est_0_last,v_0_est_0_last,Omega_g,theta,M_g,Parameter);
                if options.warning
                    warning(['Wind Speed Estimator: Mex mode requested, but Matlab mode used: ',num2str(n),' points in data, but ',num2str(options.n_MEX),' points needed.'])
                end
            end            
    end

    % update initial values 
    Omega_est_0_last    = Omega_est(end);
    v_0_est_0_last      = v_0_est(end);     

    % add new channel 
    NewTimeSeries           = timeseries(v_0_est,Time,'Name','v_0_est');
    NewTimeSeries.DataInfo.Units    = 'm/s';
    NewTimeSeries.TimeInfo  = TimeResults{iDataFile}.TimeInfo; % inherit Time Info
    TimeResults{iDataFile}  = TimeResults{iDataFile}.addts(NewTimeSeries);

    if options.AddOmega_est == true
        NewTimeSeries           = timeseries(Omega_est,Time,'Name','Omega_est_0');
        NewTimeSeries.DataInfo.Units    = 'rad/s';
        NewTimeSeries.TimeInfo  = TimeResults{iDataFile}.TimeInfo; % inherit Time Info
        TimeResults{iDataFile}  = TimeResults{iDataFile}.addts(NewTimeSeries);
    end

end

end