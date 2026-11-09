function TimeResults = AddREWSfromWindFieldToTimeResults(TimeResults,TurbSimResultFiles,R,tau,options)
% Adds REWS from a Turbsim wind field.
% 
% Function for AddDerivedTimeResults.

% inputs
arguments
    TimeResults                     cell  
    TurbSimResultFiles              cell
    R                       (1,1)   double      % [m]   rotor radius
    tau                     (1,1)   double = 0  % [s]   offset
    options.nCore           {mustBeInteger} = maxNumCompThreads % set option to 0 for no parallel processing
    options.Display         char {mustBeMember(options.Display,{'off','on'})} = 'off'
end

% get dimensions
nTurbSimResultFile  = size(TurbSimResultFiles,1);

% loop over data files
parfor (iTurbSimResultFile = 1:nTurbSimResultFile,options.nCore) 

    % display if requested
    if strcmp(options.Display,'on')                
        MyText      = "Adding REWS from TurbSim wind field: " + iTurbSimResultFile + "/" + nTurbSimResultFile + "\n";
        fprintf(MyText);
    end
    
    % calculate REWS
    ThisTurbSimResultFile   = TurbSimResultFiles{iTurbSimResultFile};
    [REWS_WindField,Time_WindField]  	= CalculateREWSfromWindField(ThisTurbSimResultFile,R,2);   

    % create time series
    TS                  = timeseries('REWS_WindField');
    TS.Time             = TimeResults{iTurbSimResultFile}.time;
    TS.Data             = interp1(Time_WindField-tau,REWS_WindField,TS.Time);
    TS.DataInfo.Units   = 'm/s';

    % add channels
    TimeResults{iTurbSimResultFile} = TimeResults{iTurbSimResultFile}.addts(TS);

end

end