function ProcessResults = EstimateRotationalSpectraLAC(ProcessResults,Statistics,SpectraModelID,RotationalSpectraID,FilterURef,OutputID,options)
% Estimates averaged autospectra for specific filter.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct  % set to struct() to initialize
    Statistics                  table
    SpectraModelID              char
    RotationalSpectraID         char 
    FilterURef                  function_handle
    OutputID                    char    = 'EstimatedRotationalSpectraLAC'
    options.IndexBaseline       (1,1) double = 1 % assuming Basline is the first struct in RotationalSpectraID 
end

% get mean wind speed
URef    = mean(FilterURef(ProcessResults,Statistics));

if ~isnan(URef)   

    % LAC spectra
    C_RL    = ProcessResults.(SpectraModelID).C_RL;
    f       = ProcessResults.(SpectraModelID).k*URef/2/pi;
    f_BL    = ProcessResults.(RotationalSpectraID)(options.IndexBaseline).f;
    S_BL    = ProcessResults.(RotationalSpectraID)(options.IndexBaseline).S;   
    S_LAC   = S_BL.*(1-interp1(f,C_RL,f_BL,'linear',NaN));
    
    % Output
    ProcessResults.(OutputID).f     = f_BL;
    ProcessResults.(OutputID).S     = S_LAC;             
    ProcessResults.(OutputID).URef  = URef;

else
    % Output
    ProcessResults.(OutputID).f     = NaN;
    ProcessResults.(OutputID).S     = NaN;             
    ProcessResults.(OutputID).URef  = NaN;

end
