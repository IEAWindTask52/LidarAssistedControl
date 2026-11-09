function ProcessResults = LoadAnalyticSpectraModel(ProcessResults,ResultFiles,OutputID,options) %,FilterTransferFunction
% Loads analytic spectra from file and calculates coherence.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct  % set to struct() to initialize
    ResultFiles                 cell
    OutputID                    char = 'AnalyticSpectraModel'
    options.f_max               double = []
%     FilterTransferFunction      double  = []
end

% TODO: restore FilterTransferFunction stuff, if really needed or remove it

nResultFiles = length(ResultFiles);

for iResultFile = 1:nResultFiles
    
    % load data
    load(ResultFiles{iResultFile}','S_LL','S_RL','S_RR','k','f');

    % reduce size
    if ~isempty(options.f_max)
        f       = f(f<=options.f_max);
        k       = k(f<=options.f_max);
        S_LL    = S_LL(f<=options.f_max);
        S_RL    = S_RL(f<=options.f_max);
        S_RR    = S_RR(f<=options.f_max);
    end

    % calculate coherence, transfer function and filtered lidar spectrum
    C_RL        = abs(S_RL).^2./S_RR./S_LL;
    G_RL        = S_RL./S_LL;

    % store in ProcessResults
    ProcessResults.(OutputID)(iResultFile).f            = f;
    ProcessResults.(OutputID)(iResultFile).k            = k;
    ProcessResults.(OutputID)(iResultFile).S_RR         = S_RR;
    ProcessResults.(OutputID)(iResultFile).S_LL         = S_LL;
    ProcessResults.(OutputID)(iResultFile).S_RL         = S_RL;
    ProcessResults.(OutputID)(iResultFile).C_RL         = C_RL;
    ProcessResults.(OutputID)(iResultFile).G_RL         = G_RL;
    ProcessResults.(OutputID)(iResultFile).G_RL_mag     = abs(G_RL);
end


% -------------------------------------------------------------------------
% FilterTransferFunction stuff: old version without multiple ResultFiles
% -------------------------------------------------------------------------
% perfectly filtered
% S_LL_pf     = S_LL.*abs(G_RL).^2;

% % filtered
% if ~isempty(FilterTransferFunction) 
%     G_f     = FilterTransferFunction;
%     if size(G_f)~=size(S_LL)
%         error('Length of FilterTransferFunction does not fit to the spectrum from ResultFile.')
%     end    
%     S_LL_f  = S_LL.*abs(G_f).^2;
% else    
%     S_LL_f  = S_LL_pf;
% end
% ProcessResults.(OutputID).fS_RR     = f.*S_RR;
% ProcessResults.(OutputID).fS_LL     = f.*S_LL;
% ProcessResults.(OutputID).S_LL_pf   = S_LL_pf;
% ProcessResults.(OutputID).fS_LL_pf  = f.*S_LL_pf;
% ProcessResults.(OutputID).S_LL_f    = S_LL_f;
% ProcessResults.(OutputID).fS_LL_f   = f.*S_LL_f;


end
