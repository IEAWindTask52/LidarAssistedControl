function TimeResults = GetLineOfSightAllBeams(TimeResults,BeamIdChannel,VlosChannelCell,BeamIDs,options)
% Gets the line-of-sight wind speed from a single beam.
% Function for AddDerivedTimeResults.

% inputs
arguments
    TimeResults         cell
    BeamIdChannel       char    
    VlosChannelCell     (1,:) cell    
    BeamIDs             (1,:) {mustBeInteger}
    options.Display     char {mustBeMember(options.Display,{'off','on'})} = 'off'    
end

% get dimensions
nBeamIDs        = size(BeamIDs,2);
nVlosChannels   = size(VlosChannelCell,2);

% loop over all combinations
for iVlosChannel = 1:nVlosChannels
    VlosChannel     = VlosChannelCell{iVlosChannel};
    for iBeamID  = 1:nBeamIDs
        BeamID  = BeamIDs(iBeamID);
        NewChannel  = [VlosChannel,'_',num2str(BeamID)];
        TimeResults = GetLineOfSightPerBeam(TimeResults,BeamIdChannel,VlosChannel,BeamID,Display=options.Display);
    end
end

end