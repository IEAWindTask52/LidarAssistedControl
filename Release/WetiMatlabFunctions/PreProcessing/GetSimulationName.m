%% GetSimulationName
% Function: Provides a standard Simulation Name from Variation
%
%% Usage:
% 
% SimulationName = GetSimulationName(PreProcessingVariation,Variation)
% or 
% SimulationName = GetSimulationName(PreProcessingVariation,Variation,Format)
%
%% Input:
% 
% * PreProcessingVariation  - Cell with String of Variation in first column
% * Variation               - Vector with floats of current Variation
% * Format (optional)       - Cell with format strings
% 
%% Output:
% GetSimulationName         - String
% 
%% Created: 
% David Schlipf on 26-Apr-2017
% 
% (c) TTI GmbH

function SimulationName = GetSimulationName(PreProcessingVariation,VariationValues)

% inputs
arguments
    PreProcessingVariation  (:,3) cell
    VariationValues         double
end

% loop
[nVariation,~]      = size(PreProcessingVariation);
SimulationNameTemp  = [];
for iVariation = 1:nVariation
    SimulationNameTemp = [SimulationNameTemp,...
        PreProcessingVariation{iVariation,1},'_'];
    Format = PreProcessingVariation{iVariation,3};
    if ~isempty(Format)
        SimulationNameTemp = [SimulationNameTemp,num2str(VariationValues(iVariation),Format),'_'];
    else
        SimulationNameTemp = [SimulationNameTemp,num2str(VariationValues(iVariation)),'_'];
    end
end

% remove last '_'
SimulationNameTemp(end) = [];

% replace special characters
SimulationName          = regexprep(SimulationNameTemp, {'\.','\-','\+'}, {'d','m','p'});

end

