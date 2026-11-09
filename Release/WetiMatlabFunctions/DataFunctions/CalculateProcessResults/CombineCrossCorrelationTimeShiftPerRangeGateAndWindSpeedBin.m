function ProcessResults = CombineCrossCorrelationTimeShiftPerRangeGateAndWindSpeedBin(ProcessResults,ConsideredRangeGatesIndices,WindSpeedBins,InputID,OutputID)
% Combine Time shifts of several CrossCorrelations to one time shift matrix
% based on wind bins and lidar range gates.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults                  struct  
    ConsideredRangeGatesIndices     double 
    WindSpeedBins                   double 
    InputID                         string = "CrossCorrelationRL"
    OutputID                        char = 'TimeShiftPerRangeGateAndWindSpeedBin'
end

T_shift     = [];
for iRangeGate = ConsideredRangeGatesIndices
    GateID  = InputID+iRangeGate;
    T_shift = [T_shift cat(1,ProcessResults.(GateID).T_shift)]; % to avoid complexity by preallocating 
end

ProcessResults.(OutputID).T_shift                       = T_shift;
ProcessResults.(OutputID).ConsideredRangeGatesIndices   = ConsideredRangeGatesIndices;
ProcessResults.(OutputID).WindSpeedBins                 = WindSpeedBins;
end