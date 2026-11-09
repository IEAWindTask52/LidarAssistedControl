function Statistics = CorrectResetIssue(Statistics,Limit,ErrorCode)
% Corrects the "Reset Issue":
% In March 2025, the reset in the LDP was sometimes activated when it was
% not perfectly 0. This has been corrected. But these files cannot be used.
% Function for AddDerivedStatistics.

% inputs
arguments
    Statistics          table
    Limit       (1,1)   double
    ErrorCode   (1,1)   double
end

% find bad data
BadData = Statistics.maxAbsDiff_CAN_REWS_filter_buffer>Limit;

% set LAC state to 0
Statistics.mean_CAN_LAC_State(BadData) = ErrorCode;

% remove maxAbsDiff_CAN_REWS_filter_buffer such that it can be combined
% with other months
Statistics.maxAbsDiff_CAN_REWS_filter_buffer = [];

end