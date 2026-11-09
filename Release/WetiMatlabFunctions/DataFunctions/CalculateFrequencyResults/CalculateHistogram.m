function Output = CalculateHistogram(Data,Time,Edges)
% Calculates Histogram-
% Function for CalculateFrequencyResults.

% inputs
arguments
    Data                (:,1) double
    Time                (:,1) double
    Edges               (1,:) double
end

Counts              = histcounts(Data,Edges);
Centers             = (Edges(1:end-1)+Edges(2:end))/2;

% output struct
Output.Edges        = Edges;
Output.Centers      = Centers;
Output.Counts       = Counts;
end