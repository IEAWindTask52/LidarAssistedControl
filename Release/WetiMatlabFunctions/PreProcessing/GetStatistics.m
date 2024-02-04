function Value = GetStatistics(StatisticsFile,Variable,URef)
load(StatisticsFile,'Statistics');
URef_vec    = Statistics.mean_Wind1VelX;
Value       = interp1(URef_vec,Statistics.(Variable),URef,'linear','extrap');
end