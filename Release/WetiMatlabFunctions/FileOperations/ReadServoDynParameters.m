% Function: ReadServoDynParameters reads out ServoDyn Parameters need to
% run the DLL with SLOW
% -----------------------------
% Usage:
% ServoDyn  = ReadServoDynParameters(ServoDynFile)
% -----------------------------
% Input:
% ServoDynFile      String with name of ServoDyn file
% -----------------------------
% Output:
% ServoDyn          struct with values
% -----------------------------
% Created: 
% David Schlipf on 15-Jan-2023
% (c) WETI
% ----------------------------------
function ServoDyn  = ReadServoDynParameters(ServoDynFile)

ServoDyn.Ptch_Cntrl     = str2num(ReadFastParameters(ServoDynFile,'Ptch_Cntrl'));
ServoDyn.Ptch_Min    	= str2num(ReadFastParameters(ServoDynFile,'Ptch_Min'));
ServoDyn.Ptch_Max    	= str2num(ReadFastParameters(ServoDynFile,'Ptch_Max'));
ServoDyn.PtchRate_Min	= str2num(ReadFastParameters(ServoDynFile,'PtchRate_Min'));
ServoDyn.PtchRate_Max 	= str2num(ReadFastParameters(ServoDynFile,'PtchRate_Max'));
ServoDyn.GenSpd_Dem   	= str2num(ReadFastParameters(ServoDynFile,'GenSpd_Dem'));
ServoDyn.GenTrq_Dem   	= str2num(ReadFastParameters(ServoDynFile,'GenTrq_Dem'));

end