function Cost = CalculateCost(ProcessResults_LAC,ProcessResults_FB)

LTE_Tower   = (ProcessResults_LAC.LTW_DEL_4_TwrBsMyt /ProcessResults_FB.LTW_DEL_4_TwrBsMyt )^(-4);
LTE_Blades  = (ProcessResults_LAC.LTW_DEL_10_RootMyb1/ProcessResults_FB.LTW_DEL_10_RootMyb1)^(-10);
LTE         = min(LTE_Tower,LTE_Blades);
AEP_FB      = ProcessResults_FB.LTW_mean_GenPwr*8760;
AEP_LAC     = ProcessResults_LAC.LTW_mean_GenPwr*8760;

Cost        = LTE*AEP_LAC/AEP_FB;

end