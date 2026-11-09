function Value = GetIC(SteadyStatesFile,Variable,URef)
IC      = load(SteadyStatesFile,'v_0','theta','Omega','x_T','M_g');
Value   = interp1(IC.v_0,IC.(Variable),URef,'linear','extrap');
end