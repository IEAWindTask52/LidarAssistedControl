function Value = GetIC2(SteadyStatesFile,Omega_rated,R,Variable,VariationValues)

% internal variables
HWindSpeed  = VariationValues(1);
BlPitch     = VariationValues(2);

% load IC
IC          = load(SteadyStatesFile,'theta','lambda','x_T_IC','x_B_IC','y_T_IC','y_B_IC');

% current theta and lambda within the range, avoiding extrapolation
theta_IC    = min(max(deg2rad(BlPitch),         min(IC.theta )),max(IC.theta ));
lambda_IC   = min(max(Omega_rated*R./HWindSpeed,min(IC.lambda)),max(IC.lambda)); 

% interpolation
Value       = interp2(IC.theta,IC.lambda,IC.(Variable),theta_IC,lambda_IC);

% avoid NaN
if isnan(Value)
    Value   = 0;
    warning(['IC of ',Variable,' was NaN, was set to 0.'])
end
end