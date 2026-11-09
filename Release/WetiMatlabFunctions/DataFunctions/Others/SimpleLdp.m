function v_0L = SimpleLdp(Time,v_LOS,BeamID,InitialValue,numberOfBeams,toCenterlineAngle,options)
% Applies a simple LDP (similar to Molas) without Blade Ghost avoidance.
% Intented to reproduce Molas results.
% Called by ApplySimpleLdpPerBeam. Needs to be cleared if necessary.

% inputs
arguments
    Time                        (:,1) double
    v_LOS                       (:,1) double
    BeamID                      (:,1) double
    InitialValue                (:,1) double
    numberOfBeams               (1,1) double
    toCenterlineAngle           (1,1) double
    options.ErrorCodeIn         (1,1) {mustBeNumeric}               = 0
    options.ErrorCodeOut        (1,1) {mustBeNumeric}               = 0
end

persistent PreviousBeamID v_0L_i
if isempty(PreviousBeamID)   
    PreviousBeamID = -1;    % force WFR on first call
end
if isempty(v_0L_i)   
    v_0L_i = InitialValue;  % should not happen
end

% loop over time
n_t         = length(Time);
v_0L        = NaN(n_t,1);
for i_t = 1:n_t
    % If there is a new measurement perform wind field reconstruction    
    if abs(BeamID(i_t)-PreviousBeamID)>0.5
        v_LOS_i         = v_LOS(i_t);
        if v_LOS_i == options.ErrorCodeIn 
            v_LOS_i     = NaN;
        end
        v_0L_i          = SimpleLDP(v_LOS_i,InitialValue,toCenterlineAngle,numberOfBeams);
        if isnan(v_0L_i)
            v_0L_i      = options.ErrorCodeOut;
        end            
    end
    v_0L(i_t)           = v_0L_i; 
    PreviousBeamID      = BeamID(i_t);
end
 

function v_0L = SimpleLDP(v_LOS_i,InitialValue,toCenterlineAngle,numberOfBeams)
% based on "Perfect Alignment Single-Distance WFR Simulation"
% main difference is the buffer, which is not using BeamID

% init u_i_est
persistent u_i_est;
if isempty(u_i_est)     
    u_i_est = ones(1,numberOfBeams)*InitialValue;
end 

% Estimate u component assuming perfect alignment
u_est 		    = v_LOS_i/cos(toCenterlineAngle);

% Update Buffer for estimated u component
u_i_est         = [u_est,u_i_est(1:numberOfBeams-1)];

% REWS is mean of all longitudinal wind speeds without NaN
v_0L  	        = mean(u_i_est,'omitnan');

end


end