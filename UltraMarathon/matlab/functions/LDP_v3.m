function REWS_b  = LDP_v3 (isValid,beamID,lineOfSightWindSpeed,DT,LDP)
% Function to postprocess lidar data to get the rotor-effective wind speed
% (REWS) equal to the LDP_v1/FFP_v1 without the need of compiling a DLL. 
% Code is intended to be as close as possible to the Fortran Code.
% v3: similar to v1, but includes ignoring of signals with invalid data.
% 2026 version to be applied in each time step.

% persistent variables
persistent PreviousBeamID REWS                  
if isempty(PreviousBeamID)      
    PreviousBeamID  = -1;  % force WFR on first call
end
if isempty(REWS)
    REWS            = 18; % only necessary if first lineOfSightWindSpeed is not valid
end

% if there is a new measurement perform wind field reconstruction    
if beamID ~= PreviousBeamID  
    REWS_new        = WindFieldReconstruction(lineOfSightWindSpeed,isValid,LDP.NumberOfBeams,LDP.AngleToCenterline);
    if ~isnan(REWS_new) % update only if not NaN
        REWS        = REWS_new;
    end
    PreviousBeamID  = beamID; % update beamID
end

% Low pass filter the REWS
if LDP.FlagLPF
	REWS_f     	    = LPFilter(REWS,DT,LDP.omega_cutoff);
else
	REWS_f      	= REWS;
end

% Get buffered and filtered REWS from buffer
REWS_b              = Buffer(REWS_f,DT,LDP.T_buffer);

end


function REWS = WindFieldReconstruction(v_los,isValid,NumberOfBeams,AngleToCenterline)
% Matlab version of the subroutine WindFieldReconstruction in LDP_v1_Subs.f90
% extended to deal with invalid data. 

% init u_est_Buffer
persistent u_est_Buffer;
if isempty(u_est_Buffer)      
    u_est_Buffer = NaN(NumberOfBeams,1);  
end 

% Estimate u component assuming perfect alignment
if isValid
    u_est       = v_los/cosd(AngleToCenterline);
else
    u_est       = NaN;
end

% Update Buffer for estimated u component
u_est_Buffer    = [u_est;u_est_Buffer(1:NumberOfBeams-1)];

% Calculate REWS from mean over all estimated u components
REWS  	        = mean(u_est_Buffer,'omitnan');

end

function OutputSignal = LPFilter(InputSignal,DT,CornerFreq)
% Matlab version of the function LPFilter in FFP_v1_Subs.f90

% Initialization
persistent OutputSignalLast InputSignalLast;
if isempty(OutputSignalLast)      
    OutputSignalLast    = InputSignal;  
    InputSignalLast     = InputSignal;
end 

% Define coefficients 
a1          = 2 + CornerFreq*DT;
a0          = CornerFreq*DT - 2;
b1          = CornerFreq*DT;
b0          = CornerFreq*DT;

% Filter
OutputSignal = 1.0/a1 * (-a0*OutputSignalLast + b1*InputSignal + b0*InputSignalLast);

% Save signals for next time step
InputSignalLast     = InputSignal;
OutputSignalLast    = OutputSignal;
end

function REWS_b = Buffer(REWS,DT,T_buffer)

% init REWS_f_Buffer
nBuffer = 400; % Size of REWS_f_buffer, max 20 seconds at 20 Hz  [-] 
persistent REWS_f_Buffer;
if isempty(REWS_f_Buffer)      
    REWS_f_Buffer = ones(nBuffer,1)*REWS;   
end 

% Update Buffer for estimated u component
REWS_f_Buffer    = [REWS;REWS_f_Buffer(1:nBuffer-1)];

% Index for entry at T_buffer, minimum 1, maximum nBuffer
Idx     = min( max(floor(T_buffer/DT),1) , nBuffer);
		
% Get buffered and filtered REWS from buffer
REWS_b  = REWS_f_Buffer(Idx);

end