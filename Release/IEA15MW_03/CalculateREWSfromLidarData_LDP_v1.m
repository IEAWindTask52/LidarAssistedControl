function R_FBFF  = CalculateREWSfromLidarData_LDP_v1(FBFF,DT,TMax,LDP)
% Function to postprocess lidar data to get the rotor-effective wind speed
% (REWS) equal to the LDP_v1/FFP_v1 without the need of compiling a DLL. 
% Code is intented to be as close as possble to the Fortran Code. 

% time
R_FBFF.Time = [0:DT:TMax]';
n_t         = length(R_FBFF.Time);

% allocation
R_FBFF.REWS     = NaN(n_t,1);
R_FBFF.REWS_f   = NaN(n_t,1);
R_FBFF.REWS_b   = NaN(n_t,1);

% internal variables
ThisChannel = "VLOS"+num2str(LDP.IndexGate,'%02d')+"LI";

% loop over time
for i_t = 1:n_t
    Idx     = max(i_t-1,1); % due to Co-simulation of OpenFAST and the controller DLLs

    % If there is a new measurement perform wind field reconstruction    
    if FBFF.NEWDATALI(Idx)  
        v_los           = FBFF.(ThisChannel)(Idx);
        REWS            = WindFieldReconstruction(v_los,LDP.NumberOfBeams,LDP.AngleToCenterline);
    end
   
    % Low pass filter the REWS
	if LDP.FlagLPF
		REWS_f      	= LPFilter(REWS,DT,LDP.f_cutoff);
    else
		REWS_f      	= REWS;
    end

    % Get buffered and filtered REWS from buffer
    REWS_b              = Buffer(REWS_f,DT,LDP.T_buffer);

    % Store in structure
    R_FBFF.REWS(i_t)    = REWS;
    R_FBFF.REWS_f(i_t)  = REWS_f;
    R_FBFF.REWS_b(i_t)  = REWS_b;
end

end


function REWS = WindFieldReconstruction(v_los,NumberOfBeams,AngleToCenterline)
% matlab version of the subroutine WindFieldReconstruction in LDP_v1_Subs.f90

% init u_est_Buffer
persistent u_est_Buffer;
if isempty(u_est_Buffer)      
    u_est_Buffer = NaN(NumberOfBeams,1);   
end 

% Estimate u component assuming perfect alignment
u_est 		    = v_los/cosd(AngleToCenterline);

% Update Buffer for estimated u component
u_est_Buffer    = [u_est;u_est_Buffer(1:NumberOfBeams-1)];

% Calculate REWS from mean over all estimated u components
REWS  	        = mean(u_est_Buffer,'omitnan');

end

function OutputSignal = LPFilter(InputSignal,DT,CornerFreq)
% matlab version of the function LPFilter in FFP_v1_Subs.f90

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
nBuffer = 2000; % Size of REWS_f_buffer, 25 seconds at 80 Hz  [-] 
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