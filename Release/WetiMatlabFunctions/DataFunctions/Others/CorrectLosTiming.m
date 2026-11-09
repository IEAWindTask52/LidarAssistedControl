function Datac = CorrectLosTiming(Time,LOS,Data,options)
% Corrects lidar signal from NL200: at some times the step in the signals
% seems to be earlier or later than the change in the LOS signal. 
% Here, only +/- 1 and +/-2 steps is corrected.
% Since Data is typically 20Hz and lidar measures with 4Hz, there should be
% a change in LOS and Data every 5th data point.
% More can be implemented if warning ever comes up. 
% 
% Called by ApplyCorrectLosTiming.


% inputs
arguments
    Time                (:,1) double
    LOS                 (:,1) double
    Data                (:,1) double
    options.Tolerance   double = 0.005 % RWS only has two digits
end

% difference in signals
dLOS            = [0;abs(diff(LOS))>options.Tolerance];    
dData           = [0;abs(diff(Data))>options.Tolerance];
IsAsynchronous  = dData & ~dLOS; % An issue exists, if there is a step in Data, but not in LOS.
Datac           = Data;

% correct data if necessary
if sum(IsAsynchronous)
    dLOSd1          = [0;dLOS(1:end-1)];                    % shift dLOS down 1
    dLOSu1          = [dLOS(2:end);0];                      % shift dLOS up 1
    
    % find issues (only one issue can be true)
    IDXd1           = find(dData & dLOSd1);                 % Data is 1 step too late,  if there is a step in Data and in the +1 step of LOS
    IDXu1           = find(dData & dLOSu1 & ~dLOSd1);       % Data is 1 step too early, if there is a step in Data and in the -1 step of LOS
    
    % correct issues    
    Datac(IDXd1-1)  = Data(IDXd1);                          % if 1 step too late,  take value from this step for -1 step
    Datac(IDXu1)    = Data(IDXu1-1);                        % if 1 step too early, take value from -1 step for this step
    
    % check if there is still an issues
    dDatac          = [0;abs(diff(Datac))>options.Tolerance];
    IsAsynchronous  = dDatac & ~dLOS;
    if sum(IsAsynchronous) % An issue exists, if there is a step in Data, but not in LOS. 
    
        % difference in signals            
        dLOSd2          = [0;0;dLOS(1:end-2)];              % shift dLOS down 2
        dLOSu2          = [dLOS(3:end);0;0];                % shift dLOS up 2
        
        % find issues (only one issue can be true)
        IDXd2           = find(dDatac & dLOSd2);            % Data is 2 steps too late,  if there is a step in Data and in the +2 step of LOS
        IDXu2           = find(dDatac & dLOSu2 & ~dLOSd2);  % Data is 2 steps too early, if there is a step in Data and in the -2 step of LOS

        % correct issues
        Datac(IDXd2-1)  = Data(IDXd2);                      % if 2 steps too late,  take value from this step for -1 step
        Datac(IDXd2-2)  = Data(IDXd2);                      % if 2 steps too late,  take value from this step for -2 step
        Datac(IDXu2)    = Data(IDXu2-1);                    % if 2 steps too early, take value from -1 step for this step
        Datac(IDXu2+1)  = Data(IDXu2-1);                    % if 2 steps too early, take value from -1 step for next step    
     
        % check if there is still an issues
        dDatac          = [0;abs(diff(Datac))>options.Tolerance];
        IsAsynchronous  = dDatac & ~dLOS;
        % Identify freeze-thaw events: dData=1 but no dLOS nearby (within 2 steps)
        dLOSu1          = [dLOS(2:end);0];
        dLOSd1          = [0;dLOS(1:end-1)];
        dLOSu2          = [dLOS(3:end);0;0];
        dLOSd2          = [0;0;dLOS(1:end-2)];
        IsNearLOS       = dLOSu1 | dLOSd1 | dLOSu2 | dLOSd2;  % LOS step anywhere nearby
        IsFreeze        = IsAsynchronous & ~IsNearLOS;          % Data step but NO nearby LOS step
        if IsAsynchronous(2) % first data point is too late
            Datac(1)    = Datac(2);
        elseif IsAsynchronous(3) % first 2 data points are too late
            Datac(1:2)  = Datac(3);
        elseif IsAsynchronous(end) % last data point is too early
            Datac(end)  = Datac(end-1);
        elseif IsAsynchronous(end-1) % last 2 data points are too early
            Datac(end-1:end)  = Datac(end-2);  
        % only warn if it is not a freeze-thaw event
        elseif sum(IsAsynchronous(4:end-3) & ~IsFreeze(4:end-3)) % something else, e.g. 3 steps too late/early
            warning('Lidar signal not in line with LOS.')
        end
    end
end


end
