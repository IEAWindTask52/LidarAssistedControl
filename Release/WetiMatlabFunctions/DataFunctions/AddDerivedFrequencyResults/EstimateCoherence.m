function [FrequencyResults] = EstimateCoherence(FrequencyResults,InputID_11,InputID_22,InputID_12,OutputID,options)
% Estimates squared coherence and MCB on frequency data.
% Function for DerivedFrequencyResults.

% inputs
arguments
    FrequencyResults                    struct  
    InputID_11                          char 
    InputID_22                          char
    InputID_12                          char
    OutputID                            char = 'Coherence'
    options.f_max                       (1,1)double = 0.09375 % Default f_max = 6/64: 6th frequency for 64 fft
    options.RelativeBandwidthFilterFlag logical = 0
    options.RelativeBandwidth           (1,1)double = 0.6     % Default set to 0.6 from experience  
end
nDataFiles = FrequencyResults.nDataFiles;

for iDataFile = 1:nDataFiles
    % load data
    S_11  = FrequencyResults.(InputID_11)(iDataFile).S;
    S_22  = FrequencyResults.(InputID_22)(iDataFile).S;
    S_12  = FrequencyResults.(InputID_12)(iDataFile).S;
    f     = FrequencyResults.(InputID_11)(iDataFile).f;

    % compute squared coherence
    C     = abs(S_12).^2./(S_11.*S_22);

%     figure
%     hold on; grid on; box on;
%     plot(f,C)
%     plot(f,C_f)
%     set(gca,'XScale','log')
%     legend('C','C_f')
%     xlabel('f [Hz]')
%     ylabel('coherence [-]')
    % check if coherence is decreasing monotonically
    considered      = f < options.f_max;
    f_considered    = f(considered);
    if options.RelativeBandwidthFilterFlag
        C_f             = RelativeBandwidthFilter(f,C,options.RelativeBandwidth);
        C_considered    = C_f(considered);
    else
        C_considered    = C(considered);
    end
    isMonotonic     = all(diff(C_considered) < 0);

    if isMonotonic
        flag = 0;
        % compute MCB
        f_MCB = interp1(C_considered,f_considered,0.5);
    else
        flag = 1;
        C_considered = cummin(C_considered);
        [C_considered,idx_C_unique] = unique(C_considered);
        f_considered_unique = f_considered(idx_C_unique);
        if length(C_considered)>1 && min(~isnan(C_considered))
            % compute MCB
            f_MCB = interp1(C_considered,f_considered_unique,0.5);
        else
            f_MCB = NaN;
        end
    end

    % compute flag
    if isnan(f_MCB)
        if min(C_considered)>0.5
            flag = 2;
        else
            flag = 3;
        end
    end
            

    % store results
    FrequencyResults.(OutputID)(iDataFile).f           = f;
    FrequencyResults.(OutputID)(iDataFile).C           = C;
    FrequencyResults.(OutputID)(iDataFile).f_MCB       = f_MCB;
    FrequencyResults.(OutputID)(iDataFile).MCB_flag    = flag;
    if options.RelativeBandwidthFilterFlag
        FrequencyResults.(OutputID)(iDataFile).C_f     = C_f;
    end
end

end

