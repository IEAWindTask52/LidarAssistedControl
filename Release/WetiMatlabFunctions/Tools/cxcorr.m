function [c,lags]    = cxcorr(a,b,scaleopt)
% DS on 06-Jan-2024
% based on https://de.mathworks.com/matlabcentral/answers/94598-is-there-a-function-that-calculates-circular-cross-correlation-of-sequences
% More details (but much slower): https://de.mathworks.com/matlabcentral/fileexchange/4810-circular-cross-correlation

% inputs
arguments
    a                   (:,1) double
    b                   (:,1) double
    scaleopt            char {mustBeMember(scaleopt,{'none','normalized','coeff'})}  = 'normalized'
end

% zero padding
[a,b]       = ZeroPadding(a,b);

% one-sided circular cross correlation
cc_OneSided = ifft(fft(a).*conj(fft(b)));

% scaling is necessary
switch scaleopt % see doc xcorr->scaleopt
    case {'normalized','coeff'}
        cc_OneSided = cc_OneSided/norm(a)/norm(b); % norm is equal autocorrelation at time zero
    otherwise
        error('Scaling option %s not implemented for circular cross correlation',scaleopt)
end

% two-sided circular cross correlation
c           = [(cc_OneSided(2:end));cc_OneSided];

% lags
n           = length(a); % same as b
lags        = [-n+1:n-1];

end


function [a,b]=ZeroPadding(a,b)
% adds zeros to the shorter signal
na = length(a);
nb = length(b);
if nb<na        % b is shorter
    b = [b,zeros(1,na-nb)];    
elseif na<nb    % a is shorter
    a = [a,zeros(1,nb-na)];
end
end