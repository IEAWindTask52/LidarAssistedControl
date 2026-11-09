function [S_f] = RelativeBandwidthFilter(f,S,RelativeBandwidth)
% function provided by D.Schlipf from his PhD Thesis
nFrequency  = length(f);
f_ll        = max(min(f),f-RelativeBandwidth*f);   % lower limit
f_ul        = min(max(f),f+RelativeBandwidth*f);   % upper limit
S_f         = NaN(nFrequency,1);

for iFrequency=1:nFrequency
    idx                 = f>=f_ll(iFrequency) & f<=f_ul(iFrequency);
    S_f(iFrequency)	    = mean(S(idx));   
end

end
