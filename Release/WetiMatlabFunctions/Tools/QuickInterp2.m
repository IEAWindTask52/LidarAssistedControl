% Function: Quick 2D-Interpolation.
% based on interp2 from FloatingWindTurbine_CP.cpp
% see http://en.wikipedia.org/wiki/Bilinear_interpolation
% Faster version from SummerGames 2026 and no extrapolation
function ZI = QuickInterp2(X,Y,Z,XI,YI)
%#codegen   

nX          = length(X);
nY          = length(Y);
X1v         = X(1);
XEnd        = X(nX);
Y1v         = Y(1);
YEnd        = Y(nY);

% keep XI and YI within the limits (X and Y are sorted ascending, so the
% end points are the extrema - no need to scan the arrays)
XIc         = min(XEnd,XI);
XIc         = max(X1v,XIc);
YIc         = min(YEnd,YI);
YIc         = max(Y1v,YIc);

% Find X and Y intervals. theta is on a uniform grid and lambda is uniform in
% 1/lambda (the tables were built from a uniform wind speed grid), so the
% interval follows from arithmetic instead of a search. 
IndexX      = floor((XIc-X1v)/((XEnd-X1v)/(nX-1))) + 1;
IndexX      = max(IndexX,1);
IndexX      = min(IndexX,nX-1);

U1          = 1/Y1v;
IndexY      = floor((1/YIc-U1)/((1/YEnd-U1)/(nY-1))) + 1;
IndexY      = max(IndexY,1);
IndexY      = min(IndexY,nY-1);

X1          = X(IndexX  );
X2          = X(IndexX+1);
Y1          = Y(IndexY  );
Y2          = Y(IndexY+1);

% weights
wX1         = XIc-X1;
wX2         = X2-XIc;
wY1         = YIc-Y1;
wY2         = Y2-YIc;

% Interpolation
ZI          =   (Z(IndexY  ,IndexX  )*wX2*wY2...
                +Z(IndexY  ,IndexX+1)*wX1*wY2...
                +Z(IndexY+1,IndexX  )*wX2*wY1...
                +Z(IndexY+1,IndexX+1)*wX1*wY1)/(X2-X1)/(Y2-Y1);
end
        
