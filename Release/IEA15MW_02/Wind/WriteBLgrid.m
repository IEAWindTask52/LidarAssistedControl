
% TurbSim (c) NREL
% source: https://wind.nrel.gov/forum/wind/viewtopic.php?t=269 by Stefan.Kapp 
function WriteBLgrid(FileName, velocity, dy, dz, dt, zOffset, z0, SummVars)


% writeBLgrid('testWind', velocity, dy, dz, dt, zOffset, z0, SummVars)
%
% writes wind velocity data to binary .wnd files
%

% inputs - FileName: Name of output .wnd-file (extension will be added)
%        - velocity: 4D-array: (time, 3D-windcomp, y, z)
%        - dz
%        - dy
%        - dt
%        - SummVars: 6 variables from the summary file {zHub, Clockwise, UBAR, TI_u, TI_v, TI_w}
%                                                         90          1    12    12   9.6     6
%        - zOffset: (=90) Reference height (m) = Z(1) + GridHeight / 2.0
%        - z0 = .03;      Roughness length (m)

%-----------------------------------------
% INITIALIZE VARIABLES
%-----------------------------------------
fileFmt  = 'int16';

fc = 4;                 % should be 4 to allow turbulence intensity to be stored in the header
lat = 0;                % latitude (deg)

zHub = SummVars(1);     % hub height [m]
MFFWS = SummVars(3);    % mean full-field wind speed
TI_U = SummVars(4);     % Turbulence Intensity of u component (%)
TI_V = SummVars(5);     % Turbulence Intensity of v component (%)
TI_W = SummVars(6);     % Turbulence Intensity of w component (%)

[nt, nffc, ny nz] = size(velocity); % determin dimensions of windfield, e.g. [1286,3,23,23]

z1 = zHub - dz*(nz-1)/2;  %this is the bottom of the grid

dx = dt*MFFWS;          % delta x in m           
nt_header = floor(nt/2);% half the number of time steps


%-----------------------------------------
% OPEN FILE
%-----------------------------------------
len    = length(FileName);
ending = FileName(len-3:len);

if strcmpi( ending, '.wnd' )
    FileName = FileName(1:len-4);
end
fid_wnd   = fopen( [ FileName '.wnd' ], 'w' ); % Open file, or create new file, for writing; discard existing contents, if any.
if ( fid_wnd <= 0 )
   error( 'Wind file could not be opened.' );
   return;
end


%-----------------------------------------
%WRITE THE HEADER OF THE BINARY FILE
%-----------------------------------------
% THE NEWER-STYLE AERODYN WIND FILE
    fwrite( fid_wnd, -99, 'int16' );             % number of components
    fwrite( fid_wnd, fc, 'int16' );              % should be 4 to allow turbulence intensity to be stored in the header
    fwrite( fid_wnd, nffc, 'int32' );            % number of components (should be 3)
    fwrite( fid_wnd, lat, 'float32' );           % latitude (deg)
    fwrite( fid_wnd, z0, 'float32' );            % Roughness length (m)
    fwrite( fid_wnd, zOffset, 'float32' );       % Reference height (m) = Z(1) + GridHeight / 2.0
    fwrite( fid_wnd, TI_U, 'float32' );          % Turbulence Intensity of u component (%)
    fwrite( fid_wnd, TI_V, 'float32' );          % Turbulence Intensity of v component (%)
    fwrite( fid_wnd, TI_W, 'float32' );          % Turbulence Intensity of w component (%)

    fwrite( fid_wnd, dz, 'float32' );            % delta z in m
    fwrite( fid_wnd, dy, 'float32' );            % delta y in m
    fwrite( fid_wnd, dx, 'float32' );            % delta x in m           
    fwrite( fid_wnd, nt_header, 'int32' );       % half the number of time steps
    fwrite( fid_wnd, MFFWS, 'float32');          % mean full-field wind speed

       fwrite( fid_wnd, [0 0 0], 'float32' );    % unused variables (for BLADED): write zeros
       fwrite( fid_wnd, [0 0], 'int32' );        % unused variables (for BLADED): write zeros
    fwrite( fid_wnd, nz, 'int32' );              % number of points in vertical direction
    fwrite( fid_wnd, ny, 'int32' );              % number of points in horizontal direction
       fwrite( fid_wnd, zeros(3*(nffc-1),1), 'int32' );     % unused variables (for BLADED): write zeros
               
%-----------------------------------------
% WRITE GRID DATA
%-----------------------------------------
Scale    = 0.00001*SummVars(3)*SummVars(4:6);
Offset   = [SummVars(3) 0 0];


if SummVars(2) > 0 %clockwise rotation
    %flip the y direction....
    %let's change the dimension of velocity so that it's 4-d instead of 3-d   
    y_ix = ny:-1:1;
else
    y_ix = 1:ny;   
end

v = zeros(nz*ny*nffc,1);

for it = 1:nt
    cnt = 1;
    for iz = 1:nz
        for iy = y_ix
            for k=1:nffc
                v(cnt) = (velocity(it,k,iy,iz) - Offset(k))/Scale(k);
                cnt = cnt + 1;
                %velocity(it,k,iy,iz) = v(cnt2)*Scale(k) + Offset(k);
            end %for k
        end %iy
    end % iz 
   
    fwrite( fid_wnd, v, fileFmt );
   
end %it

%-----------------------------------------
% CLOSE .WND FILE
%-----------------------------------------
fclose(fid_wnd);