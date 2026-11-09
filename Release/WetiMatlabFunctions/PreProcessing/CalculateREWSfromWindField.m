function	[v_0,t] = CalculateREWSfromWindField(TurbSimResultFile,R,nLoop)

% read in wind field
[velocity, y, z, nz, ny, dz, dy, dt, zHub, z1, SummVars] = ReadBLgrid(TurbSimResultFile);
h                   = SummVars(1);
[Z,Y]               = meshgrid(z-h,y); % Y is a matrix where each column is a copy of y to fit to (iy,iz)
DistanceToHub       = (Y(:).^2+Z(:).^2).^0.5;
IsInRotorDisc       = DistanceToHub<=R;

% get rotor-effective wind speed
n_t_wf  = size(velocity,1);
v_0_wf 	= NaN(n_t_wf,1);
for i_t = 1:1:n_t_wf
    CurrentWind     = squeeze(velocity(i_t,1,:,:));
    WindField       = CurrentWind(IsInRotorDisc);
    v_0_wf(i_t,1) 	= mean(WindField);
end

% combine the REWS nLoop times
t       = dt*[0:n_t_wf*nLoop-1]';
v_0     = repmat(v_0_wf,nLoop,1);

end