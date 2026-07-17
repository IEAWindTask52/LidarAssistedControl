%% rpm2radPs 
% Function: converts from rounds per minute to radians per second
%
%
%% Usage:
%
% y = rpm2radPs(u)
%
%% Input:
%
% * u
%
%% Output:
%
% * y
%
%% Modified:
%
%
%
%% ToDo:
%
%
%
%% Created: 
% David Schlipf on 11-Jan-2009
%
% Copyright: University of Stuttgart
% Stuttgart Wind Energy (SWE) @ Institute of Aircraft Design
%
%% Code:

function y = rpm2radPs(u)
y = u * 2*pi/60;