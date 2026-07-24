%% radPs2rpm 
% Function: converts from radians per second to rounds per minute
%
%
%% Usage:
%
% y = radPs2rpm(u)
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

function y = radPs2rpm(u)
y = u * 60/(2*pi);