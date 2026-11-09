% init
clearvars; close all; clc;
addpath("..")
% Execute Test for CorrectLosTiming.m
results = runtests('CorrectLosTiming_UnitTest');
table(results)   
restoredefaultpath