%% Soil Moisture Model RUN
clc,clear
name='COL_0204'; G=1;
PAR=load('X_optGA_WEB.txt');
SMmodel_GA_WEB(name,PAR,G)
%------------------------------
%% Soil Moisture Model calibration
cal_SMmodel_GA_WEB(name)
