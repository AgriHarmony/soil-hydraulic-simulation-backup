function X_OPT=cal_SMmodel_GA_WEB(name,X_ini)
if nargin==1,X_ini=ones(7,1)*.5;,end
% X_ini=rand(5,1);
% [RES,FVAL,EXITFLAG,OUTPUT]=fminsearch(@calibOK,X_ini,...
%       optimset('Display','iter','MaxIter',1000,...
%      'MaxFunEvals',1000,'TolFun',0.5*1E-3,'Largescale','off'),name)
[RES,FVAL,EXITFLAG,OUTPUT]=fmincon(@calibOK,X_ini,[],[],[],[],...
     zeros(7,1),ones(7,1),[],optimset('Display','iter','MaxIter',50,...
     'MaxFunEvals',500,'TolFun',1E-5,'TolCon',6,'Largescale','off'),name)
X=convert_adim(RES);
[NS,NS_lnQ,NS_radQ,RQ]=SMmodel_GA_WEB(name,X,1)
!del X_optGA_WEB.txt
fid=fopen('X_optGA_WEB.txt','w');
fprintf(fid,'%9.4f\n',X);
fclose(fid);
%---------------------------------------------------------------------------------
function [err]=calibOK(X_0,name)
X=convert_adim(X_0);
[NS,NS_lnQ,NS_radQ,RQ]=SMmodel_GA_WEB(name,X,0);
err=1-NS;
save X_PAR
%---------------------------------------------------------------------------------
function X=convert_adim(X_0);
LOW=[ 0.0,  40, -1.50, 0.01, 0.01,   1,  0.5]';
UP =[ 1.0, 200, -0.10, 25.0, 25.0,  45,  2.5]';
X=LOW+(UP-LOW).*X_0;
