%   SMmodel_GA_WEB(name,PAR,FIG)
%
%   SMmodel_GA_WEB: Soil Moisture Model based on Green-Ampt for infiltration
%   Authors: Luca Brocca, Florisa Melone, Tommaso Moramarco

function [NS,NS_lnQ,NS_radQ,RQ,RMSE] = SMmodel_GA_WEB(name,PAR,FIG);

% Loading input data
PTSM=load([name,'.txt']);
[M,N]=size(PTSM);
D=PTSM(:,1); PIO=PTSM(:,2); TEMPER=PTSM(:,3); WWobs=PTSM(:,4);
dt=round(nanmean(diff(D))*24*10000)/10000;
MESE=month(D);

% Model parameter
W_0_RZ=   PAR(1);
W_max=    PAR(2); % mm
Psi_av_L= PAR(3);
Ks_sup=   PAR(4); % mm/h
Ks_RZ=    PAR(5); % mm/h
m_RZ=     PAR(6);
Kc=       PAR(7);

% Parameters adjustment
Ks_sup=Ks_sup.*dt;
Ks_RZ=Ks_RZ.*dt;

% Potential Evapotranspiration parameter
L=[0.2100;0.2200;0.2300;0.2800;0.3000;0.3100;
    0.3000;0.2900;0.2700;0.2500;0.2200;0.2000];
Ka=1.26;
EPOT=(TEMPER>0).*(Kc*(Ka*L(MESE).*(0.46*TEMPER+8)-2))/(24/dt);
clear PTSM TEMPER

% Thresholds for numerical computations
soglia=0.01;  % Water Content
soglia1=0.01; % Infiltration

% Parameter initialization
F_p=0;
F=0.00001;
W_init=-9999;
WW=zeros(M,1);
W_RZ_p=W_0_RZ*W_max;
PIOprec=0; inf=0;
ii=1; i=1;

% Main ROUTINE
for t=1:M
    W_RZ=W_RZ_p;
    jj=0;
    err=1000;
    while err>soglia;
        % infiltration computation (Green-Ampt)
        jj=jj+1;
        if (PIOprec<=0.001)
            if  (PIO(t)>0)
                W_init=W_RZ;
                Psi=Psi_av_L*(W_max-W_init);
            else
                W_init=-9999;
                ii=1;
                F_p=0;
                F=0.00001;
            end
        end

        if W_init~=-9999
            if jj>1
                ii=ii-1;
            end
            inf=(ii==1)*1000+(ii~=1)*Ks_sup*(1-Psi/F);

            if inf>PIO(t)
                inf=PIO(t);
                F=F+PIO(t);
            else
                F_p=F;
                errore=1000;
                while errore>soglia1 % iteration for the infiltration
                    F1=F_p-Psi*log((F-Psi)/(F_p-Psi))+Ks_sup;
                    errore=abs((F-F1)/F);
                    F=F+0.01;
                end
                inf=F-F_p;
            end
            F_p=F;
            ii=ii+1;
        end

        e=EPOT(t)*W_RZ/W_max;
        perc=Ks_RZ*(W_RZ/W_max).^(m_RZ);
        W_RZ_corr=W_RZ_p+(inf-e-perc);

        if W_RZ_corr>=W_max
            W_RZ_corr=W_max;
        end

        err=abs((W_RZ_corr-W_RZ)/W_max);
        W_RZ=W_RZ_corr;
        if jj==100, break, end
    end
    W_RZ_p=W_RZ_corr;
    if t>3,PIOprec=sum(PIO(t-3:t));end
    WW(t)=W_RZ_corr./W_max;
end;

% Calculation of model performance
RMSE=nanmean((WW-WWobs).^2).^0.5;
NS=1-nansum((WW-WWobs).^2)./nansum((WWobs-nanmean(WWobs)).^2);
NS_radQ=1-nansum((sqrt(WW+0.00001)-sqrt(WWobs+0.00001)).^2)./...
    nansum((sqrt(WWobs+0.00001)-nanmean(sqrt(WWobs+0.00001))).^2);
NS_lnQ=1-nansum((log(WW+0.0001)-log(WWobs+0.0001)).^2)...
    ./nansum((log(WWobs+0.0001)-nanmean(log(WWobs+0.0001))).^2);
NS_lnQ=real(NS_lnQ);
NS_radQ=real(NS_radQ);
X=[WW,WWobs]; X(any(isnan(X)'),:) = [];
RRQ=corrcoef(X).^2; RQ=RRQ(2);

% Figure
if FIG==1
    set(gcf,'paperpositionmode','manual','paperposition',[1 1 20 16])
    set(gcf,'position',[560 174 1018 774])
    
    h(1) = axes('Position',[0.1 0.5 0.8 0.40]);
    set(gca,'Fontsize',12)
    s=(['NS= ',num2str(NS,'%4.3f'),...
        ' NS(log)= ',num2str(NS_lnQ,'%4.3f'),...
        ' NS(radq)= ',num2str(NS_radQ,'%4.3f'),...
        ' RQ= ',num2str(RQ,'%4.3f'),...
        ' RMSE= ',num2str(RMSE,'%4.3f')]);
    title(['\bf',s]);
    hold on
    plot(D,WWobs,'g','Linewidth',3)
    plot(D,WW,'r','Linewidth',2);
    
    legend('\theta_o_b_s','\theta_s_i_m',0);
    datetick('x',20)
    set(gca,'Xticklabel','')
    ylabel('relative soil moisture [-]')
    grid on, box on
    axis([D(1) D(end) min(WWobs)-0.05 max(WWobs)+0.05])
    
    h(2) = axes('Position',[0.1 0.1 0.8 0.40]);
    set(gca,'Fontsize',12)
    hold on
    plot(D,PIO,'color',[.5 .5 .5],'Linewidth',3)
    grid on, box on
    ylabel('rain (mm/h)')
    datetick('x',20)
    grid on, box on
    axis([D(1) D(end) 0 1.05.*max(PIO)])
        
    print(gcf,['GAmodel_',name],'-dpng','-r150')    
%     save res
end
