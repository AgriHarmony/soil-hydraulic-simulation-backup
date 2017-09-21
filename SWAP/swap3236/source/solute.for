! File VersionID:
!   $Id: solute.for 189 2010-12-06 10:36:51Z kroes006 $
! ----------------------------------------------------------------------
      subroutine solute (task)
! ----------------------------------------------------------------------
!     date               : december 2007
!     purpose            : calculation of solute concentrations
! ----------------------------------------------------------------------
      use Variables
      implicit none

!     local variables
      integer level,i,task
      real*8  cmlav,thetav,ftemp,ftheta,decact,cfluxt,cfluxb
      real*8  cdrtot,ctrans,crot,dispr,diffus,old,dummy,rer
      real*8  vpore,isqdra,afgen,tab(mabbc*2)
      real*8  tcumsol,vsmall
      logical differ

      parameter (rer    = 1.d-3)
      parameter (vsmall = 1.d-15)

! ----------------------------------------------------------------------

      goto (1000,2000) task

1000  continue

! === initialize Solute rate/state variables ===========================

! --- determine initial solute profile from input concentrations
      if (swinco.ne.3) then
        do i = 1, nconc
          tab(i*2) = cml(i)
          tab(i*2-1) = abs(zc(i))
        end do
        do i = 1, numnod
          cml(i) = afgen(tab,macp*2,abs(z(i)))
        end do
      endif

! --- determine derived solute concentrations
      samini = 0.0d0
!      samaq = 0.0d0
      do i = 1,numnod
         cmsy(i) = (theta(i)*cml(i) + 
     &        bdens(layer(i))*kf(layer(i))*cref*(cml(i)/cref)**frexp)
         samini = samini + cmsy(i) * dz(i)
      end do

2000  continue

! === calculate Solute rate variables ========================

! --- reset cumulative solute fluxes
      if (flzerocumu) then
        sqprec = 0.0d0
        sqirrig = 0.0d0
        sqbot = 0.0d0
        sqdra = 0.0d0
        sqsur = 0.0d0
        dectot = 0.0d0
        rottot = 0.0d0
        csurf = 0.0d0
        samini = sampro
      endif

      isqbot = 0.0d0
      isqtop = 0.0d0
      isqdra = 0.0d0

! --- determine maximum timestep
      dtsolu = dt
      do i = 1,numnod
        thetav = inpola(i+1)*theta(i)+inpolb(i)*theta(i+1)
        diffus = ddif * (thetav**2.33)/(thetsl(layer(i))**2)
        dispr = diffus+ldis(layer(i))*abs(q(i))/theta(i)
        if (dispr.lt.1.0d-8) dispr = 1.0d-8
        dummy = dz(i)*dz(i)*theta(i)/2.0/dispr
        dtsolu = min(dtsolu,dummy)
      enddo

      tcumsol = 0.0
      do while ((dt-tcumsol).gt.1.0d-8)

! ---    time step and cumulative time
         dtsolu = min(dtsolu,(dt-tcumsol))
         dtsolu = max(dtsolu,dtmin)
         tcumsol   = tcumsol + dtsolu

! --- solute flux at soil surface
         csurf = (nird*cirr + nraidt*cpre)*dtsolu + csurf   ! gr cm-2
         if (qtop.lt.-1.d-6) then
            cpond = csurf / (pond-qtop*dtsolu)              ! gr cm-3
            cfluxt = qtop*(1.0d0-ArMpSs)*cpond*dtsolu       ! gr cm-2
            csurf = csurf + cfluxt                          ! gr cm-2
            isqtop = qtop*(1.0d0-ArMpSs)*cpond              ! gr cm-2 d-1
         else
            cpond = 0.0d0
            cfluxt = 0.0d0
         endif

! --- calculate mass balance for each compartment

         do i = 1,numnod

! --- convective and dispersive fluxes
            if (i .lt. numnod) then
               cmlav = inpola(i+1) * cml(i) + inpolb(i) * cml(i+1)
               thetav = inpola(i+1)*theta(i)+inpolb(i)*theta(i+1)
               vpore = abs(q(i+1))/thetav
               diffus = ddif*(thetav**2.33d0)/(thetsl(layer(i))**2.0d0)
               dispr = diffus + ldis(layer(i)) * vpore + 
     &                         0.5d0 * dtsolu*vpore*vpore
               cfluxb = (q(i+1)*cmlav +thetav * dispr 
     &                  *(cml(i+1)-cml(i))/disnod(i+1))*dtsolu
            else
               if (q(i+1).gt.0.0d0) then
                  cfluxb = q(i+1)*cseep*dtsolu
               else
                  cfluxb = q(i+1)*cml(i)*dtsolu
               endif
            endif

! --- solute decomposition
            if (fltemperature) then
               if (tsoil(i) .lt. 35.0d0) then
                  ftemp = exp(gampar*(tsoil(i)-20.0d0))
               else
                  ftemp = exp(gampar*15.0d0)
               endif
            else
              ftemp = 0.0d0
            endif
            ftheta = min(1.0d0,(theta(i)/rtheta)**bexp)
            decact = decpot(layer(i))*ftemp*ftheta*fdepth(layer(i))
            ctrans = decact*theta(i)*cml(i) + decact*bdens(layer(i))
     &                 *kf(layer(i))*cref*((cml(i)/cref)**frexp)
            dectot = dectot + ctrans*dtsolu*dz(i)

! --- solute uptake by plant roots
            crot = tscf*qrot(i)*cml(i)/dz(i)
            rottot = rottot + tscf*qrot(i)*cml(i)*dtsolu

! --- lateral drainage
            cdrtot = 0.0d0
            do level = 1,nrlevs
               if (qdra(level,i) .gt. 0.0d0) then
                  cdrtot = cdrtot+qdra(level,i)*cml(i)/dz(i)
               else
                  cdrtot = cdrtot+qdra(level,i)*cdrain/dz(i)
               endif
            enddo

! --- cumulative amount of solutes to lateral drainage
            isqdra = isqdra + cdrtot*dz(i)*dtsolu
            sqdra = sqdra + cdrtot*dz(i)*dtsolu

! --- conservation equation for the substance
            cmsy(i) = cmsy(i) + (cfluxb-cfluxt) / dz(i) +
     &              (-ctrans-crot-cdrtot) * dtsolu

! --- iteration procedure for calculation of cml
            differ = .true.
            if (cmsy(i).lt.vsmall) then
               cmsy(i) = 0.0d0
               cml(i) = 0.0d0
            else
               if (abs(frexp-1.0d0).lt.0.001d0) then
                  cml(i) = cmsy(i) / 
     &                 (theta(i)+bdens(layer(i))*kf(layer(i)))
               else 
                  if (cml(i).lt.vsmall) cml(i) = vsmall
                  do while (differ)
                     old = cml(i)
                     dummy = bdens(layer(i))*kf(layer(i))*
     &                            (cml(i)/cref)**(frexp-1.0d0)
                     cml(i) = cmsy(i)/(theta(i)+dummy)
                     if (abs(cml(i)-old).lt.rer*cml(i)) differ = .false.
                  enddo
               endif
            endif

!           make top flux next compartment equal to current bottom flux
            cfluxt = cfluxb

! ---    next compartment
         enddo

! ---    solute balance in aquifer for breakthrough curve
         if (swbr .eq. 1) then
            if (qdrtot .gt. 0.0d0) then
               cdrain = cdrain + dtsolu/(poros + 
     &                  bdens(layer(numnod))*kfsat)* 
     &         ( (isqdra - qdrtot*cdrain)/daquif - decsat*cdrain*(poros+
     &                  bdens(layer(numnod))*kfsat) )
            else
               cdrain = cdrain + dtsolu/(poros + 
     &                           bdens(layer(numnod))*kfsat) *
     &                           ( isqdra/daquif - decsat*cdrain*
     &                           (poros + bdens(layer(numnod))*kfsat) )
            endif
            cseep = cdrain
         endif

! --- flux to surface water from aquifer
         if (swbr .eq. 1) then
            sqsur = sqsur + qdrtot*cdrain*dtsolu
         endif

! --- flux through bottom of soil profile
         if (qbot .gt. 0.0d0) then
            sqbot = sqbot + qbot*cseep*dtsolu
         else 
            sqbot = sqbot + qbot*cml(numnod)*dtsolu
         endif

! --- continue with next solute time step
      end do

! --- current solute flux at bottom of soil column
      if (q(numnod+1) .gt. 0.0d0) then
        isqbot = q(numnod+1) * cseep
      else
        isqbot = q(numnod+1) * cml(numnod)
      endif

! === calculate solute balance components ========================

! --- total amount in soil profile
      sampro = 0.0d0
      do i = 1,numnod
        sampro = sampro + cmsy(i) * dz(i)
      enddo
      sampro = sampro + csurf
!      if (swbr .eq. 1) samaq = cdrain*poros*daquif

! --- add time step fluxes to total cumulative values
      sqprec = sqprec + nraidt * cpre * dt
      sqirrig = sqirrig + nird * cirr * dt

! --- cumulative solute balance
      solbal = sampro - sqprec - sqirrig - sqbot + sqdra 
     &         + dectot + rottot - samini

      return
      end
      subroutine AgeTracer (task)
! ----------------------------------------------------------------------
!     date               : Oct 2010
!     purpose            : Ageing according to Goode (1996): 
!                        : "Direct simulation of groundwater age, WRR vol.32, p 289-296"
! ----------------------------------------------------------------------
      use Variables
      implicit none

!     global
      integer task

!     local variables
      integer level,i
      real*8  Agemlav,thetav,ftemp,ftheta,decact,Agefluxt,Agefluxb
      real*8  Agedrtot,Agerot,dispr,diffus,old,dummy
      real*8  vpore,isqdra,afgen,tab(mabbc*2)
      real*8  tcumsol
      logical differ
! 
      real*8  Ageirr,Agedrain,Agepre,Ageseep
      real*8  Ageevp
	real*8  Ageml(macp)    ! Array with age mass solute concentration (M/L3 water) 
      real*8  Agemsy(macp)   ! Array with dissolved solute concentration (M/L3 soil volume) 
      real*8  Agesurf
      real*8  Agepond
      real*8  Agepondm1
      real*8  AgeProd
      real*8  sum0,sum1,deltaz,zbot,ztop
      real*8  icAgeTopupw        ! Incremental (over output interval) age (d) of groundwater leaving top comp.
      real*8  icAgeTopdwn        ! Incremental (over output interval) age (d) of groundwater entering top comp.

      save  Ageirr,Agedrain,Agepre,Ageseep


! ----------------------------------------------------------------------

      goto (1000,2000) task

1000  continue

! === initialize Solute rate/state variables ===========================

! --- determine initial solute profile from input concentrations
      if (swinco.ne.3) then
        do i = 1, nconc
          tab(i*2) = cml(i)
          tab(i*2-1) = abs(zc(i))
        end do
        do i = 1, numnod
          Ageml(i) = afgen(tab,macp*2,abs(z(i)))
        end do
      else
        do i = 1, numnod
          Ageml(i) = cml(i)
        end do
      endif
! --- boundary conditions (ALL SET TO 0 ON INPUT)
      Agepre = cpre
      Ageirr = cirr
      Agedrain = cdrain
      Ageseep = cseep

! --- determine derived solute concentrations
      samini = 0.0d0
      do i = 1,numnod
         Agemsy(i) = theta(i)*Ageml(i) 
         samini = samini + Agemsy(i) * dz(i)
      end do

2000  continue

! === calculate Solute rate variables ========================

! --- reset cumulative solute fluxes
      if (flzerocumu) then
        icAgesur    = 0.0d0
        icAgetopdwn = 0.0d0
        icAgetopupw = 0.0d0
        icAgerot    = 0.0d0
        do level = 1,nrlevs
           icAgedra(level) = 0.0d0
        end do
        icAgebot = 0.0d0
      endif

      isqbot = 0.0d0
      isqtop = 0.0d0
      isqdra = 0.0d0

! --- determine maximum timestep
      dtsolu = dt
      do i = 1,numnod
        thetav = inpola(i+1)*theta(i)+inpolb(i)*theta(i+1)
        diffus = ddif * (thetav**2.33)/(thetsl(layer(i))**2)
        dispr = diffus+ldis(layer(i))*abs(q(i))/theta(i)
        if (dispr.lt.1.0d-8) dispr = 1.0d-8
        dummy = dz(i)*dz(i)*theta(i)/2.0/dispr
        dtsolu = min(dtsolu,dummy)
      enddo

      tcumsol = 0.0
      do while ((dt-tcumsol).gt.1.0d-8)

! ---    time step and cumulative time
         dtsolu = min(dtsolu,(dt-tcumsol))
         dtsolu = max(dtsolu,dtmin)
         tcumsol   = tcumsol + dtsolu

! --- solute flux at soil surface
         Agesurf = (nird*Ageirr + nraidt*Agepre)*dtsolu + 
     &                                       Pondm1*Agepondm1         ! gr cm-2
         if (qtop.lt.-1.d-6) then
            Agepond  = Agesurf / (pond-qtop*dtsolu)                   ! gr cm-3
            Agefluxt = qtop*(1.0d0-ArMpSs)*Agepond*dtsolu             ! gr cm-2
            Agesurf  = Agesurf + Agefluxt                             ! gr cm-2
            isqtop   = qtop*(1.0d0-ArMpSs)*Agepond                    ! gr cm-2 d-1
            icAgetopdwn = icAgetopdwn + qtop*(1.0d0-ArMpSs)*0.5d0*    ! gr cm-2
     &                    (Agepond+Agepondm1)  * dtsolu
         else
            Agepond  = 0.0d0
            Agefluxt = 0.0d0
         endif
         icAgesur    = icAgesur + 0.5d0*(Agepond+Agepondm1) * runots


! --- calculate mass balance for each compartment

         do i = 1,numnod

! --- convective and dispersive fluxes
            if (i .lt. numnod) then
               Agemlav = inpola(i+1) * Ageml(i) + inpolb(i) * Ageml(i+1)
               thetav = inpola(i+1)*theta(i)+inpolb(i)*theta(i+1)
               vpore = abs(q(i+1))/thetav
               diffus = ddif*(thetav**2.33d0)/(thetsl(layer(i))**2.0d0)
               dispr = diffus + ldis(layer(i)) * vpore + 
     &                         0.5d0 * dtsolu*vpore*vpore
               Agefluxb = (q(i+1)*Agemlav +thetav * dispr 
     &                  *(Ageml(i+1)-Ageml(i))/disnod(i+1))*dtsolu
            else
               if (q(i+1).gt.0.0d0) then
                  Agefluxb = q(i+1)*Agedrain*dtsolu
               else
                  Agefluxb = q(i+1)*Ageml(i)*dtsolu
               endif
            endif

! --- Age tracer uptake by plant roots
            Agerot = qrot(i)*Ageml(i)/dz(i)
            rottot = rottot + qrot(i)*Ageml(i)*dtsolu

! --- Age tracer uptake by soil evaporation ???
!            Ageevp = evso??*Ageml(i)/dz(i)

! --- lateral drainage
            Agedrtot = 0.0d0
            do level = 1,nrlevs
               if (qdra(level,i) .gt. 0.0d0) then
                  Agedrtot = Agedrtot+qdra(level,i)*Ageml(i)/dz(i)
               else
                  Agedrtot = Agedrtot+qdra(level,i)*Agedrain/dz(i)
               endif
            enddo

! --- cumulative amount of solutes to lateral drainage
            isqdra = isqdra + Agedrtot*dz(i)*dtsolu
            sqdra = sqdra + Agedrtot*dz(i)*dtsolu

! --- zero order production 
            AgeProd = 1.0d0 * 0.5d0*(theta(i)+thetm1(i))

! --- conservation equation for the substance
            Agemsy(i) = Agemsy(i) + (Agefluxb-Agefluxt) / dz(i) +
     &                  (-Agerot-Agedrtot+AgeProd) * dtsolu
            Ageml(i) = Agemsy(i) / theta(i)

!           make top flux next compartment equal to current bottom flux
            Agefluxt = Agefluxb

! ---    next compartment
         enddo

!        age of effluent terms
         icAgetopupw = icAgetopupw + max(0.0d0,q(1))*
     &                              0.5d0*(Ageml(1)+cml(1))*dtsolu
         do i = 1,numnod
            icAgerot = icAgerot + qrot(i)*
     &                              0.5d0*(Ageml(i)+cml(i))*dtsolu
            do level = 1,nrlevs
               icAgedra(level) = icAgedra(level) + qdra(level,i)* 
     &                              0.5d0*(Ageml(i)+cml(i))*dtsolu
            end do
         end do
         icAgebot = icAgebot - min(q(numnod+1),0.0d0)*
     &                         0.5d0*(Ageml(numnod)+cml(numnod))*dtsolu

      end do

!     age of variable 1m-plane of groundwater
      i = nodgwl+1
      zbot = z(i) - 0.5d0*dz(i)
      ztop = z(i) + 0.5d0*dz(i)
      do while(gwl-100.0d0.lt.zbot .and. i.le.numnod)
         if(gwl.lt.ztop) ztop = gwl
         if(gwl-100.0d0.gt.zbot) zbot = gwl-100.0d0
         deltaz = ztop - zbot
         sum1   = sum1 + deltaz * Ageml(i) * thetas(i)
         sum0   = sum0 + deltaz * thetas(i)
         i = i + 1
         ztop = z(i) + 0.5d0*dz(i)
         zbot = z(i) - 0.5d0*dz(i)
      end do
      Agegwl1m = -9999.9d0
      if(sum0.gt.0.0d0) Agegwl1m = sum1/sum0


! --- initial values next timestep
      Agepondm1 = Agepond

! --- Save for output only
      do i = 1, numnod
        cml(i) = Ageml(i)
      end do

      return
      end
