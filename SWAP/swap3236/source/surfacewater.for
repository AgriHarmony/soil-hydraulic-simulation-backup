! File VersionID:
!   $Id: surfacewater.for 192 2011-02-10 13:39:33Z kroes006 $
! ----------------------------------------------------------------------
      subroutine SurfaceWater(task)
! ----------------------------------------------------------------------
!     date               : december 2004
!     purpose            : calculate surface water balance
! ----------------------------------------------------------------------
!     global
      use Variables
      implicit none 
      integer task

!     local
      integer level, node
      real*8  afgen
      real*8  zCum,difzTopDisLay(madr),ratio,ratiodz,sumqdr(madr),dh
      integer nodeTopDisLay(madr)
      character*300 messag
d     real*8  qdrain_old(madr), qdrain_new(madr)

! ----------------------------------------------------------------------
      goto (1000,2000,3000) task

1000  continue

! === initialization ===================================================

! --- read input data
      call rddre (drfil,pathdrain,nrlevs,nrpri,nrsec,l,zbotdr,
     &  widthr,taludr,rdrain,rinfi,rentry,rexit,gwlinf,swdtyp,wlptab,
     &  swsec,wls,osswlm,nmper,wlstar,impend,swman,wlsman,wscap,
     &  swqhr,hbweir,alphaw,betaw,nqh,hqhtab,qqhtab,dropr,gwlcrit,
     &  wlstab,sttab,swstini,swst,wlsbak,swsrf,nphase,hcrit,hdepth,
     &  vcrit,wlp,nodhd,numnod,dz,wldip,numadj,logf,intwl,swnrsrf,
     &  rsurfdeep,rsurfshallow,t1900,cofintfl,expintfl,
     &  swscre,swdivd,cofani,numlay,tstart,tend,
     &  swdislay,swtopdislay,ztopdislay,ftopdislay,SwTopnrsrf,
     &  swdivdinf,FacDpthInf)                                           !  Divdra, infiltration

      hwlman = 0.0d0
      vtair = 0.0d0

!   - In case of macropores: initialise drainage basis for rapid drainage through macropores
      if (flInitDraBas) then
         if (NumLevRapDra.gt.nrlevs) then
            messag = ' NUMLEVRAPDRA greater then NRLEVS'
            call fatalerr('MacroRead',messag)
         endif
!
         if (swdtyp(NumLevRapDra).eq.1) then
            ZDraBas = zbotdr(NumLevRapDra)      ! drain tube
         elseif (Swsec.eq.1) then            
            ZDraBas = afgen (wlstab,2*maowl,t1900) ! open drain, surf.wat. level input
         elseif (Swsec.eq.2) then
            ZDraBas = WlStar                    ! open drain, srf.wat. level simulated
         endif   
!
         flInitDraBas = .false.
!
         Return
!
      endif

      return

2000  continue

! === lateral drainage fluxes to surface water ========================

! --- reset intermediate surface water and drainage fluxes
      if (flzerointr) then
        do node = 1,numnod
          do level = 1,nrlevs
            inqdra(level,node) = 0.0d0
          enddo
        enddo
        iqdra = 0.0d0
      endif

! --- reset cumulative surface water and drainage fluxes
      if (flzerocumu) then
        cqdrd = 0.0d0
        cwsupp = 0.0d0
        cwout = 0.0d0
        cqdra = 0.0d0
        do level = 1,nrlevs
          cqdrain(level) = 0.0d0
          cqdrainin(level) = 0.0d0
          cqdrainout(level) = 0.0d0
        enddo
      endif

! --- no drainage at all
      if (gwl.gt.998.0d0) then
        do level = 1,nrlevs
          qdrain(level) = 0.0d0
        end do
        return
      endif

! --- calculate lateral drainage
      call bocodre (swsec,swsrf,nrlevs,nrpri,gwl,zbotdr,taludr,
     &  widthr,pond,pondmx,swdtyp,dt,wls,wlp,drainl,l,rdrain,rinfi,
     &  rentry,rexit,gwlinf,wetper,qdrain,qdrd,impend,nmper,wscap,swst,
     &  swnrsrf,rsurfdeep,rsurfshallow,cofintfl,expintfl,t1900,
     &  swmacro,NumLevRapdra,ZDraBas,dh)

! --- partition drainage flux over compartments

      do level=1,nrlevs
         do node = 1,numnod
            qdra(level,node) = 0.0d0
         end do
      end do

      if (swdivd.eq.1) then
D        do level=1,nrlevs
D           qdrain_old(level) = qdrain(level)
D        end do
         if(flksatexm)then
           call divdra (numnod,nrlevs,dz,ksatexm,layer,cofani,gwl,l,
     &     qdrain,qdra,Swdivdinf,Swnrsrf,SwTopnrsrf,Zbotdr,             !  Divdra, infiltration
     &     dt,FacDpthInf,owltab,t1900)                                  !  Divdra, infiltration
         else
           call divdra (numnod,nrlevs,dz,ksatfit,layer,cofani,gwl,l,
     &     qdrain,qdra,Swdivdinf,Swnrsrf,SwTopnrsrf,Zbotdr,             !  Divdra, infiltration
     &     dt,FacDpthInf,owltab,t1900)                                  !  Divdra, infiltration
         endif

!        redistribute qdrain with new top boundary for discharge layers
         if(swdislay.eq.2) then
            do level=1,nrlevs
               if(swtopdislay(level).eq.1)  then
                  zTopDisLay(level) = fTopDisLay(level) * gwl  + 
     &                       (1.0d0-fTopDisLay(level)) * (gwl-dh)
               end if
            end do
         end if
         if(swdislay.eq.1 .or. swdislay.eq.2) then
            do level=1,nrlevs
               if(swtopdislay(level).eq.1)  then
!                 find node nr of new top of discharge layer
                  nodeTopDisLay(level) = 1
                  zCum               = - dz(1)
                  do while (zTopDisLay(level) .lt. zCum)
                     nodeTopDisLay(level) = nodeTopDisLay(level) + 1
                     zCum              = zCum - dz(nodeTopDisLay(level))
                  enddo
!                 saturated part (difzTopDisLay(lev)) of compartment containing waterlevel
                  difzTopDisLay(level) = zTopDisLay(level) - zCum
                  ratiodz = 
     &                     difzTopDisLay(level)/dz(nodeTopDisLay(level))
                  sumqdr(level) = 
     &                        ratiodz * qdra(level,nodeTopDisLay(level))
                  do node = nodeTopDisLay(level)+1,numnod
                     sumqdr(level) =  sumqdr(level) + qdra(level,node)
                  end do
                  if( dabs(sumqdr(level)) .lt. 1.0d-8)then
                     ratio = 1.0d0
                  else
                     ratio = qdrain(level)/sumqdr(level)
                  end if
!                 redistribute drainwater fluxes
                  do node = 1,nodeTopDisLay(level)-1
                     qdra(level,node) = 0.0d0
                  end do           
                  qdra(level,nodeTopDisLay(level)) = 
     &                qdra(level,nodeTopDisLay(level)) * ratio * ratiodz
                  do node = nodeTopDisLay(level)+1,numnod
                     qdra(level,node) = qdra(level,node)* ratio
                  end do           
               endif
            end do           
         endif

! --- error handling
D        do level=1,nrlevs
D           qdrain_new(level) = 0.0d0
D           do node = 1,numnod
D              qdrain_new(level) = qdrain_new(level) + qdra(level,node)
D           end do
D           if (abs(qdrain_new(level)-qdrain_old(level)).gt.0.001) then
D              write(messag,55) 
D    &        ' SwDislay qdrain-diff,datetime',date,t1900,'level',level,
D    &        ' qdrain_new = ',qdrain_new(level),' qdrain_old = ',
D    &         qdrain_old(level),'(cm/d)'
D  55          format(a,a11,f10.4,a,i3,a,f10.5,a,f10.5,a)
!D              call fatalerr ('Surfacwater',messag)
D              call warn ('Surfacwater',messag,logf,swscre)
D           endif
D        end do


      else
! --- drainage flux through lowest compartment 
        do level = 1,nrlevs
           qdra(level,numnod) = qdrain(level)
        end do
      endif

      qdrtot = 0.0d0
      do level=1,nrlevs
          qdrtot = qdrtot + qdrain(level)
      end do

      return

3000  continue


! === surface water balance ========================

      if (swsrf .eq. 3) then 
        wlp = afgen (wlptab,2*mawlp,t1900-1.0d0+dt)
      endif
      if (swsec.eq.2) then
! ---   water level of secondary system is simulated
        call wlevbal (tcum,nrpri,impend,nmper,swman,imper,
     &   wlstar,wls,hbweir,gwl,wlsman,gwlcrit,nphase,dropr,sttab,
     &   wscap,dt,runots,QRapDra,swst,zbotdr,alphaw,betaw,qdrd,cqdrd,
     &   cwsupp,cwout,wlsbak,osswlm,t,numnod,thetas,theta,dz,vcrit,
     &   nodhd,hcrit,h,swqhr,qqhtab,hqhtab,wldip,hwlman,vtair,overfl,
     &   numadj,intwl,t1900,logf,swscre,fldecdt,fldtmin,
     &   rsro,pond,pondmx)
      elseif (swsec.eq.1) then
! ---   water level of secondary system is input
        call wballev (wls,wlstab,swst,sttab,dt,runots,QRapDra,
     &  qdrd,cqdrd,cwsupp,cwout,wlsold,t1900)
      endif
      
      return
      end


! ----------------------------------------------------------------------
      SUBROUTINE WLEVBAL (
     & tcum,NRPRI,impend,nmper,swman,imper,
     & wlstar,wls,hbweir,gwl,wlsman,gwlcrit,nphase,dropr,sttab,wscap,
     & dt,runots,QRapDra,swst,zbotdr,alphaw,betaw,qdrd,cqdrd,cwsupp,
     & cwout,wlsbak,osswlm, T,NUMNOD,THETAS,THETA,DZ,VCRIT,NODHD,HCRIT,
     & H,SWQHR,QQHTAB,HQHTAB,wldip,hwlman,vtair,overfl,numadj,intwl,
     & t1900, logf, swscre,fldecdt, fldtmin, rsro,pond,pondmx)
! ----------------------------------------------------------------------
!     UpDate             : 20080109
!     Date               : 19990929
!     Purpose            : calculate surf. water level from water balance

! --- Set target level wlstar:
! --- SWMAN = 1: HBWEIR
! --- SWMAN = 2: from table (4e #2) and within adjustment period and 
! --- taking into account the maximum drop rate. 
! --- Calculate level for maximum supply wlstara ( = wlstar-wldip)

! --- Calculate storage at wlstar and at wlstara
! ---   If wlstara is above deepest bottom level then water supply 
! ---   capacity is set to maximum value otherwise both swsttara and
! ---   wsmax are set to zero

! --- Calculate max. new storage: old + incoming/outgoing fluxes
! --- 1 System falls dry: set supply to maximum,
!       set wls at bottom of deepest channel
! --- 2 System will not become full: set supply to maximum
!       calculate new level from storage
! --- 3 System becomes full under maximum supply conditions,
! ---   Determine how, first without supply
! --- 3a wlstara cannot be reached: calculate required supply,
! ---    to reach wlstara and set wls to wlstara
! --- 3b system can be filled above wlstara, however no discharge:
! ---    wls is calculated from storage and is somewhere between
! ---    wlstar and wlstara, no supply, no discharge
! --- 3c wlstar can be reached :wsupp = 0;
! ---    Now check whether there are automatic weirs for keeping the 
! ---    level at target value or that the discharge relationship
! ---    determines new level:
! --- 3c1 SWMAN=2: calculate discharge
! ---     check whether there is enough discharge capacity:
! ---     in case SWQHR = 1: calculate discap
! ---     in case SWQHR = 2: find discap from table
! ---     if sufficient capacity wls = wlstar otherwise set overflow
! --- 3c2 SWMAN=1 or overflow
! ---     Calculate highest possible discharge, if still unsufficient 
! ---     to handle present drain fluxes: stop - system overflow
! ---     otherwise start iteration procedure to determine new level,
! ---     storage and discharge:

! ***********************************************
! --- Iteration Procedure:
! --- Establish lower and upper bounds: hbweir(imper) and +100cm
! --- Calculate storage and discharge for point halfway: swsti, wdisi
! --- Calculate new storage: if higher than swsti then adjust lower
! --- bound to point halfway otherwise adapt upper bound to point halfway
! --- Continue until upper and lower bounds converge (< 0.001 cm)   
! --- Ready: update wls, swst and wdis
! ***********************************************

!     Subroutines called :                          
!     Functions called   : swstlev                          
!     File usage         :
! ----------------------------------------------------------------------
      IMPLICIT NONE
      Include 'arrays.fi'

! --- global
      integer nmper,swman(mamp),nphase(mamp),numnod
      integer nodhd(mamp),swqhr,nrpri,numadj,intwl(mamp)

      real*8  wlstar,hbweir(mamp),gwl,wlsman(mamp,mamte),QRapDra
      real*8  dropr(mamp*mamte),sttab(22,2),wscap(mamp),dt,runots
      real*8  wls,swst,osswlm,gwlcrit(mamp,mamte),impend(mamp)
      real*8  zbotdr(5),alphaw(mamp),betaw(mamp),qdrd,cqdrd,cwsupp
      real*8  cwout,wlsbak(4),t,dz(macp)
      real*8  vcrit(mamp,mamte),hcrit(mamp,mamte)
      real*8  qqhtab(mamp,mamte),hqhtab(mamp,mamte),wldip(mamp),hwlman
      real*8  vtair,t1900,tcum
      real*8  thetas(macp),theta(macp),h(macp),rsro,pond,pondmx
      logical overfl
      integer logf, swscre
     
! --- local
      INTEGER imper,iphase,NODE,Intday
      real*8  wlstx,swsttar,dvmax,swstmax,wsupp,wdis,wlstarb
      real*8  wover,discap,wlsl,wlsu,wlsi,swsti,wdisi,swstn
      real*8  wprod1,wprod2,oscil,wlevst,swstlev,wlstara
      real*8  swsttara,qhtab,rday,wsmax
      character messag*200, datetime*19

      logical fldecdt, fldtmin


      save
!-----------------------------------------------------------------------
! --- resetting of flag for overflowing of automatic weir
      overfl = .false.

! --- memorizing previous target level
      wlstarb = wlstar

! --- determine which management period the model is in:
      imper = 0
 100  imper = imper + 1

! --- error handling
      if (imper .gt. nmper) then
        messag = ' sw-level oscillation at '//datetime//
     &        '       advise: reduction of dtmax !'
        messag = 'error sw-management periods(IMPER), more than defined'
        call fatalerr ('Wlevbal',messag)
      endif
      
      if (t1900-1.d0+0.1d-10 .gt. impend(imper)) goto 100

! --- determine the target sw-level:
      if (swman(imper) .eq. 1) then

! --- In the case of a fixed weir the 'target level' is set to the 
! --- weir crest, for later use in calculations to determine whether 
! --- there is any outflow at all (see below):
        wlstar = hbweir(imper)
      else

! ---  For automatic weir, determine the target level of the surface 
! ---  water, dependent on groundwater level:

! ---   only adjust it if new subperiod, with length intwl(imper) has 
!       started  (or if it is is the first call)
        rday = (T+1.0D0)/intwl(imper)
        intday = int(rday)

        if (abs(rday-1.0*intday).lt.0.00001d0 .or. tcum.lt.1.0d-10) then

          iphase = nphase(imper)
          do while(gwl.gt.gwlcrit(imper,iphase).and.iphase.gt.1) 
            iphase = iphase-1
          enddo

! --- compare total air volume with VCRIT, adapt iphase
          VTAIR = 0.0d0
          do NODE = 1,NUMNOD
            VTAIR = VTAIR + (THETAS(NODE)-THETA(NODE))
     &              *abs(DZ(NODE))
          enddo
          do while (VTAIR.lt.VCRIT(imper,iphase).AND.IPHASE.gt.1)
            iphase = iphase - 1
          enddo

! --- compare H(nodhd(imper)) with HCRIT, adapt iphase 
          do while (h(nodhd(imper)).gt.hcrit(imper,iphase)
     &                            .and.iphase.gt.1) 
            iphase = iphase - 1
          enddo
          hwlman = h(nodhd(imper))

          wlstx = wlsman(imper,iphase)
        else

! --- use old level
          wlstx = wlstarb
        endif

! ---   if the level must drop, then do not let it drop at more than 
! ---   the specified rate:
        if (wlstx .lt. wlstar .and. dropr(imper) .gt. 0.001d0) then
          wlstar = wlstar - dropr(imper)*dt
          if (wlstar .lt. wlstx) wlstar = wlstx
        else
          wlstar = wlstx
        endif
      endif

! --- counter of adjustments
      if (abs(wlstar-wlstarb) .gt. 0.00001d0) then
         numadj = numadj + 1
      endif

! --- storage for the 'target level'
      swsttar = swstlev(wlstar,sttab)

! --- level and storage for "max. level for supply"
      wlstara = wlstar - wldip(imper)
      if (wlstara .gt. (zbotdr(1+nrpri)+1.d-4)) then
         swsttara = swstlev(wlstara,sttab)
         wsmax = wscap(imper)
      else
         swsttara = 0.0d0
         wsmax = 0.0d0
      endif
      
! --- determine whether the system will become full (target level or
! --- level of weir crest):
      dvmax = (qdrd + QRapDra + wsmax) * dt + runots
      swstmax = swst + dvmax

      if (swstmax .lt. 1.0d-7) then
! ---   storage decreases to zero, then the surface water system 
! ---   falls dry; set surface water supply to maximum: 

        if (swstmax .lt. -0.1d0) then
          messag = 'error algorithm for sw falling dry'
          call fatalerr ('Wlevbal',messag)
        endif

        wsupp = wsmax
        wdis = 0.0d0
        swst = 0.0d0
        wls = zbotdr(nrpri+1)

      elseif (swstmax .ge. 0.0d0 .and. swstmax .lt. swsttara) then
! --- system will not become full - set supply to max. capacity
        wsupp = wsmax
        wdis = 0.0d0
        swst = swstmax

! --- calculate new level from storage
        wls = wlevst(swst,sttab)
      else

! --- determine how system will become full: with or without needing
!     surface water supply; try first without any supply:
        dvmax = (qdrd + QRapDra) * dt + runots
        swstmax = swst + dvmax
        if (swstmax .le. swsttara) then

! --- apparently supply is needed for reaching target level, system 
!     is made full up to level wlstara, because supply is controllable:
          wsupp = (swsttara - swst - (qdrd+QRapDra)*dt-runots)/dt
          wdis = 0.0d0
          swst = swsttara
          wls = wlstara
        elseif (swstmax .le. swsttar) then

! --- apparently drainage is more than sufficient for filling system
!     to a level above the target level FOR SUPPLY, but not enough
!     for generating discharge; so calculate level from storage:
          wsupp = 0.0d0
          wdis = 0.0d0
          swst = swstmax
          wls = wlevst(swst,sttab)
        else

! --- drainage water is more than sufficient for reaching target
!     level;  now see whether there are automatic weirs for keeping
!     the level at target value, or that the discharge relationship
!     determines new level:
          wsupp = 0.
          if (swman(imper) .eq. 2) then

! --- the outflow equals the drainage flux, plus the storage
!     excess (or deficit !) in the target situation compared to
!     the actual situation: 
            wdis = (swst-swsttar + (qdrd+QRapDra)*dt + runots )/dt

! --- now check whether the weir has enough discharge capacity 
!     at this water level
            if (SWQHR.eq.1) then
              wover = wls - hbweir(imper)
              discap = alphaw(imper) * (wover**betaw(imper))
            elseif (SWQHR.eq.2) then

! --- interpolate QH table
              discap = qhtab(wlstar,imper,hqhtab,qqhtab)
            endif
            if (discap .gt. wdis) then
              wls = wlstar
              swst = swsttar
              overfl = .false.
            else
              overfl = .true.
            endif
          endif
          if (swman(imper) .eq. 1 .or. overfl) then

! --- determine level from q-h relationship, and also account
!     for change in storage (see below)
!     check first that the system does not overflow 
            if (SWQHR.eq.1) then
              wover = sttab(1,1) - hbweir(imper)
              discap = alphaw(imper) * (wover**betaw(imper))
            elseif (SWQHR.eq.2) then
              discap = QQHTAB(imper,1)
            endif
            
! ---       error handling
            swstn = swst + (qdrd + QRapDra - discap)*dt + runots
            if ( swstn .gt. sttab(1,2) ) then
              messag = 'surface water system has overflowed!'
              call fatalerr ('Wlevbal',messag)
            endif

! --- iteration procedure for determining new level, storage, 
!     and discharge
            wlsl = hbweir(imper)
            wlsu = sttab(1,1)

! --- find storage and discharge for intermediate point
 700        wlsi = (wlsl + wlsu) * 0.5
            swsti = swstlev(wlsi,sttab)
            if (SWQHR.eq.1) then
              wdisi = alphaw(imper)*(wlsi-hbweir(imper))**betaw(imper)
            else
              wdisi = qhtab(wlsi,imper,hqhtab,qqhtab)
            endif
            swstn = swst + (qdrd + QRapDra - wdisi)*dt + runots
            if (swstn .lt. swsti) then
              wlsu = wlsi 
            else
              wlsl = wlsi
            endif
            if ((wlsu - wlsl) .gt. 0.001d0) then

! --- continue iteration procedure:
              goto 700
            else

! --- updating of sw-parameters:
              wls = wlsi
              swst = swstn
              wdis = wdisi
            endif
          endif
        endif
      endif

!        ponding in case of extended drainage may limit timestep

      if (wls.gt.pondmx .or. pond.gt.pondmx) then
        if(dt .gt. 0.02*rsro) fldecdt = .true.
        return
      endif

! --- updating registration of last four levels, for noting oscillation
      wlsbak(1) = wlsbak(2)
      wlsbak(2) = wlsbak(3)
      wlsbak(3) = wlsbak(4)
      wlsbak(4) = wls
      wprod1 = (wlsbak(2)-wlsbak(1))*(wlsbak(3)-wlsbak(2))
      wprod2 = (wlsbak(3)-wlsbak(2))*(wlsbak(4)-wlsbak(3))
      if (wprod1.lt.0.0d0 .and. wprod2.lt.0.0d0) then
        oscil = abs(wlsbak(3)-wlsbak(2)) 
        if (oscil .gt. osswlm) then
           if (.not.fldtmin ) then
              fldecdt = .true.
              return  
           else
              call dtdpst 
     &        ('year-month-day,hour:minute:seconds',t1900,datetime)
              messag = ' sw-level oscillation at '//datetime//
     &        '       advise: reduction of dtmax !'
              call warn ('Wlevbal',messag,logf,swscre)
              call fatalerr ('Wlevbal',messag)
           endif
        endif
      endif

! --- cumulative terms:
      cqdrd = cqdrd  + qdrd*dt
      cwsupp = cwsupp + wsupp*dt
      cwout = cwout  + wdis*dt



      return
      end

! ----------------------------------------------------------------------
      subroutine WBALLEV (wls,wlstab,swst,sttab,dt,runots,QRapDra,
     &                    qdrd,cqdrd,cwsupp,cwout,WLSOLD,t1900)
! ----------------------------------------------------------------------
!     Date               : 29/9/99
!     Purpose            : close surface water balance using 
!                          given (input) surface water levels   

! --- Compare storage at T + drainage fluxes with storage at T+DT
! --- The difference will be either discharge or supply
! --- Update totals (of discharge, supply and qdrd)

!     Subroutines called :                          
!     Functions called   : swstlev                          
!     File usage         :
!     Differences SWAP/SWAPS: None        
! ----------------------------------------------------------------------
      IMPLICIT NONE
      Include 'arrays.fi'

! --- global
      real*8  wls,wlstab(2*MAWLS),t1900
      real*8  swst,sttab(22,2),dt,runots,QRapDra,qdrd,cqdrd,cwsupp
      real*8  cwout,AFGEN

! --- local
      real*8  wlsold,swstold,swstrest,wdis,wsupp,swstlev

! ----------------------------------------------------------------------
! --- wlsold gets w-level of previous time step
      wlsold = wls

! --- fetch new level from input series
      wls = AFGEN (WLSTAB,2*MAWLS,t1900-1.d0+DT)

! --- determine surface water storage for level(t-dt) and level(t)
      swstold = swstlev(wlsold,sttab) 
      swst = swstlev(wls,sttab)

! --- determine from the surface water storages and the qdrain whether
! --- supply has taken place during period (t)-(t+dt) or water has 
! --- been discharged
      swstrest = swstold + (qdrd + QRapDra)*dt + runots - swst

! --- if supply was needed, set discharge to zero
      if (swstrest.le.0.0d0) then
        wdis = 0.0d0
        wsupp = -swstrest/dt

! --- if discharge has taken place, set supply to zero
      else
        wdis = swstrest/dt
        wsupp = 0.0d0
      endif

! --- cumulation of water balance terms
      cqdrd = cqdrd  + qdrd*dt
      cwsupp = cwsupp + wsupp*dt
      cwout = cwout  + wdis*dt

      return
      end


