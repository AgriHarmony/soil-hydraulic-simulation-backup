! File VersionID:
!   $Id: irrigation.for 176 2010-03-13 11:36:14Z kroes006 $
! ----------------------------------------------------------------------
      subroutine irrigation(task)
! ----------------------------------------------------------------------
!     Date               : November 2004
!     Purpose            : evaluate and schedule irrigations
! ----------------------------------------------------------------------
! --  global variables
      use variables
      implicit none

! --  local variables
      integer irr,node,nodsen,task,tcs,tcsfix,dcslim,dcs
      integer ifnd,i,datea(6),getun2,dayfix,irgdayfix
      integer endirr(2),startirr(2)
      integer yearendcrp, yearstacrp
      real*8  frlow,phlo,phhi,phme,awlh,awmh,awah,cdef
      real*8  wclo,wcme,wchi,wcac,tps1,tps2,tps3,tps4,tps5,depl,phcrit
      real*8  watcon,prhead,dps1,dps2,afgen,Tred
      real*8  dvstage(7),trel(7),raw(7),taw(7),dwa(7),hcri(7),tcri(7)
      real*8  di(7),fid(7),irgdepmax,irgdepmin,irgthreshold
      real*4  fsec
      real*8  tstairryrx,tendirryrx, grai_red
      logical flIrriTime
      logical rdinqr
      character filnam*80, messag*200

!     save local variables
      save    

!     dcs(1)  = Amount of under- or over-irrigation (L) in case of a scheduled irrigation event
!     dcs(2)  = Prescribed fixed irrigation depth (L) for each scheduled irrigation event

! ----------------------------------------------------------------------
      goto (1000,2000) task

1000  continue

! == = initialization calculated irrigation ============================

      dayfix = 1

      if (schedule .eq. 1) then

! ---   open crop file
        filnam = trim(pathcrop)//trim(cropfil(icrop))//'.crp'
        irr = getun2 (10,90,2)
        call rdinit(irr,logf,filnam)

! ---   no scheduled irrigation before this date
        call rdfinr('startirr',1,31,startirr,2,2)
        call rdfinr('endirr',1,31,endirr,2,2)
        datea(1) = 1900
        datea(2) = startirr(2)
        datea(3) = startirr(1)        
        fsec = 0.0
        call dtardp (datea, fsec, tstairrig)
        datea(2) = endirr(2)
        datea(3) = endirr(1)        
        call dtardp (datea, fsec, tendirrig)

! ---   application method
        call rdsinr ('isuas',0,1,isuas)
! ---   solute concentration irrigation water
        call rdsdor ('cirrs',0.0d0,100.0d0,cirrs)


! ---   timing criteria
        call rdsinr ('tcs',1,6,tcs)


! -1-   timing - allowable daily stress
!       minimum of ratio actual/potential transpiration
        if (tcs.eq.1) then
          call rdador('dvs_tc1',0.0d0,2.0d0,dvstage,7,ifnd)
          call rdfdor('trel',0.0d0,1.0d0,trel,7,ifnd)
          do i = 1, ifnd
            treltab(i*2) = trel(i)
            treltab(i*2-1) = dvstage(i)
          end do
        endif

! -2-   timing - depletion of readily available water
!       minimum fraction of readily available water
        if (tcs.eq.2) then
          call rdador('dvs_tc2',0.0d0,2.0d0,dvstage,7,ifnd)
          call rdfdor('raw',0.0d0,1.0d0,raw,7,ifnd)
          do i = 1, ifnd
            rawtab(i*2) = raw(i)
            rawtab(i*2-1) = dvstage(i)
          end do
        endif

! -3-   timing - depletion of totally available water
!       minimum fraction of totally available water
        if (tcs.eq.3) then
          call rdador('dvs_tc3',0.0d0,2.0d0,dvstage,7,ifnd)
          call rdfdor('taw',0.0d0,1.0d0,taw,7,ifnd)
          do i = 1, ifnd
            tawtab(i*2) = taw(i)
            tawtab(i*2-1) = dvstage(i)
          end do
        endif

! -4-   timing - allowable depletion amount            
!       maximum amount of water (L) depleted below field capacity
        if (tcs.eq.4) then
          call rdador('dvs_tc4',0.0d0,2.0d0,dvstage,7,ifnd)
          call rdfdor('dwa',0.0d0,500.0d0,dwa,7,ifnd)
          do i = 1, ifnd
            dwatab(i*2) = dwa(i)
            dwatab(i*2-1) = dvstage(i)
          end do
        endif

! -5-   timing - critical press. head or moist. content at sensor depth
!       minimum pressure head or water content
        if (tcs.eq.5) then
          call rdsinr ('phormc',0,1,phormc)
          call rdsdor ('dcrit',-100.0d0,0.0d0,dcrit)
          call rdador('dvs_tc5',0.0d0,2.0d0,dvstage,7,ifnd)
          if (phormc.eq.0) then
            call rdfdor('value_tc5',-1.0d6,100.0d0,hcri,7,ifnd)
            do i = 1, ifnd
              hcritab(i*2) = hcri(i)
              hcritab(i*2-1) = dvstage(i)
            end do
          else
            call rdfdor('value_tc5',0.0d0,1.0d0,tcri,7,ifnd)
            do i = 1, ifnd
              tcritab(i*2) = tcri(i)
              tcritab(i*2-1) = dvstage(i)
            end do
          endif
        endif


! -6-   Timing - fixed intervals, weekly with threshold (below it: no irrigation)
        if (tcs.eq.6) then
          call rdsdor ('irgthreshold',0.0d0,20.0d0,irgthreshold)
          dayfix = 6
        endif


! -7-   Timing - fixed intervals
        call rdsinr ('tcsfix',0,1,tcsfix)
        if (tcsfix.eq.1) then
          if (tcs.eq.6) then   
            messag = 'Timing criteria: conflict with fixed intervals'//
     &               ' tcsfix=1 AND tcs=1 not allowed, adapt input !'
            call fatalerr ('Irrigation',messag)
          endif
          call rdsinr('irgdayfix',1,366,irgdayfix)
        endif


! ---   depth criteria
        call rdsinr ('dcs',1,2,dcs)


! ---   field capacity as input for timing and depth options
        if (tcs.eq.2 .or. tcs.eq.3 .or. tcs.eq.4 .or. dcs.eq.1) then
          call rdsdor('phFieldCapacity',-500.0d0,0.0d0,phlo)
        else
          phlo = -100.0d0
        endif


! -1-   depth - back to field capacity
        if (dcs.eq.1) then
          call rdador('dvs_dc1',0.0d0,2.0d0,dvstage,7,ifnd)
          call rdfdor('di',-100.0d0,+100.0d0,di,7,ifnd)
          raithreshold = 0.0d0
          if(rdinqr('raithreshold')) then
            call rdsdor('raithreshold',0.0d0,1000.0d0,raithreshold)
          endif
          do i = 1, ifnd
            ditab(i*2) = di(i)
            ditab(i*2-1) = dvstage(i)
          end do
        endif

! -2-   depth - fixed depth
        if (dcs.eq.2) then
          call rdador('dvs_dc2',0.0d0,2.0d0,dvstage,7,ifnd)
          call rdfdor('fid',0.0d0,400.0d0,fid,7,ifnd)
          do i = 1, ifnd
            fidtab(i*2) = fid(i)
            fidtab(i*2-1) = dvstage(i)
          end do
        endif

! ---  depth - limited by min and max
        call rdsinr ('dcslim',0,1,dcslim)
        if (dcslim.eq.1) then
          call rdsdor('irgdepmin',0.0d0,100.0d0,irgdepmin)
          call rdsdor('irgdepmax',irgdepmin,1.0d7,irgdepmax)
        else
          irgdepmin = 0.0d0
          irgdepmax = 1.0d7
        endif

! -     close input file
        close(irr)

      endif


      return

2000  continue

! == = determine irrigation rates and states  =============================================

! --- reset intermediate soil water fluxes
      if (flzerointr) then
        igird = 0.0d0
        inird = 0.0d0
      endif

! --- reset cumulative soil water fluxes
      if (flzerocumu) then
        cgird = 0.0d0
        cnird = 0.0d0
      endif

      gird = 0.0d0
      irrigevent = 0

! --- fixed irrigations
      if (swirfix .eq. 1) then
        if (abs(irdate(nirri) - t1900) .lt. 1.d-3) then
          gird = irdepth(nirri)
          cirr = irconc(nirri)
          isua = irtype(nirri)
          nirri = nirri + 1
          irrigevent = 1
        endif
      endif

! --- scheduling mode - current timing and depth criterion

!     scheduled timing within desired period ?
      if (schedule.eq.1) then
        call dtdpar (cropstart(icrop),datea,fsec)
        yearstacrp = datea(1)
        call dtdpar (cropend(icrop),datea,fsec)
        yearendcrp = datea(1)
        flIrriTime = .false.
        if (yearendcrp.gt.yearstacrp) then
          datea(1) = yearstacrp
          datea(2) = startirr(2)
          datea(3) = startirr(1)        
          fsec = 0.0
          call dtardp (datea, fsec, tstairryrx)
          datea(1) = yearendcrp
          datea(2) = endirr(2)
          datea(3) = endirr(1)        
          call dtardp (datea, fsec, tendirryrx)
          if ( (t1900-tstairryrx).gt.1.0d-3 .and. 
     &         (t1900-tendirryrx).le.1.0d-3 ) then
            flIrriTime = .true.
          endif
        else
          if ((t-tstairrig).gt.1.0d-3.and.(t-tendirrig).le.1.0d-3) then
            flIrriTime = .true.
          endif
        endif
      endif

      if (schedule.eq.1 .and. irrigevent.eq.0 .and.
     &                        flCropGrowth .and. flIrriTime) then
        cirr = cirrs
        isua = isuas

! ---   determine lowest compartment containing roots
        i = 1
        do while ((z(i) - dz(i)*0.5d0) .gt. (-rd + 1.d-8))
          i = i + 1
        end do
        noddrz = i
        frlow = (z(noddrz) + dz(noddrz)*0.5d0 + rd) / dz(noddrz)

! ---   determine water holding capacity, readily available water, 
! ---   actual available water and water deficit
!        phlo = -100.0d0 ! field capacity became input (from swap32(6)
        phme = (hlim3l+hlim3h)/2.0d0
        phhi = hlim4
        awlh = 0.0d0
        awmh = 0.0d0
        awah = 0.0d0
        cdef = 0.0d0
        do node = 1,noddrz
          wclo = watcon(node,phlo,cofgen(1,node),swsophy,numtab,sptab) 
     &                                                        * dz(node)
          if (node.eq.noddrz) wclo = wclo*frlow
          wcme = watcon(node,phme,cofgen(1,node),swsophy,numtab,sptab)
     &                                                        * dz(node)
          if (node.eq.noddrz) wcme = wcme*frlow
          wchi = watcon(node,phhi,cofgen(1,node),swsophy,numtab,sptab)
     &                                                        * dz(node)
          if (node.eq.noddrz) wchi = wchi*frlow
          wcac = watcon(node,h(node),cofgen(1,node),swsophy,
     &                                          numtab,sptab) * dz(node)
          if (node.eq.noddrz) wcac = wcac*frlow
          awlh = awlh+(wclo-wchi)
          awmh = awmh+(wcme-wchi)
          awah = awah+(wcac-wchi)
          cdef = cdef+(wclo-wcac) 
        end do

! -1-   timing - allowable daily stress - only under dry stress circumstances
        if (tcs.eq.1) then
          tps1 = afgen(treltab,14,dvs)
! ---     transpiration fraction due to drought and salinity stress
          if (qrosum .gt. 1.d-8) then
            Tred = 1.0d0 - (qreddrysum+qredsolsum)/ptra
          else
            Tred = 1.0d0
          endif
          if (Tred .lt. tps1) irrigevent = 2
        endif

! -2-   timing - depletion of readily available water (fraction)
        if (tcs.eq.2) then
! ---     compare readily available water and actual available water
          tps2 = afgen(rawtab,14,dvs)
          depl = tps2*(awlh-awmh)
          if (depl.gt.awlh) depl=awlh 
          if (awah .lt. (awlh-depl)) irrigevent = 2
        endif

! -3-   timing - depletion of totally available water (fraction)
        if (tcs.eq.3) then
! ---     compare totally available water and actual available water
          tps3 = afgen(tawtab,14,dvs)
          depl = tps3*awlh
          if (awah.lt.(awlh-depl)) irrigevent = 2
        endif

! -4-   timing - allowable amount of depletion
        if (tcs.eq.4) then
! ---     check if depletion amount has been exceeded                
          tps4 = afgen(dwatab,14,dvs)
          if ((awlh-awah).gt.(tps4/10.d0)) irrigevent = 2
        endif

! -5-   timing - critical pressure head or moisture content exceeded
        if (tcs.eq.5) then
! ---     determine compartment number of sensor depth
          i = 1
          do while (z(i) .gt. (dcrit + 1.d-5))
            i = i+1
          end do
          nodsen = i

! ---     calculation of critical pressure head
          if (phormc.eq.0) then
            tps5 = afgen(hcritab,14,dvs)
! PG/JK start  15-feb-2010
! originally not intended to simulate paddy rice fields,
! but made applicable for paddy by changing the statement:
            phcrit = tps5        ! old statement was: phcrit = -abs(tps5)
! PG/JK end    15-feb-2010
          elseif (phormc.eq.1) then
            tps5 = afgen(tcritab,14,dvs)
            phcrit = prhead(nodsen,disnod(nodsen),cofgen,tps5,h,
     &                      swsophy,numtab,sptab)
          endif

! ---     compare critical pressure head and actual pressure head
          if (h(nodsen).le.phcrit) irrigevent = 2
        endif



! -6-   Timing - fixed irrigation time (weekly during crop growth)
        if (tcs.eq.6) then

!       (weekly) irrigation only when deficit is higher then threshold
!        cdef (cm) en IrgThreshold (mm)
          dayfix = dayfix + 1
          if (dayfix.eq.7)  then
            dayfix = 0
            if (10.0*cdef.gt.irgthreshold) then
              irrigevent = 2
            endif
          endif
        endif



! -7-   Timing - fixed intervals
        if (tcsfix.eq.1) then
          if(irrigevent.eq.2 .and. (dayfix .eq. irgdayfix)) then
             irrigevent = 2
             dayfix = 1
          else
             irrigevent = 0
             if (dayfix .lt. irgdayfix) dayfix = dayfix + 1
          endif
        endif


! ---   depth - back to field capacity [cm]
        if ((irrigevent.eq.2).and.(dcs.eq.1)) then
! ---     correct for over- or under irrigation
          dps1 = afgen(ditab,14,dvs)
! PG/JK start  15-feb-2010
! option to reduce irrigation on rainy (> raithreshold) day
! raithreshold =     ! threshold (cm/d) to define rainy days;  used to reduce irrigation
          grai_red = 0.0d0
          if(grai .gt. raithreshold) grai_red = grai
          gird = max (0.0d0,cdef+dps1/10.0d0-grai_red) 
! PG/JK start  15-feb-2010
        endif

! ---   depth - fixed depth [cm]
        if ((irrigevent.eq.2).and.(dcs.eq.2)) then
          dps2 = afgen(fidtab,14,dvs)
          gird = dps2/10.0d0
        endif

! ---   depth - limited depth [cm]
        if ((irrigevent.eq.2).and.(dcslim.eq.1)) then
          gird = max(gird,irgdepmin/10.0d0)
          gird = min(gird,irgdepmax/10.0d0)
        endif

      endif

      if (irrigevent .ne. 0) flIrrigationOutput = .true.

      return
      end
