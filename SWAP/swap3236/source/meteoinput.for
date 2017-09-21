! File VersionID:
!   $Id: meteoinput.for 193 2011-02-18 10:03:50Z kroes006 $
! ----------------------------------------------------------------------
      subroutine MeteoInput 
! ----------------------------------------------------------------------
!     Last modified      : August 2006              
!     Purpose            : returns meteorological fluxes of current day
! ----------------------------------------------------------------------
      use variables
      implicit none

! --- local
      integer   i,nofd,ifnd,task
      integer   detrecord(nmetfile),irecord,irectotal,first,last
      integer   count,getun2,wth
      integer   daynrfirst,daynrlast
      real*8    afgen,rainflux,netrainflux
      real*8    a,b,cofbb,hum,etr,sumtav
      real*8    rcs,rpd,svp,wfrac,win
      real*8    pfree,pstem,scanopy,avprec,avevap,psatcan,cGash
      real*8    dettav(nmetfile),detrad(nmetfile),dethum(nmetfile)
      real*8    detwind(nmetfile),detrain(nmetfile),dettime(nmetfile)
      real*8    arain(96),atav(96),awind(96),restint,interc
      real*8    arad(366),atmn(366),atmx(366),ahum(366),awin(366)
      real*8    arai(366),aetr(366),wet(366)
      real*8    aintc              ! Amount of rainfall interception during current day (L)
      real*8    raintime           ! Time from start of calendar year (T)

      character ext*3,messag*200,detdate*11, filnam*200

      save      arad,atmn,atmx,ahum,awin,arai,aetr,wet,irectotal,detrad,
     &          dethum,dettav,detwind,detrain,detrecord,dettime,
     &          daynrfirst,daynrlast

      data      rcs/0.15d0/
! ----------------------------------------------------------------------

! --- reset intermediate meteorological fluxes
      if (flzerointr) then
        iprec = 0.0d0
        igrai = 0.0d0
        inrai = 0.0d0
      endif

! --- reset cumulative meteorological fluxes
      if (flzerocumu) then
        cgrai = 0.0d0
        cnrai = 0.0d0
        caintc = 0.0d0
      endif

! --- two main parts: 1. for daily ET records; 2. for more detailed ET records
      if (swmetdetail.eq.1) then
        task = 2
      else
        task = 1
      endif

      goto (1000,2000) task

1000  continue

! === this part only in case of daily weather records of ET ==============

! --- next reading part only at start simulation and at new meteorological year
      if (abs(t1900-tstart) .lt. 1.d-12 .or. yearmeteo.ne.iyear) then

        call ReadMeteo(logf,swetr,swmeteo,swcalt,swrain,swsnow,swfrost,
     &              yearmeteo,pathatm,metfil, daynrfirst,daynrlast,
     &              arad,atmn,atmx,ahum,awin,arai,aetr,wet,raintab)

! --- for rain options 1 and 2: convert daily rain quantities and intensities or durations
!     into rain events by creating raintime and rainflux arrays conform option 3 
        if (swrain.eq.1 .or. swrain.eq.2) then
           raintimearray(1) = 0.d0 + tcum
           rainfluxarray(1) = 0.d0
           rainrec = 0
           do i = 1, (daynrlast-daynrfirst+1)
              if (arai(i).gt.1.d-7) then
                 if (swrain.eq.1) then  
! ---            mean rainfall intensities are specified
                    rainflux = afgen(raintab,60,dble(i)) 
                    raintime = dmin1(1.d0,arai(i)/rainflux)
                 elseif (swrain.eq.2) then
! ---            rainfall durations are specified
                    raintime = wet(i)
                 endif
                 if (i.eq.1) then
                    rainrec = rainrec + 1
                 else
                    rainrec = rainrec + 2
                    raintimearray(rainrec) = dble(i-1) + tcum
                    rainfluxarray(rainrec) = 0.d0
                 endif
                 raintimearray(rainrec+1)  = dble(i-1) + tcum + raintime
                 rainfluxarray(rainrec+1)  = 0.1d0 * arai(i) / raintime               
              endif
           enddo
! ---      extend array with records at end of simulation period
           raintimearray(rainrec+2) = tend + 1.d0 - tstart
           rainfluxarray(rainrec+2) = 0.d0
! ---      current rain record
           rainrec = 2
        elseif (swrain .eq. 3) then
! ---   rainfall events are specified
           call ReadRain("Meteoinput",logf,pathatm,rainfil,yearmeteo,
     &                   tcum,tend,tstart,rainamount,rainfluxarray,
     &                   raintimearray)
           rainrec = 2
        endif
      endif


! --- end of reading meteo data file *************************************************


! --- check whether meteo data are available of today
      if (daymeteo.lt.daynrfirst .or. daymeteo.gt.daynrlast) then
        messag ='In meteo file no meteo data are'//
     &  ' available for '//date//'. First adapt meteo file!'
        call fatalerr ('meteo',messag)
      end if

! --- the weather of today   
      rad = arad(daymeteo+1-daynrfirst)
      tmn = atmn(daymeteo+1-daynrfirst)
      tmx = atmx(daymeteo+1-daynrfirst)
      hum = ahum(daymeteo+1-daynrfirst)
      win = awin(daymeteo+1-daynrfirst)
      grai = arai(daymeteo+1-daynrfirst)
      etr = aetr(daymeteo+1-daynrfirst)

! --- calculate running average of minimum temperature ----------------- 
      nofd = min(daymeteo-daynrfirst+1, 7)
      tmnr = 0.0d0
      do i = daymeteo-nofd+1,daymeteo
        tmnr = tmnr + atmn(i+1-daynrfirst)/nofd
      end do

! --- if hum is missing or tav cannot be calculated: set rh at -99.0
      rh = 1.0d0
      if (hum.lt.-98.0d0.or.tmn.lt.-98.0d0.or.tmx.lt.-98.0d0) rh=-99.0d0

! --- convert radiation from kj/m2 to j/m2
      rad = rad*1000.0d0

! --- calculate 24h average temperature
      tav = (tmx+tmn)/2.0d0
! --- calculate average day temperature
      tavd = (tmx+tav)/2.0d0

      if (rh.ge.-98.0d0) then
! ---   calculate saturated vapour pressure [kpa]
        svp = 0.611*exp(17.4*tav/(tav+239.0d0))
! ---   calculate relative humidity [fraction]
        rh = min(hum/svp,1.0d0)
      endif
 
      if (swrain .lt. 3) then
! ---   gross rainfall in cm
        grai = grai/10.0d0
      else
! ---   determine daily rainfall amount from detailed rainfall data
        grai = 0.0d0
        i = rainrec 
        if (raintimearray(i) .gt. (tcum + 1.d0 - 1.d-5)) then
! ---     no measurements on current day
          grai = rainfluxarray(i) * 1.d0 ! multiplication by 1 day!
        else
          grai = (raintimearray(i) - tcum) * rainfluxarray(i)
          i = i + 1
          do while (raintimearray(i) .lt. (tcum + 1.d0 - 1.d-5))
            grai = grai + rainamount(i)
            i = i + 1
          enddo
! ---     last part of day gets rain flux of first event of next day
          grai = grai+(tcum+1.d0-raintimearray(i-1)) * rainfluxarray(i)
        endif
      endif

! --- determine whether the precipitations falls as rain or snow 
      if (swsnow.eq.1) then
        if (tav.gt.TePrRain) then
          gsnow = 0.0d0
        elseif (tav.lt.TePrSnow) then
          gsnow = grai 
        else
!   - distribution over snow and rain according to linear relation between
!     the two transition temperatures TePrRain and TePrSnow
          gsnow = grai * (TePrRain-tav) / (TePrRain-TePrSnow)  
        endif

! ---   rain fall rate on snowpack
        if (ssnow.gt.1.0d-6) then
          snrai = grai - gsnow
        else 
          snrai = 0.0d0
        endif 
! ---   fraction of precipitation that is not snow nor rain on snowpack
        if (grai.gt.0.d0) then
           fprecnosnow = 1.0d0 - (gsnow + snrai) / grai
        else
           fprecnosnow = 0.0d0
        endif
      else
        fprecnosnow = 1.0d0
      endif

! --- calculation of net rain & net irrigation depth [cm] --------------

      if ((lai .lt. 1.d-3) .or. (grai+gird .lt. 1.d-5) .or.
     &    (swinter.eq.0) .or. (gsnow.gt.0.0d0) .or.(ssnow.gt.0.0d0))then
! --- no vegetation, rainfall/irrigation or interception calculation
        aintc = 0.d0

      else if (swinter .eq. 1) then
! ---   calculate interception, method Von Hoyningen-Hune and Braden

        rpd = grai*10.0d0
        if (isua.eq.0) rpd = (grai+gird)*10.0d0

!       exponential relation between soil cover and lai
        cofbb = 1.0d0 - exp(-1.0d0*kdif*kdir*lai)
        cofbb = min(cofbb,1.0d0)
        if (cofab.gt.0.000001d0) then
          aintc = (cofab*lai*(1.0d0-(1/(1.0d0+rpd*cofbb/
     &    (cofab*lai)))))/10.0d0
        else
          aintc = 0.0d0
        endif

      else if (swinter .eq. 2) then
! ---   calculate interception for forests according to Gash (1995)
        pfree = afgen(pfreetb,(2*magrs),t)
        pstem = afgen(pstemtb,(2*magrs),t)
        cGash = 1.d0-pfree-pstem
        scanopy = afgen(scanopytb,(2*magrs),t) / cGash
        avprec = afgen(avprectb,(2*magrs),t)
        avevap = afgen(avevaptb,(2*magrs),t) / cGash

        if (isua.eq.0) then
          rpd = grai+gird
        else
          rpd = grai
        endif

        if ( (1.0d0 - avevap/avprec) .gt. 1.0d-4) then
          psatcan = -avprec*scanopy/avevap * 
     &              dlog(1.0d0 - avevap/avprec)
        else
          psatcan = avprec*scanopy/avevap
        endif

        if (grai .lt. psatcan) then
          aintc = cGash * rpd
        else
          aintc = cGash * ( psatcan +
     &            avevap*cGash / avprec * (rpd - psatcan) )
        endif
      end if

! --- divide interception into rain part and irrigation part
      if (aintc.lt.0.001d0) then
        nraida = grai - gsnow - snrai
        nird = gird
      else
         if (isua.eq.0) then
           nraida = grai-aintc*(grai/(grai+gird)) 
           nird = gird-aintc*(gird/(grai+gird)) 
         else 
           nraida = grai-aintc
           nird = gird
         endif
      endif

! ----------------------------------------------------------------------
! --- calculation of angstrom coefficients a and b
      a = 0.4885d0-0.0052*lat
      b = 0.1563d0+0.0074*lat

! --- calculate evapotranspiration: et0, ew0, es0

! --- reference evapotranspiration has been specified 
      if (swetr.eq.1) then
        if (flbaresoil) then
! ---   no crop ---------------------------------------
          et0 = 0.0d0
          ew0 = 0.0d0
          es0 = etr
          if (swcfbs.eq.1) es0 = cfbs*etr
        else
! ---   crop is present --------------------------------
          et0 = cf*etr
          ew0 = cf*etr
          es0 = etr
          if (swcfbs.eq.1) es0 = cfbs*etr
        endif   
! --- reference evapotranspiration must be calculated
      else
        irecord = 1
        call PenMon (logf,swscre,daymeteo,lat,alt,Altw,a,b,rcs,
     &        rad,tav,hum,win,rsc,es0,et0,ew0,swcf,ch,flbaresoil,
     &        daylp,flmetdetail,irecord,nmetdetail,albedo,tmn,tmx,rsw)
        if (flbaresoil) then
! ---   no crop ---------------------------------------
          if (swcfbs .eq. 1) es0 = cfbs*et0
          et0 = 0.0d0
          ew0 = 0.0d0
        else
! ---   crop is present --------------------------------
          if (swcfbs .eq. 1) es0 = cfbs*et0
          if (swcf.eq.1) then 
            et0 = cf*et0
            ew0 = cf*ew0
          endif
        endif
      endif

! --- et0, ew0 and es0 are always defined !

! --- calculate atmospheric demand [cm]
      atmdem = et0/10.0d0

! --- calculate fraction of the day the crop is wet
      if (ew0.lt.0.0001d0) then 
        wfrac = 0.0d0
      else
        wfrac = max(min(aintc*10.0d0/ew0,1.0d0),0.0d0)
      endif

! --- potential soil evap. (peva) & transpiration (ptra) [cm]
      peva = max (0.0d0,es0*exp(-1.0d0*kdir*kdif*lai)/10.0d0 *
     &                    (1.0d0-wfrac) )
! --- adapt peva in case of ponding
      if (pond .gt. 1.0d-10) then
        if (SwETr .eq. 0 .and. es0 .gt. 1.0d-8) then
          peva = ew0/es0 * peva
        elseif (es0 .gt. 1.0d-8) then
          peva = cfevappond * peva
        endif
      endif
      ptra = max (1.0d-9,(1.0d0-wfrac)*et0-peva*10.0d0)/10.0d0

! --- alternative (simple model, soil cover fraction specified)
      if (.not.flbaresoil) then
        if (croptype(icrop).eq.1 .and. swgc.eq.2) then
          peva = (1.0d0-gc)*(1.0d0-wfrac)*es0/10.0d0
! ---     adapt peva in case of ponding
          if (pond .gt. 1.0d-10) then
            if (SwETr .eq. 0 .and. es0 .gt. 1.0d-8) then
              peva = ew0/es0 * peva
            elseif (es0 .gt. 1.0d-8) then
              peva = cfevappond * peva
            endif
          endif
          ptra = max(1.0d-9,(1.0d0-wfrac)*et0)/10.0d0 - peva
          ptra = max(1.0d-9,ptra)
        endif
      endif

! ==== Actual rainflux ====================================================

      if (nraida.gt.1.d-5) then
        finterception = nraida / grai
        if (aintc.lt.1.0d-5) finterception = 1.0d0
! --- just daily values
        if (swrain.eq.0) then
           rainflux = fprecnosnow * grai

! --- rainfall intensities or duration
        else
           rainflux = fprecnosnow * rainfluxarray(rainrec)
        endif
        netrainflux = finterception * rainflux
! ---   set actual gross and net rainflux, and interception on TIMESTEP basis
        graidt  = rainflux
        nraidt  = netrainflux
        aintcdt = rainflux - netrainflux
      else
        finterception = 1.0d0
        graidt  = 0.0d0
        nraidt  = 0.0d0
        aintcdt = 0.0d0
      endif

! === actual soil evaporation rate of today ======================================
      if (.not. fletsine) then
        call reduceva (1,swredu,empreva,peva,nraida,nird,
     &          cofred,rsigni,swinco,ldwet,spev,saev,fldaystart,dt,pond)
      endif

! --- save daily potential values for use in ETSine
      ptraday = ptra
      pevaday = peva

      return

2000  continue

! === this part only in case of detailed weather records of ET ==============

! --- next reading part only at start simulation and at new meteorological year
      if (abs(t1900-tstart) .lt. 1.d-12 .or. yearmeteo.ne.iyear) then

! ---   compose filename meteorological file
        write (ext,'(i3.3)') mod(yearmeteo,1000)
        filnam = trim(pathatm)//trim(metfil)//'.'//trim(ext)

! ---   initialise and start reading
        wth = getun2 (10,90,2)
        call rdinit(wth,logf,filnam)

! ---   get values from file
        call rdatim ('date',dettime,nmetfile,ifnd)
        call rdfinr ('record',1,nmetdetail,detrecord,nmetfile,ifnd)
        call rdfdor ('rad',-1.0d5,5.0d6,detrad,nmetfile,ifnd)
        call rdfdor ('temp',-50.d0,60.d0,dettav,nmetfile,ifnd)
        call rdfdor ('hum',0.0d0,1.0d5,dethum,nmetfile,ifnd)
        call rdfdor ('wind',0.0d0,1.0d5,detwind,nmetfile,ifnd)
        call rdfdor ('rain',0.0d0,1000.d0,detrain,nmetfile,ifnd)

! ---   close meteorological file
        close (wth)

! ---   initialize total record number for new weather file
        irectotal = int(t1900-dettime(1)+0.1d0)*nmetdetail

! ---   initialize number of days for running average Tmin
        nofd = 0

      endif
! --- end of reading meteo data file *************************************************

! --- read and check weather records of today
      do i = 1, nmetdetail
        irectotal = irectotal + 1
        arad(i) = detrad(irectotal)
        ahum(i) = dethum(irectotal)
        atav(i) = dettav(irectotal)
        awind(i) = detwind(irectotal)
        arain(i) = detrain(irectotal) / 10.d0
        if (i .ne. detrecord(irectotal)) then
          messag ='In meteo file '//trim(filnam)//' record number(s)'//
     &    ' are not correct at '//date//'. First adapt meteo file!'
          call fatalerr ('meteo',messag)
        end if
        call dtdpst('day-monthst-year',dettime(irectotal)+0.1d0,detdate)
        call dtdpst('day-monthst-year',t1900+0.1d0,date)
        if (detdate .ne. date) then
          messag ='In meteo file '//trim(filnam)//' the amount of '//
     &    'records deviate near '//date//'. First adapt meteo file!'
          call fatalerr ('meteo',messag)
        end if
      enddo

! --- total amount of rainfall (cm) today
      grai = 0.0d0
      do i = 1, nmetdetail
        grai = grai + arain(i)
      enddo

! --- remaining amount of interception
      restint = 0.0d0

! --- omit rain fall rate on snowpack
      snrai = 0.0d0

! --- calculation of net rain & net irrigation depth [cm] --------------

      if ((lai .lt. 1.d-3) .or. (grai+gird .lt. 1.d-5) .or.
     &    (swinter.eq.0)) then
! --- no vegetation, rainfall/irrigation or interception calculation
        aintc = 0.d0

      else if (swinter .eq. 1) then
! ---   calculate interception, method Von Hoyningen-Hune and Braden

        rpd = grai*10.0d0
        if (isua.eq.0) rpd = (grai+gird)*10.0d0

!       exponential relation between soil cover and lai
        cofbb = 1.0d0 - exp(-1.0d0*kdif*kdir*lai)
        cofbb = min(cofbb,1.0d0)
        if (cofab.gt.0.000001d0) then
          aintc = (cofab*lai*(1.0d0-(1/(1.0d0+rpd*cofbb/
     &    (cofab*lai)))))/10.0d0
        else
          aintc = 0.0d0
        endif

      else if (swinter .eq. 2) then
! ---   calculate interception for forests according to Gash (1995)
        pfree = afgen(pfreetb,(2*magrs),t)
        pstem = afgen(pstemtb,(2*magrs),t)
        cGash = 1.d0-pfree-pstem
        scanopy = afgen(scanopytb,(2*magrs),t) / cGash
        avprec = afgen(avprectb,(2*magrs),t)
        avevap = afgen(avevaptb,(2*magrs),t) / cGash

        if (isua.eq.0) then
          rpd = grai+gird
        else
          rpd = grai
        endif

        if ( (1.0d0 - avevap/avprec) .gt. 1.0d-4) then
          psatcan = -avprec*scanopy/avevap * 
     &              dlog(1.0d0 - avevap/avprec)
        else
          psatcan = avprec*scanopy/avevap
        endif

        if (grai .lt. psatcan) then
          aintc = cGash * rpd
        else
          aintc = cGash * ( psatcan +
     &            avevap*cGash / avprec * (rpd - psatcan) )
        endif
      end if

! --- divide interception into rain part and irrigation part
      if (aintc.lt.0.001d0) then
        nraida = grai - snrai
        nird = gird
      else
         if (isua.eq.0) then
           nraida = grai-aintc*(grai/(grai+gird)) 
           nird = gird-aintc*(gird/(grai+gird)) 
         else 
           nraida = grai-aintc
           nird = gird
         endif
      endif

! --- calculation of ängstrom coefficients a and b
      a = 0.4885d0-0.0052*lat
      b = 0.1563d0+0.0074*lat

! --- evapotranspiration rates for each weather record of today
      do irecord = 1, nmetdetail

! ---   define weather variables of current record
        rad = arad(irecord)
        tav = atav(irecord)
        hum = ahum(irecord)
        win = awind(irecord)

! ---   convert radiation from kj/m2/period to j/m2/d
        rad = rad * 1000.0d0 * nmetdetail
 
! ---   calculate evapotranspiration: et0, ew0, es0 (mm/d)
        call PenMon (logf,swscre,daymeteo,lat,alt,Altw,a,b,rcs,
     &       rad,tav,hum,win,rsc,es0,et0,ew0,swcf,ch,flbaresoil,
     &       daylp,flmetdetail,irecord,nmetdetail,albedo,tmn,tmx,rsw)
        if (flbaresoil) then
! ---   no crop ---------------------------------------
          if (swcfbs .eq. 1) es0 = cfbs*et0
          et0 = 0.0d0
          if (swcf.eq.1) ew0 = cf*ew0
        else
! ---   crop is present --------------------------------
          if (swcfbs .eq. 1) es0 = cfbs*et0
          if (swcf.eq.1) then 
            et0 = cf*et0
            ew0 = cf*ew0
          endif
        endif

! ---   calculate fraction of the period the crop is wet
        if (grai .lt. 1.0d-12) then
          wfrac = 0.0d0
        else
          interc = restint + aintc * arain(irecord) / grai
          if (ew0.lt.0.0001d0) then 
            wfrac = 0.0d0
          else
            wfrac = max(min(interc*10.0d0/ew0/metperiod,1.0d0),0.0d0)
          endif
        endif

! ---   remaining amount of interception
        restint = max(interc - wfrac * metperiod * ew0 / 10.d0, 0.d0)

! ---   potential soil evap. (peva) & transpiration (ptra) [cm/d]
        peva = max ( 0.0d0, (es0*exp(-1.0d0*kdir*kdif*lai)/10.0d0)* 
     &                      (1.0d0-wfrac) )
! ---   adapt peva in case of ponding
        if (pond .gt. 1.0d-10) then
          if (SwETr .eq. 0 .and. es0 .gt. 1.0d-8) then
            peva = ew0/es0 * peva
          elseif (es0 .gt. 1.0d-8) then
            peva = cfevappond * peva
          endif
        endif
        ptra = max (1.0d-9,(1.0d0-wfrac)*et0-peva*10.0d0)/10.0d0

! ---   alternative (simple model, soil cover fraction specified)
        if (.not.flbaresoil) then
          if (croptype(icrop).eq.1 .and. swgc.eq.2) then
            peva = (1.0d0-gc)*(1.0d0-wfrac)*es0/10.0d0
! ---       adapt peva in case of ponding
            if (pond .gt. 1.0d-10) then
              if (SwETr .eq. 0 .and. es0 .gt. 1.0d-8) then
                peva = ew0/es0 * peva
              elseif (es0 .gt. 1.0d-8) then
                peva = cfevappond * peva
              endif
            endif
            ptra = max(1.0d-9,gc*(1.0d0-wfrac)*et0)/10.0d0
          endif
        endif

! ---   result of detailed weather records (cm/d)
        tpot(irecord) = ptra
        epot(irecord) = peva
        if (grai .lt. 1.0d-12) then
          grain(irecord) = 0.0d0
          nrain(irecord) = 0.0d0
        else
          grain(irecord) = arain(irecord) * nmetdetail
          nrain(irecord) = arain(irecord) * nmetdetail * nraida / grai
        endif
      enddo

! === set daily weather values ==============================================

! --- average temperature of today
      sumtav = 0.d0
      do i = 1, nmetdetail
        sumtav = sumtav + atav(i)
      enddo
      tav = sumtav / nmetdetail

! --- calculate saturated vapour pressure [kpa]
      svp = 0.611*exp(17.4*tav/(tav+239.0d0))

! --- calculate relative humidity [fraction]
      rh = min(hum/svp,1.0d0)

! --- average temperature between 6 and 18 hour
      sumtav = 0.d0
      count = 0
      first = int(0.25*nmetdetail) + 1
      last = int(0.75*nmetdetail)
      do i = first, last
        sumtav = sumtav + atav(i)
        count = count + 1
      enddo
      tavd = sumtav / count

! --- daily radiation (J/m2/d) and atmospheric demand (cm/d)
      rad = 0.d0
      atmdem = 0.d0
      do i = 1,nmetdetail
        rad = rad + arad(i) * 1000.d0
        atmdem = atmdem + tpot(i)
      enddo

! --- calculate running average of minimum temperature ----------------- 
      nofd = min (nofd+1, 7)
      tmnr = 0.0d0
      i = irectotal - last
      do count = 1,nofd
        tmnr = tmnr + dettav(i)/nofd
        i = i - nmetdetail
      end do

! === fluxes of current time step (start of the day) ======================
      ptra = tpot(1)
      peva = epot(1)
      graidt = grain(1)
      nraidt = nrain(1)
      aintcdt = graidt - nraidt

      return
      end subroutine MeteoInput


! ----------------------------------------------------------------------
      subroutine ETSine
! ----------------------------------------------------------------------
!     Last modified      : oktober 2008
!     Purpose            : distributes potential transpiration and evaporation
!                          according to sine wave during photoperiodic daylight
! ----------------------------------------------------------------------
      use variables
      implicit none

! --- local
      real*8    daytime,pi,dayl,sinld,cosld,dsinb,dsinbe,dso,fraction
      real*8, save  :: tsunrise, tsunset
      data      pi/3.14159265d0/    ! number pi [-]

      if (fldaystart) then
! ---   determine duration photoperiodic daylight in hours
        call astro(logf,swscre,daymeteo,lat,
     &               dayl,daylp,sinld,cosld,dsinb,dsinbe,dso)
! ---   determine tsunrise, tsunset and daytime
        tsunrise = 0.5d0 - daylp / 48.d0
        tsunset = 0.5d0 + daylp / 48.d0
      endif

! --- set time as fraction of the day
      daytime = t1900 + dt - int(t1900)

! --- determine fraction of fluxes according to sine wave during this time step
      if (daytime.lt.tsunrise) then
        fraction = 0.d0
      elseif (daytime.gt.tsunrise .and. (daytime-dt).lt.tsunrise) then
        fraction = 0.5d0 * (cos(pi/2.d0 + (tsunrise - 0.5d0)/
     &     (tsunset-tsunrise)*pi) - cos(pi/2.d0 + (daytime - 0.5d0)/
     &     (tsunset-tsunrise)*pi))
      elseif ((daytime-dt).gt.tsunrise .and. (daytime).lt.tsunset) then
        fraction = 0.5d0 * (cos(pi/2.d0 + (daytime - dt - 0.5d0)/
     &     (tsunset-tsunrise)*pi) - cos(pi/2.d0 + (daytime - 0.5d0)/
     &     (tsunset-tsunrise)*pi))
      elseif (daytime.gt.tsunset .and. (daytime-dt).lt.tsunset) then
        fraction = 0.5d0 * (cos(pi/2.d0 + (daytime - dt - 0.5d0)/
     &     (tsunset-tsunrise)*pi) - cos(pi/2.d0 + (tsunset - 0.5d0)/
     &     (tsunset-tsunrise)*pi))
      else
        fraction = 0.d0
      endif

! --- set E and T fluxes
      peva = pevaday * fraction / dt
      ptra = ptraday * fraction / dt

! --- actual soil evaporation rate of current moment 
      call reduceva (2,swredu,empreva,peva,nraida,nird,
     &        cofred,rsigni,swinco,ldwet,spev,saev,fldaystart,dt,pond)

      return
      end subroutine ETSine

! ----------------------------------------------------------------------
      subroutine ReadMeteo(logf,swetr,swmeteo,swcalt,swrain,swsnow,
     &            swfrost,yearmeteo,pathatm,metfil,daynrfirst,daynrlast,
     &            arad,atmn,atmx,ahum,awin,arai,aetr,wet,raintab)
! ----------------------------------------------------------------------
!     Last modified      : January 2007
!     Purpose            : read meteorological data of one calendar year
! ----------------------------------------------------------------------
      implicit none
      include 'arrays.fi'

! --- global
      character metfil*(*),pathatm*(*)
      integer   logf,swetr,swmeteo,swcalt,swrain,swsnow,swfrost
      integer   daynrfirst,daynrlast,yearmeteo
      real*8    arad(366),atmn(366),atmx(366),ahum(366),awin(366)
      real*8    arai(366),aetr(366),wet(366),raintab(60)

! --- local
      character messag*400, filnam*200, datedum*11, ext*3
      character*80 station(366)
      integer   wth,getun2,ifnd,i,datea(6),daynumber
      integer   ad(mrain),am(mrain)
      real*4    fsec
      real*8    timjan1,tmeteo,RainIntensity,afgen
      real*8    radmin,radmax,tmnmin,tmnmax,tmxmin,tmxmax,hummin
      real*8    hummax,winmin,winmax,raimin,raimax,etrmin,etrmax


! --- detection & handling of missing values ---------------------------
!       default ranges of parameter values
      radmin = -1.0d5
      radmax = 5.0d6
      tmnmin = -1.0d5
      tmnmax = 1.0d5
      tmxmin = -1.0d5
      tmxmax = 1.0d5
      hummin = -1.0d5
      hummax = 1.0d5
      winmin = -1.0d5
      winmax = 1.0d5
      raimin = 0.0d0
      raimax = 1.0d3
      etrmin = -1.0d5
      etrmax = 1.0d5
! --- no missing values allowed if penmon must be executed
      if (swetr.eq.0) then
        radmin = 0.0d0
        tmnmin = -50.0d0
        tmnmax = 35.0d0
        tmxmin = -50.0d0
        tmxmax = 60.0d0
        hummin = 0.0d0
        hummax = 10.0d0
        winmin = 0.0d0
        winmax = 50.0d0
      endif
! --- error in case etref missing
      if (swetr.eq.1) then
        etrmin = -0.00001d0
        etrmax = 1.0d2
      endif  
! --- no missing values for tmn and tmx allowed if crop development or
! ---   numerical soil temperatures must be simulated
      if (swmeteo.eq.2 .or. swcalt.eq.2) then
        tmnmin = -60.0d0
        tmnmax = 50.0d0
        tmxmin = -50.0d0
        tmxmax = 60.0d0
      endif
! --- no missing value for rad allowed in case the detailed crop model
! ---   or the grass routine is active 
      if (swmeteo .eq. 2) then
        radmin = 0.0d0
        radmax = 5.0d6
      endif
! --- end of handling missing values -----------------------------------

! --- compose filename meteorological file
      write (ext,'(i3.3)') mod(yearmeteo,1000)
      filnam = trim(pathatm)//trim(metfil)//'.'//trim(ext)

! --- initialise and start reading
      wth = getun2 (10,90,2)
      call rdinit(wth,logf,filnam)

! --- get values from file
      call rdacha ('station',station,366,ifnd)
      call rdfinr ('dd',1,31,ad,366,ifnd)
      call rdfinr ('mm',1,12,am,366,ifnd)
      call rdfdor ('rad',radmin,radmax,arad,366,ifnd)
      call rdfdor ('tmin',tmnmin,tmnmax,atmn,366,ifnd)
      call rdfdor ('tmax',tmxmin,tmxmax,atmx,366,ifnd)
      call rdfdor ('hum',hummin,hummax,ahum,366,ifnd)
      call rdfdor ('wind',winmin,winmax,awin,366,ifnd)
      if (swrain.lt.3) call rdfdor('rain',raimin,raimax,arai,366,ifnd)
      call rdfdor ('etref',etrmin,etrmax,aetr,366,ifnd)
      if (swrain.eq.2) call rdfdor('wet',0.d0,1.d0,wet,366,ifnd)

! --- close meteorological file
      close (wth)

! --- determine first and last day numbers        
      datea(1) = yearmeteo
      datea(2) = 1
      datea(3) = 1
!     PWB: initialized dataa(4) = datea(5) = datea(6) = 0
      datea(4) = 0
      datea(5) = 0
      datea(6) = 0
      fsec = 0.0
      call dtardp (datea,fsec,timjan1)
      datea(2) = am(1)
      datea(3) = ad(1)
      call dtardp (datea,fsec,tmeteo)
      daynrfirst = nint ( tmeteo - timjan1 + 1.0d0 )
      datea(2) = am(ifnd)
      datea(3) = ad(ifnd)
      call dtardp (datea,fsec,tmeteo)
      daynrlast = nint ( tmeteo - timjan1 + 1.0d0 )

! --- check date 
      do i = 2, ifnd-1
          datea(2) = am(i)
          datea(3) = ad(i)
          call dtardp (datea,fsec,tmeteo)
          daynumber = nint ( tmeteo - timjan1 + 1.0d0 )
          if (daynumber .ne. daynrfirst+i-1) then 
!           wrong date after daynumber i-1
            datea(2) = am(i-1)
            datea(3) = ad(i-1)
            call dtardp (datea,fsec,tmeteo)
            call dtdpst ('day-monthst-year',tmeteo,datedum)            
            messag ='In meteo file '//trim(filnam)//' the date after '//
     &      datedum//' is not correct! First adapt meteo file!'
            call fatalerr ('meteo',messag)
          endif
      end do
! --- snow and frost calculation conditions require realistic air temperatures
      do i = 1, ifnd
          if ( (swsnow.eq.1.or.swfrost.eq.1) .and. 
     &         (atmn(i).lt.-98.9d0.or.atmx(i).lt.-98.9d0) ) then    
            messag ='In meteo file '//trim(filnam)//' temperatures'//
     &        ' must be input to calculate snow conditions (SWSNOW=1)'//
     &        ' and/or frost conditions (SWFROST=1)! adapt meteo file.'
            call fatalerr ('meteo',messag)
          endif
      enddo
! --- realistic air temperatures are required when soil temp. is simulated 
!     using the numerical model  (not if data from other file 
!! requires additional verification)
!      if (swhea.eq.1 .and. swcalt.eq.2) then
!        if (atmn(i).lt.-98.9d0 .or. atmx(i).lt.-98.9d0) then
!            messag ='In meteo file realistic temperatures are '//
!     &        ' required for simulation of temperature profiles (SWHEA=1) using'//
!     &        'num.model (SWCALT=2)!  Adapt meteo data! '
!            call fatalerr ('meteo',messag)
!        endif
!      endif


! --- in case of mean rain fluxes (swrain = 1), calculate wet fraction of the day
! --- this is required for the first time step conditions in TimeControl
      if (swrain .eq. 1) then
        do i = 1, ifnd
          RainIntensity = afgen(raintab,60,dble(i))
          if (arai(i) .lt. 1.d-10) then 
            wet(i) = 0.d0
          else
            wet(i) = min(1.0d0,(arai(i)/RainIntensity))
          endif  
        enddo
      endif
! --- in case of swrain=2 then make sure that there Rain and Wet correspond
      if (swrain.eq.2) then
        do i = 1, ifnd
          if ((arai(i).gt.1.d-10 .and. wet(i).lt.1.d-10) .or.
     &        (arai(i).lt.1.d-10 .and. wet(i).gt.1.d-10) ) then
            write(messag,1001) trim(filnam), i
 1001       format(' In meteo file ',a,' rec=',i4,'  SwRain=2'//
     &        ' Rain and Wet donot correspond.  Adapt meteo data! ')
            call fatalerr ('ReadMeteo',messag)
          endif  
        enddo
      endif

      return
      end subroutine ReadMeteo


! ----------------------------------------------------------------------
      subroutine ReadRain(callroutine,logf,pathatm,rainfil,yearmeteo,
     &                    tcum,tend,tstart,rainamount,rainfluxarray,
     &                    raintimearray)
! ----------------------------------------------------------------------
!     Last modified      : January 2007
!     Purpose            : read rainfall data (events) of one calendar year 
!     Interface:
!       I   - logf,pathatm,rainfil,yearmeteo,tcum,tend,
!       O   - rainamount,rainfluxarray,raintimearray,rainrec
! ----------------------------------------------------------------------
      implicit none
      include 'arrays.fi'

! --- global
      character rainfil*(*),pathatm*(*),callroutine*(*)
      integer   logf,yearmeteo
      real*8    tcum               ! Time cumulative since start simulation (T)
      real*8    tend               ! End date of simulation run
      real*8    tstart             ! End date of simulation run
      real*8    rainamount(mrain)  ! Array with short duration rainfall amounts (L)
      real*8    rainfluxarray(mrain) ! Array with short duration rainfall intensities (L/T)
      real*8    raintimearray(mrain) ! Array with times (T) at which rainfall intensity changes
      integer   rainrec            ! Actual rain record when detailed rainfall are used from separate rain file

! --- local
      character messag*300, filnam*200, ext*3
      character chday*2, chmonth*2, chtime*5
      integer   getun2,ifnd,datea(6),pre,i,numneg, nxtr
      integer   ad(mrain),am(mrain),ay(mrain)
      real*4    fsec, time4(mrain)
      real*8    rattim, tday, vsmall
      vsmall = 1.0d-7


! --- in case of detailed rainfall, open file once a year
      write (ext,'(i3.3)') mod(yearmeteo,1000)
      filnam = trim(pathatm)//trim(rainfil)//'.'//trim(ext)

! --- initialise and start reading
      pre = getun2 (10,90,2)
      call rdinit(pre,logf,filnam)

! --- get values from file
      call rdainr ('day',1,31,ad,mrain,ifnd)
      call rdfinr ('month',1,12,am,mrain,ifnd)
      call rdfinr ('year',1,3000,ay,mrain,ifnd)
      call rdfdor('time',0.0d0,1.0d0,raintimearray,mrain,ifnd)
      call rdfdor('amount',0.0d0,10000.0d0,rainamount,mrain,ifnd)

      close (pre)
! --- convert year, day, month and fraction of day to absolute time
      numneg = 0
      fsec = 0.0
      do i = 1,ifnd
         datea(1) = ay(i)
         datea(2) = am(i)
         datea(3) = ad(i)
         call dtardp (datea,fsec,tday)
         time4(i)         = real(raintimearray(i))
         raintimearray(i) = tday - tstart + raintimearray(i)
! --- negative rain day number when tday < tstart
         if (raintimearray(i).lt.0.d0) then
            numneg = numneg + 1
         endif
      end do
! --- if negative rain day numbers: repair
      if (numneg.gt.0) then
         rattim = raintimearray(numneg+1) / 
     &            (raintimearray(numneg+1) - raintimearray(numneg)) 
         if (raintimearray(numneg+1).gt.tcum+1.d-4) then
            nxtr = 1
            raintimearray(1) = tcum
            rainamount(1)    = 0.d0
         else
            nxtr = 0
         endif
         do i = numneg+1, ifnd
            raintimearray(i+nxtr-numneg) = raintimearray(i)
            rainamount(i+nxtr-numneg)    = rainamount(i)
         enddo
         ifnd = ifnd + nxtr - numneg
         if (nxtr.eq.1) rainamount(2) = rattim * rainamount(2) 
      endif
! --- check time of first rain record
      if (raintimearray(1) .gt. (tcum+1.d-4)) then
         messag ='Error-message from module '//trim(callroutine)//
     &        'In detailed rain file '//trim(filnam)//
     &        ' the time of the first rainrecord should be smaller'//
     &        ' or equal to time start simulation! Adapt rain file .'
         call fatalerr ('ReadRain',messag)
      endif
! --- check second rain record: rainamount first time = 0 and second = not 0: 
!     introduce one extra record
      if (raintimearray(2) .gt. tcum+1.d-4 .and. rainamount(2).gt.0.d0) 
     &                                                              then
         do i = ifnd, 2, -1
            raintimearray(i+1) = raintimearray(i)
            rainamount(i+1)    = rainamount(i)
         enddo
         raintimearray(2) = raintimearray(1) + 1.d-6
         rainamount(2)    = 0.d0
         ifnd = ifnd + 1
      endif
! --- convert rainfall amounts from mm to cm
      do i = 1,ifnd
         rainamount(i) = rainamount(i) / 10.d0
      end do
! --- extend array with records at end of simulation period
      raintimearray(ifnd+1) =                                           
     &                 dmax1(tend+1.1d0-tstart,raintimearray(ifnd)+1.d0)
      rainamount(ifnd+1) = 0.d0

! --- determine array with rainfluxes (cm/d)
      do i = 2,ifnd+1
         if ( (raintimearray(i)-raintimearray(i-1)) .lt. vsmall) then
            write(chmonth,'(i2)')  am(i)
            write(chday,'(i2)')    ad(i)
            write(chtime,'(f5.3)') time4(i)
           messag ='Error-message from module '//trim(callroutine)//
     &        '. In detailed rain file '//trim(filnam)//
     &        ' the time of a rainrecord should be greater than the'//
     &        ' time of its preceding rainrecord! Adapt rain file.'//
     &        '      Month: '//chmonth//'; Day: '//chday//'; Time: '//
     &        chtime//'!'
            call fatalerr ('ReadRain',messag)
         endif
         rainfluxarray(i) = rainamount(i) / 
     &                         (raintimearray(i)-raintimearray(i-1))
      enddo

! --- determine current rain record

      return
      end subroutine ReadRain
