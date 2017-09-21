! File VersionID:
!   $Id: readswap.for 205 2011-11-07 12:05:07Z kroes006 $
! ----------------------------------------------------------------------
      subroutine readswap
! ----------------------------------------------------------------------
!     Date               : Aug 2004   
!     Purpose            : read main input file .SWP
! ----------------------------------------------------------------------
      use variables
      implicit none

!     PWB (11 feb 2009) changed type of posarg and numchar to standard
!     (32 bit) integer to make them compatible with get_command_argument
!      integer*2 posarg,numchar
      integer posarg, numchar
      integer swp,i,datea(6),getun,getun2,runf,swrunon
      integer swirgfil,datefix(2),ifnd,swyrvar,swmonth
      integer swerror,swbbcfile
      integer lay,swsp,swdc,ini,tss
      integer isublay(macp),bbc,il, sol

      real*4  fsec
      real*8  outdate1,rainflux(30),raintime(30)
      real*8  ores(maho),osat(maho),alfa(maho),npar(maho)
      real*8  maxdepth,lexp(maho),alfaw(maho)
      real*8  dates(mabbc),gwlevel(mabbc)
      real*8  haquif(mabbc),hbottom(mabbc),ttop(mabbc)
      real*8  hhtab(mabbc),qhtab(mabbc),qboti(mabbc),tbot(mabbc)
      real*8  hthr,help,term1
      real*8  headtab(matab),thetatab(matab),conductab(matab)
      real*8  dydx(matab),pondmxtb(mairg),datepmx(mairg)
      character*200 filenamesophy(maho)
      character swpfilnam*400,filnam*200,inifil*200,filtext*80,rufil*200
      character bbcfil*32,irgfil*32,swpfil*32,tsoilfile*32
      character tmp*11,messag*400

      logical   toscr, flsat, rdinqr


!     pressure head where interpolation is used to calculate K from VG and ksatexm
      data hthr  /-2.0d0/
! ----------------------------------------------------------------------

! --- delete existing file Swap.ok
      call delfil ('Swap.ok',.false.)

! --- write message running to screen
      write (*,'(/,a)') '  running swap ....'

! --- path and filename of executable through argument command line
!     PWB (11 feb 2009) replace getarg() with standard fortran2003 syntax.
!     get_command_argument() 
      PosArg = 1
!      Call Get_Command_Argument (PosArg,swpfil,NumChar)  F95
      Call GetArg (PosArg,swpfil)
      NumChar = len_trim(swpfil)
      if (NumChar.lt.1) swpfil = 'swap'
      project = swpfil

      if(NumChar.gt.3) then
         if(swpfil(NumChar-3:NumChar).eq.'.swp' .or.
     &                       swpfil(NumChar-3:NumChar).eq.'.SWP') then
            swpfilnam = trim(swpfil)
         else
            swpfilnam = trim(swpfil)//'.swp'
         end if
      else 
        swpfilnam = trim(swpfil)//'.swp'
      end if


! --- open log file
      logf = getun (20,99)
      call fopens(logf,'swap.log','new','del')

! --  write head to logfile
      filtext = 'Main input variables'
      call writehead(logf,1,swpfilnam,filtext,project)
      write (logf,*)
      
! --- open swp file
      swp = getun2 (10,90,2)
      call rdinit(swp,logf,swpfilnam)

! -   environment
      call rdscha ('project',project)
      call rdscha ('pathwork',pathwork)
      call rdscha ('pathatm',pathatm)
      call rdscha ('pathcrop',pathcrop)
      call rdscha ('pathdrain',pathdrain)
      call rdsinr ('swscre',0,2,swscre)
      call rdsinr ('swerror',0,1,swerror)
      if (swerror .eq. 1) then
        toscr = .true.
      else
        toscr = .false.
      endif
      call messini(toscr,.true.,logf)

! --  Shared simulation
      flSwapShared = .false.
      if(rdinqr('flSwapShared')) then
        call rdslog ('flSwapShared',flSwapShared)
        if(flSwapShared) then
          messag = 'Simulation with shared data exchange (flSwapShared)'
          call warn ('Readswap',messag,logf,swscre)
        endif
      endif

! -   simulation period
      call rdstim('tstart',tstart)
      call dtdpar (tstart+0.1d0, datea, fsec)
      iyear = datea(1)
      imonth = datea(2)
      call dtdpar (tstart, datea, fsec)
      call rdstim('tend',tend)

! -   check begin and end date of simulation
      if ((tend - tstart) .lt. 0.0d0) then
        messag = 'The end date of the simulation'//
     &                    ' should be larger than the begin date!'
        call fatalerr ('readswap',messag)
      endif

! -   Output dates for balances
      call rdsinr ('swyrvar',0,1,swyrvar)
      if (swyrvar .eq. 0) then 
        call rdfinr('datefix',1,31,datefix,2,2)
        datea(1) = iyear
        datea(2) = datefix(2)
        datea(3) = datefix(1)
        fsec = 0.0
        call dtardp (datea, fsec, outdate1)
        if (outdate1 .lt. tstart) then
          datea(1) = datea(1) + 1
          call dtardp (datea, fsec, outdate1)
        endif
        i = 1
        outdat(i) = outdate1
        datea(1) = datea(1) + 1
        call dtardp (datea, fsec, outdate1)
        do while (outdate1 .lt. (tend + 0.1d0))
          i = i + 1
          outdat(i) = outdate1
          datea(1) = datea(1) + 1
          call dtardp (datea, fsec, outdate1)
        end do
      else
        call rdatim ('outdat',outdat,maout,ifnd)
      endif

! -   Intermediate output dates
      call rdsinr ('nprintday',1,1000,nprintday)

! --- output each time interval dt
      flprintdt = .false.
      if(rdinqr('flprintdt')) then
        call rdslog ('flprintdt',flprintdt)
      endif

      call rdsinr ('swmonth',0,1,swmonth)
      if (swmonth .eq. 1) then 
        datea(1) = iyear
        datea(2) = imonth
        if (datea(2) .lt. 12) then
          datea(2) = datea(2) + 1          
        else
          datea(1) = datea(1) + 1          
          datea(2) = 1          
        endif
        datea(3) = 1
        fsec = 0.0
        call dtardp (datea, fsec, outdate1)
        i = 0
        do while ((outdate1 - 1.d0) .lt. (tend + 0.1d0))
          i = i + 1
          outdatint(i) = outdate1 - 1.d0
          if (datea(2) .lt. 12) then
            datea(2) = datea(2) + 1          
          else
            datea(1) = datea(1) + 1          
            datea(2) = 1          
          endif
          call dtardp (datea, fsec, outdate1)
        enddo
        period = 0
        swres = 0
      else
        call rdsinr ('period',0,366,period)
        call rdsinr ('swres',0,1,swres)
      endif

      call rdsinr ('swodat',0,1,swodat)
      if (swodat.eq.1 .and. swmonth .eq.0) 
     &   call rdatim ('outdatint',outdatint,maout,ifnd)

! -   output files
      call rdscha ('outfil',outfil)
      call rdsinr ('swheader',0,1,swheader)

      call rdsinr ('swafo',0,2,swafo)
      call rdsinr ('swaun',0,2,swaun)
      if (swaun.ge.1 .or. swafo.ge.1) then
        call rdsinr ('swdiscrvert',0,1,swdiscrvert)
        if (swdiscrvert.eq.1) then

! -       read values for numnodNew and dzNew
          call rdsinr ('numnodNew',1,macp,numnodNew)
          call rdfdor ('dzNew',1.0d-6,5.0d2,dzNew,macp,numnodNew)
        endif
        call rdsdor ('CritDevMasBal',
     &                1.0d-30, 1.0d0,CritDevMasBal)
      endif
      call rdsinr ('swvap',0,1,swvap)
      call rdsinr ('swate',0,1,swtem)
      call rdsinr ('swblc',0,1,swblc)
      call rdsinr ('swbma',0,1,swbma) 

! --- output of recharge/storage info for ModFlow
      swoutputmodflow = 0
      if(rdinqr('swoutputmodflow')) then
        call rdsinr ('swoutputmodflow',0,1,swoutputmodflow)
        if(swoutputmodflow.eq.1) then
          messag = 'simulation with addtional output for ModFlow'
          call warn ('Readswap',messag,logf,swscre)
        endif
      endif


! -   meteo
      call rdscha ('metfil',metfil)
      call rdsdor ('lat',-90.0d0,90.0d0,lat)
      call rdsdor ('alt',-400.0d0,3000.0d0,alt)

!     detailed meteo input as option
      call rdsinr ('swmetdetail',0,1,swmetdetail)
      if (swmetdetail.eq.1) then
        call rdsinr ('nmetdetail',1,96,nmetdetail)
        swrain = 0
        swetr = 0
      else
        call rdsinr ('swrain',0,3,swrain)
        call rdsinr ('swetsine',0,1,swetsine)
        call rdsinr ('swetr',0,1,swetr)
      endif

!     data only required for Penman
      if (swetr.eq.0) then
        call rdsdor ('altw',0.0d0,99.0d0,altw)
      endif

      if (swrain .eq. 1) then
        call rdador ('time',0.d0,366.d0,raintime,30,ifnd)
        call rdfdor ('rainflux',0.d0,1000.d0,rainflux,30,ifnd)                
        do i = 1, ifnd
          raintab(i*2) = rainflux(i)
          raintab(i*2-1) = raintime(i)
        end do
      endif

      if (swrain .eq. 3) then
        call rdscha ('rainfil',rainfil)
      endif

! -   crop rotation scheme
      flbaresoil = .true.
      ifnd = 0
      if(rdinqr('cropstart')) then
        flbaresoil = .false.
        call rdatim ('cropstart',cropstart,macrop,ifnd)
        call rdftim ('cropend',cropend,macrop,ifnd)
        call rdfcha ('cropname',cropname,macrop,ifnd)
        call rdfcha ('cropfil',cropfil,macrop,ifnd)
        call rdfinr ('croptype',1,3,croptype,macrop,ifnd)
      endif
      if (ifnd .eq. 0) then
        flbaresoil = .true.                      ! no cultivation -> bare soil
        flCultivate = .false.
      else
        flCultivate = .true.
        flOpenCropOutput = .true.
! -     check dates of cropping periods
        if ((cropstart(ifnd) - tstart) .lt. -1.d-3) then
          write(tmp,'(i11)') ifnd
          tmp = adjustl(tmp)
          messag = 'The start date of the first crop (cropstart) must'//
     &    ' be larger than the start date of simulation (tstart)!'
          call fatalerr ('readswap',messag)
        endif

        do i = 1,ifnd
          write(tmp,'(i11)') i
          tmp = adjustl(tmp)
          if ((cropend(i) - cropstart(i)) .lt. 0.5d0) then
            messag = 'The end date of crop number '//trim(tmp)//
     &      ' should be larger than the begin date!'
            call fatalerr ('readswap',messag)
          endif
          if ((cropstart(i+1)-cropend(i)).lt.0.5d0 .and. i.lt.ifnd) then
            write(tmp,'(i11)') i+1
            tmp = adjustl(tmp)
            messag = 'The begin date of crop number '//trim(tmp)//
     &      ' should be larger than the end date of the former crop!'
            call fatalerr ('readswap',messag)
          endif
! -     check combination detailed crop with LAT>60.
          if(croptype(i).ne.1 .and. lat.gt.60.0d0) then
            messag = 'Fatal combination for crop number '//trim(tmp)//
     &       'detailed crop module within polar circle (LAT>60)!'
            call fatalerr ('readswap',messag)
          endif
        end do
      endif

! -   fixed irrigation applications
      swirgfil = 0
      call rdsinr ('swirfix',0,1,swirfix)
      if (swirfix .eq. 1) then
        do i = 1,mairg
          irdate(i) = 0.d0
        end do
        call rdsinr ('swirgfil',0,1,swirgfil)
        if (swirgfil .eq. 0) then
          call rdatim ('irdate',irdate,mairg,ifnd)
          call rdfdor ('irdepth',0.d0,1000.d0,irdepth,mairg,ifnd)
          call rdfdor ('irconc',0.d0,1000.d0,irconc,mairg,ifnd)
          call rdfinr ('irtype',0,1,irtype,mairg,ifnd)
! -     at least one date must be within simulation period
          call checkdate(ifnd,irdate,tend,tstart,'irdate',
     &                   'readswap/swirfix=1      ')
! ---   convert mm irrigation to cm
          do i = 1, ifnd
            irdepth(i) = irdepth(i) / 10.d0
          end do
        else if (swirgfil .eq. 1) then
          call rdscha ('irgfil',irgfil)
        endif
      endif

! =================================================================
!     Section soil profile

! --- initial water conditions
      call rdsinr ('swinco',1,3,swinco)
      if (swinco.eq.1) then
        call rdador ('zi',-1.d5,0.d0,zi,macp,ifnd)
        call rdfdor ('h',-1.d10,1.d4,h,macp,ifnd)
        nhead = ifnd
      elseif (swinco.eq.2) then
        call rdsdor ('gwli',-10000.0d0,1000.0d0,gwli)
      elseif (swinco.eq.3) then
!       read data from file with results from previous simulation
        call rdscha ('inifil',inifil)
      endif

! --  drainage resistance of surface runoff
      call rdsdor ('rsro',0.001d0,1.0d0,rsro)
      call rdsdor ('rsroexp',0.01d0,10.0d0,rsroexp)

! --- ponding
      swpondmx = 0
      if(rdinqr('swpondmx')) then
        call rdsinr ('swpondmx',0,1,swpondmx)
        if(swpondmx.eq.1) then
          messag = 'Simulation with time dependent ponding-threshold'
          call warn ('Readswap',messag,logf,swscre)
          call rdatim ('datepmx',datepmx,mairg,ifnd)
! -     at least one date must be within simulation period
          call checkdate(ifnd,datepmx,tend,tstart,'datepmx ',
     &                      'readswap/swpondmx=1')
          call rdfdor('pondmxtb',0.0d0,1000.d0,pondmxtb,mairg,ifnd)
          do i = 1, ifnd
            pondmxtab(i*2-1) = datepmx(i)
            pondmxtab(i*2)   = pondmxtb(i)
          enddo
          pondmx = pondmxtb(1)
        endif
      endif
      if(swpondmx.eq.0) then
        call rdsdor ('pondmx',0.0d0,1000.0d0,pondmx)
      endif

! --  drainage resistance of surface runoff
      call rdsdor ('rsro',0.001d0,1.0d0,rsro)
      call rdsdor ('rsroexp',0.01d0,10.0d0,rsroexp)
! --  check combination of time-dependent ponding and runoff-resistance
      if(swpondmx.eq.1 .and. rsro.lt.1.0d-02) then
        messag = 'Fatal error: time-dependent threshold for ponding '//
     &       ' (SWPONDMX=1) requires resistance (RSRO) > 0.01 d-1!'
        call fatalerr ('readswap',messag)
      endif

! --- soil evaporation
      call rdsinr ('swcfbs',0,1,swcfbs)
      if (swcfbs.eq.1) then
        call rdsdor ('cfbs',0.1d0,1.5d0,cfbs)
      else
        cfbs = 1.0d0
      endif
      call rdsinr ('swredu',0,2,swredu)
      if (swredu .gt.0) then
        call rdsdor ('cofred',0.0d0,1.0d0,cofred)
        call rdsdor ('rsigni',0.0d0,1.0d0,rsigni)  
      endif
      cfevappond = 1.25d0
      if(rdinqr('cfevappond')) then
        call rdsdor ('cfevappond',0.0d0,3.0d0,cfevappond)
      endif
 
! --- vertical discretization of soil profile
      call rdainr ('isoillay',1,maho,isoillay,macp,ifnd)
      call rdfinr ('isublay',1,macp,isublay,macp,ifnd)
      call rdfdor ('hsublay',0.d0,10000.d0,hsublay,macp,ifnd)
      call rdfdor ('hcomp',0.d0,1000.d0,hcomp,macp,ifnd)
      call rdfinr ('ncomp',0,macp,ncomp,macp,ifnd)
      nsublay = ifnd
      numlay = isoillay(ifnd)

! --- check input vertical discretization of soil profile
      do i = 1, nsublay
        if (abs(hsublay(i) - hcomp(i)*ncomp(i)) .gt. 1.d-6) lay = 1
        if (i .gt. 1)then
          if( (isoillay(i) .ne. isoillay(i-1) .or.
     &        isoillay(i) .ne. isoillay(i-1) + 1)) lay = 1
        end if
      end do

! --- Soil Hydraulic relation: as MVG-functions or as Tables
      swsophy = 0
! -   Tables for each soil layer
      if(rdinqr('swsophy')) then
        call rdsinr ('swsophy',0,1,swsophy)
        if(swsophy.eq.1) then
!         not allowed: table-option combined with output with 
!         adjusted vertical discrectization (swdiscrvert=1)
          if(swdiscrvert.eq.1) then
            messag = 'combi of sophys-tables (swsophy=1) '//
     &                'and swdiscrvert=1 not allowed'
            call fatalerr ('Readswap',messag)
          endif
          call rdacha ('filenamesophy',filenamesophy,maho,numlay)
          messag = 'simulation with tables for soil hydraulic relations'
          call warn ('Readswap',messag,logf,swscre)
        endif
      endif
! -   MVG-functions: parameters of functions of each soil layer
      if(swsophy.eq.0) then
        call rdfdor ('ores',0.d0,1.0d0,ores,maho,numlay)
        call rdfdor ('osat',0.d0,1.0d0,osat,maho,numlay)
        call rdfdor ('alfa',1.d-4,100.d0,alfa,maho,numlay)
        call rdfdor ('alfaw',1.d-4,100.d0,alfaw,maho,numlay)
!        call rdfdor ('npar',1.001d0,4.d0,npar,maho,numlay)
        call rdfdor ('npar',1.001d0,9.d0,npar,maho,numlay)
        call rdfdor ('lexp',-25.d0,25d0,lexp,maho,numlay)
        call rdfdor ('h_enpr',-40.d0,0.d0,h_enpr,maho,numlay)
!       to allow downward compatibility from Swap3.2.23
        if(rdinqr('ksatfit')) then
          call rdfdor ('ksatfit',1.d-5,1.d5,ksatfit,maho,numlay)
        else
          call rdfdor ('ksat',1.d-5,1.d5,ksatfit,maho,numlay)
        endif
        flksatexm  =.false.
        if(rdinqr('ksatexm')) then
          call rdfdor ('ksatexm',1.d-5,1.d5,ksatexm,maho,numlay)
          flksatexm  =.true.
          messag = 'simulation with additonal Ksat value (Ksatexm)'
          call warn ('Readswap',messag,logf,swscre)
        endif
!       assign sophy-values to paramvg (input of cofgen)
        paramvg = 0.0d0
        do lay = 1,numlay
          paramvg(1,lay) = ores(lay)
          paramvg(2,lay) = osat(lay)
          paramvg(3,lay) = ksatfit(lay)
          paramvg(4,lay) = alfa(lay)
          paramvg(5,lay) = lexp(lay)
          paramvg(6,lay) = npar(lay)
          paramvg(7,lay) = 1.d0 - (1.d0 / paramvg(6,lay))
          paramvg(8,lay) = alfaw(lay)
          paramvg(9,lay) = h_enpr(lay)
          paramvg(10,lay) = -999.d0
          if (flksatexm) then
            paramvg(10,lay) = ksatexm(lay)
            help = abs(hthr * paramvg(4,lay))**paramvg(6,lay)
            help = (1.0d0 + help) ** paramvg(7,lay)
            relsatthr(lay) = 1.0d0/help
            term1 = ( 1.0d0-relsatthr(lay)**(1.0/paramvg(7,lay)) )
     &                                                  **paramvg(7,lay)
            ksatthr(lay)=paramvg(3,lay)*(relsatthr(lay)**paramvg(5,lay))
     &                                     * (1.0d0-term1)*(1.0d0-term1)
            if(ksatthr(lay).ge.ksatexm(lay)) then
              write(messag,'(a,i2)') 'ksatexm < ksatthr for layer ',lay
              call fatalerr ('readswap',messag)
            endif
          endif
        end do
      endif

! --- hysteresis
      call rdsinr ('swhyst',0,2,swhyst)
      if (swhyst.gt.0) then
        call rdsdor ('tau',0.0d0,1.0d0,tau)
      endif

!     combination of soil hysical tables and hysteresis is not possible
      if (swhyst.eq.1 .and. swsophy.eq.1) then
         messag = 'Combination of hysteresis (swhyst=1) and tabulated '
     &//'soil physics (swsophy=1) is not possible !'
          call fatalerr('Readswap',messag)
      end if

! --- rooting depth limitation
      call rdsdor ('rds',1.d0,5000.0d0,rds)

! --- preferential flow due to macropores
      flInitDraBas = .false.
      call rdsinr ('swmacro',0,1,swmacro)
      if (swmacro.eq.1) then
!- a. PARAMETERS FOR GEOMETRY MACROPORES
         MaxDepth= 0.d0
         do il = 1, nsublay
            MaxDepth= MaxDepth - hsublay(il)
         enddo
         call rdsdor('Z_Ah',maxdepth,0.d0,Z_Ah)
         call rdsdor('Z_Ic',maxdepth,0.d0,Z_Ic)
         call rdsdor('Z_St',maxdepth,0.d0,Z_St)
         call rdsdor('VlMpStSs',0.0d0,0.5d0,VlMpStSs)
         call rdsdor('PpIcSs',0.0d0,0.99d0,PpIcSs)
         call rdsinr('NumSbDm',0,(MaDm-2),NumSbDm)
         if (NumSbDm.eq.0 .and. PpIcSs.gt.0.d0) then
            messag = ' NumSbDm .eq.0 .and. PpIcSs.gt.0.d0'
            call fatalerr('MacroRead',messag)
         endif
         call rdsdor('DiPoMi',0.1d0,1000.0d0,DiPoMi)
         call rdsdor('DiPoMa',0.1d0,1000.0d0,DiPoMa)
         call rdsdor('PndmxMp',0.0d0,10.0d0,PndmxMp)    
!   - optional: 
         call rdsdor('PowM',0.0d0,100.0d0,PowM)    ! default 1.0
         call rdsdor('Rzah',0.0d0,1.0d0,Rzah)      ! default 0.0
         call rdsdor('Spoint',0.0d0,1.0d0,Spoint)  ! default 1.0
         call rdsinr('SwPowM',0,1,SwPowM)          ! default 0
         call rdsdor('PndmxMp',0.0d0,10.0d0,PndmxMp)    
!- b. PARAMETERS FOR SHRINKAGE CHARACTERISTICS
         call rdfinr('SwSoilShr',0,2,SwSoilShr,MaHo,numlay)
         call rdfinr('SwShrInp',1,3,SwShrInp,MaHo,numlay)
         call rdfdor('ThetCrMP',0.0d0,1.0d0,ThetCrMp,MaHo,numlay)
         call rdfdor('GeomFac',0.0d0,10.0d0,GeomFac,MaHo,numlay)
         call rdfdor('ShrParA',0.0d0,10.0d0,ShrParA,MaHo,numlay)
         call rdfdor('ShrParB',-10.0d0,100.0d0,ShrParB,MaHo,numlay)
         call rdfdor('ShrParC',-10.0d0,100.0d0,ShrParC,MaHo,numlay)
         call rdfdor('ShrParD',0.0d0,100.0d0,ShrParD,MaHo,numlay)
         call rdfdor('ShrParE',-10.0d0,10.0d0,ShrParE,MaHo,numlay)
         call rdsdor('ZnCrAr',Z_Ah,0.0d0,ZnCrAr)
!- c. PARAMETERS FOR SORPTIVITY
         call rdfinr('SwSorp',1,2,SwSorp,MaHo,numlay)
         call rdfdor('SorpFacParl',0.0d0,1.0d2,SorpFacParl,MaHo,numlay)
         call rdfdor('SorpMax',0.0d0,100.0d0,SorpMax,MaHo,numlay)
         call rdfdor('SorpAlfa',-10.0d0,10.0d0,SorpAlfa,MaHo,numlay)
!- d. SHAPE FACTOR for saturated exchange between macropores and matrix
         call rdsdor('ShapeFacMp',0.0d0,100.0d0,ShapeFacMp)
!- e. CRITICAL value for undersaturation volume
         call rdsdor('CritUndSatVol',0.0d0,10.0d0,CritUndSatVol)
         call rdsinr('SwDarcy',0,1,SwDarcy)
!- f. PARAMETERS FOR RAPID DRAINAGE
!       only possible when at least one drainege level!!!
         call rdsinr('SwDrRap',0,1,SwDrRap)
         if (SwDrRap.eq.1) then
!          at this moment for 1 system only, may be extended to other systems 
            call rdsdor
     &           ('RapDraResRef',0.d0,1.d10,RapDraResRef(1))
            do il= 2,madr
               RapDraResRef(il) = RapDraResRef(1)
            enddo
            call rdsdor('RapDrareaExp',0.0d0,100.0d0,RapDraReaExp)
            call rdsinr('NumLevRapDra',1,5,NumLevRapDra)
!
!-    set flag for initialisation of drainage basis rapid drainage
            flInitDraBas = .true.
         endif
      endif

! -   snow and frost conditions 
      call rdsinr ('swsnow',0,1,swsnow)
      if (swsnow.eq.1) then
        call rdsdor ('snowinco',0.0d0,1000.0d0,snowinco)
        call rdsdor ('teprrain',0.0d0,10.0d0,teprrain)
        call rdsdor ('teprsnow',-10.0d0,0.0d0,teprsnow)
        call rdsdor ('snowcoef',0.0d0,10.0d0,snowcoef)
      endif
      call rdsinr ('swfrost',0,1,swfrost)
      if (swfrost.eq.1) then
        call rdsdor ('tfroststa',-10.0d0,5.0d0,tfroststa)
        call rdsdor ('tfrostend',-10.0d0,5.0d0,tfrostend)
      endif
!     combination of frost and macropore-flow is not possible (yet)
      if (swfrost.eq.1 .and. swmacro.eq.1) then
         messag = 'Combination of frost (swfrost=1) and macropore-flow '
     &//'(swmacro=1) is not operational !'
          call fatalerr('Readswap',messag)
      end if

!     combination of snow and Et variation during the day (SwEtSine)
      if (swmetdetail.eq.1 .and. swsnow.eq.1) then
        messag = 'In case of snow the sublimation is not simulated 
     & correctly if short time meteorological records are used
     &  (swmetdetail=1 and swsnow=1), please adapt input !'
        call warn ('Readswap',messag,logf,swscre)
      end if

! --- parameters numerical scheme
      call rdsdor ('dtmin', 1.0d-7,0.1d0,dtmin)
      call rdsdor ('dtmax', dtmin, 1.0d0,dtmax)
      call rdsdor ('gwlconv',1.0d-5,1000.0d0,gwlconv)
      call rdsdor ('CritDevPondDt',1.0d-6,1.0d-01,CritDevPondDt)
! Convergence criteria (optional input)
      CritDevh1Cp = 1.0d-2
      if(rdinqr('CritDevh1Cp')) then
        call rdsdor ('CritDevh1Cp',1.0d-10,1.0d3,CritDevh1Cp)
      endif
      CritDevh2Cp = 1.0d-1
      if(rdinqr('CritDevh2Cp')) then
        call rdsdor ('CritDevh2Cp',1.0d-10,1.0d3,CritDevh2Cp)
      endif
! flag to generate additional output about convergence-warnings from subr Headcalc
      fldumpconvcrit = .false.
      if(rdinqr('fldumpconvcrit')) then
        call rdslog ('fldumpconvcrit',fldumpconvcrit)
      endif
! Maximum number of iterations [5,100]
      call rdsinr ('MaxIt',5,100,MaxIt)
! Maximum number of back track cycles within an iteration cycle [1,10]
      call rdsinr ('MaxBackTr',1,10,MaxBackTr)
! Maximum number of iterations: no input
      msteps = 100000000
! Switch for mean of hydraulic conductivity 
!  SwkMean=1,2:unweighted arithmic mean,weighted arithmic mean
!  SwkMean=3,4:unweighted geometric mean, weighted geometric mean
      call rdsinr ('SWkmean',1,4,SWkmean)
! Switch for implicit solution with hydraulic conductivity: 0 = explicit, 1 = implicit
      call rdsinr ('SwkImpl',0,1,SwkImpl)

! =================================================================
!     Lateral drainage section
!     extended or basic drainage
      call rdsinr ('swdra',0,2,swdra)
      if (swdra .ne. 0) call rdscha ('drfil',drfil)
      if (SwDrRap.eq.1 .and. SwDra.eq.0) then
          messag = ' There are no drainage levels, so rapid drainage
     &is not possible !'
          call fatalerr('MacroRead',messag)
      endif

!     runon from external source (field)
      call rdsinr ('swrunon',0,1,swrunon)
      if (swrunon .eq. 1) then
        call rdscha ('rufil',rufil)
      endif

!     output-options drainage fluxes, surface reservoir 
      if (swdra.eq.2) then
        call rdsinr ('swdrf',0,1,swdrf)
        call rdsinr ('swswb',0,1,swswb)
      endif


! =================================================================
!     Bottom boundary section

! --- Initialise
      do i = 1,2*mabbc
        gwltab(i) = 0.0D0
        hbotab(i) = 0.0D0
        qbotab(i) = 0.0D0
        haqtab(i) = 0.0D0
      end do

! --- option for input of bottom boundary condition
      call rdsinr ('swbbcfile',0,1,swbbcfile)
      if (swbbcfile .eq. 1) then
        call rdscha ('bbcfil',bbcfil)
      endif

! =================================================================
!     Section heat flow

! --- Switch whether simulation includes heat simulation or not
      call rdsinr ('swhea',0,1,swhea)

      if (swhea .eq. 1) then
! ---   analytical or numerical method 
        call rdsinr ('swcalt',1,2,swcalt)

        if (swcalt.eq.1) then
          call rdsdor ('tampli',0.0d0,50.0d0,tampli)
          call rdsdor ('tmean',-10.0d0,30.0d0,tmean)
          call rdsdor ('timref',0.0d0,366.0d0,timref)
          call rdsdor ('ddamp',1.0d0,500.0d0,ddamp)
        else
          call rdador ('zh',-1.0d5,0.0d0,zh,macp,ifnd)
          call rdfdor ('tsoil',-50.0d0,50.0d0,tsoil,macp,ifnd)
          call rdfdor ('psand',0.0d0,1.0d0,psand,maho,numlay)
          call rdfdor ('psilt',0.0d0,1.0d0,psilt,maho,numlay)
          call rdfdor ('pclay',0.0d0,1.0d0,pclay,maho,numlay)
          call rdfdor ('orgmat',0.0d0,1.0d0,orgmat,maho,numlay)
          nheat = ifnd

!   -     top boundary temperature
          call rdsinr ('SwTopbHea',1,2,SwTopbHea)
          if (swtopbhea .eq. 2) then
            call rdscha ('TSoilFile',tsoilfile)
          endif

!   -     bottom boundary temperature
          call rdsinr ('SwBotbHea',1,2,SwBotbHea)
          if (SwBotbHea.eq.2) then
             call rdatim ('datet',dates,mabbc,ifnd)
! -     at least one date must be within simulation period
             call checkdate(ifnd,dates,tend,tstart,'datet ',
     &                      'readswap/swbotbhea=2    ')
             call rdfdor('tbot',-50.0d0,50.d0,tbot,mabbc,ifnd)
!
             do i = 1, ifnd
               tembtab(i*2)   = tbot(i)
               tembtab(i*2-1) = dates(i)
             enddo
!
           endif
!
        endif
      endif

! -   fatal error when frost or snow is simulated without heat flow
      if ((swfrost.eq.1 .or. swsnow.eq.1) .and. swhea.eq.0) then
        messag = 'In case of snow or frost the soil heat flow should be
     & simulated! Adapt the .swp input file.'
        call fatalerr ('readswap',messag)
      endif

! =================================================================
!     Section solute transport

! --- Switch whether simulation includes solute transport or not
      call rdsinr ('swsolu',0,1,swsolu)

      if (swsolu .eq. 1) then

! ---   boundary and initial condition
        call rdsdor ('cpre',0.0d0,100.0d0,cpre)
        call rdsdor ('cdrain',0.0d0,100.0d0,cdrain)
        if (rdinqr('cseep')) then
          call rdsdor ('cseep',0.0d0,100.0d0,cseep)
        else
          cseep = cdrain
        endif
        call rdador ('zc',-1.0d5,0.0d0,zc,macp,ifnd)
        call rdfdor ('cml',0.0d0,1000.0d0,cml,macp,ifnd)
        nconc = ifnd

! ---   diffusion, dispersion and solute uptake by roots
        call rdsdor ('ddif',0.0d0,10.0d0,ddif)
        call rdfdor ('ldis',0.0d0,100.0d0,ldis,maho,numlay) 
        call rdsdor ('tscf',0.0d0,10.0d0,tscf)

! ---   sorption
        call rdsinr ('swsp',0,1,swsp)
        if (swsp .eq. 1) then
          call rdsdor ('frexp',0.0d0,   10.0d0,frexp)  
          call rdsdor ('cref', 0.0d0, 1000.0d0,cref)
          call rdfdor ('kf',   0.0d0,10000.0d0,kf,maho,numlay)
          call rdfdor ('bdens',0.0d0,10000.0d0,bdens,maho,numlay)
        else
          cref  = 1.0d0
          frexp = 1.0d0
          do lay = 1,numlay
            kf(lay) = 0.0d0
            bdens(lay) = 0.0d0
          end do
        endif

! ---   decomposition
        call rdsinr ('swdc',0,1,swdc)
        if (swdc .eq. 1) then
          call rdfdor ('decpot',0.0d0,10.0d0,decpot,maho,numlay)
          call rdsdor ('gampar',0.0d0,0.5d0,gampar)
          call rdsdor ('rtheta',0.0d0,0.4d0,rtheta)
          call rdsdor ('bexp',0.0d0,2.0d0,bexp)
          call rdfdor ('fdepth',0.0d0,1.0d0,fdepth,maho,numlay)
        else 
          gampar = 0.0d0
          rtheta = 0.5d0
          bexp = 0.0d0
          do lay = 1,numlay
            decpot(lay) = 0.0d0
            fdepth(lay) = 0.0d0
          end do
        endif

! ---   breakthrough
        call rdsinr ('swbr',0,1,swbr)
        if (swbr .eq. 0) then
          daquif = 100.0d0
          poros = 1.0d0
          kfsat = 0.0d0
          decsat = 0.0d0
        else
          call rdsdor ('daquif',0.0d0,10000.0d0,daquif)
          call rdsdor ('poros',0.0d0,0.6d0,poros)
          call rdsdor ('kfsat',0.0d0,100.0d0,kfsat)
          call rdsdor ('decsat',0.0d0,10.0d0,decsat)
          call rdsdor ('cdraini',0.0d0,100.0d0,cdrain)
          cseep = cdrain
        endif

      endif

! =================================================================
!     Section Ageing according to Goode (1996): 
!      "Direct simulation of groundwater age, WRR vol.32, p 289-296"
      flAgeTracer = .false.
      if (rdinqr('flAgeTracer')) then
        call rdslog ('flAgeTracer',flAgeTracer)
        if (flAgeTracer) then

! ---     top and bottom boundary and initial condition
          cpre = 0.0d0
          cirr = 0.0d0
          cdrain = 0.0d0
          cseep = 0.0d0
          call rdador ('zc',-1.0d5,0.0d0,zc,macp,ifnd)
          call rdfdor ('cml',0.0d0,1000.0d0,cml,macp,ifnd)
          nconc = ifnd

! ---     diffusion, dispersion and uniform solute uptake by roots
          call rdsdor ('ddif',0.0d0,10.0d0,ddif)
          call rdfdor ('ldis',0.0d0,100.0d0,ldis,maho,numlay) 
          tscf = 1.0d0

!         warnings and errors
          messag = 'simulation with Age Tracer option (flAgeTracer)'
          call warn ('Readswap',messag,logf,swscre)
          if(swSolu.eq.1) then
            write(messag,'(3a)') 
     &      'Combination of solute transport and Ageing is not allowed',
     &      '(flAgeTracer=.true.  AND   swSolu=1 is not allowed !' 
            call fatalerr ('Readswap',messag)
          endif
          if(swSnow.eq.1) then
            write(messag,'(3a)') 
     &       'Combination of snow and Ageing is not allowed',
     &       '(flAgeTracer=.true.  AND   swSnow=1 is not allowed !' 
            call fatalerr ('Readswap',messag)
          endif
        endif
      endif


! =================================================================
!     Section bottom boundary condition

      if (swbbcfile .eq. 1) then
        close (swp)
        bbc = getun2 (10,90,2)
        filnam = trim(pathwork)//trim(bbcfil)//'.bbc'
        call rdinit(bbc,logf,filnam)
      endif

! --- option for bottom boundary condition
      call rdsinr ('swbotb',1,8,swbotb)

! --- given groundwaterlevel
      if (swbotb.eq.1) then
        call rdatim ('date1',dates,mabbc,ifnd)
        call rdfdor ('gwlevel',-10000.0d0,1000.d0,gwlevel,mabbc,ifnd)

! -     check: at least one date must be within simulation period
        call checkdate(ifnd,dates,tend,tstart,'date1 ',
     &                      'readswap//swbotb=1      ')
! -     check if gwlevel is above soil surface, eliminate certain combinations
        flsat = .false.
        do i = 1,ifnd
          if (gwlevel(i) .gt. -0.5d0*(hcomp(1)))  flsat = .true.
        enddo
        if(flsat) then
           if (swKimpl.eq.1) then
              write(messag,'(3a)') 
     &         'Groundwaterlevel above soil surface, combined with ',
     &         'Implicit Conductivity (swbotb=1 AND swKimpl=1 AND  ',
     &         'gwl>z(1))  is not allowed !' 
              call fatalerr ('Readswap',messag)
           endif
           if (swmacro.eq.1) then
              write(messag,'(3a)') 
     &         'Groundwaterlevel above soil surface, combined with ',
     &         'MacroPore flow (swbotb=1 AND swMacro=1 AND gwl>z(1)) ',
     &         'is not allowed !' 
              call fatalerr ('Readswap',messag)
           endif
           if (swfrost.eq.1) then
              write(messag,'(3a)') 
     &         'Groundwaterlevel above soil surface, combined with ',
     &         'frost conditions (swbotb=1 AND swforst=1 AND gwl>z(1))',
     &         ' not well tested, be aware of balance errors !' 
              call warn ('Readswap',messag,logf,swscre)
           endif
        endif

! -     store values in gwltab
        do i = 1,ifnd
          gwltab(i*2) = gwlevel(i) 
          gwltab(i*2-1) = dates(i)
        enddo
      endif

! --- regional bottom flux is given                             
      if (swbotb.eq.2) then
        call rdsinr ('sw2',1,2,sw2)
        if (sw2 .eq. 1) then
          call rdsdor ('sinave',-10.0d0,10.0d0,sinave)
          call rdsdor ('sinamp',-10.0d0,10.0d0,sinamp)
          call rdsdor ('sinmax',0.d0,366.d0,sinmax)
        else
! -       read tabular data
          call rdatim ('date2',dates,mabbc,ifnd)
          call rdfdor ('qbot2',-100.0d0,100.0d0,qboti,mabbc,ifnd)
! -     at least one date must be within simulation period
          call checkdate(ifnd,dates,tend,tstart,'date2 ',
     &                      'readswap//swbotb=2      ')
! -       fill qbotab table
          do i = 1,ifnd
            qbotab(i*2) = qboti(i) 
            qbotab(i*2-1) = dates(i)
          enddo
        endif
      endif

! --- calculated flux through the bottom of the profile
      if (swbotb.eq.3) then

!       Switch for implicit solution with lower boundary option 3 (Cauchy): 0 = explicit, 1 = implicit
        call rdsinr ('swbotb3Impl',0,1,swbotb3Impl)

        call rdsdor ('shape',0.0d0,1.0d0,shape)
        if (swbotb3Impl.eq.1 .and. abs(shape-1.0d0).gt.1.0d-7) then
          write(messag,'(a)')
     &    ' Possible lower boundary inconsistency using SwBotb=3: ',
     &    '  Combination of swbotb3Impl AND shape not equal 1.0',
     &    '  This is not recommended. Suggestion is: swbotb3Impl=0'
          call warn ('readswap',messag,logf,swscre)
        endif

        call rdsdor ('hdrain',-1.0d4,0.0d0,hdrain)
        call rdsdor ('rimlay',0.0d0,1.0d5,rimlay)

!       Switch to suppress addition of vertical resistance 
!                      between bottom of model and groundwater level
        call rdsinr ('SwBotb3ResVert ',0,1,SwBotb3ResVert)

        call rdsinr ('sw3',1,2,sw3)
        if (sw3 .eq. 1) then
          call rdsdor ('aqave',-10000.0d0,1000.0d0,aqave)
          call rdsdor ('aqamp',0.0d0,1000.0d0, aqamp)
          call rdsdor ('aqtmax',0.0d0,366.d0,aqtmax)
          call rdsdor ('aqper',0.0d0,366.0d0,aqper)
        else
! -       read tabular data
          call rdatim ('date3',dates,mabbc,ifnd)
          call rdfdor ('haquif',-10000.0d0,1000.d0,haquif,mabbc,ifnd)
! -     at least one date must be within simulation period
          call checkdate(ifnd,dates,tend,tstart,'date3 ',
     &                   'readswap//swbotb=3      ')
! -       fill haqtab table
          do i = 1,ifnd
            haqtab(i*2) = haquif(i) 
            haqtab(i*2-1) = dates(i)
          enddo
        endif
        call rdsinr ('sw4',0,1,sw4)
        if (sw4 .eq. 1) then
          if (swbotb3Impl.eq.1) then
            write(messag,'(3a)') 
     &       ' Implicit solution of Cauchy, combined with fluxes ',
     &       'is active !(swbotb3Impl=1 AND sw4=1)'
            call warn ('readswap',messag,logf,swscre)
          endif
! -       read tabular data
          call rdatim ('date4',dates,mabbc,ifnd)
          call rdfdor ('qbot4',-100.0d0,100.d0,qboti,mabbc,ifnd)
! -     at least one date must be within simulation period
          call checkdate(ifnd,dates,tend,tstart,'date4 ',
     &                   'readswap//swbotb=3/sw4=1')
! -       fill qbotab table
          do i = 1,ifnd
            qbotab(i*2) = qboti(i) 
            qbotab(i*2-1) = dates(i)
          enddo
        endif
      endif

! --- flux-groundwater level relationship 
      if (swbotb.eq.4) then
        call rdsinr ('swqhbot',1,2,swqhbot)
        if (swqhbot.eq.1) then
          call rdsdor ('cofqha',-100.0d0,100.0d0,cofqha)
          call rdsdor ('cofqhb',-1.0d0,1.0d0,cofqhb)
        else if (swqhbot.eq.2) then
          call rdador ('qtab',-100.0d0,100.d0,qhtab,mabbc,ifnd)
          call rdfdor ('htab', -1.0d4, 0.0d0, hhtab,mabbc,ifnd)
          do i = 1,ifnd
            qbotab(i*2) = qhtab(i) 
            qbotab(i*2-1) = abs(hhtab(i))
          enddo
        endif
      endif

! --- pressure head of lowest compartment is given 
      if (swbotb.eq.5) then
        call rdatim ('date5',dates,mabbc,ifnd)
        call rdfdor('hbot5',-1.0d10,1000.d0,hbottom,mabbc,ifnd)
! -     at least one date must be within simulation period
        call checkdate(ifnd,dates,tend,tstart,'date5 ',
     &                 'readswap//swbotb=5      ')
! -     store pressure head values in hbotab
        do i = 1, ifnd
          hbotab(i*2) = hbottom(i) 
          hbotab(i*2-1) = dates(i)
        end do
      endif

      if (swbbcfile .eq. 0) then
        close (swp)
      elseif (swbbcfile .eq. 1) then
        close (bbc)
      endif

! -   Tables for each soil layer
      if(swsophy.eq.1) then
          do lay = 1,numlay
            filnam = filenamesophy(lay)
            sol = getun2 (50,90,2)
            call rdinit(sol,logf,filnam)
            call rdador ('headtab',-1.0d09,1.0d09,headtab,matab,ifnd)
            call rdfdor ('thetatab',0.0d0,1.0d0,thetatab,matab,ifnd)
            call rdfdor 
     &                ('conductab',0.0d0,1000.0d0,conductab,matab,ifnd)
            close (sol)
            numtablay(lay) = ifnd
            do i = 1,numtablay(lay)
              sptablay(1,lay,i) = headtab(i)
              sptablay(2,lay,i) = thetatab(i)
              sptablay(3,lay,i) = conductab(i)
            enddo
            call PreProcTabulatedFunction(1,
     &                             numtablay(lay),headtab,thetatab,dydx)
            do i = 1,numtablay(lay)
              sptablay(4,lay,i) = dydx(i)
            enddo
            call PreProcTabulatedFunction(2,
     &                           numtablay(lay),thetatab,conductab,dydx)
            do i = 1,numtablay(lay)
              sptablay(5,lay,i) = dydx(i)
            enddo
 
 !          tables must have values for a head=0
            if (sptablay(1,lay,numtablay(lay)) .gt. 1.d-20) then
               messag = ' No values for head=0 in tabulated soil physic'
     &          //'in input file '//trim(filnam)
              call fatalerr('Readswap',messag)
            end if
          enddo
      endif


! =================================================================
!     Read data of drainage input file
      if (swdra.eq.1) 
     &    call rddrb (swallo,swdtyp,swmacro,dramet,dra,ipos,nrlevs,
     &    logf,swdivd,l,zbotdr,owltab,drares,infres,qdrtab,basegw,
     &    wetper,khtop,khbot,kvtop,kvbot,zintf,entres,geofac,drfil,
     &    pathdrain,numlay,cofani,swnrsrf,swscre,cofintfl,expintfl,
     &    swdislay,swtopdislay,ztopdislay,ftopdislay,shape,SwTopnrsrf,
     &    swdivdinf,FacDpthInf)                                         !  Divdra, infiltration

! =================================================================
! -   read data of fixed irrigation from separate file
      if (swirgfil .eq. 1 .and. swirfix .eq. 1) then
        filnam = trim(pathwork)//trim(irgfil)//'.irg'
        irg = getun2 (10,90,2)
        call rdinit(irg,logf,filnam)
        call rdatim ('irdate',irdate,mairg,ifnd)
        call rdfdor ('irdepth',0.d0,1000.d0,irdepth,mairg,ifnd)
        call rdfdor ('irconc',0.d0,1000.d0,irconc,mairg,ifnd)
        call rdfinr ('irtype',0,1,irtype,mairg,ifnd)
        close (irg)
! -     at least one date must be within simulation period
        call checkdate(ifnd,irdate,tend,tstart,'irdate',
     &                 'readswap//swirgfil=1     ')
! ---   convert mm irrigation to cm
        do i = 1, ifnd
          irdepth(i) = irdepth(i) / 10.d0
        end do
      endif

! --- read data from file with results from previous simulation
! --- set initial ponding conditions
      if (swinco.eq.3) then
        ini = getun2 (10,90,2)
        call rdinit(ini,logf,inifil)
        write (messag,107) 
     &          '*  I/O of variables from file (SWINCO=3)',
     &          '   should be considered carefully',
     &          '   this option is incomplete and poorly tested!'
 107    format (3a)
        call warn ('readswap',messag,logf,swscre)

        call rdsdor ('ssnow',0.0d0,1000.0d0,ssnow)
        if (swsnow.ne.1) then
          write (messag,107) 
     &         'No Simulation of snow, therefore : Initial',
     &         '  storage of snow set to 0.0',
     &         '  neglecting ssnow-value from file '//trim(inifil)
          call warn ('readswap',messag,logf,swscre)
          ssnow = 0.0d0
        endif
        call rdsdor ('pond',0.0d0,100.0d0,pond)
        call rdador ('z_h',-1.0d5,0.0d0,zi,macp,ifnd)
        call rdfdor ('h',-1.0d10,1.0d4,h,macp,ifnd)
        nhead = ifnd
        if (swhea.eq.1 .and. swcalt.eq.2) then
          call rdador ('z_Tsoil',-1.0d5,0.0d0,zh,macp,ifnd)
          call rdfdor ('Tsoil',-50.0d0,50.0d0,Tsoil,macp,ifnd)
        endif
        if (swsolu.eq.1 .or. flAgeTracer) then
          call rdador ('z_Cml',-1.0d5,0.0d0,zc,macp,ifnd)
          call rdfdor ('Cml',0.0d0,1000000.0d0,cml,macp,ifnd)
        endif
!       surface water level
        if(swdra.eq.2) then
          if(rdinqr('wls')) then
            call rdsdor ('wls',-1000.0d0,1000.0d0,wls)
            messag = 'Initial wls read from file (SWINCO=3)'//
     &      ' not implemented yet'
            call warn ('readswap',messag,logf,swscre)
          endif
        endif
!       write soil evaporation reservoirs
        if(swredu.eq.1) then
          messag = 'Initial ldwet read from file (SWINCO=3)'//
     &     ' if absent, then default of 0.0 is assumed'
          call warn ('reduceva',messag,logf,swscre)
          if(rdinqr('ldwet')) then
!           Time after significant rainfall (d)  (Black)
            call rdsdor ('ldwet',0.0d0,300.0d0,ldwet)
          else
            ldwet = 1.0d0
          endif
        endif
        if(swredu.eq.2) then
          messag = 'Initial spev read from file (SWINCO=3)'//
     &     ' if absent, then default of 0.0 is assumed'
          call warn ('reduceva',messag,logf,swscre)
          if(rdinqr('spev')) then
!           Rainfall excess (cm) (Boesten/Stroosnijder)
            call rdsdor ('spev',0.0d0,1000.0d0,spev)
          else
            spev = 0.0d0
          endif
        endif
!       length of final timestep (d)
        if(rdinqr('dt')) then
          call rdsdor ('dt',dtmin,dtmax,dt)
        endif

!       close file
        close(ini)

!       warnings about I/O from external file using of swinco=3
!         (wofost) crop data about leaves, stems and storage organs
        call warn ('readswap',messag,logf,swscre) 
        if(croptype(1).ge.2) then
          write (messag,107) 
     &          '*  I/O of wofost crop variables from file (SWINCO=3)',
     &          '   not implemented yet !'
          call warn ('readswap',messag,logf,swscre) 
        endif
!         macropore variables
        if(swmacro.eq.1) then
          write (messag,107) 
     &          '*  I/O of macropore variables from file ',
     &          '   (SWINCO=3) not implemented yet !'
          call warn ('readswap',messag,logf,swscre) 
        endif

      endif

! --- runon from external file
      if (swrunon .eq. 1) then
        runf = getun2 (10,90,2)
        call rdinit(runf,logf,trim(rufil))
        call rdador ('runoff',0.d0,1000.d0,runonarr,magrs,ifnd)
        flrunon = .true.
        close (runf)
      else
        flrunon = .false.
      endif

! --- read soil surface temperatures
      if (swtopbhea .eq. 2) then
        filnam = trim(pathwork)//trim(tsoilfile)//'.tss'
        tss = getun2 (10,90,2)
        call rdinit(tss,logf,filnam)
        call rdatim ('datet',dates,mabbc,ifnd)
        call rdfdor('ttop',-50.0d0,50.d0,ttop,mabbc,ifnd)
        do i = 1, ifnd
           temtoptab(i*2)   = ttop(i)
           temtoptab(i*2-1) = dates(i)
        enddo
        close (tss)
      endif

! --- copy content of key-file to log-file
      write (logf,14)  
 14   format('*',70('-'),'*',/,' Echo of input file:',/)
      call copfl2(swp,swpfilnam,logf,.true.)

      return
      end

! ----------------------------------------------------------------------
      subroutine rddrb (swallo,swdtyp,swmacro,dramet,dra,ipos,nrlevs,
     &    logf,swdivd,l,zbotdr,owltab,drares,infres,qdrtab,basegw,
     &    wetper,khtop,khbot,kvtop,kvbot,zintf,entres,geofac,drfil,
     &    pathdrain,numlay,cofani,swnrsrf,swscre,cofintfl,expintfl,
     &    swdislay,swtopdislay,ztopdislay,ftopdislay,shape,SwTopnrsrf,
     &    swdivdinf,FacDpthInf)                                         !  Divdra, infiltration
! ----------------------------------------------------------------------
!     Date               : July 2002
!     Purpose            : read input data for basic drainage    
! ----------------------------------------------------------------------
      implicit none
      include 'arrays.fi'

      integer   swallo(madr),swdtyp(madr),dramet,dra,ipos,nrlevs,logf
      integer   swdivd,swdivdinf,numlay,swnrsrf,swscre,swmacro          !  Divdra, infiltration

      real*8    l(madr),zbotdr(madr),owltab(madr,2*maowl)
      real*8    drares(madr),infres(madr),shape
      real*8    qdrtab(50),basegw,wetper(madr),khtop,khbot,kvtop,kvbot
      real*8    zintf,entres,geofac,cofani(maho)
      real*8    cofintfl,expintfl
      integer   swdislay,swtopdislay(madr),SwTopnrsrf
      real*8    ztopdislay(Madr),ftopdislay(Madr),FacDpthInf            !  Divdra, infiltration

      character drfil*16,pathdrain*80
      logical   rdinqr
! ----------------------------------------------------------------------
! --- local
      integer   ifnd,i,getun2
      real*8    datowl(maowl),level(maowl),qdrain(25),gwl(25)
      character filnam*80,messag*200

! ----------------------------------------------------------------------

! --- open file with drainage data
      filnam = trim(pathdrain)//trim(drfil)//'.dra'
      dra = getun2 (10,90,2)
      call rdinit(dra,logf,filnam)

! --- method to establish drainage/infiltration fluxes
      call rdsinr ('dramet',1,3,dramet) 

      if (dramet.ne.3) nrlevs = 1

! --- division of drainage fluxes
      call rdsinr ('swdivd',0,1,swdivd)
      if (swdivd.eq.0) then
          write(messag,'(3a)')
     &    ' Variabel SWDIVD=0 in input file : ',trim(filnam),
     &    ' this is not recommended and may cause numerical instability'
          call warn ('rddrb',messag,logf,swscre)
      endif
      if (swdivd.eq.1 .and. dramet.eq.1) then
          write(messag,'(3a,i3)')
     &    ' Variabel SWDIVD=1 and DRAMET=1 in inputfile :',trim(filnam),
     &    ' this is not allowed for drainage method (dramet)= ',dramet
          call fatalerr ('Rddrb',messag)
      endif
      if (swdivd .eq. 1) then
        if(rdinqr('swdivdinf')) then
          call rdsinr ('swdivdinf',0,1,swdivdinf)                       !  Divdra, infiltration
            if (swdivdinf.eq.1 .and. dramet.ne.3) then
              write(messag,'(3a,i3)')
     &        ' Variabel SWDIVDINF=1 and DRAMET not 1 in inputfile :',
     &        trim(filnam),' this option is not allowed for drainage ',
     &        ' method (dramet)= ',dramet
              call fatalerr ('Rddrb',messag)
            endif
          call rdsdor ('FacDpthInf',0.d0,1.d0,FacDpthInf)
        else
          swdivdinf = 0
        endif
        call rdfdor ('cofani',1.d-4,1000.d0,cofani,maho,numlay)
      endif


! --- input table of drainage flux as function of groundwater level
      if (dramet.eq.1) then  
        call rdsdor ('lm1',1.0d0,1000.d0,l(1))
        call rdador ('gwl',-10000.0d0,10.0d0,gwl,25,ifnd)
        call rdfdor ('qdrain',-100.d0,1000.0d0,qdrain,25,ifnd)

        l(1) = 100.0d0*l(1)
        do  i = 1,50
          qdrtab(i) = 0.0d0
        end do
        do i = 1,ifnd
          qdrtab(i*2-1) = abs(gwl(i))
          qdrtab(i*2) = qdrain(i)
        end do

!   - in case of rapid drainage due to macropore flow: read zbotdr
        if (swmacro.eq.1) then
           call rdsinr ('swdtyp' ,1,2,swdtyp(1))
           call rdsdor ('zdrabas',-1000.0d0,0.0d0,zbotdr(1))
        endif

! --- input drainage formula of Hooghoudt or Ernst
      elseif (dramet.eq.2) then
   
! ---   read drain characteristics
        call rdsdor ('lm2',1.0d0,1000.0d0,l(1))
        l(1) = 100.0d0*l(1)
        call rdsdor ('shape',0.0d0,1.0d0,shape)
        call rdsdor ('wetper',0.0d0,1000.0d0,wetper(1))
        call rdsdor ('zbotdr',-1000.0d0,0.0d0,zbotdr(1))
        call rdsdor ('entres',0.0d0,1000.0d0,entres)

! ---   read profile characteristics
        call rdsinr ('ipos',1,5,ipos)
        call rdsdor ('basegw',-1.0d4,0.0d0,basegw)
        call rdsdor ('khtop',0.0d0,1000.0d0,khtop)

        if (ipos.ge.3) then
          call rdsdor ('khbot',0.0d0,1000.0d0,khbot)
          call rdsdor ('zintf',-1.0d4,0.0d0,zintf)
        endif
        if (ipos.ge.4) then
          call rdsdor ('kvtop',0.0d0,1000.0d0,kvtop)
          call rdsdor ('kvbot',0.0d0,1000.0d0,kvbot)  
        endif
        if (ipos.eq.5) then
          call rdsdor ('geofac',0.0d0,100.0d0,geofac)
        endif

! --- drainage and infiltration resistance
      elseif (dramet.eq.3) then
   
        call rdsinr ('nrlevs',1,Madr,nrlevs)
        if (nrlevs.gt.5) then
          write(messag,'(3a)')
     &    ' Number of Drainage levels >5 in inputfile : ',trim(filnam),
     &    '   part of the output is limited to 5 levels'
          call warn ('rddrb',messag,logf,swscre)
        endif

! --- type of highest drainage level
        call rdsinr ('swintfl',0,1,swnrsrf)
        if (swnrsrf.eq.1) then
          call rdsdor ('cofintflb',0.01d0,10.0d0,cofintfl)     
          call rdsdor ('expintflb',0.1d0,1.0d0,expintfl) 
        endif

! kroes 20080707 : allow disabling of option
        if (swdivd .eq. 1) then
          if (swnrsrf.gt.0)  call rdsinr ('SwTopnrsrf',0,1,SwTopnrsrf)
        endif

! ---   drainage level 1
        if (nrlevs.ge.1) then
          call rdsdor ('drares1',1.0d0,1.0d5,drares(1))
          call rdsdor ('infres1',0.0d0,1.0d5,infres(1))
          call rdsinr ('swallo1',1,3,swallo(1))
          if (swdivd .eq. 1) then
            call rdsdor ('l1',1.0d0,100000.0d0,l(1))
            l(1) = 100.0d0*l(1)
          endif
          call rdsdor ('zbotdr1',-10000.0d0,0.0d0,zbotdr(1))
          call rdsinr ('swdtyp1',1,2,swdtyp(1))
          if (swdtyp(1).eq.2) then
            call rdatim ('datowl1',datowl,maowl,ifnd)
            call rdfdor ('level1',-10000.0d0,10.0d0,level,maowl,ifnd)
! -         store values in table
            do i = 1,ifnd
              owltab(1,i*2) = level(i)
              owltab(1,i*2-1) = datowl(i)
            end do
          endif
        endif 

        if (nrlevs.ge.2) then
          call rdsdor ('drares2',1.0d0,1.0d5,drares(2))
          call rdsdor ('infres2',0.0d0,1.0d5,infres(2))
          call rdsinr ('swallo2',1,3,swallo(2))
          if (swdivd .eq. 1) then
            call rdsdor ('l2',1.0d0,100000.0d0,l(2))
            l(2) = 100.0d0*l(2)
          endif
          call rdsdor ('zbotdr2',-10000.0d0,0.0d0,zbotdr(2))
          call rdsinr ('swdtyp2',1,2,swdtyp(2))
          if (swdtyp(2).eq.2) then
            call rdatim ('datowl2',datowl,maowl,ifnd)
            call rdfdor ('level2',-10000.0d0,10.0d0,level,maowl,ifnd)
! -         store values in table
            do i = 1,ifnd
              owltab(2,i*2) = level(i)
              owltab(2,i*2-1) = datowl(i)
            end do
          endif
        endif 

        if (nrlevs.ge.3) then
          call rdsdor ('drares3',1.0d0,1.0d5,drares(3))
          call rdsdor ('infres3',0.0d0,1.0d5,infres(3))
          call rdsinr ('swallo3',1,3,swallo(3))
          if (swdivd .eq. 1) then
            call rdsdor ('l3',1.0d0,100000.0d0,l(3))
            l(3) = 100.0d0*l(3)
          endif
          call rdsdor ('zbotdr3',-10000.0d0,0.0d0,zbotdr(3))
          call rdsinr ('swdtyp3',1,2,swdtyp(3))
          if (swdtyp(3).eq.2) then
            call rdatim ('datowl3',datowl,maowl,ifnd)
            call rdfdor ('level3',-10000.0d0,10.0d0,level,maowl,ifnd)
! -         store values in table
            do i = 1,ifnd
              owltab(3,i*2) = level(i)
              owltab(3,i*2-1) = datowl(i)
            end do
          endif
        endif 

        if (nrlevs.ge.4) then
          call rdsdor ('drares4',1.0d0,1.0d5,drares(4))
          call rdsdor ('infres4',0.0d0,1.0d5,infres(4))
          call rdsinr ('swallo4',1,3,swallo(4))
          if (swdivd .eq. 1) then
            call rdsdor ('l4',1.0d0,100000.0d0,l(4))
            l(4) = 100.0d0*l(4)
          endif
          call rdsdor ('zbotdr4',-10000.0d0,0.0d0,zbotdr(4))
          call rdsinr ('swdtyp4',1,2,swdtyp(4))
          if (swdtyp(4).eq.2) then
            call rdatim ('datowl4',datowl,maowl,ifnd)
            call rdfdor ('level4',-10000.0d0,10.0d0,level,maowl,ifnd)
! -         store values in table
            do i = 1,ifnd
              owltab(4,i*2) = level(i)
              owltab(4,i*2-1) = datowl(i)
            end do
          endif
        endif 

        if (nrlevs.ge.5) then
          call rdsdor ('drares5',1.0d0,1.0d5,drares(5))
          call rdsdor ('infres5',0.0d0,1.0d5,infres(5))
          call rdsinr ('swallo5',1,3,swallo(2))
          if (swdivd .eq. 1) then
            call rdsdor ('l5',1.0d0,100000.0d0,l(5))
            l(5) = 100.0d0*l(5)
          endif
          call rdsdor ('zbotdr5',-10000.0d0,0.0d0,zbotdr(5))
          call rdsinr ('swdtyp5',1,2,swdtyp(5))
          if (swdtyp(5).eq.2) then
            call rdatim ('datowl1',datowl,maowl,ifnd)
            call rdfdor ('level1',-10000.0d0,10.0d0,level,maowl,ifnd)
! -         store values in table
            do i = 1,ifnd
              owltab(5,i*2) = level(i)
              owltab(5,i*2-1) = datowl(i)
            end do
          endif
        endif 
      endif 

!     top of model dicharge layer, determined by factor or direct input
      if (swdivd .eq. 1) then
        call rdsinr ('swdislay',0,2,swdislay)
        if (swdislay .eq. 1) then
          call rdfinr ('swtopdislay',0,1,swtopdislay,madr,nrlevs)
          call rdfdor('ztopdislay',-1.0d4,0.0d0,ztopdislay,madr,nrlevs)
        elseif (swdislay .eq. 2) then
          call rdfinr ('swtopdislay',0,1,swtopdislay,madr,nrlevs)
          call rdfdor('ftopdislay',0.0d0,1.0d0,ftopdislay,madr,nrlevs)
        endif
      endif


! --- close file with drainage data
      close (dra)         

      return
      end

! ----------------------------------------------------------------------
      subroutine readwofost (crpfil,pathcrop,swcf,cftb,idsl,dlo,dlc,
     &  tsumea,tsumam,dtsmtb,dvsend,tdwi,laiem,rgrlai,slatb,spa,
     &  ssa,span,tbase,kdif,kdir,eff,amaxtb,tmpftb,tmnftb,cvl,cvo,cvr,
     &  cvs,q10,rml,rmo,rmr,rms,rfsetb,frtb,fltb,fstb,fotb,perdl,rdrrtb,
     &  rdrstb,hlim1,hlim2u,hlim2l,hlim3h,hlim3l,hlim4,rsc,adcrh,
     &  adcrl,cofab,rdi,rri,rdc,rdctb,logf,schedule,cumdens,chtb,albedo,
     &  swetr,flsolute,ecmax,ecslop,c2eca,c2ecb,c2ecf,numlay,relni,cfet,
     &  alphacrit,swroottyp,wiltpoint,rootradius,rootcoefa,rsw)           ! NwRootExtr
! ----------------------------------------------------------------------
!     purpose: read parameters for wofost crop growth routine
! ----------------------------------------------------------------------
      implicit none
      include  'arrays.fi'


      integer crp,i,idsl,logf,ifnd,swcf,getun2,schedule,swetr,numlay,
     &        swroottyp                                                 ! NwRootExtr
      logical flsolute,rdinqr
      real*8 cftb((2*magrs)),dtsmtb(30),slatb(30),amaxtb(30),tmpftb(30)
      real*8 tmnftb(30),rfsetb(30),frtb(30),fltb(30),fstb(30),fotb(30)
      real*8 rdrrtb(30),rdrstb(30),kdif,kdir,cofab,laiem
      real*8 adcrh,adcrl,cvl,cvo,cvr,cvs,dlc,dlo,dvsend,eff
      real*8 perdl,q10,rdc,rdi,rgrlai,rml,rmo,rmr,rms,rri,spa,span
      real*8 ssa,tbase,tdwi,tsumam,tsumea,rdctb(22),chtb(2*magrs)
      real*8 hlim1,hlim2u,hlim2l,hlim3h,hlim3l,hlim4,rsc
      real*8 sum,afgen,depth,rootdis(202),cumdens(202)
      real*8 dvsinput(magrs),cfinput(magrs),chinput(magrs),albedo
      real*8 ecmax,ecslop,c2eca,c2ecb,c2ecf(maho),relni,cfet,alphacrit
      real*8 wiltpoint,rootradius,rootcoefa,rsw                             ! NwRootExtr
      character crpfil*(*),pathcrop*(*)
! locals
      integer   swc2ecf
      character message*200,filnam*200

! ----------------------------------------------------------------------

! --- initialise and start reading
      filnam = trim(pathcrop)//trim(crpfil)//'.crp'
      crp = getun2 (10,90,2)
      call rdinit(crp,logf,filnam)

! --- crop factor or crop height
      call rdsinr ('swcf',1,2,swcf)

! --- check use of crop factors in case of ETref
      if (swetr.eq.1 .and. swcf.eq.2) then
        message = 'If ETref is used (SWETR = 1), always define crop '//
     &           'factors (SWCF = 1)' 
        call fatalerr ('ReadWofost',message)
      endif

      if (swcf.eq.1) then
! ---   crop factor is input
        call rdador ('dvs',0.0d0,2.0d0,dvsinput,(magrs),ifnd)
        call rdfdor ('cf',0.0d0,2.0d0,cfinput,(magrs),ifnd)
! ---   store values in cftb
        do i = 1,ifnd
          cftb(i*2) = cfinput(i) 
          cftb(i*2-1) = dvsinput(i)
        enddo
        chtb = -99.99d0
      else
! ---   crop height is input
        call rdador ('dvs',0.0d0,2.0d0,dvsinput,(magrs),ifnd)
        call rdfdor ('ch',0.0d0,1.0d4,chinput,(magrs),ifnd)
! ---   store values in chtb
        do i = 1,ifnd
          chtb(i*2) = chinput(i) 
          chtb(i*2-1) = dvsinput(i)
        enddo
        cftb = -99.99d0
      endif

! --- reflection coefficient and crop resistance
      if (swcf.eq.1) then
! ---   use standard values for ETref
        albedo = 0.23d0
        rsc = 70.0d0
        rsw = 0.0d0
      else
! ---   use crop specific values
        call rdsdor ('albedo',0.0d0,1.0d0,albedo)
        call rdsdor ('rsc',0.0d0,1.0d6,rsc)
        call rdsdor ('rsw',0.0d0,1.0d6,rsw)
      endif

! --- phenology
      call rdsinr ('idsl',0,2,idsl)
      if (idsl.eq.1.or.idsl.eq.2) then
        call rdsdor ('dlo',0.0d0,24.0d0,dlo)
        call rdsdor ('dlc',0.0d0,24.0d0,dlc)
      endif
      if (idsl.eq.0.or.idsl.eq.2) then
        call rdsdor ('tsumea',0.0d0,10000.0d0,tsumea)
        call rdsdor ('tsumam',0.0d0,10000.0d0,tsumam)
      endif
      call rdador ('dtsmtb',0.0d0,100.0d0,dtsmtb,30,ifnd) 
      call rdsdor ('dvsend',0.0d0,3.0d0,dvsend)

! --- initial
      call rdsdor ('tdwi',0.0d0,10000.0d0,tdwi)
      call rdsdor ('laiem',0.0d0,10.0d0,laiem)
      call rdsdor ('rgrlai',0.0d0,1.0d0,rgrlai)

! --- green area
      call rdador ('slatb',0.0d0,2.0d0,slatb,30,ifnd)
      call rdsdor ('spa',0.0d0,1.0d0,spa)
      call rdsdor ('ssa',0.0d0,1.0d0,ssa)
      call rdsdor ('span',0.0d0,366.0d0,span)
      call rdsdor ('tbase',-10.0d0,30.0d0,tbase)

! --- assimilation
      call rdsdor ('kdif',0.0d0,2.0d0,kdif)
      call rdsdor ('kdir',0.0d0,2.0d0,kdir)
      call rdsdor ('eff',0.0d0,10.0d0,eff)
      call rdador ('amaxtb',0.0d0,100.0d0,amaxtb,30,ifnd)
      call rdador ('tmpftb',-10.0d0,50.0d0,tmpftb,30,ifnd)
      call rdador ('tmnftb',-10.0d0,50.0d0,tmnftb,30,ifnd)

! --- conversion of assimilates into biomass
      call rdsdor ('cvl',0.0d0,1.0d0,cvl)
      call rdsdor ('cvo',0.0d0,1.0d0,cvo)
      call rdsdor ('cvr',0.0d0,1.0d0,cvr)
      call rdsdor ('cvs',0.0d0,1.0d0,cvs)

! --- maintenance respiration
      call rdsdor ('q10',0.0d0,5.0d0,q10)
      call rdsdor ('rml',0.0d0,1.0d0,rml)
      call rdsdor ('rmo',0.0d0,1.0d0,rmo)
      call rdsdor ('rmr',0.0d0,1.0d0,rmr)
      call rdsdor ('rms',0.0d0,1.0d0,rms)
      call rdador ('rfsetb',0.0d0,3.0d0,rfsetb,30,ifnd)

! --- partitioning
      call rdador ('frtb',0.0d0,3.0d0,frtb,30,ifnd)
      call rdador ('fltb',0.0d0,3.0d0,fltb,30,ifnd)
      call rdador ('fstb',0.0d0,3.0d0,fstb,30,ifnd)
      call rdador ('fotb',0.0d0,3.0d0,fotb,30,ifnd)

! --- death rates
      call rdsdor ('perdl',0.0d0,3.0d0,perdl)
      call rdador ('rdrrtb',0.0d0,3.0d0,rdrrtb,30,ifnd)
      call rdador ('rdrstb',0.0d0,3.0d0,rdrstb,30,ifnd)

! --- water use
      swroottyp = 1
      if(rdinqr('swroottyp')) then
        call rdsinr ('swroottyp',1,2,swroottyp)                         ! NwRootExtr
      endif
      if (swroottyp.eq.1) then                                          !
        call rdsdor ('hlim1' ,-100.0d0,100.0d0,hlim1)                   !
        call rdsdor ('hlim2u',-1000.0d0,100.0d0,hlim2u)                 !
        call rdsdor ('hlim2l',-1000.0d0,100.0d0,hlim2l)                 !
        call rdsdor ('hlim3h',-10000.0d0,100.0d0,hlim3h)                !
        call rdsdor ('hlim3l',-10000.0d0,100.0d0,hlim3l)                !
        call rdsdor ('hlim4' ,-16000.0d0,100.0d0,hlim4)                 !
                                                                        !
        call rdsdor ('adcrh',0.0d0,5.0d0,adcrh)                         !
        call rdsdor ('adcrl',0.0d0,5.0d0,adcrl)                         !
!       Criticial stress index for compensation of root water uptake (-)
        alphacrit = 1.0d0
        if(rdinqr('alphacrit')) then
          call rdsdor ('alphacrit',0.2d0,1.0d0,alphacrit)
        endif
      else                                                              !
        call rdsdor ('wiltpoint',-1.0d6,-1.0d3,wiltpoint)               !
        call rdsdor ('rootradius',0.0001d0,1.0d0,rootradius)            !
        call rdsdor ('rootcoefa',0.0d0,1.0d0,rootcoefa)                 !
      endif                                                             ! NwRootExtr

!     correction factor to relate potential transpiration to reference crop
!     (default = 1.0, range = 0.8 - 1.2)
      cfet = 1.0d0
      if(rdinqr('cfet')) then
        call rdsdor ('cfet',0.5d0,1.5d0,cfet)
      endif


! --- salt stress
      if (flsolute) then
        call rdsdor ('ecmax', 0.0d0,20.0d0,ecmax)
        call rdsdor ('ecslop',0.0d0,40.0d0,ecslop)
        call rdsdor ('c2eca', 0.0d0,1000.0d0,c2eca)
        call rdsdor ('c2ecb', 0.0d0,10.0d0,c2ecb)
        call rdsinr ('swc2ecf',1,2,swc2ecf)
        if (swc2ecf.eq.1) then
          call rdsdor ('c2ecf', 0.d0, 10.d0, c2ecf(1))
          if(numlay.ge.2) then
            do i = 2,numlay
              c2ecf(i) = c2ecf(1)
            enddo
          endif
        else if (swc2ecf.eq.2) then
          call rdfdor ('c2ecf', 0.d0, 10.d0, c2ecf,maho,numlay)
        endif
      endif

! --- management factor to account for other forms of stress
      relni = 1.0d0
      if(rdinqr('relni')) then
        call rdsdor ('relni',0.0d0,1.0d0,relni)                         ! NwRootExtr
      endif


! --- interception
      call rdsdor ('cofab',0.0d0,1.0d0,cofab)

! --- rooting
      call rdador ('rdctb',0.0d0,100.0d0,rdctb,22,ifnd)
      call rdsdor ('rdi',0.0d0,1000.0d0,rdi)
      call rdsdor ('rri',0.0d0,100.0d0,rri)
      call rdsdor ('rdc',0.0d0,1000.0d0,rdc)

! --- determine whether irrigation scheduling is applied
      call rdsinr ('schedule',0,1,schedule)

      close (crp)

! --- CALCULATE NORMALIZED CUMULATIVE ROOT DENSITY FUNCTION

      if (swroottyp .eq. 1) then                                        ! NwRootExtr

! ---   specify array ROOTDIS with root density distribution
        do i = 0,100
          depth = 0.01d0 * dble(i)
          rootdis(i*2+1) = depth
          rootdis(i*2+2) = afgen(rdctb,22,depth)
        enddo

! ---   calculate cumulative root density function
        do i = 1,202,2
! ---     relative depths
          cumdens(i) = rootdis(i)
        enddo
        sum = 0.d0
        cumdens(2) = 0.d0
        do i = 4,202,2
! ---     cumulative root density
          sum = sum + (rootdis(i-2)+rootdis(i)) * 0.5d0
     &               * (cumdens(i-1)-cumdens(i-3))
          cumdens(i) = sum
        enddo

! ---   normalize cumulative root density function to one
        do i = 2,202,2
          cumdens(i) = cumdens(i) / sum
        enddo
      endif                                                             ! NwRootExtr



      return
      end

! ----------------------------------------------------------------------
      subroutine readgrass (crpfil,pathcrop,tdwi,laiem,rgrlai,slatb,
     &  ssa,span,tbase,kdif,kdir,eff,amaxtb,tmpftb,tmnftb,cvl,cvr,cvs,
     &  q10,rml,rmr,rms,rfsetb,frtb,fltb,fstb,perdl,rdrrtb,
     &  rdrstb,hlim1,hlim2u,hlim2l,hlim3h,hlim3l,hlim4,rsc,adcrh,adcrl,
     &  cofab,rdi,rri,rdc,rdctb,rlwtb,logf,schedule,cumdens,
     &  flsolute,ecmax,ecslop,c2eca,c2ecb,c2ecf,numlay,dateharvest,
     &  swharvest,dmharvest1,dmharvest2,swgrazing,grazingfactor,
     &  daylastharvest,dmlastharvest,wrtmax,                            ! Nwgrassland
     &  nsuptab,dmfac,relnitab,nsupply,                                 ! Nwgrassland
     &  swcf,swetr,cftb,chtb,cfet,alphacrit,
     &  swroottyp,wiltpoint,rootradius,rootcoefa,rsw)                   ! NwRootExtr

! ----------------------------------------------------------------------
!     date               : november 2004   
!     purpose            : read parameters for grass growth routine
! ----------------------------------------------------------------------
      implicit none
      include  'arrays.fi'

      integer crp,i,logf,ifnd,getun2,schedule,numlay,swroottyp
      integer swcf,swetr
      logical flsolute,rdinqr
      real*8 slatb(30),amaxtb(30),tmpftb(30),depth,rootdis(202)
      real*8 tmnftb(30),rfsetb(30),frtb(30),fltb(30),rdrrtb(30)
      real*8 rdrstb(30),kdif,kdir,laiem,cofab,fstb(30)
      real*8 hlim1,hlim2u,hlim2l,hlim3h,hlim3l,hlim4,rdctb(22),rlwtb(22)
      real*8 sum,adcrh,adcrl,afgen,cvl,cvr,cvs,eff,rmr
      real*8 perdl,q10,rdc,rdi,rgrlai,rml,rms,rri,span,ssa,tbase,tdwi
      real*8 cumdens(202)
      real*8 ecmax,ecslop,c2eca,c2ecb,c2ecf(maho)
      real*8 cftb(2*magrs),chtb(2*magrs)
      real*8 albedo,rsc,rsw,cfet,alphacrit
      real*8 wiltpoint,rootradius,rootcoefa                           ! NwRootExtr 
      integer daylastharvest,swharvest,swgrazing                      ! Nwgrassland
      real*8 dmharvest1,dmharvest2, dmlastharvest,dateharvest(999)    ! Nwgrassland
      real*8 nsuptab(magrs),dmfac(magrs),relnitab(2*magrs),nsupply    ! Nwgrassland
      real*8 wrtmax,grazingfactor                                     ! Nwgrassland
      character crpfil*(*),pathcrop*(*)
! locals
      integer   swc2ecf
      real*8    dnrinput(magrs),cfinput(magrs),chinput(magrs)
      character message*200,filnam*200
! ----------------------------------------------------------------------


! --- initialise and start reading
      filnam = trim(pathcrop)//trim(crpfil)//'.crp'
      crp = getun2 (10,90,2)
      call rdinit(crp,logf,filnam)

! --- ET related params  ---------

! --- crop factor or crop height
      call rdsinr ('swcf',1,2,swcf)

! --- check use of crop factors in case of ETref
      if (swetr.eq.1 .and. swcf.eq.2) then
        message = 'If ETref is used (SWETR = 1), always define crop '//
     &           'factors (SWCF = 1)' 
        call fatalerr ('ReadGrass',message)
      endif

      if (swcf.eq.1) then
! ---   crop factor is input
        call rdador ('dnr',0.0d0,366.0d0,dnrinput,(magrs),ifnd)
        call rdfdor ('cf',0.0d0,2.0d0,cfinput,(magrs),ifnd)
! ---   store values in cftb
        do i = 1,ifnd
          cftb(i*2) = cfinput(i) 
          cftb(i*2-1) = dnrinput(i)
        enddo
        chtb = -99.99d0
      else
! ---   crop height is input
        call rdador ('dnr',0.0d0,366.0d0,dnrinput,(magrs),ifnd)
        call rdfdor ('ch',0.0d0,1.0d4,chinput,(magrs),ifnd)
! ---   store values in chtb
        do i = 1,ifnd
          chtb(i*2) = chinput(i) 
          chtb(i*2-1) = dnrinput(i)
        enddo
        cftb = -99.99d0
      endif

! --- reflection coefficient and crop resistance
      if (swcf.eq.1) then
! ---   use standard values for ETref
        albedo = 0.23d0
        rsc = 70.0d0
        rsw = 0.0d0
      else
! ---   use crop specific values
        call rdsdor ('albedo',0.0d0,1.0d0,albedo)
        call rdsdor ('rsc',0.0d0,1.0d6,rsc)
        call rdsdor ('rsw',0.0d0,1.0d6,rsw)
      endif


! --- crop growth related params ---------

! --- initial
      call rdsdor ('tdwi',0.0d0,10000.0d0,tdwi)
      call rdsdor ('laiem',0.0d0,10.0d0,laiem)
      call rdsdor ('rgrlai',0.0d0,1.0d0,rgrlai)

! --- green area
      call rdador ('slatb',0.0d0,366.0d0,slatb,30,ifnd)
      call rdsdor ('ssa',0.0d0,1.0d0,ssa)
      call rdsdor ('span',0.0d0,366.0d0,span)
      call rdsdor ('tbase',-10.0d0,30.0d0,tbase)

! --- assimilation
      call rdsdor ('kdif',0.0d0,2.0d0,kdif)
      call rdsdor ('kdir',0.0d0,2.0d0,kdir)
      call rdsdor ('eff',0.0d0,10.0d0,eff)
      call rdador ('amaxtb',0.0d0,366.0d0,amaxtb,30,ifnd)
      call rdador ('tmpftb',-10.0d0,50.0d0,tmpftb,30,ifnd)
      call rdador ('tmnftb',-10.0d0,50.0d0,tmnftb,30,ifnd)

! --- conversion of assimilates into biomass
      call rdsdor ('cvl',0.0d0,1.0d0,cvl)
      call rdsdor ('cvr',0.0d0,1.0d0,cvr)
      call rdsdor ('cvs',0.0d0,1.0d0,cvs)

! --- maintenance respiration
      call rdsdor ('q10',0.0d0,5.0d0,q10)
      call rdsdor ('rml',0.0d0,1.0d0,rml)
      call rdsdor ('rmr',0.0d0,1.0d0,rmr)
      call rdsdor ('rms',0.0d0,1.0d0,rms)
      call rdador ('rfsetb',0.0d0,366.0d0,rfsetb,30,ifnd)

! --- partitioning
      call rdador ('frtb',0.0d0,366.0d0,frtb,30,ifnd)
      call rdador ('fltb',0.0d0,366.0d0,fltb,30,ifnd)
      call rdador ('fstb',0.0d0,366.0d0,fstb,30,ifnd)

! --- death rates
      call rdsdor ('perdl',0.0d0,3.0d0,perdl)
      call rdador ('rdrrtb',0.0d0,366.0d0,rdrrtb,30,ifnd)
      call rdador ('rdrstb',0.0d0,366.0d0,rdrstb,30,ifnd)

! --- water use
      swroottyp = 1
      if(rdinqr('swroottyp')) then
        call rdsinr ('swroottyp',1,2,swroottyp)                         ! NwRootExtr
      endif
      if (swroottyp.eq.1) then                                          ! NwRootExtr
        call rdsdor ('hlim1' ,-100.0d0,100.0d0,hlim1)                   !
        call rdsdor ('hlim2u',-1000.0d0,100.0d0,hlim2u)                 !
        call rdsdor ('hlim2l',-1000.0d0,100.0d0,hlim2l)                 !
        call rdsdor ('hlim3h',-10000.0d0,100.0d0,hlim3h)                !
        call rdsdor ('hlim3l',-10000.0d0,100.0d0,hlim3l)                !
        call rdsdor ('hlim4' ,-16000.0d0,100.0d0,hlim4)                 !
                                                                        !
        call rdsdor ('adcrh',0.0d0,5.0d0,adcrh)                         !
        call rdsdor ('adcrl',0.0d0,5.0d0,adcrl)                         !
!       Criticial stress index for compensation of root water uptake (-)
        alphacrit = 1.0d0
        if(rdinqr('alphacrit')) then
          call rdsdor ('alphacrit',0.2d0,1.0d0,alphacrit)
        endif

      else                                                              !
        call rdsdor ('wiltpoint',-1.0d6,-1.0d3,wiltpoint)               !
        call rdsdor ('rootradius',0.0001d0,1.0d0,rootradius)            !
        call rdsdor ('rootcoefa',0.0d0,1.0d0,rootcoefa)                 !
      endif                                                             ! NwRootExtr

!     correction factor to relate potential transpiration to reference crop
!     (default = 1.0, range = 0.8 - 1.2)
      cfet = 1.0d0
      if(rdinqr('cfet')) then
        call rdsdor ('cfet',0.5d0,1.5d0,cfet)
      endif


! --- salt stress
      if (flsolute) then
        call rdsdor ('ecmax', 0.0d0,20.0d0,ecmax)
        call rdsdor ('ecslop',0.0d0,40.0d0,ecslop)
        call rdsdor ('c2eca', 0.0d0,1000.0d0,c2eca)
        call rdsdor ('c2ecb', 0.0d0,10.0d0,c2ecb)
        call rdsinr ('swc2ecf',1,2,swc2ecf)
        if (swc2ecf.eq.1) then
          call rdsdor ('c2ecf', 0.d0, 10.d0, c2ecf(1))
          if(numlay.ge.2) then
            do i = 2,numlay
              c2ecf(i) = c2ecf(1)
            enddo
          endif
        else if (swc2ecf.eq.2) then
          call rdfdor ('c2ecf', 0.d0, 10.d0, c2ecf,maho,numlay)
        endif
      endif

! --- interception
      call rdsdor ('cofab',0.0d0,1.0d0,cofab)

! --- rooting
      call rdador ('rdctb',0.0d0,100.0d0,rdctb,22,ifnd)
      call rdador ('rlwtb',0.0d0,5000.0d0,rlwtb,22,ifnd)
      call rdsdor ('rdi',0.0d0,1000.0d0,rdi)
      call rdsdor ('rri',0.0d0,100.0d0,rri)
      call rdsdor ('rdc',0.0d0,1000.0d0,rdc)

! --- nutrient stress
      call rdador ('nsuptab',0.0d0,1000.0d0,nsuptab,magrs,ifnd)
      call rdfdor ('dmfac',0.0d0,1.0d0,dmfac,magrs,ifnd)
!     store values in relni
      do i = 1,ifnd
        relnitab(i*2) = dmfac(i)
        relnitab(i*2-1) = nsuptab(i)
      enddo
      call rdsdor ('Nsupply',0.0d0,1000.0d0,Nsupply)


! --- harvest: dm or dates
      call rdsinr ('swharvest',1,2,swharvest)
      if(swharvest.eq.1) then
        call rdsdor ('dmharvest1',0.0d0,100000.0d0,dmharvest1)
        call rdsinr ('swgrazing',1,2,swgrazing)
        if(swgrazing.eq.2) then
            call rdsdor ('dmharvest2',0.0d0,100000.0d0,dmharvest2)
        else if (swgrazing.eq.1) then
            call rdsdor ('grazingfactor',0.0d0,1.0d0,grazingfactor)
        end if
        call rdsinr ('daylastharvest',1,366,daylastharvest)
        call rdsdor ('dmlastharvest',0.0d0,100000.0d0,dmlastharvest)
      elseif(swharvest.eq.2) then
        call rdatim ('dateharvest',dateharvest,999,ifnd)
      endif

! --- maximum weight of roots (kg/ha dm)
      call rdsdor ('wrtmax',0.0d0,100000.0d0,wrtmax)

! --- determine whether irrigation scheduling is applied
      call rdsinr ('schedule',0,1,schedule)

      close (crp)

! --- CALCULATE NORMALIZED CUMULATIVE ROOT DENSITY FUNCTION

      if (swroottyp .eq. 1) then                                        ! NwRootExtr

! ---   specify array ROOTDIS with root density distribution
        do i = 0,100
          depth = 0.01d0 * dble(i)
          rootdis(i*2+1) = depth
          rootdis(i*2+2) = afgen(rdctb,22,depth)
        enddo

! ---   calculate cumulative root density function
        do i = 1,202,2
! ---     relative depths
          cumdens(i) = rootdis(i)
        enddo
        sum = 0.d0
        cumdens(2) = 0.d0
        do i = 4,202,2
! ---     cumulative root density
          sum = sum + (rootdis(i-2)+rootdis(i)) * 0.5d0
     &               * (cumdens(i-1)-cumdens(i-3))
          cumdens(i) = sum
        enddo

! ---   normalize cumulative root density function to one
        do i = 2,202,2
          cumdens(i) = cumdens(i) / sum
        enddo

      endif                                                             ! NwRootExtr


      return
      end

! ----------------------------------------------------------------------
      subroutine rddre (drfil,pathdrain,nrlevs,nrpri,nrsec,l,
     & zbotdr,widthr,taludr,rdrain,rinfi,rentry,rexit,gwlinf,swdtyp,
     & wlptab,swsec,wls1,osswlm,nmper,wlstar,impend,swman,wlsman,
     & wscap,swqhr,hbweir,alphaw,betaw,nqh,hqhtab,qqhtab,dropr,
     & gwlcrit,wlstab,sttab,swstini,swst,wlsbak,swsrf,nphase,hcrit,
     & hdepth,vcrit,wlp1,nodhd,numnod,dz,wldip,numadj,logf,intwl,
     & swnrsrf,rsurfdeep,rsurfshallow,t1900,cofintfl,expintfl,swscre,
     & swdivd,cofani,numlay,tstart,tend,swdislay,swtopdislay,ztopdislay,
     & ftopdislay,SwTopnrsrf,         swdivdinf,FacDpthInf)             !  Divdra, infiltration

! ----------------------------------------------------------------------
!     UpDate             : 20080109
!     Date               : 20010605                   
!     Purpose            : reading multilevel drainage characteristics 
!                          and specification of the surface water system
!                          for a period up to one year;  
!
! --- 1 Reading extended drainage input from .dra-file
! --- 2 Initializations
!
! --- Initializations:
! -1- wlp1: water level in primary system   (SWSRF = 3)
! -2- wls1: water level in secondary system (SWSRF = 2 or 3, SWSEC = 1 or 2) 
! -3- HBWEIR(IMPER) in case of table discharge relation (SWQHR = 2)
! -4- NUMADJ: number of target level adjustments
! -5- sttab(22,2) table with storage as a function of water level
! ---    sttab(i,1) contains levels:
! ---    i=1: +100 cm; i=2: 0 cm; i=22: bottom of deepest dr. medium
! ---    sttab(i,2) contains storage expressed as surface layer 
! ----------------------------------------------------------------------
      implicit none
      include 'arrays.fi'

! --- global
      integer   nrpri,nrsec,nrlevs,swdtyp(Madr),nmper,swman(mamp)
      integer   swqhr,nqh(mamp),swsec,swsrf,nphase(mamp)
      integer   nodhd(mamp),numnod,numadj,logf,intwl(mamp),swnrsrf
      integer   swdivd,numlay,swscre, swdivdinf                         !  Divdra, infiltration
      real*8    l(Madr),zbotdr(Madr),widthr(Madr),taludr(Madr)
      real*8    rdrain(Madr),rinfi(Madr),rentry(Madr),rexit(Madr)
      real*8    gwlinf(Madr),wlptab(2*mawlp)
      real*8    rsurfdeep,rsurfshallow,impend(mamp),t1900
      real*8    wls1,osswlm,wlstar,wscap(mamp),hbweir(mamp),alphaw(mamp)
      real*8    hqhtab(mamp,mamte),qqhtab(mamp,mamte)
      real*8    betaw(mamp),dropr(mamp*mamte),hdepth(mamp*mamte)
      real*8    wlsman(mamp,mamte),wlstab(2*mawls),sttab(22,2),wlsbak(4)
      real*8    swstini,swst,hcrit(mamp,mamte),vcrit(mamp,mamte)
      real*8    afgen,wlp1,dz(macp),gwlcrit(mamp,mamte),wldip(mamp)
      real*8    cofintfl,expintfl,cofani(maho)
      real*8    tend,tstart,FacDpthInf                                  !  Divdra, infiltration
      integer   swdislay,swtopdislay(madr),SwTopnrsrf
      real*8    ztopdislay(Madr),ftopdislay(madr)
      character drfil*16,pathdrain*80

! --- local
      integer   dra,level(Madr),itab,i,ilev,getun2
      integer   iph,nrman1,nrman2,node,imper,imperb,imperi,ifnd
      integer   imper_4b(mamp),imper_4c(mamp),imper_4d(mamp)
      integer   imper_4e1(mamp*mamte),imper_4e2(mamp*mamte)
      integer   imptab(mamte),impphase(mamp*mamte), nmper2
      real*8    wlp(mawlp),wls(mawlp),zb, hhtab(mamp),qhtab(mamp)
      real*8    wlsman_t(mamp*mamte),gwlcrit_t(mamp*mamte)
      real*8    hcrit_t(mamp*mamte),vcrit_t(mamp*mamte)
      real*8    dates(mabbc)
      real*8    wdepth,wbreadth,wvolum,dep,swstlev,sofcu,altcu
      logical   flweir(mamp),exists(mamp),flzero(mamp), rdinqr          !  Divdra, infiltration
      character filnam*80,messag*200
      real*8    small
      data      small     /0.0001d0/

! ----------------------------------------------------------------------

! --- open file with extended drainage data
      filnam = trim(pathdrain)//trim(drfil)//'.dra'
      dra = getun2 (10,90,2)
      call rdinit(dra,logf,filnam)

! --- division of drainage fluxes
      call rdsinr ('swdivd',0,1,swdivd)
      if (swdivd.eq.0) then
          write(messag,'(3a)')
     &    ' Variabel SWDIVD=0 in input file : ',trim(filnam),
     &    ' this is not recommended and may cause numerical instability'
          call warn ('rddre',messag,logf,swscre)
      endif
      if (swdivd .eq. 1) then
        if(rdinqr('swdivdinf')) then
          call rdsinr ('swdivdinf',0,1,swdivdinf)                       !  Divdra, infiltration
          call rdsdor ('FacDpthInf',0.d0,1.d0,FacDpthInf)
        else
          swdivdinf = 0
        endif                                                           !  Divdra, infiltration

        call rdfdor ('cofani',0.d0,1000.d0,cofani,maho,numlay)
      endif

! --- altitude of control unit (relative to reference level)
      call rdsdor ('altcu',-3.0d5,3.0d5,altcu)

! --- part 1

! --  number of drainage levels
      call rdsinr ('nrsrf',1,Madr,nrlevs)
      if (nrlevs.gt.5) then
          write(messag,'(3a)')
     &    ' Number of Drainage levels >5 in inputfile : ',trim(filnam),
     &    '   part of the output is limited to 5 levels'
          call warn ('rddre',messag,logf,swscre)
      endif

! --  top of model dicharge layer, determined by factor or direct input
      if (swdivd .eq. 1) then
        call rdsinr ('swdislay',0,2,swdislay)
        if (swdislay .eq. 1) then
          call rdfinr ('swtopdislay',0,1,swtopdislay,madr,nrlevs)
          call rdfdor('ztopdislay',-1.0d4,0.0d0,ztopdislay,madr,nrlevs)
        elseif (swdislay .eq. 2) then
          call rdfinr ('swtopdislay',0,1,swtopdislay,madr,nrlevs)
          call rdfdor('ftopdislay',0.0d0,1.0d0,ftopdislay,madr,nrlevs)
        endif
      endif


! --- characteristics of each drainage level 
      call rdfinr ('lev',1,Madr,level,Madr,nrlevs)
      call rdfinr ('swdtyp',0,1,swdtyp,Madr,nrlevs)
      call rdfdor ('l',1.0d0,100000.0d0,l,Madr,nrlevs)
      call rdfdor ('zbotdre',(altcu-1.0d3),(altcu-1.0d-2),
     &              zbotdr,Madr,nrlevs)
      call rdfdor ('gwlinf',-10000.0d0,0.0d0,gwlinf,Madr,nrlevs)
      call rdfdor ('rdrain',1.0d0,1.0d5,rdrain,Madr,nrlevs)
      call rdfdor ('rinfi',1.0d0,1.0d5,rinfi,Madr,nrlevs)
      call rdfdor ('rentry',0.0d0,10.0d0,rentry,Madr,nrlevs)
      call rdfdor ('rexit',0.0d0,10.0d0,rexit,Madr,nrlevs)
      call rdfdor ('widthr',0.0d0,10000.0d0,widthr,Madr,nrlevs)
      call rdfdor ('taludr',1.0d-2,5.0d0,taludr,Madr,nrlevs)

! -   conversions and security checks....
      do i = 1, nrlevs
!       conversions
        l(i) = l(i)*100.0d0
        zbotdr(i) = zbotdr(i) -altcu
        if (swdivd.eq.1 .and. swdislay.eq.1) then
          ztopdislay(i) = ztopdislay(i) - altcu
          if(ztopdislay(i).lt.zbotdr(i)) then
            messag = 'Ztopdislay cannot be below Zbotdre, verify input!' 
            call fatalerr ('Rddra',messag)
          endif
        endif
! -     security checks....
! -     0 - verify levels-index
! -     1 - levels must be ordered, starting with the deepest
! -     2 - zbotdr always below surface level
! -     3 - gwlinf must be below bottom of deepest drainage medium
! -     4 - widthr must be greater than zero for non-tube drain systems
        if (level(i).ne.i) then
          messag = 'Drainage level index is not consistent' 
          call fatalerr ('Rddra',messag)
        endif
        if (i .gt. 1) then
          if (zbotdr(i) .lt. zbotdr(i-1)) then
            messag = 'Levels must be ordered, starting with deepest' 
            call fatalerr ('Rddra',messag)
          endif
        endif
        if (gwlinf(i).gt.zbotdr(i)) then
            messag = 'Gwlinf should be lower than Zbotdr !' 
            call fatalerr ('Rddra',messag)
        endif
        if (swdtyp(i).eq.0 .and.widthr(i).lt.small) then
            write(messag,'(a,f10.5)') 
     &              'widthr must be greater than',small
            call fatalerr ('Rddra',messag)
        endif

      enddo

! --- type of highest drainage level
      call rdsinr ('swnrsrf',0,2,swnrsrf)
      if (swnrsrf.eq.1) then
        call rdsdor ('rsurfdeep',0.001d0,1000.0d0,rsurfdeep)
        call rdsdor ('rsurfshallow',0.001d0,1000.0d0,rsurfshallow)
      else if (swnrsrf.eq.2) then
        call rdsdor ('cofintfl',0.01d0,10.0d0,cofintfl)     
        call rdsdor ('expintfl',0.1d0,1.0d0,expintfl) 
      endif


! kroes 20080707 : allow disabling of option
      if (swdivd .eq. 1) then
        if (swnrsrf.gt.0)  call rdsinr ('SwTopnrsrf',0,1,SwTopnrsrf)
      endif


! --- part 2a
      call rdsinr ('swsrf',1,3,swsrf)
      if (swsrf.eq.3) then
        nrpri = 1
        nrsec = nrlevs-nrpri
      elseif (swsrf.eq.2) then
        nrpri = 0
        nrsec = nrlevs-nrpri
      elseif (swsrf.eq.1) then
! ---   no surface water system...ready
        close (dra)
        return
      endif

      if (swdtyp(1+nrpri).ne.0) then
        messag = 'Deepest sec. level must be open.' 
        call fatalerr ('Rddra',messag)
      endif

! --- part 2b

! --- read table with water levels in the primary system (SWSRF=3)
      if (swsrf.eq.3) then
! ---   init table
        do 8 i = 1,2*mawlp
          wlptab(i) = 0.0d0
    8   continue
! -     read and store
        call rdatim ('date1',dates,mawlp,ifnd)
! -     at least one date must be within simulation period
        call checkdate(ifnd,dates,tend,tstart,'date1 ',
     &                 'readswap//swsrf=3        ')
        call rdfdor
     &        ('wlp',(altcu-1000.0d0),(altcu-0.01d0),wlp,mawlp,ifnd)
        do i = 1, ifnd
          wlptab(i*2) = wlp(i) - altcu
          wlptab(i*2-1) = dates(i)
        enddo
! ---   set initial value
        wlp1 = afgen (wlptab,2*mawlp,t1900-1.d0)

! ---   Ready in case of only a primary system ??
!       if (NRSEC.LT.1) then
!          CLOSE(DRE)
!          Return
!       endif 

      endif

! --- part 2c

      call rdsinr ('swsec',1,2,swsec)

! --- part 3

      if (swsec.eq.1) then
! ---   surface water level of secondary system is input
! ---   position file pointer

! ---   init table
        do 60 i = 1,2*mawls
          wlstab(i) = 0.0d0
   60   continue
! -     read and store
        call rdatim ('date2',dates,mawlp,ifnd)
! -     at least one date must be within simulation period
        call checkdate(ifnd,dates,tend,tstart,'date2 ',
     &                 'readswap//swsec=1        ')

        call rdfdor
     &         ('wls',(altcu-1000.0d0),(altcu-0.01d0),wls,mawlp,ifnd)
        do i = 1, ifnd
          wlstab(i*2) = wls(i) -altcu
          wlstab(i*2-1) = dates(i)
        enddo
! ---   set initial value
        wls1 = afgen (wlstab,2*mawls,t1900-1.0d0)

! ---   part 4a

! --- surface water level of secondary system is simulated
      elseif (swsec.eq.2) then
        call rdsdor ('wlact',dble(zbotdr(1+nrpri)+altcu)
     &       ,dble(altcu),wls1)
        wls1 = wls1 - altcu
        call rdsdor ('osswlm',0.0d0,10.0d0,osswlm)

! ---   part 4b

        call rdsinr ('nmper',1,mamp,nmper)
        wlstar = wls1

        call rdfinr ('imper_4b',1,nmper,imper_4b,mamp,nmper)
        call rdftim ('impend',impend,mamp,nmper)
        call rdfinr ('swman',1,2,swman,mamp,nmper)
        call rdfdor ('wscap',0.0d0,10.0d0,wscap,mamp,nmper)
        call rdfdor ('wldip',0.0d0,100.0d0,wldip,mamp,nmper)
        call rdfinr ('intwl',1,31,intwl,mamp,nmper)

! -     for each type of management: count number of periods 
        nrman1 = 0
        nrman2 = 0
        do imper = 1, nmper
          if (swman(imper).eq.2 .and. intwl(imper).lt.1) then
            messag = 'intwl (management interval) must be > = 1d'
            call fatalerr ('Rddra',messag)
          endif
          wldip(imper) = abs(wldip(imper))
          if (swman(imper).eq.1) then 
            nrman1 = nrman1+1
          elseif (swman(imper).eq.2) then
            nrman2 = nrman2+1
          else
            messag = 'SWMAN is out of range.'
            call fatalerr ('Rddra',messag)
          endif
        enddo
        if ((nrman1+nrman2).ne.nmper) then
          messag = 'nrman1+nrman2 does not match nmper'
          call fatalerr ('Rddra',messag)
        endif

! ---   type of discharge relationship
        call rdsinr ('swqhr',1,2,swqhr)

        IF (SWQHR.EQ.1) THEN

! ---     part 4c

          call rdsdor ('sofcu',0.1d0 ,100000.0d0,sofcu)

          call rdfinr ('imper_4c',1,nmper,imper_4c,mamp,nmper)
          zb = min(zbotdr(1),zbotdr(2))
          call rdfdor 
     &           ('hbweir',(altcu+zb),(altcu+100.0d0),hbweir,mamp,nmper)
          call rdfdor ('alphaw',0.1d0,50.0d0,alphaw,mamp,nmper)
          call rdfdor ('betaw',0.5d0,3.0d0,betaw,mamp,nmper)

! ---     convert and check values
!         initialise
          imperb = 0
          imperi = 0
          do imper = 1, nmper
            hbweir(imper) = hbweir(imper)-ALTCU
! ---       correction for units
            alphaw(imper) = alphaw(imper)* (8.64d0* 100.0d0**
     &                      (1.0d0-betaw(imper))/SOFCU)     
            if (hbweir(imper).lt.zbotdr(1+NRPRI)) then
              messag = 'Weir crest level is below bottom of'
     &                 //' deepest channel of secondary system.'
              call fatalerr ('Rddra',messag)
            endif
! --- check for target level above channel bottom when supply is 
! --- attempted (system may never become dry in this case)
            if (swman(imper).eq.1 .and. wscap(imper).gt.1.d-7 .and.
     &         (hbweir(imper)-wldip(imper)).lt.
     &         (zbotdr(1+NRPRI)+1.d-4)) then
              messag = 'HBWEIR/WLDIP !'
     &                 //' Supply not possible =< zbotdr !'
              call fatalerr ('Rddra',messag)
            endif

            if (imper.ne.imperb) then
              imperb = imper
              imperi = imperi + 1
            else
              messag = '4c imper not unique'
              call fatalerr ('Rddra',messag)
            endif
          enddo

! ---     check number of records.. 
          if (imperi.ne.nmper) then
              messag = 'part 4c - not enough records'
              call fatalerr ('Rddra',messag)
          endif

        elseif (swqhr.eq.2) then

! ---     part 4d

! --      initialise
          imperb = 0
          imperi = 0
          do 34 imper = 1,nmper
            nqh (imper) = 0
            flweir (imper) = .false.
            exists (imper) = .false.
            flzero (imper) = .false.
34        continue

! --      read and store
          call rdainr ('imper_4d',1,nmper,imper_4d,mamp,ifnd)
          call rdfinr ('imptab',1,mamte,imptab,mamte,ifnd)
          call rdfdor 
     &         ('htab',(altcu-1000.0d0),(altcu+100.0d0),hhtab,mamp,ifnd)
          call rdfdor ('qtab',0.0d0,500.0d0,qhtab,mamp,ifnd)

! ---     convert
          do i = 1, ifnd
            hqhtab(imper_4d(i),imptab(i)) = hhtab(i) - Altcu
            qqhtab(imper_4d(i),imptab(i)) = qhtab(i)
          enddo

! ---     check values
          do i = 1, ifnd
            imper = imper_4d(i)
            itab = imptab(i)
            nqh(imper) = nqh(imper)+1
            exists(imper) = .true.
            if (qqhtab(imper,itab).lt.0.000001d0) flzero(imper)=.true.
            if (hqhtab(imper,itab).lt.zbotdr(1+nrpri)) then
              messag = 'Level in q-h table below bottom of'
     &                 //' deepest channel of secondary system.'
              call fatalerr ('Rddra',messag)
            endif
            if (imper.ne.imperb) then
              imperb = imper
              imperi = imperi + 1
            endif

! -         establish hbweir (at level where qtab = 0) 
            if (qqhtab(imper,itab).lt.0.0001d0.and..not.flweir(imper)) 
     &      then
              hbweir(imper) = hqhtab(imper,itab)
              flweir(imper) = .true.
            endif 

! -         consistency checks
            if (nqh(imper).ne.itab) then
              messag = 'qh-table / imper - itab mismatch'
              call fatalerr ('Rddra',messag)
            endif
            if (itab.eq.1) then
              if (abs(hqhtab(imper,itab)-100.0d0).gt.0.00001d0) then
              messag = 'First value in htab should be altcu+100.0'
              call fatalerr ('Rddra',messag)
              endif
            endif
            if (itab.gt.1) then
              if ((hqhtab(imper,itab).ge.hqhtab(imper,itab-1)).or.
     &           (qqhtab(imper,itab).gt.qqhtab(imper,itab-1))) then
                 messag = 'qh-table - no descending values'
                 call fatalerr ('Rddra',messag)
              endif
            endif

          enddo

! ---     check number of periods.. 
          if (imperi.ne.nmper) then
            messag = 'part 4d - number of periods incorrect.'
            call fatalerr ('Rddra',messag)
          endif

! ---     check that QQHTAB goes down to zero
          do imper = 1,nmper
            if (exists(imper).and..not.flzero(imper)) then
              messag = 'qqhtab not going down to zero.'
              call fatalerr ('Rddra',messag)
            endif
          enddo

        endif

! ---   part 4e1

        if (nrman2.gt.0) then

          imperb = 0
          imperi = 0

! ---     read table with drop rates (length of table must be equal 
!            to the nr of management periods referred to in tabel 4e2
          call rdainr('imper_4e1',1,nmper,imper_4e1,(mamp*mamte),nmper2)
          call rdfdor('dropr',0.0d0,100.0d0,dropr,(mamp*mamte),nmper2)
          call rdfdor('hdepth',-100d0,0.0d0,hdepth,(mamp*mamte),nmper2)

          do i = 1, nmper2
            imper = imper_4e1(i)
            hdepth(i) = -abs(hdepth(i))

! ---       determine compartment number related to hdepth
            dep = 0.0d0
            node = 1
            do while (hdepth(i).lt.(dep-1.0d-6).and.node.le.numnod) 
              nodhd(imper) = node
              dep = dep-dz(node)
              node = node + 1
            enddo

            if (swman(imper) .ne. 2) then
              messag = '#4e1 swman - imper mismatch.'
              call fatalerr ('Rddra',messag)
            endif
            if (imper.ne.imperb) then
              imperb = imper
              imperi = imperi + 1
            else
              messag = 'Two drop rates at the same period.'
              call fatalerr ('Rddra',messag)
            endif
          enddo
          if (imperi .ne. nrman2) then
            messag = '#4e1 number of periods for drop rate incorrect.'
            call fatalerr ('Rddra',messag)
          endif

! ---   part 4e2

          imperb = 0
          imperi = 0

          do 54 imper = 1,nmper
            nphase(imper) = 0
   54     continue

! --      read and store
          call rdainr('imper_4e2',1,nmper,imper_4e2,(mamte*mamp),ifnd)
          call rdfinr('impphase',1,mamte,impphase,(mamte*mamp),ifnd)
          call rdfdor 
     &       ('wlsman',(altcu-500.0d0),altcu,wlsman_t,(mamp*mamte),ifnd)
          call rdfdor 
     &       ('gwlcrit',-500.0d0,0.0d0,gwlcrit_t,(mamp*mamte),ifnd)
          call rdfdor('hcrit',-1000.0d0,0.0d0,hcrit_t,(mamp*mamte),ifnd)
          call rdfdor('vcrit',0.0d0,20.0d0,vcrit_t,(mamp*mamte),ifnd)

! ---     convert
          do i = 1, ifnd
            wlsman(imper_4e2(i),impphase(i)) = wlsman_t(i) - Altcu
            gwlcrit(imper_4e2(i),impphase(i)) = gwlcrit_t(i)
            hcrit(imper_4e2(i),impphase(i)) = hcrit_t(i)
            vcrit(imper_4e2(i),impphase(i)) = vcrit_t(i)
          enddo

! ---     check values
          do i = 1, ifnd
            imper = imper_4e2(i)
            iph = impphase(i)

            nphase(imper) = nphase(imper) + 1
            if (wlsman(imper,iph).lt.zbotdr(1+nrpri)) then
              messag = 'Level of automatic weir below bottom'
     &                 //' deepest channel of secondary system.'
              call fatalerr ('Rddra',messag)
            endif
            if (swman(imper) .ne. 2) then
              messag = '#4e2 swman - imper mismatch'
              call fatalerr ('Rddra',messag)
            endif
            if (imper.ne.imperb) then
              imperb = imper
              imperi = imperi + 1
            endif
          enddo

          if (imperi .ne. nrman2) then
            messag = '#4e2 inconsistency between the nr of periods'
     &            //'with automatic weir (tabel 4e (IMPER4e2)) and'
     &            //' swman in tabel4b (IMPER_4b)'
            call fatalerr ('Rddra',messag)
          endif

! ---     consistency checks WLSMAN, GWLCRIT, HCRIT and VCRIT
          do 446 imper = 1,nmper
            if (swman(imper).eq.2) then
              if (abs(gwlcrit(imper,1)) .gt. 0.01d0) then
                messag = '#4e2 - gwlcrit(1) must be 0.'
                call fatalerr ('rddra',messag)
              endif
              if (abs(vcrit(imper,1)) .gt. 0.01d0) then
                messag = '#4e2 - vcrit(1) must be 0.'
                call fatalerr ('rddra',messag)
              endif
              if (abs(hcrit(imper,1)) .gt. 0.01d0) then
                messag = '#4e2 - hcrit(1) must be 0.'
                call fatalerr ('rddra',messag)
              endif
              if (hbweir(imper).gt. (wlsman(imper,1)-0.99999d0)) then
                messag = '#4e2 - HBWEIR within 1 cm of wlsman(1)'
                call fatalerr ('Rddra',messag)
              endif

              do 448 iph = 2,nphase(imper)
                if (wlsman(imper,iph).lt.wlsman(imper,iph-1)) then
                  messag = '#4e2 - wlsman inconsistent'
                  call fatalerr ('rddra',messag)
                endif
                if (gwlcrit(imper,iph).gt.gwlcrit(imper,iph-1)) then
                  messag = '#4e2 - gwlcrit inconsistent'
                  call fatalerr ('rddra',messag)
                endif
                if (hcrit(imper,iph).gt.hcrit(imper,iph-1)) then
                  messag = '#4e2 - hcrit inconsistent'
                  call fatalerr ('rddra',messag)
                endif
                if (vcrit(imper,iph).lt.vcrit(imper,iph-1)) then
                  messag = '#4e2 - vcrit inconsistent'
                  call fatalerr ('rddra',messag)
                endif
448           continue
            endif 
446       continue
        endif
      endif
! ----------------------------------------------------------------------
! --- initialize counter for number of target level adjustments
      numadj = 0

! --- sttab(i,1) contains depths

! --- sw-levels, to be used in piece-wise linear functions (first level)
! --- is 100 cm above soil surface to allow for situations with ponding)
      sttab(1,1) = 100.0d0
      sttab(2,1) =   0.0d0
      do 100 i = 3,22
! ---   layer between surface level and deepest
! ---   drain/channel bottom is divided into 20 compartments
        sttab(i,1) = zbotdr(1+nrpri)*(i-2)/20.0d0
100   continue

! --- sttab(i,2) contains storage expressed in cm

! --- calculation of surface water storage (in cm, i.e. volume
! --- per unit area), as a function of sw-level, only for open channels:
      do 104 i = 1,22
        sttab(i,2) = 0.0d0
        do 108 ilev = 1+nrpri,nrlevs
          if (swdtyp(ilev).eq.0.and.sttab(i,1).gt.zbotdr(ilev)) then
! ---       for levels above soil surface the volume-increment is
! ---       computed for a rectangle, and not a trapezium
            if (sttab(i,1) .le. 0.0d0) then
              wdepth = sttab(i,1)-zbotdr(ilev)
              wvolum = wdepth*(widthr(ilev)+wdepth/taludr(ilev))
            else
              wdepth = -zbotdr(ilev)
              wvolum = wdepth*(widthr(ilev)+wdepth/taludr(ilev))
              wbreadth = widthr(ilev) + 2*wdepth/taludr(ilev)
              wdepth = sttab(i,1)
              wvolum = wvolum + wbreadth*wdepth
            endif
            sttab(i,2) = sttab(i,2)+wvolum/l(ilev)
          endif
108     continue
104   continue

! --- initial storage swstini
      swstini = swstlev (wls1,sttab)
      swst = swstini

! --- initialize memorization of wls for most recent 4 timesteps 
      do i=1,4
        wlsbak(i) = 0.0d0
      enddo

! --- close input file with lateral boundary conditions
      CLOSE (DRA)         

      RETURN
      END

! ----------------------------------------------------------------------
      subroutine checkdate(ifnd,dates,tend,tstart,namedat,topic) 
! ----------------------------------------------------------------------
!     Date               : April 2006   
!     Purpose            : check range of input dates to range of simulation period
! ----------------------------------------------------------------------
      implicit none

! --- global
      integer   ifnd
      real*8    dates(ifnd), tend, tstart
      character messag*200, namedat*5, topic*(*)

! ----------------------------------------------------------------------
! --- local
      integer i
      logical fldaterr

! ----------------------------------------------------------------------

!   - at least one input date must be within simulation period or 
!     simulation period should be completely within range of input dates
      fldaterr = .true.
      i = 1
      do while (fldaterr .and. i.le.ifnd)
         if(dates(i).gt.tstart-1.d-6.and.dates(i).lt.tend+1.d-6) 
     &      fldaterr=.false.
         i = i + 1
      enddo
      if(fldaterr) then
         if(dates(1).lt.tstart+1.d-6 .and. dates(ifnd).gt.tend-1.d-6)
     &      fldaterr=.false.
      endif
      if(fldaterr) then
        messag = 'Fatal '//namedat//
     &           ', no input date within simulation period'
        call fatalerr(topic,messag)
      endif

      return
      end
