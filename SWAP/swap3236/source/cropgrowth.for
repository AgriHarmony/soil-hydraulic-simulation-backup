! File VersionID:
!   $Id: cropgrowth.for 197 2011-02-18 10:42:47Z kroes006 $
! ----------------------------------------------------------------------
      subroutine CropGrowth(task) 
! ----------------------------------------------------------------------
!     Date               : Aug 2004   
!     Purpose            : Call proper crop routines for initialization,
!                          calculation of rate/state variables and output
! ----------------------------------------------------------------------

      use variables
      implicit none

      integer task,numcrop
      character messag*200

      goto (1000, 2000, 3000) task

1000  continue

! === initialization =========================================================

      if (flbaresoil) then
! --- set bare soil condition  -----------------------------------------------
        call nocrop (rd,lai,cf,ch,albedo,rsc)
      else
! ---   fixed crop development -----------------------------------------------
        if (croptype(icrop) .eq. 1) call CropFixed(1)
! ---   detailed crop growth -------------------------------------------------
        if (croptype(icrop) .eq. 2) call Wofost(1)
! ---   detailed grass growth  -----------------------------------------------
        if (croptype(icrop) .eq. 3) call Grass(1)
      endif

!     error message in case of wofost crop growht and initial data from file 
      if(.not.flbaresoil) then
        if(swinco.eq.3 .and. croptype(icrop).ge.2) then
          write(messag,'(2a)')
     &     'Warning: Wofost crop growth in combination with ',
     &     'initial input from file is not implemented yet !'
          call warn ('CropGrowth',messag,logf,swscre)
        endif
      endif

      return

2000  continue

! === calculation of potential crop rate and state variables =================

! --- number of crop
      if (flcropend) then
        numcrop = icrop-1
      else
        numcrop = icrop
      endif

! --- detailed crop growth -------------------------------------------------
      if(numcrop.gt.0) then
        if (croptype(numcrop) .eq. 2) call Wofost(2)
        if (croptype(numcrop) .eq. 3) call Grass(2)
      endif

      return

3000  continue

! === calculation of actual crop rate and state variables =================

! --- number of crop
      if (flcropend) then
        numcrop = icrop-1
      else
        numcrop = icrop
      endif

      if(numcrop.gt.0) then
! ---   fixed crop development -----------------------------------------------
        if (croptype(numcrop) .eq. 1) call CropFixed(3)
! ---   detailed crop growth -------------------------------------------------
        if (croptype(numcrop) .eq. 2) call Wofost(3)
! ---   detailed grass growth  -----------------------------------------------
        if (croptype(numcrop) .eq. 3) call Grass(3)
      endif

      return
      end 

! ----------------------------------------------------------------------
      subroutine astro 
     &   (logf,swscre,daynr,lat,dayl,daylp,sinld,cosld,dsinb,dsinbe,dso)
! ----------------------------------------------------------------------
! Subroutine astro (daynr,lat,dayl,daylp,sinld,cosld)
! Authors: this routine Astro is based on Sastro (Daniel van Kraalingen)
! Date   : 28-November-2005 
! Purpose: This subroutine calculates solar constant, daily
!          extraterrestrial radiation, daylength and some intermediate
!          variables required by other routines. The routine has been
!          rewritten such that latitudes from pole to pole can be used.
!
! Formal parameters:  (I=input,O=output,C=control,IN=init,T=time)
! name   type meaning                                     units  class
! ----   ---- -------                                     -----  -----
! logf    I4  Internal number of logbook output file *.LOG   -      I
! swscre  I4  Switch of screen display:  0 = no display;     -      I
!             1 = summary water balance; 2 = daynumber
! daynr   I4  Day of year (Jan 1st = 1)                      d      I  
! lat     R8  Latitude of the site                       degrees    I  
! dayl    R8  Astronomical daylength (base = 0 degrees)      h      O  
! daylp   R8  Photoperiodic daylength (base = -4 degrees)    h      O  
! sinld   R8  Intermediate variable for other subroutine     -      O  
! cosld   R8  Intermediate variable for other subroutine     -      O  
! dsinb   R8  Daily total of sine of solar height            s      O  
! dsinbe  R8  Daily integral of sine of solar height         s      O  
!             corrected for lower transmission at low                  
!             elevation                                                
! sc      R8  Solar constant at day=daynr                   W/m2     O  
! dso     R8  Daily extraterrestrial radiation            J/m2/d    O  
!                                                                      
! Fatal error checks (on input): lat > 90, lat < -90
! Warnings          : lat above polar circle, lat within polar circle  
! Subprograms called: Warning
! File usage        : none
!----------------------------------------------------------------------
      implicit none
 
!     formal parameters
      integer logf,swscre,daynr
      real*8  lat,dayl,daylp,sinld,cosld,dsinb,dsinbe,dso

!     local parameters
      real*8  angle,aob,dec,pi,rad,zza,zzcos,zzsin,sc,help1
      character messag*200

      data    pi /3.1415926d0/,angle /-4.0d0/
! ----------------------------------------------------------------------
! --- declination of the sun as a function of daynr
!     (see ref.manual: Radiation term: 23.45*rad=0.409 en (90-10)*rad=1.39)
      rad = pi/180.d0
      dec = -asin(sin(23.45d0*rad)*cos(2.d0*pi*dble(daynr+10)/365.0d0))
! --- some intermediate variables
      sinld = sin(rad*lat)*sin(dec)
      cosld = cos(rad*lat)*cos(dec)
      aob = sinld/cosld
! --- calculation of daylenght and photoperiodic daylength
!     solution for polar circle altutude adopted from 
!     Daniel van Kraalingen (routine Sastro, dd 12-june-1996,version 1.1)
      if (aob.lt.-1.0d0) then
        messag = 'Warning: latitude above polar circle, daylength= 0hrs'
        call warn ('Astro',messag,logf,swscre)
        dayl = 0.0d0
        zzcos =  0.0d0
        zzsin =  1.0d0
      else if (aob.gt.1.0d0) then
        messag = 'Warning: latitude within polar circle,daylength=24hrs'
        call warn ('Astro',messag,logf,swscre)
        dayl = 24.0d0
        zzcos =  0.0d0
        zzsin = -1.0d0
      else
        dayl  = 12.0d0*(1.0d0+2.0d0*asin(aob)/pi)
        help1 = (-sin(angle*rad)+sinld)/cosld
        if (help1.gt.1.0d0) then
          daylp = 24.0d0
        else
          daylp = 12.0d0*(1.0d0+2.0d0*asin(help1)/pi)
        endif
!        write(logf,*) 'help1=',help1,'daylp=',daylp
        zza   = pi*(12.0d0+dayl)/24.0d0
        zzcos = cos (zza)
        zzsin = sin (zza)
      endif

!     Daily integral of sine of solar height (DSINB) with a
!     correction for lower atmospheric transmission at lower solar
!     elevations (DSINBE)
      dsinb  = 2.0d0*3600.0d0*(dayl*0.50d0*sinld-12.0d0*cosld*zzcos/pi)
      dsinbe = 2.0d0*3600.0d0*(dayl*(0.50d0*sinld+0.20d0*sinld**2.0d0+
     &      0.10d0*cosld**2.0d0)-(12.0d0*cosld*zzcos+
     &      9.6d0*sinld*cosld*zzcos+2.4d0*cosld**2.0d0*zzcos*zzsin)/pi)

!     Solar constant and daily extraterrestrial radiation
      sc = 1370.0d0*(1.0d0+0.033d0*cos (2.0d0*pi*daynr/365.d0))
      dso  = sc*dsinb

      return
      end
! ----------------------------------------------------------------------
      subroutine cropfixed (task)
! ----------------------------------------------------------------------
!     date               : august 2004                           
!     purpose            : simple crop growth routine for swap 
! ----------------------------------------------------------------------
      use variables
      implicit none

! --- local variables
      integer stepnr,i,icgs,task,lcc,node
      
      real*8  gctb(2*magrs),rdtb(2*magrs),kytb(2*magrs),nihil,
     &        afgen,cptr0,ctr0,cptr(magrs),ctr(magrs),crt(magrs),
     &        rely(magrs),help(magrs),dtsum,dvr,phead                   ! NwRootExtr

      parameter (nihil=1.0d-7)

      save
! ----------------------------------------------------------------------

      goto (1000,2000,3000) task

1000  continue

! === initialization ===================================================

! --- read crop data
      gctb = 0.0d0
      cftb = 0.0d0
      chtb = 0.0d0
      rdtb = 0.0d0
      call readcropfixed (cropfil(icrop),pathcrop,idev,lcc,tsumea,
     & tsumam,tbase,kdif,kdir,gctb,swgc,cftb,swcf,rdtb,rdctb,hlim1,
     & hlim2u,hlim2l,hlim3h,hlim3l,hlim4,rsc,adcrh,adcrl,kytb,
     & cofab,logf,schedule,swinter,pfreetb,pstemtb,scanopytb,
     & avprectb,avevaptb,cumdens,chtb,albedo,swetr,
     & flsolute,ecmax,ecslop,c2eca,c2ecb,c2ecf,numlay,
     & alphacrit,swroottyp,wiltpoint,rootradius,rootcoefa,rsw)           ! NwRootExtr

! --- development stage
      dvs = 0.0d0

! --- initial lai or sc
      lai = afgen (gctb,(2*magrs),dvs)
      if (swgc.eq.2) then
        gc = lai
        lai = lai*3.0d0
      endif

! --- initial crop factor or crop height
      cf = afgen (cftb,(2*magrs),dvs)
      ch = afgen (chtb,(2*magrs),dvs)

! --- actual rooting depth [cm]
      rd = min (rds,afgen (rdtb,(2*magrs),dvs))

! --- initial summation variables of the crop
      cptr0 = 0.
      ctr0 = 0.

! --- init arrays with cum. pot. and act. transpiration 
      cptr = 0.0d0
      ctr = 0.0d0

! --- initialize matric flux potential                                  ! NwRootExtr
      phead = 0.d0   ! dummy                                            !
      if (swroottyp .eq. 2) then                                        !
        node = 1                                                        ! dummy node nr
        call MatricFlux(1,phead,node)                                   !
! ---   output of matric flux potential                                 !
!        if (swmfp.eq.1) call outmatricflux(2,mfp,numnod,tcum,          !
!     &   mflux,z,outfil,pathwork,project,ptra,h)                       !
      endif                                                             ! NwRootExtr

      return          

2000  continue

! === calculate potential rate and state variables ======================

3000  continue

! === calculate actual rate and state variables ======================

! --- increase in temperature sum
      dtsum = max (0.0d0,tav-tbase)

! --- development rate
      if (idev.eq.1) then
        dvr = 2.0/lcc
      elseif (idev.eq.2) then
        if (dvs.lt.1.0d0) then
          dvr = dtsum/tsumea
        else
          dvr = dtsum/tsumam
        endif
      endif

! --- determination of current growing stage
      do i = 1,magrs
        help(i) = kytb(2*i-1)
      end do
      icgs = stepnr(help,magrs,dvs)

! --- water stress
      if(abs(ptra).lt.nihil) then
        reltr = 1.0d0
      else
        reltr = max(min(tra/ptra,1.0d0),0.0d0)
      endif

! ----integrals of the crop --------------------------------------------

! --- phenological development stage
      dvs = dvs+dvr

! --- leaf area index or soil cover fraction    
      lai = afgen (gctb,(2*magrs),dvs)
      if (swgc.eq.2) then
        gc = lai
        lai = lai*3.0d0
      endif

! --- crop factor or crop height
      cf = afgen (cftb,(2*magrs),dvs)
      ch = afgen (chtb,(2*magrs),dvs)

! --- rooting depth [cm]
      rd = min (rds,afgen (rdtb,(2*magrs),dvs))

! --- cumulative relative transpiration, total growing season 
      cptr0 = cptr0 + ptra  
      ctr0 = ctr0  + tra
      if (cptr0.le.nihil) then
        crt0 = 0.0d0
      else
        crt0 = max(min(ctr0/cptr0,1.0d0),0.0d0)
      endif

! --- cumulative relative transpiration, current growing stage
      cptr(icgs) = cptr(icgs) + ptra
      ctr(icgs) = ctr(icgs)  + tra
      if (cptr(icgs).le.nihil) then
        crt(icgs) = 0.0d0
      else
        crt(icgs) = max(min(ctr(icgs)/cptr(icgs),1.0d0),0.0d0)
      endif

! --- relative yield per growing stage and cumulated
      crely = 1.0d0 
      do i = 1,icgs
        rely(i) = 1.0d0-((1.0d0-crt(i))*kytb(2*i))
        crely = crely*rely(i)
      end do

      return
      end

! ----------------------------------------------------------------------
      subroutine cropoutput(task) 
! ----------------------------------------------------------------------
!     Date               : Aug 2004   
!     Purpose            : open and write crop output files 
! ----------------------------------------------------------------------

      use variables
      implicit none

! --- local variables ------------------
      integer task,getun,numcrop
      character messag*200
      character*160 filnam,filtext

      goto (1000, 2000, 3000) task

1000  continue

! === open crop output file and write headers =====================

! --- open crop output file
      if (flopencropoutput) then
! ---   open crop output file and write general header
        if (trim(outfil).eq.trim(cropfil(1))) then
          Messag = 'The name of the input crop-file 
     &    ('//trim(cropfil(icrop))//') cannot be equal to the name of '
     &   //'the output crop-file '//trim(outfil)//' Adjust a filename !'
          call fatalerr ('crops',messag)
        endif
        filnam = trim(pathwork)//trim(outfil)//'.crp'
        crp = getun (20,90)
        call fopens(crp,filnam,'new','del')
        filtext = 'output data of simple or detailed crop growth model'
        call writehead (crp,1,filnam,filtext,project)

! ---   write header fixed crop growth
        if (croptype(icrop) .eq. 1) 
     &    call OutCropFixed(1,date,t,daycrop,dvs,lai,cf,rd,
     &          crt0,crely,crp,ch)
! ---   write header detailed crop growth 
        if (croptype(icrop) .eq. 2) 
     &    call OutWofost(1,date,daycrop,crp,t,dvs,lai,cf,rd,ch,
     &                        crt0,crely,crt1,cwdmpot,cwdm,wsopot,wso)
! ---   write header detailed grass growth
        if (croptype(icrop) .eq. 3) 
     &    call OutGrass(1,date,daycrop,crp,t,lai,rd,dvs,cf,ch,
     &                    crt0,crt1,tagppot,tagp,tagptpot,tagpt)

          flopencropoutput = .false.

      else
! ---   header for second and subsequent crops

! ---   write header fixed crop growth
        if (croptype(icrop).eq.1 .and. swheader.eq.1) 
     &    call OutCropFixed(1,date,t,daycrop,dvs,lai,cf,rd,
     &           crt0,crely,crp,ch)

! ---   write header detailed crop growth 
        if (croptype(icrop).eq.2 .and. swheader.eq.1) 
     &    call OutWofost(1,date,daycrop,crp,t,dvs,lai,cf,rd,ch,
     &                        crt0,crely,crt1,cwdmpot,cwdm,wsopot,wso)
! ---   write header detailed grass growth
        if (croptype(icrop).eq.3 .and. swheader.eq.1) 
     &    call OutGrass(1,date,daycrop,crp,t,lai,rd,dvs,cf,ch,
     &                    crt0,crt1,tagppot,tagp,tagptpot,tagpt)

      endif

      return

2000  continue

! --- write actual data ----------------------------------------------------

! --- number of crop
      if (flcropend) then
        numcrop = icrop-1
      else
        numcrop = icrop
      endif

! --- fixed crop file
      if (croptype(numcrop) .eq. 1) 
     &  call OutCropFixed(2,date,t,daycrop,dvs,lai,cf,rd,
     &           crt0,crely,crp,ch)

! --- detailed crop growth 
      if (croptype(numcrop) .eq. 2) 
     &  call OutWofost(2,date,daycrop,crp,t,dvs,lai,cf,rd,ch,
     &                        crt0,crely,crt1,cwdmpot,cwdm,wsopot,wso)

! --- detailed grass growth
      if (croptype(numcrop) .eq. 3) 
     &  call OutGrass(2,date,daycrop,crp,t,lai,rd,dvs,cf,ch,
     &                    crt0,crt1,tagppot,tagp,tagptpot,tagpt)

      return

3000  continue
! --- close crop output file ------------------------------------------------

      close (crp)

      return
      end 

! ----------------------------------------------------------------------
      subroutine nocrop (rd,lai,cf,ch,albedo,rsc)
! ----------------------------------------------------------------------
      real*8  rd,lai,cf,ch,albedo,rsc
! ----------------------------------------------------------------------
      rd = 0.0d0
      lai = 0.0d0
      cf = 0.0d0
      ch = 12.d0
      albedo = 0.23d0
      rsc = 70.d0

      return
      end

! ----------------------------------------------------------------------
      subroutine readcropfixed (crpfil,pathcrop,idev,lcc,tsumea,
     & tsumam,tbase,kdif,kdir,gctb,swgc,cftb,swcf,rdtb,rdctb,hlim1,
     & hlim2u,hlim2l,hlim3h,hlim3l,hlim4,rsc,adcrh,adcrl,kytb,
     & cofab,logf,schedule,swinter,pfreetb,pstemtb,scanopytb,
     & avprectb,avevaptb,cumdens,chtb,albedo,swetr,
     & flsolute,ecmax,ecslop,c2eca,c2ecb,c2ecf,numlay,
     & alphacrit,swroottyp,wiltpoint,rootradius,rootcoefa,rsw)              ! NwRootExtr
! ----------------------------------------------------------------------
!     Update             : July 2009
!     date               : July 2002             
!     purpose            : get crop parameters from cropfile
! ----------------------------------------------------------------------
      implicit  none
      include  'arrays.fi'
      
      integer   crp,idev,i,lcc,swgc,swcf,logf,ifnd,getun2,schedule
      integer   swinter,swetr,numlay,swroottyp                              ! NwRootExtr
      logical   flsolute 
      real*8    gctb(2*magrs),cftb(2*magrs),rdtb(2*magrs),kytb(2*magrs)
      real*8    adcrh,adcrl,tbase,tsumam,tsumea,rdctb(22)
      real*8    hlim1,hlim2u,hlim2l,hlim3h,hlim3l,hlim4,rsc
      real*8    sum,afgen,kdif,kdir,cofab,chtb(2*magrs)
      real*8    tinter(magrs),pfree(magrs),pstem(magrs)
      real*8    scanopy(magrs),avprec(magrs),avevap(magrs)
      real*8    pfreetb(2*magrs),pstemtb(2*magrs),scanopytb(2*magrs)
      real*8    avprectb(2*magrs),avevaptb(2*magrs)
      real*8    depth,rootdis(202),cumdens(202)
      real*8    dvsinput(magrs),cfinput(magrs),chinput(magrs)
      real*8    ecmax,ecslop,c2eca,c2ecb,c2ecf(maho),albedo,alphacrit
      real*8    wiltpoint,rootradius,rootcoefa,rsw                          ! NwRootExtr
      logical   rdinqr
      character crpfil*(*),pathcrop*(*)
! locals
      integer   swc2ecf
      character message*200,filnam*200
! ----------------------------------------------------------------------

! --- initialise and start reading
      filnam = trim(pathcrop)//trim(crpfil)//'.crp'
      crp = getun2 (10,90,2)
      call rdinit(crp,logf,filnam)

! --- phenology
      call rdsinr ('idev',1,2,idev)
      if (idev.eq.1) then
        call rdsinr ('lcc',1,366,lcc)
      elseif (idev.eq.2) then
        call rdsdor ('tsumea',0.0d0,10000.0d0,tsumea)
        call rdsdor ('tsumam',0.0d0,10000.0d0,tsumam)
        call rdsdor ('tbase',-10.0d0, 30.0d0,tbase)
      endif

! --- assimilation                        
      call rdsdor ('kdif',0.0d0,2.0d0,kdif)
      call rdsdor ('kdir',0.0d0,2.0d0,kdir)
     
! --- LAI or soil cover fraction 
      call rdsinr ('swgc',1,2,swgc)
      if (swgc.eq.1) then
        call rdador ('gctb',0.0d0,12.0d0,gctb,(2*magrs),ifnd)
      elseif (swgc.eq.2) then
        call rdador ('gctb',0.0d0,2.0d0,gctb,(2*magrs),ifnd)
      endif

! --- Crop factor or crop height
      call rdsinr ('swcf',1,2,swcf)

! --- check use of crop factors in case of ETref
      if (swetr.eq.1 .and. swcf.eq.2) then
        message = 'If ETref is used (SWETR = 1), always define crop '//
     &           'factors (SWCF = 1)' 
        call fatalerr ('ReadCropFixed',message)
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

! --- rooting depth
      call rdador ('rdtb',0.0d0,1000.0d0,rdtb,(2*magrs),ifnd)

! --- yield response
      call rdador ('kytb',0.0d0,5.0d0,kytb,(2*magrs),ifnd)

! --- water use
      swroottyp = 1
      if(rdinqr('swroottyp')) then
        call rdsinr ('swroottyp',1,2,swroottyp)                         ! NwRootExtr
      endif
      if (swroottyp.eq.1) then                                          !
         call rdsdor ('hlim1' ,-100.0d0,100.0d0,hlim1)                  !
         call rdsdor ('hlim2u',-1000.0d0,100.0d0,hlim2u)                !
         call rdsdor ('hlim2l',-1000.0d0,100.0d0,hlim2l)                !
         call rdsdor ('hlim3h',-10000.0d0,100.0d0,hlim3h)               !
         call rdsdor ('hlim3l',-10000.0d0,100.0d0,hlim3l)               !
         call rdsdor ('hlim4' ,-16000.0d0,100.0d0,hlim4)                !
         call rdsdor ('adcrh',0.0d0,5.0d0,adcrh)                        !
         call rdsdor ('adcrl',0.0d0,5.0d0,adcrl)                        !
!       Criticial stress index for compensation of root water uptake (-)
        alphacrit = 1.0d0
        if(rdinqr('alphacrit')) then
          call rdsdor ('alphacrit',0.2d0,1.0d0,alphacrit)
        endif
      else                                                              !
        call rdsdor ('wiltpoint',-1.0d6,-1.0d2,wiltpoint)               !
        call rdsdor ('rootradius',0.0001d0,1.0d0,rootradius)            !
        call rdsdor ('rootcoefa',0.0d0,1.0d0,rootcoefa)                 !
      endif                                                             ! NwRootExtr


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
      call rdsinr ('swinter',0,2,swinter)
      if (swinter .eq. 1) then
        call rdsdor ('cofab',0.0d0,1.0d0,cofab)
      else if (swinter .eq. 2) then
        call rdador ('t',0.d0,366.d0,tinter,(magrs),ifnd)
        call rdfdor ('pfree',0.d0,1.d0,pfree,(magrs),ifnd)
        call rdfdor ('pstem',0.d0,1.d0,pstem,(magrs),ifnd)
        call rdfdor ('scanopy',0.d0,10.d0,scanopy,(magrs),ifnd)
        call rdfdor ('avprec',0.d0,100.d0,avprec,(magrs),ifnd)
        call rdfdor ('avevap',0.d0,10.d0,avevap,(magrs),ifnd)
        do i = 1, ifnd
          pfreetb(i*2) = pfree(i)
          pfreetb(i*2-1) = tinter(i)
          pstemtb(i*2) = pstem(i)
          pstemtb(i*2-1) = tinter(i)
          scanopytb(i*2) = scanopy(i)
          scanopytb(i*2-1) = tinter(i)
          avprectb(i*2) = avprec(i)
          avprectb(i*2-1) = tinter(i)
          avevaptb(i*2) = avevap(i)
          avevaptb(i*2-1) = tinter(i)
        end do
      endif

! --- read table with root distribution coefficients
      call rdador ('rdctb',0.0d0,100.0d0,rdctb,22,ifnd)


! --- determine whether irrigation scheduling is applied
      call rdsinr ('schedule',0,1,schedule)

! --- close file with crop data
      close (crp)


! --- CALCULATE NORMALIZED CUMULATIVE ROOT DENSITY FUNCTION
      if (swroottyp .eq. 1) then                                        !
! ---   root water extraction according to Feddes function              ! NwRootExtr


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
      subroutine wofost(task)
! ----------------------------------------------------------------------
!     date               : october 2004
!     purpose            : detailed crop growth routine
! ----------------------------------------------------------------------
      use variables
      implicit none
 
      integer   i1,ilvold,task,ilvoldpot,node

      real*8    lv(366),lvage(366),sla(366)
      real*8    lvpot(366),lvagepot(366),slapot(366)
      real*8    wlv,wrt,wst
      real*8    admi,afgen,amax,asrc,ccheck,cosld,cvf
      real*8    laicr,laiexp,laimax,lasum,mres,mrest,rdm
      real*8    dalv,dayl,delt,dmi,drlv,cptr0,ctr0,cptr1,ctr1
      real*8    drrt,drst,dslv,dslv1,dslv2,dslvt,dteff,dtga,dtsum,dvr
      real*8    dvred,dwlv,dwrt,dwst,fcheck,fl,fo,fr,fs
      real*8    fysdel,gass,gasst,gla,glaiex,glasol,grlv,grrt,grst
      real*8    gwrt,gwso,gwst,pgass,rest,rmres,rr
      real*8    sinld,slat,tadw,teff,twlv,twst
      real*8    wlvpot,lasumpot,wrtpot,wstpot
      real*8    dwrtpot,dwlvpot,dwstpot,dtgapot,tadwpot
      real*8    pgasspot,gasspot,rmrespot,mrespot,asrcpot,dmipot,rrpot
      real*8    admipot,grrtpot,drrtpot,gwrtpot,grlvpot
      real*8    dslvpot,restpot,dalvpot,drlvpot,gwsopot
      real*8    glasolpot,slatpot,glapot,grstpot,drstpot,gwstpot
      real*8    dslvtpot,twlvpot,twstpot
      real*8    dsinb,dsinbe,dso
      real*8    nihil,phead,dvspast1
      character tmp*11,messag*200
d     real*8    co2rootfix,co2rootloss,co2shootfix
d     character komma*1

      parameter (nihil=1.0d-7)
      parameter (delt=1.0d0)

      save
! ----------------------------------------------------------------------
      goto (1000,2000,3000) task

1000  continue

! === initialization ====================================================

! --- read crop data
      call readwofost (cropfil(icrop),pathcrop,swcf,cftb,idsl,dlo,dlc,
     &  tsumea,tsumam,dtsmtb,dvsend,tdwi,laiem,rgrlai,slatb,spa,
     &  ssa,span,tbase,kdif,kdir,eff,amaxtb,tmpftb,tmnftb,cvl,cvo,cvr,
     &  cvs,q10,rml,rmo,rmr,rms,rfsetb,frtb,fltb,fstb,fotb,perdl,rdrrtb,
     &  rdrstb,hlim1,hlim2u,hlim2l,hlim3h,hlim3l,hlim4,rsc,adcrh,adcrl,
     &  cofab,rdi,rri,rdc,rdctb,logf,schedule,cumdens,chtb,albedo,swetr,
     &  flsolute,ecmax,ecslop,c2eca,c2ecb,c2ecf,numlay,relni,cfet,
     &  alphacrit,swroottyp,wiltpoint,rootradius,rootcoefa,rsw)          ! NwRootExtr


d     open(unit=87,file='crop_CO2.csv',status='unknown')
d     write(87,*) 'date,co2rootfix,co2rootloss,co2shootfix'
d     open(unit=88,file='crop_dm.csv',status='unknown')
d     write(88,*) 'date,wrtpot,wrt,wstpot,wst,rdpot,rd,laipot,lai,
d    &gwrt,gwst,drrt,drlv,drst'

! --- maximum rooting depth & actual rooting depth
      rdm = min(rds,rdc)
      rd = min(rdi,rdm)
      rdpot = min(rdi,rdm)

! --- initial values of crop parameters
      swinter = 1
      dvs = 0.
      fr = afgen (frtb,30,dvs)
      fl = afgen (fltb,30,dvs)
      fs = afgen (fstb,30,dvs)
      fo = afgen (fotb,30,dvs)
      sla(1) = afgen (slatb,30,dvs)
      lvage(1) = 0.0d0
      ilvold = 1
      slapot(1) = afgen (slatb,30,dvs)
      lvagepot(1) = 0.0d0
      ilvoldpot = 1

! --- initial state variables of the crop
      wrt = fr*tdwi
      wrtpot = wrt
      tadw = (1.0d0-fr)*tdwi
      tadwpot = tadw
      wst = fs*tadw
      wstpot = wst
      wso = fo*tadw
      wsopot = wso
      wlv = fl*tadw
      wlvpot = wlv
!      laiem = wlv*sla(1)  is input !
      lv(1) = wlv
      lvpot(1) = wlv
      lasum = laiem     
      lasumpot = laiem     
      laiexp = laiem     
      glaiex = 0.0d0
      laimax = laiem
      lai = lasum+ssa*wst+spa*wso 
      laipot = lai 
      dwrt = 0.0d0
      dwrtpot = 0.0d0
      dwlv = 0.0d0
      dwlvpot = 0.0d0
      dwst = 0.0d0
      dwstpot = 0.0d0
      cf = afgen (cftb,(2*magrs),dvs)
      ch = afgen (chtb,(2*magrs),dvs)

! --- initial summation variables of the crop
      gasst = 0.0d0
      gasstpot = 0.0d0
      mrest = 0.0d0 
      mrestpot = 0.0d0 
      cptr0 = 0.0d0
      ctr0 = 0.0d0
      cptr1 = 0.0d0
      ctr1 = 0.0d0
      cwdm = 0.0d0
      cwdmpot = 0.0d0

! --- initialize matric flux potential                                  ! NwRootExtr
      phead = 0.d0   ! dummy                                            !
      if (swroottyp .eq. 2) then                                        !
        node = 1                                                        ! dummy node nr
        call MatricFlux(1,phead,node)                                   !
! ---   output of matric flux potential                                 !
!        if (swmfp.eq.1) call outmatricflux(2,mfp,numnod,tcum,          !
!     &   mflux,z,outfil,pathwork,project,ptra,h)                       !
      endif                                                             ! NwRootExtr

      return

2000  continue

! === calculate potential rate and state variables =====================

! --- rates of change of the crop variables ----------------------------

! --- phenological development rate  
      call astro(logf,swscre,daymeteo,lat,
     &           dayl,daylp,sinld,cosld,dsinb,dsinbe,dso)

! --- increase in temperature sum
      dtsum = afgen (dtsmtb,30,tav)

      if (dvs.lt.1.0d0) then     
! --- development during vegetative phase
        dvred = 1.0d0
        if (idsl.ge.1) 
     &          dvred = max(0.0d0,min(1.0d0,(daylp-dlc)/(dlo-dlc)))
        dvr = dvred*dtsum/tsumea
      else
! --- development during generative phase
        dvr = dtsum/tsumam
      endif    

! --- Correction for transition to values above 1.0 
!     (as suggested by PVWalsum, 20090821)
      if (dvs .lt. 1.0d0 .and. (dvs+dvr*delt) .gt. 1.0d0) then
        dvspast1 = (dvs+dvr*delt) - 1.0d0
        dvspast1 = dvspast1*(tsumea/dvred)/tsumam
        dvr      = ((1.0d0 + dvspast1) - dvs)/delt
      endif


! == = daily dry matter production 

! --- gross assimilation

      amax = afgen (amaxtb,30,dvs)
! --- correction for sub-optimum average daytemperature
      amax = amax * afgen (tmpftb,30,tavd)
      call totass (daynr,dayl,amax,eff,laipot,kdif,rad,sinld,cosld,
     &             dtgapot)
! --- correction for low minimum temperature
      dtgapot = dtgapot * afgen (tmnftb,30,tmnr)
! --- potential assimilation in kg ch2o per ha
      pgasspot = dtgapot * 30.0d0/44.0

! --- water stress reduction of pgass to gass (not for potential conditions)
      reltr = 1.0d0
      gasspot = pgasspot * reltr

! --- respiration and partitioning of carbohydrates between growth and
! --- maintenance respiration
      rmrespot = (rmr*wrtpot+rml*wlvpot+rms*wstpot+rmo*wsopot)*
     &            afgen(rfsetb,30,dvs)
      teff = q10**((tav-25.0d0)/10.0d0)
      mrespot = dmin1(gasspot,rmrespot*teff)
      asrcpot = gasspot - mrespot

! --- partitioning factors
      fr = afgen(frtb,30,dvs)
      fl = afgen(fltb,30,dvs)
      fs = afgen(fstb,30,dvs)
      fo = afgen(fotb,30,dvs)
! --- check on partitioning
      fcheck = fr+(fl+fs+fo)*(1.0d0-fr) - 1.0d0
      if (abs(fcheck).gt.0.0001d0) then
        write(tmp,'(f6.3)') dvs
        tmp = adjustl (tmp)
        Messag ='The sum of partitioning factors for leaves, stems'//
     &    ' and storage organs is not equal to one at development stage'
     &    //trim(tmp)//'.'
        call fatalerr ('cropd',messag)
      endif

! --- dry matter increase
      cvf = 1.0d0/((fl/cvl+fs/cvs+fo/cvo)*(1.0d0-fr)+fr/cvr)
      dmipot = cvf*asrcpot
! --- check on carbon balance
      ccheck = (gasspot-mrespot-(fr+(fl+fs+fo)*(1.0d0-fr))*dmipot/cvf)
     &         /max(0.0001d0,gasspot)      
      if (abs(ccheck).gt.0.0001d0) then
        Messag ='The carbon balance is not correct'
        call fatalerr ('cropd',messag)
      endif

! == = growth rate by plant organ

! --- root extension
      rrpot = min (rdm-rdpot,rri)
      if (fr.le.0.0d0.or.pgasspot.lt.1.0d0) rrpot = 0.0d0

! --- growth rate roots and aerial parts
      admipot = (1.0d0-fr)*dmipot
      grrtpot = fr*dmipot
      drrtpot = wrtpot*afgen (rdrrtb,30,dvs)
      gwrtpot = grrtpot - drrtpot

! --- weight of new leaves
      grlvpot = fl*admipot

! --- death of leaves due to water stress or high lai
      laicr = 3.2d0/kdif
      dslvpot = wlvpot*max(0.0d0,min(0.03d0,0.03d0*
     &           (laipot-laicr)/laicr))

! --- death of leaves due to exceeding life span:

! --- first: leaf death due to water stress or high lai is imposed 
! ---        on array until no more leaves have to die or all leaves
! ---        are gone

      restpot = dslvpot*delt
      i1 = ilvoldpot

      do while (restpot.gt.lvpot(i1).and.i1.ge.1)
        restpot = restpot - lvpot(i1) 
        i1 = i1-1
      enddo

! --- then: check if some of the remaining leaves are older than span,
! ---       sum their weights

      dalvpot = 0.0d0
      if (lvagepot(i1).gt.span .and. restpot.gt.0.0d0 .and.i1.ge.1) then
        dalvpot = lvpot(i1) - restpot
        restpot = 0.0d0
        i1 = i1-1
      endif

      do while (i1.ge.1.and.lvagepot(i1).gt.span)
        dalvpot = dalvpot+lvpot(i1)
        i1 = i1-1
      enddo

      dalvpot = dalvpot/delt

! --- finally: calculate total death rate leaves
      drlvpot = dslvpot + dalvpot

! --- physiologic ageing of leaves per time step
      fysdel = max (0.0d0,(tav-tbase)/(35.0d0-tbase))

! --- specific leaf area valid for current timestep
      slatpot = afgen (slatb,30,dvs)

! --- calculation of specific leaf area in case of exponential growth:
! --- leaf area not to exceed exponential growth curve
      if (laiexp.lt.6.0d0) then
        dteff = max (0.0d0,tav-tbase)
! ---   increase in leaf area during exponential growth
        glaiex = laiexp*rgrlai*dteff
! ---   source-limited increase in leaf area
        glasolpot = grlvpot*slatpot
! ---   actual increase is determined by lowest value
        glapot = min (glaiex,glasolpot)
! ---   slat will be modified in case gla equals glaiex
        if (grlvpot.gt.0.0d0) slatpot = glapot/grlvpot
      endif  

! --- growth rate stems
      grstpot = fs*admipot
! --- death rate stems
      drstpot = afgen (rdrstb,30,dvs)*wstpot
! --- net growth rate stems
      gwstpot = grstpot - drstpot

! --- growth rate storage organs
      gwsopot = fo*admipot

! ----integrals of the crop --------------------------------------------

! --- leaf death (due to water stress or high lai) is imposed on array 
! --- untill no more leaves have to die or all leaves are gone

      dslvtpot = dslvpot*delt
      i1 = ilvoldpot
      do while (dslvtpot.gt.0.and.i1.ge.1)
        if (dslvtpot.ge.lvpot(i1)) then
          dslvtpot = dslvtpot-lvpot(i1)
          lvpot(i1) = 0.0d0
          i1 = i1-1
        else
          lvpot(i1) = lvpot(i1)-dslvtpot
          dslvtpot = 0.0d0
        endif
      enddo

! --- leaves older than span die
      do while (lvagepot(i1).ge.span.and.i1.ge.1)
        lvpot(i1) = 0.0d0
        i1 = i1-1
      enddo

! --- oldest class with leaves
      ilvoldpot = i1

! --- shifting of contents, updating of physiological age
      do i1 = ilvoldpot,1,-1
        lvpot(i1+1) = lvpot(i1)
        slapot(i1+1) = slapot(i1)
        lvagepot(i1+1) = lvagepot(i1)+fysdel*delt
      enddo
      ilvoldpot = ilvoldpot + 1

! --- new leaves in class 1
      lvpot(1) = grlvpot*delt
      slapot(1) = slatpot
      lvagepot(1) = 0.0d0 

! --- calculation of new leaf area and weight
      lasumpot = 0.0d0
      wlvpot = 0.0d0
      do i1 = 1,ilvoldpot
        lasumpot = lasumpot + lvpot(i1)*slapot(i1)
        wlvpot = wlvpot + lvpot(i1)
      enddo

! --- leaf area index in case of exponential growth
      laiexp = laiexp+glaiex*delt

! --- dry weight of living plant organs
      wrtpot = wrtpot + gwrtpot*delt
      wstpot = wstpot + gwstpot*delt
      wsopot = wsopot + gwsopot*delt

! --- total above ground biomass
      tadwpot = wlvpot + wstpot + wsopot
      tadwpot = tadwpot ! for Forcheck

! --- dry weight of dead plant organs (roots,leaves & stems)
      dwrtpot = dwrtpot + drrtpot*delt
      dwlvpot = dwlvpot + drlvpot*delt
      dwstpot = dwstpot + drstpot*delt

! --- dry weight of dead and living plant organs
      twlvpot = wlvpot + dwlvpot
      twstpot = wstpot + dwstpot
      cwdmpot = twlvpot + twstpot + wsopot

! --- total gross assimilation and maintenance respiration
      gasstpot = gasspot + gasstpot
      mrestpot = mrespot + mrestpot

! --- leaf area index
      laipot = lasumpot + ssa*wstpot + spa*wsopot

! --- rooting depth
      rdpot = rdpot + rrpot

      return

3000  continue

! === calculate actual rate and state variables =====================

! --- rates of change of the crop variables ----------------------------

!     correction of potential transpiration in relation to reference crop
!     (default = 1.0, range = 0.8 - 1.2)
      ptra = cfet*ptra

! --- gross assimilation

      call totass (daynr,dayl,amax,eff,lai,kdif,rad,sinld,cosld,dtga)
! --- correction for low minimum temperature
      dtga = dtga * afgen (tmnftb,30,tmnr)
! --- potential assimilation in kg ch2o per ha
      pgass = dtga * 30.0d0/44.0

! --- water stress reduction of pgass to gass
      if(abs(ptra).lt.nihil) then
        reltr = 1.0d0
      else
        reltr = max(0.0d0,min(1.0d0,tra/ptra))
      endif
      gass = pgass * reltr
! --- management factor 
!     (nitrogen and other forms of stress, not accounted for)
      gass = gass * relni

! --- respiration and partitioning of carbohydrates between growth and
! --- maintenance respiration
      rmres = (rmr*wrt+rml*wlv+rms*wst+rmo*wso)*afgen(rfsetb,30,dvs)
      mres = dmin1(gass,rmres*teff)
      asrc = gass-mres

! --- dry matter increase
      dmi = cvf*asrc
! --- check on carbon balance
      ccheck = (gass-mres-(fr+(fl+fs+fo)*(1.0d0-fr))*dmi/cvf)
     &         /max(0.0001d0,gass)      
      if (abs(ccheck).gt.0.0001d0) then
        Messag ='The carbon balance is not correct'
        call fatalerr ('cropd',messag)
      endif

! --- growth rate by plant organ

! --- root extension
      rr = min (rdm-rd,rri)
      if (fr.le.0.0d0.or.pgass.lt.1.0d0) rr = 0.0d0

! --- growth rate roots and aerial parts
      admi = (1.0d0-fr)*dmi
      grrt = fr*dmi
      drrt = wrt*afgen (rdrrtb,30,dvs)
      gwrt = grrt-drrt

!       CO2 fixation and loss
d       co2rootfix = grrt*44.0d0/33.0d0
d       co2rootloss = drrt*44.0d0/33.0d0


! --- weight of new leaves
      grlv = fl*admi

! --- death of leaves due to water stress or high lai
      if(abs(ptra).lt.nihil) then
        dslv1 = 0.0d0
      else
        dslv1 = wlv*(1.0d0-tra/ptra)*perdl
      endif
      laicr = 3.2d0/kdif
      dslv2 = wlv*max(0.0d0,min(0.03d0,0.03d0*(lai-laicr)/laicr))
      dslv = max (dslv1,dslv2) 

! --- death of leaves due to exceeding life span:

! --- first: leaf death due to water stress or high lai is imposed on array
! ---        until no more leaves have to die or all leaves are gone

      rest = dslv*delt
      i1 = ilvold

      do while (rest.gt.lv(i1).and.i1.ge.1)
        rest = rest-lv(i1) 
        i1 = i1-1
      enddo

! --- then: check if some of the remaining leaves are older than span,
! ---       sum their weights

      dalv = 0.0d0
      if (lvage(i1).gt.span .and. rest.gt.0.0d0 .and.i1.ge.1) then
        dalv = lv(i1)-rest
        rest = 0.0d0
        i1 = i1-1
      endif

      do while (i1.ge.1.and.lvage(i1).gt.span)
        dalv = dalv+lv(i1)
        i1 = i1-1
      enddo

      dalv = dalv/delt

! --- finally: calculate total death rate leaves
      drlv = dslv+dalv

! --- specific leaf area valid for current timestep
      slat = afgen (slatb,30,dvs)

! --- calculation of specific leaf area in case of exponential growth:
! --- leaf area not to exceed exponential growth curve
      if (laiexp.lt.6.0d0) then
! ---   source-limited increase in leaf area
        glasol = grlv*slat
! ---   actual increase is determined by lowest value
        gla = min (glaiex,glasol)
! ---   slat will be modified in case gla equals glaiex
        if (grlv.gt.0.0d0) slat = gla/grlv
      endif  

! --- growth rate stems
      grst = fs*admi
! --- death rate stems
      drst = afgen (rdrstb,30,dvs)*wst
! --- net growth rate stems
      gwst = grst-drst

! --- growth rate storage organs
      gwso = fo*admi

! ----integrals of the crop --------------------------------------------

! --- phenological development stage
      dvs = dvs+dvr*delt

! --- leaf death (due to water stress or high lai) is imposed on array 
! --- untill no more leaves have to die or all leaves are gone

      dslvt = dslv*delt
      i1 = ilvold
      do while (dslvt.gt.0.and.i1.ge.1)
        if (dslvt.ge.lv(i1)) then
          dslvt = dslvt-lv(i1)
          lv(i1) = 0.0d0
          i1 = i1-1
        else
          lv(i1) = lv(i1)-dslvt
          dslvt = 0.0d0
        endif
      enddo

! --- leaves older than span die
      do while (lvage(i1).ge.span.and.i1.ge.1)
        lv(i1) = 0.0d0
        i1 = i1-1
      enddo

! --- oldest class with leaves
      ilvold = i1

! --- shifting of contents, updating of physiological age
      do i1 = ilvold,1,-1
        lv(i1+1) = lv(i1)
        sla(i1+1) = sla(i1)
        lvage(i1+1) = lvage(i1)+fysdel*delt
      enddo
      ilvold = ilvold+1

! --- new leaves in class 1
      lv(1) = grlv*delt
      sla(1) = slat
      lvage(1) = 0.0d0 

! --- calculation of new leaf area and weight
      lasum = 0.0d0
      wlv = 0.0d0
      do i1 = 1,ilvold
        lasum = lasum+lv(i1)*sla(i1)
        wlv = wlv+lv(i1)
      enddo

! --- dry weight of living plant organs
      wrt = wrt+gwrt*delt
      wst = wst+gwst*delt
      wso = wso+gwso*delt

! --- total above ground biomass
      tadw = wlv+wst+wso

! --- dry weight of dead plant organs (roots,leaves & stems)
      dwrt = dwrt+drrt*delt
      dwlv = dwlv+drlv*delt
      dwst = dwst+drst*delt

! --- dry weight of dead and living plant organs
!     twrt = wrt+dwrt
      twlv = wlv+dwlv
      twst = wst+dwst
      cwdm = twlv+twst+wso

! --- total gross assimilation and maintenance respiration
      gasst = gass + gasst
      mrest = mres + mrest

! --- leaf area index
      lai = lasum+ssa*wst+spa*wso
! --- determine maximum lai
      laimax = max (lai,laimax)

! --- rooting depth
      rd = rd+rr

! --- crop factor or crop height
      cf = afgen (cftb,(2*magrs),dvs)
      ch = afgen (chtb,(2*magrs),dvs)

!     CO2 fixation and root,shoot developm
d     co2shootfix = tagp * 44.0d0/33.0d0
d     komma = ","
d     write(87,'(a12,1x,3(a,f12.4))') 
d    &  date,komma,co2rootfix,komma,co2rootloss,komma,co2shootfix
d     write(88,'(a12,1x,20(a,f12.4))') 
d    &  date,komma,wrtpot,komma,wrt,komma,wstpot,komma,wst,
d    &  komma,rdpot,komma,rd,komma,laipot,komma,lai,
d    &  komma,gwrt,komma,gwst,komma,drrt,komma,drlv,komma,drst

! --- cumulative relative transpiration
      cptr0 = cptr0 + ptra  
      ctr0 = ctr0  + tra
      if (cptr0.le.nihil) then
        crt0 = 0.0d0
      else 
        crt0 = max(0.0d0,min(1.0d0,ctr0/cptr0))
      endif

      if (dvs.ge.1.0d0) then
        cptr1 = cptr1+ptra
        ctr1 = ctr1 + tra
        if (cptr1.le.nihil) then
          crt1 = 0.0d0
        else 
          crt1 = max(0.0d0,min(1.0d0,ctr1/cptr1))
        endif
      else
        crt1 = 1.0d0
      endif

! --- crop finish conditions based on dvs or lai
      if ( ((dvs.ge.dvsend) .or. (lai.le.0.002d0.and.dvs.gt.0.5d0)) 
     &     .and. (.not. flCropEnd) ) then
        flCropOutput =.true.
        flCropEnd = .true.
        icrop = icrop + 1
        flBareSoil = .true.
      endif

      return
      end

! ----------------------------------------------------------------------
      subroutine totass (daynr,dayl,amax,eff,lai,kdif,avrad,
     $sinld,cosld,dtga)
! ----------------------------------------------------------------------
! --- author: daniel van kraalingen, 1986
! --- calculates daily total gross assimilation (dtga) by performing
! --- a gaussian integration over time. at three different times of 
! --- the day, irradiance is computed and used to calculate the instan- 
! --- taneous canopy assimilation, whereafter integration takes place.
! --- more information on this routine is given by spitters et al./1988
! --- subroutines and functions called: assim, radiat
! ----------------------------------------------------------------------
      implicit none

      integer i,daynr

      real*8  lai,kdif
      real*8  amax,avrad,cosld,dayl,dtga,eff,fgros,gausr,hour,pardif
      real*8  pardir,sinb,sinld

      data    gausr /0.3872983d0/
! ----------------------------------------------------------------------
! --- three point gaussian integration over day
      dtga = 0.
      if (amax.lt.1.0d-10) return
      do 10 i=1,3
        hour = 12.0d0+dayl*0.5*(0.5d0+(i-2)*gausr)
! --- at a specified hour, diffuse and direct irradiance is computed
        call radiat (daynr,hour,dayl,sinld,cosld,avrad,sinb,pardir,
     $              pardif)
! --- irradiance and crop properties determine assimilation
        call assim (amax,eff,lai,kdif,sinb,pardir,pardif,fgros)
        if(i.eq.2) fgros=fgros*1.6
        dtga = dtga+fgros
10    continue
      dtga =dtga*dayl/3.6

      return
      end

! ----------------------------------------------------------------------
      subroutine radiat (daynr,hour,dayl,sinld,cosld,avrad,sinb,
     $                   pardir,pardif)
! ----------------------------------------------------------------------
! --- author: daniel van kraalingen, 1986
! --- calculates the fluxes of diffuse and direct photosynthetically
! --- active radiation from the total daily shortwave radiation actually
! --- received (avrad) for a given day of the year and hour of the day.
! --- the input variables dayl, sinld and cosld are calculated in astro.
! --- for more information: see spitters et al. (1988).
! ----------------------------------------------------------------------
      implicit none

      integer daynr

      real*8  aob,atmtr,avrad,cosld,dayl,dsinb,dsinbe,dso,frdif,hour
      real*8  par,pardif,pardir,pi,sc,sinb,sinld

      data    pi /3.1415926d0/
! ----------------------------------------------------------------------
! --- calculations on solar elevation
! --- sine of solar elevation sinb
      aob = sinld/cosld
      sinb = max (0.0d0,sinld+cosld*cos(2.0*pi*(hour+12.0d0)/24.0))
! --- integral of sinb
      dsinb = 3600.*(dayl*sinld+24.*cosld*sqrt(1.0d0-aob*aob)/pi)
! --- integral of sinb, corrected for lower atmospheric transmission
! --- at low solar elevations
      dsinbe = 3600.*(dayl*(sinld+0.4*(sinld*sinld+cosld*cosld*0.5))+
     $         12.0*cosld*(2.0d0+3.0*0.4*sinld)*sqrt(1.0d0-aob*aob)/pi)

! --- solar constant and daily extraterrestrial radiation
      sc = 1370.*(1.0d0+0.033*cos(2.0*pi*daynr/365.))
      dso = sc*dsinb

! --- diffuse light fraction from atmospheric transmission
      atmtr = avrad/dso
      if (atmtr.gt.0.75d0) frdif = 0.23d0
      if (atmtr.le.0.75d0.and.atmtr.gt.0.35d0) frdif = 1.33d0-1.46*atmtr
      if (atmtr.le.0.35d0.and.atmtr.gt.0.07d0) 
     $ frdif = 1.0d0-2.3*(atmtr-0.07d0)**2
      if (atmtr.le.0.07d0) frdif = 1.0d0

! --- photosynthetic active radiation, diffuse and direct
      par = 0.5*avrad*sinb*(1.0d0+0.4*sinb)/dsinbe
      pardif = min (par,sinb*frdif*atmtr*0.5*sc)
      pardir = par-pardif

      return
      end

! ----------------------------------------------------------------------
      subroutine assim (amax,eff,lai,kdif,sinb,pardir,pardif,fgros)
! ----------------------------------------------------------------------
!     author: daniel van kraalingen, 1986
!     calculates the gross co2 assimilation rate of the whole crop, 
!     fgros, by performing a gaussian integration over depth in the 
!     crop canopy. at three different depths in the canopy, i.e. for
!     different values of lai, the assimilation rate is computed for
!     given fluxes of photosynthetically active radiation, whereafter
!     integration over depth takes place. for more information: see 
!     spitters et al. (1988). the input variables sinb, pardir and 
!     pardif are calculated in radiat.
! ----------------------------------------------------------------------
      implicit none

      integer i

      real*8  lai,laic,kdif,kdirbl,kdirt
      real*8  amax,eff,fgl,fgros,fgrsh,fgrsun,fslla,gausr,pardif,pardir
      real*8  refh,refs,scv,sinb,visd,visdf,vispp,visshd,vist

      data    gausr /0.3872983d0/
! ----------------------------------------------------------------------
! --- extinction coefficients kdif,kdirbl,kdirt
      scv = 0.2d0
      refh = (1.0d0-sqrt(1.0d0-scv))/(1.0d0+sqrt(1.0d0-scv))
      refs = refh*2.0/(1.0d0+1.6*sinb)
      kdirbl = (0.5/sinb)*kdif/(0.8*sqrt(1.0d0-scv))
      kdirt = kdirbl*sqrt(1.0d0-scv)

! --- three point gaussian integration over lai
      fgros = 0.
      do 10 i = 1,3
        laic = 0.5*lai+gausr*(i-2)*lai
! --- absorbed diffuse radiation (vidf),light from direct
! --- origine (vist) and direct light(visd)
        visdf = (1.0d0-refs)*pardif*kdif  *exp(-kdif  *laic)
        vist = (1.0d0-refs)*pardir*kdirt *exp(-kdirt *laic)
        visd = (1.0d0-scv) *pardir*kdirbl*exp(-kdirbl*laic)
! --- absorbed flux in w/m2 for shaded leaves and assimilation
        visshd = visdf+vist-visd
        fgrsh = amax*(1.0d0-exp(-visshd*eff/amax))
! --- direct light absorbed by leaves perpendicular on direct
! --- beam and assimilation of sunlit leaf area
        vispp = (1.0d0-scv)*pardir/sinb
        if (vispp.le.0.0d0) fgrsun = fgrsh
        if (vispp.gt.0.0d0) fgrsun = amax*(1.0d0-
     $    (amax-fgrsh)*(1.0d0-exp(-vispp*eff/amax))/ (eff*vispp))
! --- fraction of sunlit leaf area (fslla) and local
! --- assimilation rate (fgl)
        fslla = exp(-kdirbl*laic)
        fgl = fslla*fgrsun+(1.0d0-fslla)*fgrsh
! --- integration
        if (i.eq.2) fgl = fgl*1.6
        fgros = fgros+fgl
10    continue
      fgros = fgros*lai/3.6

      return
      end


! ----------------------------------------------------------------------
      subroutine grass(task)
! ----------------------------------------------------------------------
!     Date               : November 2004
!     Purpose            : detailed grass growth routine 
! ----------------------------------------------------------------------
      use variables
      implicit none
 
      integer   i1,ilvold,task,ilvoldpot,idregr,idregrpot
      integer   idelaypot,idelay,node

      real*8    laicr,laiexp,laimax,lasum,mres,rid
!     real*8    rdm,rr,rrpot
      real*8    lv(366),lvage(366),sla(366)
      real*8    admi,afgen,amax,asrc,ccheck,cosld,cvf,rlwtb(22)
      real*8    dalv,dayl,delt,dmi,drlv,cptr0,ctr0
      real*8    drrt,drst,dslv,dslv1,dslv2,dslvt,dteff,dtga
      real*8    dwlv,dwrt,dwst,fcheck,fl,fr,fs,laiexppot
      real*8    fysdel,gass,gla,glaiex,glasol,grlv,grrt,grst
      real*8    gwrt,gwst,pgass,rest,rmres
      real*8    sinld,slat,teff,twlv,twst,wlv,wrt,wst,glaiexpot
      real*8    lvpot(366),lvagepot(366),slapot(366)
      real*8    wlvpot,lasumpot,wrtpot,wstpot,drst1,drst2
      real*8    dwrtpot,dwlvpot,dwstpot,dtgapot,drst1pot,drst2pot
      real*8    pgasspot,gasspot,rmrespot,mrespot,asrcpot,dmipot
      real*8    admipot,grrtpot,drrtpot,gwrtpot,grlvpot,dslv1pot
      real*8    dslv2pot,dslvpot,restpot,dalvpot,drlvpot
      real*8    glasolpot,slatpot,glapot,grstpot,drstpot,gwstpot
      real*8    dslvtpot,twlvpot,twstpot,tagpspot,tagps
      real*8    dsinb,dsinbe,dso
      real*8    nihil,phead
      real*8    dmharvest1,dmharvest2,dmlastharvest,dateharvest(999)
      real*8    wrtmax,rdm
      real*8    grazingfactor
      real*8    nsuptab(magrs),dmfac(magrs),relnitab(2*magrs),nsupply        ! Nwgrassland
      integer   daylastharvest,iharvest,iharvestpot,swharvest,swgrazing
      logical   flharvestpot,flharvest,flgrazing,flgrazingpot
      character tmp*11,messag*200
d     real*8    co2rootfix,co2rootloss,co2shootfix
d     character komma*1

      parameter (nihil=1.0d-7)
      parameter (delt=1.0d0)

      save


! ----------------------------------------------------------------------
      goto (1000,2000,3000) task

1000  continue

! === initialization ====================================================

! --- read grass input data
      call readgrass (cropfil(icrop),pathcrop,tdwi,laiem,rgrlai,slatb,
     &  ssa,span,tbase,kdif,kdir,eff,amaxtb,tmpftb,tmnftb,cvl,cvr,cvs,
     &  q10,rml,rmr,rms,rfsetb,frtb,fltb,fstb,perdl,rdrrtb,
     &  rdrstb,hlim1,hlim2u,hlim2l,hlim3h,hlim3l,hlim4,rsc,adcrh,adcrl,
     &  cofab,rdi,rri,rdc,rdctb,rlwtb,logf,schedule,cumdens,
     &  flsolute,ecmax,ecslop,c2eca,c2ecb,c2ecf,numlay,dateharvest,
     &  swharvest,dmharvest1,dmharvest2,swgrazing,grazingfactor,
     &  daylastharvest,dmlastharvest,wrtmax,
     &  nsuptab,dmfac,relnitab,nsupply,
     &  swcf,swetr,cftb,chtb,cfet,alphacrit,
     &  swroottyp,wiltpoint,rootradius,rootcoefa,rsw)      

! --- initial values
      swinter = 1
      iharvest = 1
      iharvestpot = 1
      flharvest = .false.
      flharvestpot = .false.
      
      flgrazing = .false.
      flgrazingpot = .false.

d     open(unit=87,file='grass_CO2.csv',status='unknown')
d     write(87,*) 'date,co2rootfix,co2rootloss,co2shootfix'
d     open(unit=88,file='grass_dm.csv',status='unknown')
d     write(88,*) 'date,wrtpot,wrt,wstpot,wst,rdpot,rd,laipot,lai,
d    &gwrt,gwst,drrt,drlv,drst'

! --- development stage (not used by Grassland, instead Daynrs are used)
      dvs = -99.99d0

! --- maximum rooting depth & actual rooting depth
      rdm = min(rds,rdc)
      rd = min(rdi,rdm)
      rdpot = min(rdi,rdm)

! --- initial values of crop parameters
      rid = 1.0d0
      fr = afgen (frtb,30,rid)
      fl = afgen (fltb,30,rid)
      fs = afgen (fstb,30,rid)
      sla(1) = afgen (slatb,30,rid)
      lvage(1) = 0.d0
      ilvold = 1
      idregr = 0
      slapot(1) = afgen (slatb,30,rid)
      lvagepot(1) = 0.d0
      ilvoldpot = 1
      idregrpot = 0

! --- initial state variables of the crop
      wrt = fr*tdwi
      wrtpot = wrt
      wst = fs*(1.0d0-fr)*tdwi
      wstpot = wst
      wlv = laiem/sla(1)
      wlvpot = wlv
      lv(1) = wlv
      lvpot(1) = lv(1)
      lasum = laiem
      lasumpot = lasum     
      glaiex = 0.0d0
      laiexp = laiem
      laiexppot = laiem
      laimax = laiem
      lai = lasum+ssa*wst
      laipot = lai
      dwrt = 0.d0
      dwrtpot = dwrt
      dwlv = 0.d0
      dwlvpot = dwlv
      dwst = 0.d0
      dwstpot = dwst
      rid = dble(daycrop)
      cf = afgen (cftb,(2*magrs),rid)
      ch = afgen (chtb,(2*magrs),rid)

! --- initial summation variables of the crop
      tagp = wlv+wst
      tagppot = tagp
      cptr0 = 0.0d0
      ctr0 = 0.0d0
      tagpt = 0.0d0
      tagptpot = tagpt

! --- initialize matric flux potential                                  ! NwRootExtr
      phead = 0.d0   ! dummy                                            !
      if (swroottyp .eq. 2) then                                        !
        node = 1                                                        ! dummy node nr
        call MatricFlux(1,phead,node)                                   !
! ---   output of matric flux potential                                 !
!        if (swmfp.eq.1) call outmatricflux(2,mfp,numnod,tcum,          !
!     &   mflux,z,outfil,pathwork,project,ptra,h)                       !
      endif                                                             ! NwRootExtr

      return

2000  continue

! === calculate potential rate and state variables ======================================

! --- rates of change of the grass variables ---------------------------------------------

      rid = dble(daycrop)
      cf = afgen (cftb,(2*magrs),rid)
      ch = afgen (chtb,(2*magrs),rid)

! --- skip in case of regrowth
!      if (daycrop.ne.0.and.daycrop.lt.idregrpot) goto 2100
      if (daycrop.eq.0 .or.daycrop.ge.idregrpot) then

! ===   daily dry matter production ===

! ---   gross assimilation
        amax = afgen (amaxtb,30,rid)
! ---   correction for sub-optimum average daytemperature
        amax = amax * afgen (tmpftb,30,tavd)
        call astro(logf,swscre,daymeteo,lat,
     &           dayl,daylp,sinld,cosld,dsinb,dsinbe,dso)
        call totass (daynr,dayl,amax,eff,laipot,kdif,rad,sinld,cosld,
     &             dtgapot)
! ---   correction for low minimum temperature
        dtgapot = dtgapot * afgen (tmnftb,30,tmnr)
! ---   potential assimilation in kg ch2o per ha
        pgasspot = dtgapot * 30.0d0/44.0d0

! --- water stress reduction of pgass to gass (not for potential conditions)
        reltr = 1.0d0
        gasspot = pgasspot * reltr

! ---   respiration and partitioning of carbohydrates between growth and
! ---   maintenance respiration
        rmrespot=(rmr*wrtpot+rml*wlvpot+rms*wstpot)*afgen(rfsetb,30,rid)
        teff = q10**((tav-25.0d0)/10.0d0)
        mrespot = min (gasspot,rmrespot*teff)
        asrcpot = gasspot-mrespot

! ---   partitioning factors
        fr = afgen(frtb,30,rid)
        fl = afgen(fltb,30,rid)
        fs = afgen(fstb,30,rid)
! ---   check on partitioning
        fcheck = fr+(fl+fs)*(1.0d0-fr) - 1.0d0
        if (abs(fcheck).gt.0.0001d0) then
          write(tmp,'(f6.3)') rid
          tmp = adjustl (tmp)
          Messag ='The sum of partitioning factors for leaves, stems'//
     &    ' and storage organs is not equal to one at time '
     &    //trim(tmp)//'.'
          call fatalerr ('cropd',messag)
        endif

! ---   dry matter increase
        cvf = 1.0d0/((fl/cvl+fs/cvs)*(1.0d0-fr)+fr/cvr)
        dmipot = cvf*asrcpot
! ---   check on carbon balance
        ccheck = (gasspot-mrespot-(fr+(fl+fs)*(1.0d0-fr))*dmipot/cvf)
     &         /max(0.0001d0,gasspot)      
        if (abs(ccheck).gt.0.0001d0) then
          Messag ='The carbon balance is not correct'
          call fatalerr ('cropd',messag)
        endif


! ===   growth rate by plant organ ===

! ---   root length (not used, because Rooting depth is (for grassland) 
!       dependent on available root biomass (weight)
!        rrpot = min (rdm-rdpot,rri)
!        if (fr.le.0.or.pgasspot.lt.1.0d0) rrpot = 0.0d0

! ---   growth rate roots and aerial parts
! ---   after reaching a live weight of wrtmax (default 2500 kg), the
! ---   growth of the roots is balanced by the death of root tissue
        grrtpot = fr*dmipot
        if (wrtpot.gt.wrtmax) then
          drrtpot = grrtpot
        else
          drrtpot = wrtpot*afgen (rdrrtb,30,rid)
        endif
        gwrtpot = grrtpot-drrtpot

! ---   growth rate leaves

! ---   weight of new leaves
        admipot = (1.0d0-fr)*dmipot
        grlvpot = fl*admipot

! ---   death of leaves due to water stress or high lai
        dslv1pot = 0.0d0
        laicr = 3.2d0/kdif
        dslv2pot=wlvpot*max(0.0d0,
     &                  min(0.03d0,0.03d0*(laipot-laicr)/laicr))
        dslvpot = max (dslv1pot,dslv2pot) 

! ---   death of leaves due to exceeding life span;
! ---   leaf death is imposed on array until no more leaves have
! ---   to die or all leaves are gone

        restpot = dslvpot*delt
        i1 = ilvoldpot

        do while (restpot.gt.lvpot(i1).and.i1.ge.1)
          restpot = restpot-lvpot(i1) 
          i1 = i1-1
        enddo

! ---   check if some of the remaining leaves are older than span,
! ---   sum their weights

        dalvpot = 0.0d0
        if (lvagepot(i1).gt.span.and.restpot.gt.0.and.i1.ge.1) then
          dalvpot = lvpot(i1)-restpot
          restpot = 0.0d0
          i1 = i1-1
        endif

        do while (i1.ge.1.and.lvagepot(i1).gt.span)
          dalvpot = dalvpot+lvpot(i1)
          i1 = i1-1
        enddo

        dalvpot = dalvpot/delt

! ---   death rate leaves and growth rate living leaves
        drlvpot = dslvpot+dalvpot

! ---   physiologic ageing of leaves per time step
        fysdel = max (0.0d0,(tav-tbase)/(35.0d0-tbase))

! ---   leaf area not to exceed exponential growth curve
        slatpot = afgen (slatb,30,rid)
        if (laiexppot.lt.6.0d0) then
          dteff = max (0.0d0,tav-tbase)
          glaiexpot = laiexppot*rgrlai*dteff
! ---   source-limited increase in leaf area
          glasolpot = grlvpot*slatpot
          glapot = min (glaiexpot,glasolpot)
! ---   adjustment of specific leaf area of youngest leaf class
          if (grlvpot.gt.0.0d0) slatpot = glapot/grlvpot
        endif  

! ---   growth rate stems
        grstpot = fs*admipot
! ---   death of stems due to water stress is zero in case of potential growth
        drst1pot = 0.0d0
! ---   death of stems due to ageing
        drst2pot = afgen (rdrstb,30,rid)*wstpot
        drstpot = (drst1pot+drst2pot)/delt 
        gwstpot = grstpot-drstpot

! ----  integrals of the crop --------------------------------------------
!       after cutting, growth is initialized again and the weight of the sward is stored
! ---   harvest criteria (open to personal choice of user) 
!  INPUT    dmharvest, daylastharvest, dmlastharvest
!      if (tagppot.gt.4200.0d0 .or. 
!     &             (daycrop.gt.210 .and. tagppot.gt.3700.0d0)) then
        if (swharvest.eq.1) then
          if(tagppot.gt.dmharvest1 .or. (daycrop.gt.daylastharvest .and. 
     &                                 tagppot.gt.dmlastharvest).or. 
     &                                 flgrazingpot .eqv. .true.) then
            if (swgrazing.eq.1) then        ! grazing
                flgrazingpot = .true.
                      
                grlvpot = 0.0d0                           ! growing rate leaves adaption
                gwstpot = -1.0d0*grazingfactor* wstpot    ! growing rate stems adaption 
                 i1 = ilvoldpot
                 do while (i1.ge.1)
                    lvpot(i1) = (1.0d0-grazingfactor)*lvpot(i1)
                    i1 = i1 -1
                 end do
                 if(tagppot.lt.800.0d00) then            ! assumed last grazing day
                    flgrazingpot = .false.
                    flharvestpot = .true.
                 endif 
            else if (swgrazing.eq.2) then         ! growth continues followed by mowing
                if (tagppot.gt.dmharvest2.or. 
     &                             (daycrop.gt.daylastharvest
     &                              .and.tagppot.gt.dmlastharvest)) then
                    flharvestpot = .true.
                else
                    flharvestpot = .false.
                endif
            end if
          else
            flharvestpot = .false.
          endif
        endif
        if (swharvest.eq.2) then                         ! mowing using mowing dates
          if(t1900.gt.dateharvest(iharvestpot)) then
            iharvestpot = iharvestpot + 1
            flharvestpot = .true.
          else
            flharvestpot = .false.
          endif
        endif
        if (flharvestpot) then
          flharvestpot = .false.
          lasumpot = laiem
          slapot(1) = afgen (slatb,30,rid)
          wlvpot = lasumpot/slapot(1)
          fl = afgen (fltb,30,rid)
          fs = afgen (fstb,30,rid)
          wstpot = fs/fl*wlvpot
          dwlvpot = 0.0d0
          dwstpot = 0.0d0
          lvagepot(1) = 0.0d0
          ilvoldpot = 1
          laiexppot = laiem
          lvpot(1) = wlvpot

          gwstpot = 0.0d0
          gwrtpot = 0.0d0
          drlvpot = 0.0d0
          drstpot = 0.0d0
          drrtpot = 0.0d0

          tagpspot =max(0.0d0,(tagppot-(wlvpot+dwlvpot+wstpot+dwstpot)))
          tagptpot = tagptpot + tagpspot

! ---     regrowth delay after handbook p.r.

          if (tagpspot.lt.2000.0d0) idelaypot=1
          if (tagpspot.ge.2000.0d0.and.tagpspot.lt.2500.0d0) idelaypot=2
          if (tagpspot.ge.2500.0d0.and.tagpspot.lt.3000.0d0) idelaypot=3
          if (tagpspot.ge.3000.0d0.and.tagpspot.lt.3500.0d0) idelaypot=4
          if (tagpspot.ge.3500.0d0.and.tagpspot.lt.4000.0d0) idelaypot=5
          if (tagpspot.ge.4000.0d0) idelaypot = 6

          idregrpot = daycrop + idelaypot + 3

        endif

        if (daycrop.ge.idregrpot) then

! ---     leaf death is imposed on array untill no more leaves have to die or all leaves are gone

          dslvtpot = dslvpot*delt
          i1 = ilvoldpot
          do while (dslvtpot.gt.0.and.i1.ge.1)
            if (dslvtpot.ge.lvpot(i1)) then
              dslvtpot = dslvtpot-lvpot(i1)
              lvpot(i1) = 0.0d0
              i1 = i1-1
            else
              lvpot(i1) = lvpot(i1)-dslvtpot
              dslvtpot = 0.0d0
            endif
          enddo

          do while (lvagepot(i1).ge.span.and.i1.ge.1)
            lvpot(i1) = 0.0d0
            i1 = i1-1
          enddo

          ilvoldpot = i1

! ---     shifting of contents, integration of physiological age
          do i1 = ilvoldpot,1,-1
            lvpot(i1+1) = lvpot(i1)
            slapot(i1+1) = slapot(i1)
            lvagepot(i1+1) = lvagepot(i1)+fysdel*delt
          enddo
          ilvoldpot = ilvoldpot+1

! ---     new leaves in class 1
          lvpot(1) = grlvpot*delt
          slapot(1) = slatpot
          lvagepot(1) = 0.d0 

! ---     calculation of new leaf area and weight
          lasumpot = 0.d0
          wlvpot = 0.d0
          do i1 = 1,ilvoldpot
            lasumpot = lasumpot+lvpot(i1)*slapot(i1)
            wlvpot = wlvpot+lvpot(i1)
          enddo

          laiexppot = laiexppot+glaiexpot*delt

        endif
      endif

! --- dry weight of living plant organs
      wrtpot = wrtpot+gwrtpot*delt
      wstpot = wstpot+gwstpot*delt

! --- dry weight of dead plant organs (roots,leaves & stems)
      dwrtpot = dwrtpot+drrtpot*delt
      dwlvpot = dwlvpot+drlvpot*delt
      dwstpot = dwstpot+drstpot*delt

! --- dry weight of dead and living plant organs
      twlvpot = wlvpot+dwlvpot
      twstpot = wstpot+dwstpot
      tagppot = twlvpot+twstpot

! --- leaf area index
      laipot = lasumpot+ssa*wstpot

! --- rooting depth as function of available root weight (not root rate!)
!      rdpot = rdpot+rrpot
      rdpot = afgen (rlwtb,22,wrtpot)
      rdpot = min(rdpot,rdm)

      return

3000  continue

! === calculate actual rate and state variables ======================================

! --- rates of change of the crop variables ---------------------------------------------

!     correction of potential transpiration in relation to reference crop
!     (default = 1.0, range = 0.8 - 1.2)
      ptra = cfet*ptra

! --- skip in case of regrowth
      if (daycrop.eq.0.or.daycrop.ge.idregr) then

! ===   daily dry matter production ===

! ---   gross assimilation
        amax = afgen (amaxtb,30,rid)
! ---   correction for sub-optimum average daytemperature
        amax = amax * afgen (tmpftb,30,tavd)
! ---   gross assimilation
        call astro(logf,swscre,daymeteo,lat, 
     &           dayl,daylp,sinld,cosld,dsinb,dsinbe,dso)
        call totass (daynr,dayl,amax,eff,lai,kdif,rad,sinld,cosld,dtga)
! ---   correction for low minimum temperature
        dtga = dtga * afgen (tmnftb,30,tmnr)
! ---   potential assimilation in kg ch2o per ha
        pgass = dtga * 30.0d0/44.0d0

! ---   water stress reduction of pgass to gass
        if(abs(ptra).lt.nihil) then
          reltr = 1.0d0
        else
          reltr = max(0.0d0,min(1.0d0,tra/ptra))
        endif
        gass = pgass * reltr
! ---   nitrogen stress reduction of pgass to gass
        relni = afgen(relnitab,magrs,nsupply)
        gass = gass * relni

! ---   respiration and partitioning of carbohydrates between growth and
! ---   maintenance respiration
        rmres = (rmr*wrt+rml*wlv+rms*wst)*afgen(rfsetb,30,rid)
        teff = q10**((tav-25.0d0)/10.0d0)
        mres = min (gass,rmres*teff)
        asrc = gass-mres

! ---   dry matter increase
        cvf = 1.0d0/((fl/cvl+fs/cvs)*(1.0d0-fr)+fr/cvr)
        dmi = cvf*asrc
! ---   check on carbon balance
        ccheck = (gass-mres-(fr+(fl+fs)*(1.0d0-fr))*dmi/cvf)
     &         /max(0.0001d0,gass)      
        if (abs(ccheck).gt.0.0001d0) then
          Messag ='The carbon balance is not correct'
          call fatalerr ('cropd',messag)
        endif

! ===   growth rate by plant organ ===

! ---   root length
!        rr = min (rdm-rd,rri)
!        if (fr.le.0.or.pgass.lt.1.0d0) rr = 0.0d0
!        rr = 0.0d0

! ---   growth rate roots and aerial parts
! ---   after reaching a live weight of wrtmax (default 2500 kg), the
! ---   growth of the roots is balanced by the death of root tissue
        grrt = fr*dmi
        if (wrt.gt.wrtmax) then
!original drrt = wrt - wrtmax
          drrt = grrt
!         CO2 loss
d         co2rootloss = drrt*44.0d0/33.0d0
!original  grrt = 0.0d0
        else
          drrt = wrt*afgen (rdrrtb,30,rid)
        endif
        admi = (1.0d0-fr)*dmi
!original drrt = 0.0d0
        gwrt = grrt-drrt

!       CO2 fixation and loss
d       co2rootfix = grrt*44.0d0/33.0d0
d       co2rootloss = co2rootloss + drrt*44.0d0/33.0d0

! ---   growth rate leaves

! ---   weight of new leaves
        grlv = fl*admi

! ---   death of leaves due to water stress or high lai
        if(abs(ptra).lt.nihil) then
          dslv1 = 0.0d0
        else
          dslv1 = wlv*(1.0d0-tra/ptra)*perdl
        endif
        laicr = 3.2d0/kdif
        dslv2 = wlv*max(0.0d0,min(0.03d0,0.03d0*(lai-laicr)/laicr))
        dslv = max (dslv1,dslv2) 

! ---   death of leaves due to exceeding life span;
! ---   leaf death is imposed on array until no more leaves have
! ---   to die or all leaves are gone

        rest = dslv*delt
        i1 = ilvold

        do while (rest.gt.lv(i1).and.i1.ge.1)
          rest = rest-lv(i1) 
          i1 = i1-1
        enddo

! ---   check if some of the remaining leaves are older than span,
! ---   sum their weights

        dalv = 0.0d0
        if (lvage(i1).gt.span.and.rest.gt.0.and.i1.ge.1) then
          dalv = lv(i1)-rest
          rest = 0.0d0
          i1 = i1-1
        endif

        do while (i1.ge.1.and.lvage(i1).gt.span)
          dalv = dalv+lv(i1)
          i1 = i1-1
        enddo

        dalv = dalv/delt

! ---   death rate leaves and growth rate living leaves
        drlv = dslv+dalv

! ---   physiologic ageing of leaves per time step
        slat = afgen (slatb,30,rid)

! ---   leaf area not to exceed exponential growth curve
        if (laiexp.lt.6.0d0) then
          dteff = max (0.0d0,tav-tbase)
          glaiex = laiexp*rgrlai*dteff
! ---     source-limited increase in leaf area
          glasol = grlv*slat
          gla = min (glaiex,glasol)
! ---     adjustment of specific leaf area of youngest leaf class
          if (grlv.gt.0.0d0) slat = gla/grlv
        endif  

! ---   growth rate stems
        grst = fs*admi
! ---   death of stems due to water stress
        if(abs(ptra).lt.nihil) then
          drst1 = 0.0d0
        else
          drst1 = wst*(1.0d0-tra/ptra)*perdl
        endif
! ---   death of stems due to ageing
        drst2 = afgen (rdrstb,30,rid)*wst
        drst = (drst1+drst2)/delt 
        gwst = grst-drst

! ----  integrals of the crop --------------------------------------------

!       after cutting, growth is initialized again and the weight of the sward is stored

        if (swharvest.eq.1) then
          if(tagp.gt.dmharvest1 .or. (daycrop.gt.daylastharvest .and. 
     &                              tagp.gt.dmlastharvest).or. 
     &                              flgrazing .eqv. .true.) then
            if (swgrazing.eq.1) then               ! grazing
                flgrazing = .true.
                grlv = 0.0d0                       ! growing rate leaves adaption
                gwst = -1.0d0*grazingfactor* wst   ! growing rate stems adaption
                 i1 = ilvold
                 do while (i1.ge.1)
                    lv(i1) = (1.0d0-grazingfactor)*lv(i1) ! reduction of leaf weight
                    i1 = i1 -1
                 end do
                 if(tagp.lt.800.0d00) then  ! assumed last grazing day
                    flgrazing = .false.
                    flharvest = .true.
                 endif 
            else if (swgrazing.eq.2) then   ! growth continues followed by harvest
                if (tagp.gt.dmharvest2.or. 
     &                         (daycrop.gt.daylastharvest .and. 
     &                                   tagp.gt.dmlastharvest)) then
                    flharvest = .true.
                else
                    flharvest = .false.
                endif
            end if
          else
            flharvest = .false.
          endif
        endif
        if (swharvest.eq.2) then            ! mowing using mowing dates
          if(t1900.gt.dateharvest(iharvest)) then
            iharvest = iharvest + 1
            flharvest = .true.
          else
            flharvest = .false.
          endif
        endif
        if (flharvest) then
          flharvest = .false.
          lasum = laiem
          sla(1) = afgen (slatb,30,rid)
          wlv = lasum/sla(1)
          wst = fs/fl*wlv
          dwlv = 0.0d0
          dwst = 0.0d0
          lvage(1) = 0.0d0
          ilvold = 1
          laiexp = laiem
          lv(1) = wlv

          gwst = 0.0d0
          gwrt = 0.0d0
          drlv = 0.0d0
          drst = 0.0d0
          drrt = 0.0d0

          tagps = max (0.0d0,(tagp-(wlv+dwlv+wst+dwst)))
          tagpt = tagpt + tagps

! ---     regrowth delay after handbook p.r.

          if (tagps.lt.2000.0d0) idelay = 1           
          if (tagps.ge.2000.0d0.and.tagps.lt.2500.0d0) idelay = 2
          if (tagps.ge.2500.0d0.and.tagps.lt.3000.0d0) idelay = 3
          if (tagps.ge.3000.0d0.and.tagps.lt.3500.0d0) idelay = 4
          if (tagps.ge.3500.0d0.and.tagps.lt.4000.0d0) idelay = 5
          if (tagps.ge.4000.0d0) idelay = 6

          idregr = daycrop + idelay + 3

        endif

        if (daycrop.ge.idregr) then

! ---     physiologic ageing of leaves per time step
          fysdel = max (0.0d0,(tav-tbase)/(35.0d0-tbase))

! ---     leaf death is imposed on array untill no more leaves have to die or all leaves are gone

          dslvt = dslv*delt
          i1 = ilvold
          do while (dslvt.gt.0.and.i1.ge.1)
            if (dslvt.ge.lv(i1)) then
              dslvt = dslvt-lv(i1)
              lv(i1) = 0.0d0
              i1 = i1-1
            else
              lv(i1) = lv(i1)-dslvt
              dslvt = 0.0d0
            endif
          enddo

          do while (lvage(i1).ge.span.and.i1.ge.1)
            lv(i1) = 0.0d0
            i1 = i1-1
          enddo

          ilvold = i1

! ---     shifting of contents, integration of physiological age
          do i1 = ilvold,1,-1
            lv(i1+1) = lv(i1)
            sla(i1+1) = sla(i1)
            lvage(i1+1) = lvage(i1)+fysdel*delt
          enddo
          ilvold = ilvold+1

! ---     new leaves in class 1
          lv(1) = grlv*delt
          sla(1) = slat
          lvage(1) = 0.d0 

! ---     calculation of new leaf area and weight
          lasum = 0.d0
          wlv = 0.d0
          do i1 = 1,ilvold
            lasum = lasum+lv(i1)*sla(i1)
            wlv = wlv+lv(i1)
          enddo

          laiexp = laiexp+glaiex*delt

        endif
      endif

! --- dry weight of living plant organs
      wrt = wrt+gwrt*delt
      wst = wst+gwst*delt

! --- dry weight of dead plant organs (roots,leaves & stems)
      dwrt = dwrt+drrt*delt
      dwlv = dwlv+drlv*delt
      dwst = dwst+drst*delt

! --- dry weight of dead and living plant organs
!     twrt = wrt+dwrt
      twlv = wlv+dwlv
      twst = wst+dwst
      tagp = twlv+twst

! --- leaf area index
      lai = lasum+ssa*wst
      laimax = max (lai,laimax)

! --- rooting depth as function of root weight
!      rd = rd+rr
      rd = afgen (rlwtb,22,wrt)
      rd= min(rd,rdm)

!     CO2 fixation and root,shoot developm
d     co2shootfix = tagp * 44.0d0/33.0d0
d     komma = ","
d     write(87,'(a12,1x,3(a,f12.4))') 
d    &  date,komma,co2rootfix,komma,co2rootloss,komma,co2shootfix
d     write(88,'(a12,1x,20(a,f12.4))') 
d    &  date,komma,wrtpot,komma,wrt,komma,wstpot,komma,wst,
d    &  komma,rdpot,komma,rd,komma,laipot,komma,lai,
d    &  komma,gwrt,komma,gwst,komma,drrt,komma,drlv,komma,drst

! --- cumulative relative transpiration
      cptr0 = cptr0 + ptra  
      ctr0 = ctr0  + tra
      if (cptr0.le.nihil) then
        crt0 = 0.0d0
      else
        crt0 = max(0.0d0,min(1.0d0,ctr0/cptr0))
      endif

      return
      end

! NwRootExtr New Subroutine for new Rootextraction ! NwRootExtr 

! ----------------------------------------------------------------------
      subroutine MatricFlux(task,phead,node) 
! ----------------------------------------------------------------------
!     Date               : October 2006   
!     Purpose            : Initialize and calculate matric flux potential
! ----------------------------------------------------------------------

      use variables
      implicit none

      integer task,lay,count,start,node
      real*8  phead1,phead2,wcontent,conduc1,conduc2,watcon,hconduc
      real*8  phead,logphead

      goto (1000, 2000) task

1000  continue

! === initialization =========================================================

      do lay = 1,numlay
        do count = 1,600
          mfluxtable(lay,count) = 0.0d0
        enddo
      enddo

      start = int(100.d0*log10(-wiltpoint))
      do lay = 1,numlay
        phead1 = -10.d0**(dble(start)/100.d0)

!       find first Node of the Layer
        Node = nod1lay(lay)

        wcontent = watcon(Node,phead1,cofgen(1,Node),
     &                    swsophy,numtab,sptab)
        conduc1 = hconduc (Node,wcontent,cofgen(1,Node),swfrost,10.d0,
     &                     swsophy,numtab,sptab)

        do count = start-1,1,-1
          phead2 = -10.d0**(dble(count)/100.d0)
          wcontent = watcon(Node,phead2,cofgen(1,Node),
     &                      swsophy,numtab,sptab)
          conduc2 = hconduc (Node,wcontent,cofgen(1,Node),swfrost,10.d0,
     &                       swsophy,numtab,sptab)
          mfluxtable(lay,count) = mfluxtable(lay,count+1) + 
     &                 0.5d0 * (conduc1 + conduc2) * (phead2 - phead1) 
          phead1 = phead2
          conduc1 = conduc2
        enddo
      enddo

      return

2000  continue

! === calculation of matric flux potential ===================================

      lay = layer(node)
      if (phead .lt. wiltpoint) then
! ---   very dry range 
         mflux(node) = 0.0d0
      elseif (phead .gt. -1.023293d0) then
! ---   very wet range (> -10^0.01)
         mflux(node) = mfluxtable(lay,1)
      else  
! ---   direct access table, with linear interpolation
        logphead = 100.d0*log10(-phead)
        count = int(logphead)
        mflux(node) = (logphead-dble(count))*mfluxtable(lay,count+1) +
     &                (dble(count+1)-logphead)*mfluxtable(lay,count)
      endif

      return
      end 