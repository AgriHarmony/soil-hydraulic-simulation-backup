! File VersionID:
!   $Id: penmon.for 184 2010-11-19 11:12:27Z kroes006 $
! ----------------------------------------------------------------------
      subroutine PenMon (logf,swscre,daymeteo,lat,alt,altw,a,b,rcs,
     & rad,tav,hum,win,rsc,es0,et0,ew0,swcf,ch,flbaresoil,daylp,
     & flmetdetail,irecord,nmetdetail,albedo,tmn,tmx,rsw)
! ----------------------------------------------------------------------
!     date               : 14/01/99
!     purpose            : calculation of potential evaporation & 
!                          transpiration rates from a bare soil surface,
!                          a dry and a wet crop canopy;           
!                          based on the penman - monteith approach.
! global
!     formal parameters  : (i = input, o = output)
!     logf    Internal number of logbook output file *.LOG ............i
!     swscre  Switch of screen display:  0 = no display; ..............i
!             1 = summary water balance; 2 = daynumber
!     daymeteo  daynumber (january 1st = 1) [-]........................i
!     lat    latitude [degr.,dec.degr., n=+,s=-].......................i
!     alt    altitude above mean sea level [m].........................i
!     altw   altitude of wind speed measurement [m] ...................i
!     a      first  angstrom coefficient [-]...........................i
!     b      second angstrom coefficient [-]...........................i
!     rcs    reflection coefficient soil [-]...........................i
!     albedo reflection coefficient crop [-]...........................i
!     rad    incoming short wave radiation [j/m2/d]....................i
!     tav    average temperature (24 hour) [C].........................i
!     hum    vapour pressure [kpa].....................................i
!     win    windspeed at 2 m height [m/s].............................i
!     rsc    minimum canopy resistance of dry crop [s/m]...............i
!     rsw    canopy resistance of intercepted water [s/m]..............i
!     swcf   switch use crop factor (=1) or crop height (=2)...........i
!     ch     crop height [cm]..........................................i
!     es0    potential evaporation rate from a wet bare soil [mm/d]....o
!     et0    potential transpiration rate from a dry crop [mm/d].......o
!     ew0    potential transpiration rate from a wet crop [mm/d].......o
!    
! local
!     ckarman von karman constant [-]
!     zm      height of wind speed [cm]
!     zh      height of temperature and humidity measurement [cm]
!     d       zero displacement of wind profile [cm]
!     zom     roughness parameter for momentum [cm]
!     zoh     roughness parameter for heat and vapour [cm]
!     zmeasw  altitude of wind speed measurement [m] ...................i
! ----------------------------------------------------------------------
      implicit none
! global
      integer daymeteo,swcf,logf,swscre,irecord,nmetdetail
      real*8  lat,alt,altw,albedo,tmn,tmx,ch
      real*8  a,b,rcs,rsc,rsw,es0,et0,ew0,hum,rad,tav,win
      logical flbaresoil,flmetdetail
! local
      real*8  lambda,atmtr,cosld,dayl,delta,dsinb,dsinbe,dso,ea
      real*8  ed,etaerc,etaers,etaerw,etradc,etrads,etradw
      real*8  gamma,gammoc,gammos,gammow,palt,rac,ras,raw,cp
      real*8  relssd,rho,rnc,rnl,rns,rnw,rss,sinld,tavk,tkv
      real*8  ud,vpd,daylp,gs,gc,gw,radial,dec,aob,tmnk,tmxk
      real*8  ckarman, chgrass, chsoil, d, dgrass
      real*8  zm, zmeasw, zmeash, zom, zomgrass, zh,zoh
      real*8  zact, dact, zomact, fact, fmeas
      real*8  sunrise,sunset,startrec,endrec,t1,t2,pi,sc
      character messag*200
! local parameters
      data    chgrass  /12.0d0/      ! height of reference crop grassland [cm]
      data    zmeash   /200.0d0/     ! default height of humidity and temperature measurement [cm !]
      data    ckarman  /0.41d0/      ! von karman constant [-]
      data    chsoil   /0.1d0/       ! nihil crop height for a wet bare soil [cm]
      data    pi       /3.141593d0/  ! number pi
! ----------------------------------------------------------------------

! --- conversion to cm
      zmeasw = 100.0d0 * altw

! --- avoid zero crop height
      if (flbaresoil) then
        ch = chgrass
      else 
        if (swcf.eq.1) then
          ch = chgrass
        else
          ch = max (ch,0.1d0)
        endif
      endif

! --- conversion of temperature from [c] to [k]
      tavk = tav+273.15d0
      tmnk = tmn+273.15d0
      tmxk = tmx+273.15d0

! --- atmospheric pressure at elevation alt [kpa]
      palt = 101.3d0*((tavk-0.0065d0*alt)/tavk)**5.26

! --- latent heat of vaporization [mj/kg]
      lambda = 2.501d0-0.002361d0*tav

! --- saturation vapour pressure [kpa]
      if (flmetdetail) then
        ea = 0.611d0*exp(17.27d0*tav/(tav+237.3d0))
      else
        ea = 0.3055d0*(exp(17.27d0*tmn/(tmn+237.3d0)) + 
     &                 exp(17.27d0*tmx/(tmx+237.3d0)))
      endif

! --- measured vapour pressure not to exceed saturated vapour pressure
      ed = min(hum,ea)
! --- vapour pressure deficit [kpa]
      vpd = ea-ed

! --- slope vapour pressure curve [kpa/c]
      delta = 4098.0d0*ea/(tav+237.3d0)**2

! --- psychrometric constant [kpa/c]
      gamma = 0.00163d0*palt/lambda

! --- atmospheric density [kg/m3]
      tkv = tavk/(1.0d0-0.378d0*ed/palt)
      rho = 3.486d0*palt/tkv

! --- specific heat moist air [kj/kg/c]
      cp = 622.0d0*gamma*lambda/palt

! --- aerodynamic resistance [s/m] - soil, crop & wet crop

! -   day wind [m/s] for daily records, avoid zero windspeed
      ud = max (win, 0.0001d0)

!     adjust wind speed if crop height deviates from measurement height of wind speed
!     assuming equal wind speed at 100 meter (1.0d4 cm !) above the soil surface
!     (all length-units in cm ! )
      if (ch.gt.zmeash .or. zmeasw.gt.zmeash) then
        dgrass = 2.0d0/3.0d0 * chgrass
        zomgrass = 0.123d0 * chgrass
        fmeas = log((1.0d4-dgrass)/zomgrass) / 
     &         log((zmeasw-dgrass)/zomgrass) 
        zact = max(ch,200.0d0)
        dact = 2.0d0/3.0d0 * ch
        zomact = 0.123d0 * ch
       fact = log((zact-dact)/zomact) / log((1.0d4-dact)/zomact)
       ud = ud * fact * fmeas
      endif

! -   constants to determine aerodynamic resistance
      zm = max(ch,zmeash)     ! measured height of wind speed measurement [cm !]
      zh = zm                 ! height of humidity and temperature measurement [cm !]
      d = 2.0d0/3.0d0 * ch   ! zero displacement of wind profile [cm !]
      zom = 0.123d0 * ch       ! roughness parameter for momentum [cm]
      zoh = 0.1d0 * zom        ! roughness parameter for heat and vapour [cm]
! -   aerodynamic resistance for dry and wet crop
      rac = log ((zm - d)/zom) * log((zh - d)/zoh) / ckarman**2/ud 
      raw = rac

! -   aerodynamic resistance for bare wet soil with a crop height set to 0.1 cm
      d = 2.0d0/3.0d0 * chsoil ! zero displacement of wind profile at low height [cm !]
      zom = 0.123d0 * chsoil     ! roughness parameter for momentum at low height [cm]
      zoh = 0.1d0 * zom          ! roughness parameter for heat and vapour [cm]
      ras = log ((zm - d)/zom) * log((zh - d)/zoh) / ckarman**2/ud

! --- surface resistance of wet soil [s/m]
      rss = 0.0d0

! --- modified psychrometric constant [kpa/c] - soil, crop & wet crop 
      gammos = gamma*(1.0d0+rss/ras)
      gammoc = gamma*(1.0d0+rsc/rac)
      gammow = gamma*(1.0d0+rsw/raw)

! --- net short wave radiation [mj/m**2/d] - soil, crop & wet crop
      rns = (1.0d0-rcs)*rad/1000000.0d0
      rnc = (1.0d0-albedo)*rad/1000000.0d0
      rnw = (1.0d0-albedo)*rad/1000000.0d0

! --- procedure to derive extraterrestrial radiation [mj/m2/day]
      if (flmetdetail) then

! ---   declination of the sun as a function of daynr
        radial = pi/180.d0
        dec = -asin(sin(23.45d0*radial)*
     &                        cos(2.d0*pi*dble(daymeteo+10)/365.0d0))
! ---   some intermediate variables
        sinld = sin(radial*lat)*sin(dec)
        cosld = cos(radial*lat)*cos(dec)
        aob = sinld/cosld
! ---   calculation of astronomical daylength
        if (aob.lt.-1.0d0) then
          messag='Warning: latitude above polar circle, daylength= 0hrs'
          call warn ('Astro',messag,logf,swscre)
        else if (aob.gt.1.0d0) then
          messag='Warning: latitude within polar circle,daylength=24hrs'
          call warn ('Astro',messag,logf,swscre)
        else
          dayl  = 12.0d0*(1.0d0+2.0d0*asin(aob)/pi)
        endif

!       Solar constant [j/m2/day]
        sc = 118.08d6*(1.0d0+0.033d0*cos (2.0d0*pi*daymeteo/365.d0))

! ---   Extraterrestrial radiation of current period (dso) [j/m2/day]
        sunrise = 0.5d0 - dayl / 48.d0
        sunset = 0.5d0 + dayl / 48.d0
        startrec = dble(real(irecord-1)/real(nmetdetail))
        endrec = dble(real(irecord)/real(nmetdetail))

        if (endrec .lt. sunrise) then
          dso = 0.d0
        else if (startrec .gt. sunset) then
          dso = 0.d0
        else if (startrec .lt. sunrise) then
! ---     solar time angle at beginning of radiation period
          t1 = (sunrise - 0.5d0) * 2.d0 * pi
! ---     solar time angle at end of radiation period
          t2 = (endrec - 0.5d0) * 2.d0 * pi
          dso = sc/2.d0/pi*((t2 - t1)*sinld + cosld*(sin(t2) - sin(t1))) 
     &          * nmetdetail
        else if (endrec .gt. sunset) then
! ---     solar time angle at beginning of radiation period
          t1 = (startrec - 0.5d0) * 2.d0 * pi
! ---     solar time angle at end of radiation period
          t2 = (sunset - 0.5d0) * 2.d0 * pi
          dso = sc/2.d0/pi*((t2 - t1)*sinld + cosld*(sin(t2) - sin(t1)))
     &          * nmetdetail
        else 
! ---     solar time angle at beginning of radiation period
          t1 = (startrec - 0.5d0) * 2.d0 * pi
! ---     solar time angle at end of radiation period
          t2 = (endrec - 0.5d0) * 2.d0 * pi
          dso = sc/2.d0/pi*((t2 - t1)*sinld + cosld*(sin(t2) - sin(t1)))
     &          * nmetdetail
        endif

      else
! ---   just daily extraterrestrial radiation (one record per day)
        call astro(logf,swscre,daymeteo,lat,
     &           dayl,daylp,sinld,cosld,dsinb,dsinbe,dso)
      endif

! --- net long wave radiation (mj/m2/d)
      atmtr = 0.0d0

      if (dso.gt.1.0d-5)  atmtr = rad/dso
      relssd = max(min((atmtr-a)/b,1.0d0),0.0d0)

!      rnl = 4.9d-9*tavk**4*(0.34d0-0.14d0*sqrt(ed))*(0.1d0+0.9d0*relssd)
      rnl = 4.9d-9* 0.5d0 * (tmxk**4 + tmnk**4) *
     &      (0.34d0-0.14d0*sqrt(ed))*(0.1d0+0.9d0*relssd)
 
! --- soil heat flux [mj/m2/d]
      if (flmetdetail) then
        if ((startrec+endrec)/2.d0 .gt. sunrise .and.
     &      (startrec+endrec)/2.d0 .lt. sunset) then
! ---     daytime period
          gs = 0.1d0 * (rns - rnl)
          gc = 0.1d0 * (rnc - rnl)
          gw = 0.1d0 * (rnw - rnl)
        else
! ---     nighttime period
          gs = 0.5d0 * (rns - rnl)
          gc = 0.5d0 * (rnc - rnl)
          gw = 0.5d0 * (rnw - rnl)
        endif
      else
! --- daily record, net flux negligable
        gs = 0.d0
        gc = 0.d0
        gw = 0.d0
      endif

! --- aerodynamic term of the pm equation [mm/d] - soil, crop & wet crop
      etaers = (86.4d0/lambda)*(1.0d0/(delta+gammos))*(rho*cp*vpd/ras)
      etaerc = (86.4d0/lambda)*(1.0d0/(delta+gammoc))*(rho*cp*vpd/rac)
      etaerw = (86.4d0/lambda)*(1.0d0/(delta+gammow))*(rho*cp*vpd/raw)

! --- radiation term of the pm equation [mm/d] - soil, crop & wet crop
      etrads = delta/(delta+gammos)*(rns-rnl-gs)*1.0d0/lambda      
      etradc = delta/(delta+gammoc)*(rnc-rnl-gc)*1.0d0/lambda      
      etradw = delta/(delta+gammow)*(rnw-rnl-gw)*1.0d0/lambda      

! --- sum of both terms [mm/d] - soil, crop and wet crop      
      es0 = max (0.0d0,etaers+etrads)
      et0 = max (0.0d0,etaerc+etradc)
      ew0 = max (0.0d0,etaerw+etradw)

      return
      end
