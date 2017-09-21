! File VersionID:
!   $Id: boundtop.for 176 2010-03-13 11:36:14Z kroes006 $
! ----------------------------------------------------------------------
      subroutine boundtop 
! ----------------------------------------------------------------------
!     date               : august 2004 / sept 2005
!     purpose            : determine soil profile top boundary condition      
! ----------------------------------------------------------------------
      use variables
      implicit none

! --- local variables

      real*8  emax,ks,theatm,ksurf
      real*8  watcon,hconduc

! ----------------------------------------------------------------------
! --- local variables
      real*8  h0,hcomean,hconode_vsmall,k1Atm,p1,p2,p2Mp,q1,RsRoMp

      data    hconode_vsmall  /1.0d-10/  ! Hydraulic conductivity for complete frozen soils

! ----------------------------------------------------------------------
! --- Initialisation

! --- runon of present day
      if (flDayStart .and. flrunon) runon = runonarr(daycum+1)      


      FlRunoff = .false.
      QMpLatSs = 0.0d0


!     S O I L   E V A P O R A T I O N

! --- Calculate hydraulic conductivity corresponding with hAtm
      if (hAtm.lt.0.0d0) Then
         TheAtm = watcon(1,dble(hatm),cofgen(1,1),swsophy,numtab,sptab)
         ksurf  = hconduc (1,TheAtm,cofgen(1,1),swfrost,rfcp(1),
     &                     swsophy,numtab,sptab)
         if(SwMacro.eq.1) then
            ksurf = FrArMtrx(1) * ksurf
         endif
      else

! --- This only occurs if RH is 100% in SWAPS, never used for SWAP
         kSurf = k(1)
      endif
      k1Atm = hcomean(swkmean,kSurf,k(1),dz(1),dz(1))

! --- maximum evaporation rate according to Darcy
      Emax = -k1Atm * ((hatm-h(1))/disnod(1)+1.0d0)
      
! --- determine reduced soil evaporation rate
      if (swredu .eq. 0) then
        reva = min(peva,max(0.0d0,Emax))
      else
        reva = min(empreva,max(0.0d0,Emax))
      endif

!     H I G H   A T M O S P H E R I C   D E M A N D
!     flux through ground surface based on precipitation - evaporation 
!     and remaining ponding of previous timestep
      q0 = (nraidt+nird+melt)*(1.0d0-ArMpSs) + runon - reva 
      q1 = - q0 - pondm1/dt

!     check whether the atmospheric demand condition applies
      if (q1 .ge. 0.0d0 .and. q1.gt.Emax) then
         ftoph    = .true.
         hsurf    = hAtm
         kmean(1) = k1Atm
         pond     = 0.0d0
         runots   = 0.0d0
         return
      endif             

!     maximum conductivity assuming saturation at ground surface (z=0)
      if(flksatexm)then
         ks = rfcp(1)*ksatexm(1) + (1.0d0-rfcp(1))*hconode_vsmall
      else
         ks = rfcp(1)*ksatfit(1) + (1.0d0-rfcp(1))*hconode_vsmall
      endif
      k1max = hcomean(swkmean,ks,k(1),dz(1),dz(1))
!     check whether application of flux=q1 will yield a pressure head >0 
!     at ground surface. If not: flux boundary condition is valid
      h0    = h(1) - disnod(1)*(q1/k1max+1.0d0)
      if (h0.le.1.0d-6) then
         ftoph    = .false.
         kmean(1) = 0.0d0
         hsurf    = 0.0d0
         pond     = 0.0d0
         runots   = 0.0d0
         qtop     = q1
      else                 ! ponding occurs
         ftoph    = .true.
         kmean(1) = k1max
         FlRunoff = .true. ! runoff potential possible

! --- calculate max value of pond without runoff
         p1     = k1max/disnod(1) * dt
         p2     = 1.0d0/(p1+1.0d0)
         h0max  = p2 * ( pondm1 + q0*dt - k1max*dt + p1*h(1) ) 

! --- in case of macropores, calc. potential overland flow into macrop.: QMpLatSs
         if (SwMacro.eq.1) then
            if (h0max.gt.PndmxMp) then
               RsRoMp  = (h0max + (nraidt+nird+melt)*ArMpSs*dt) / KsMpSs
               p2Mp    = 1.0d0 / (p1 + 1.0d0 + dt/RsRoMp)
               pond    = (h0max - PndmxMp) * p2Mp/p2
               QMpLatSs= pond * dt/RsRoMp
               QMpLatSs= dmin1(QMpLatSs,h0max)
               if (QMpLatSs.lt.1.0d-7) QMpLatSs = 0.0d0
            else
               QMpLatSs = 0.0d0
            endif
         endif
      endif
!  
      return
      end


! ----------------------------------------------------------------------
      SUBROUTINE PONDRUNOFF (swdra,swmacro,FlRunoff,disnod,dt,h,H0max,
     &              k1max,pondm1,pondmx,q0,rsro,rsroexp,sttab,wls,swst,
     &              QMpLatSs,hsurf,pond,runots,swpondmx,pondmxtab,t1900)
! ----------------------------------------------------------------------
!     Date               : 4/5/2005
!     Purpose            : determines ponding height and calculates runoff        
!     Formal parameters  :                                             
!     Subroutines called : -                                           
!     Functions called   : runoff                                 
!     File usage         : -                                           
! ----------------------------------------------------------------------
      IMPLICIT NONE
      Include 'arrays.fi'

! --- global                                                       In
      integer swdra,swmacro,swpondmx
      real*8  disnod(macp+1),dt,h(macp),pondm1,pondmx,rsro,rsroexp
      real*8  sttab(22,2),wls,swst
      real*8  h0max,k1max,q0                 ! from subroutine BOCOTOP
      real*8  QMpLatSs                       ! from subroutine MACRORATE
      real*8  pondmxtab(2*mairg),t1900
      logical FlRunoff
!                                                                  Out
      real*8  hsurf,pond,runots 

! ----------------------------------------------------------------------
! --- local variables
      INTEGER i
      real*8  h0,h0min,p1,p2,runoff,afgen

! ----------------------------------------------------------------------

! --  in case of time dependent ponding: determine pondmx
      if (swpondmx.eq.1) then
         pondmx = afgen (pondmxtab,2*mairg,t1900+dt)
      endif

! --- in case of Macropores: 
      if (swmacro.eq.1) then
         if (FlRunoff) then
!   - h0max is reduced with overland flow into Macropores
            q0     = q0 - QMpLatSs/dt
            p1     = k1max/disnod(1) * dt
            p2     = 1.0d0/(p1+1.0d0)
            h0max  = p2 * ( pondm1 + q0*dt - k1max*dt + p1*h(1) )   
         else
!   - inflow excess by direct precipitation into macropores is added to ponding
            pond= - QMpLatSs
            return
         endif
      endif
!
! --- check whether h0max, the max value of pond, yields a runoff

!      if(swdra.ne.2 .and. h0max.le.pondmx)then

      if(h0max.le.pondmx)then
         runots   = 0.0d0
         pond     = h0max 
         hsurf    = pond
         return
      end if

      runots = runoff(swdra,h0max,pondmx,rsro,rsroexp,wls,swst,sttab,dt)
      if(dabs(runots).lt.1.0d-6)then
!        if no runoff occurs: first estimation of pond is OK 
         pond     = h0max 
         hsurf    = pond
         return
      else if(dabs(runots).ge.1.0d-6 .and. swdra.ne.2 .and.
     &                                dabs(rsroexp-1.0d0).lt.1.0d-6)then
         p1 = k1max/disnod(1) * dt
         p2 = 1.0d0 / (p1 + 1.0d0 + dt/rsro)

         pond     = p2 * ( pondm1 + q0*dt - k1max*dt + p1*h(1) + 
     &                     dt/rsro * pondmx )
         runots = runoff(swdra,pond,pondmx,rsro,rsroexp,wls,
     &                   swst,sttab,dt) 
         hsurf    = pond
         return
      else             

!        if runoff occurs: find values for pond and runots iteratively

         p1 = k1max/disnod(1) * dt
         p2 = 1.0d0/(p1+1.0d0)

!        estimation of maximum ponding: ignore runoff
         h0max = p2 * ( pondm1 + q0*dt - k1max*dt + p1*h(1) )            
         h0min = 0.0d0
         do i=1,30
            pond   = 0.5d0 * (h0max + h0min)
            runots = runoff(swdra,pond,pondmx,rsro,
     &                         rsroexp,wls,swst,sttab,dt)
            h0     = p2 * ( pondm1 +q0*dt -k1max*dt +p1*h(1) -runots)

            if(dabs(pond-h0).lt.1.0d-6)then
               hsurf    = pond
               return
            else
               if(h0.gt.pond)then
                  h0min = pond
               else
                  h0max = pond
               end if 
            end if
         end do
      end if

!     if convergence has not been reached: proceed with final value
      pond   = 0.5d0 * (h0max + h0min)
      runots = runoff(swdra,pond,pondmx,rsro,rsroexp,wls,swst,sttab,dt)
      hsurf  = pond

      return
      end


