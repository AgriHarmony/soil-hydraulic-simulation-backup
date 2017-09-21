! File VersionID:
!   $Id: fluxes.for 109 2009-01-08 11:41:16Z kroes006 $
! ----------------------------------------------------------------------
      subroutine fluxes (q,qbot,dt,inq,numnod,thetm1,theta,dz,qrot,
     & qdra,qimmob,qtop,qrosum,qdrtot,volact,volm1,swbotb,FrArMtrx,
     & QExcMpMtx,QMaPo,nrlevs,fllowgwl)
! ----------------------------------------------------------------------
!     date               : 29/9/99
!     purpose            : calculates the fluxes between compartments
! ----------------------------------------------------------------------
      implicit none
      include 'arrays.fi'

! --- global
      integer numnod,swbotb,nrlevs

      real*8  qbot,dt,inq(macp+1),dz(macp),qrot(macp),qdra(Madr,macp)
      real*8  qimmob(macp),QExcMpMtx(macp),qtop,qrosum,qdrtot,QMaPo
      real*8  q(macp+1),thetm1(macp),theta(macp),volact,volm1
      real*8  FrArMtrx(MaCp)
      logical fllowgwl
! ----------------------------------------------------------------------
! --- local
      integer i,level

! ----------------------------------------------------------------------

! --- determine qbot if not specified
      if (swbotb .eq. 5 .or. swbotb .eq. 7 .or.
     &    swbotb .eq. 8 .or. swbotb .eq. -2 .or.
     &    (swbotb .eq. 1 .and. fllowgwl)) then
        qbot = qtop + qrosum + qdrtot - QMaPo + (volact-volm1)/dt
      endif

! --- calculate fluxes (cm/d) from changes in volume per compartment
      i = numnod+1
      q(i) = qbot
      inq(i) = inq(i) + q(i)*dt
      do i = numnod,1,-1
        q(i) = - (theta(i)-thetm1(i)+qimmob(i))*FrArMtrx(i)*dz(i)/dt + 
     &                q(i+1)-qrot(i)+QExcMpMtx(i)
     
        do level=1,nrlevs
           q(i) = q(i) - qdra(level,i)
        enddo
        inq(i) = inq(i) + q(i)*dt
      end do

      return
      end

