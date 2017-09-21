! File VersionID:
!   $Id: reduceva.for 203 2011-10-20 12:00:27Z kroes006 $
! ----------------------------------------------------------------------
      subroutine reduceva (task,swredu,empreva,peva,nrai,
     &     nird,cofred,rsigni,swinco,ldwet,spev,saev,fldaystart,dt,pond)
! ----------------------------------------------------------------------
!     date               : august 2006                                         
!     purpose            : calculates reduction of soil evaporation
! ----------------------------------------------------------------------
      implicit none

! --- global
      integer swredu,task,swinco
      real*8  empreva,peva,nrai,nird,cofred,rsigni,ldwet,spev,saev,dt
      real*8  pond
      logical fldaystart
! ----------------------------------------------------------------------
! --- local
      real*8  saevm1

! ----------------------------------------------------------------------
      goto (1000,2000) task

1000  continue

! === reduction potential soil evaporation on daily basis ==============


      if (pond .gt. 1.0d-10) then
! ---   in case of ponding no reduction ---
        empreva = peva
        ldwet = 0.0d0
        spev = 0.0d0
        saev = 0.0d0
      elseif (swredu.eq.1) then
! ---   reduction with black model ---
        if ((nrai+nird) .gt. rsigni) ldwet = 0.0d0
        ldwet = ldwet + 1.0d0
        empreva = cofred * (sqrt(ldwet)-sqrt(ldwet-1.0d0))
        empreva = min(empreva,peva)
      elseif (swredu.eq.2) then
! ---   reduction with boesten and stroosnijder model ---
        if ((nrai+nird) .lt. peva) then
ckro      if(swinco.ne.3) then
           spev = spev + (peva - (nrai+nird))
ckro      endif
          saevm1 = saev
          if (spev .lt. cofred**2) then
            saev = spev
          else
            saev = cofred * sqrt(spev)
          endif
          empreva = nrai + nird + saev - saevm1
        else
          empreva = peva
          saev = max (0.0d0,saev - (nrai+nird-peva))
          if (saev .lt. (cofred**2)) then
            spev = saev
          else
            spev = (saev/cofred)**2
          endif
        endif
      endif

      return

2000  continue

! === reduction potential soil evaporation for every time step ==============

      if (pond .gt. 1.0d-10) then
! ---   in case of ponding no reduction ---
        empreva = peva
        ldwet = 0.0d0
        spev = 0.0d0
        saev = 0.0d0
      elseif (swredu.eq.1) then
! ---   reduction with black model ---
        if (fldaystart .and. (nrai+nird) .gt. rsigni) ldwet = 0.0d0
        empreva = (cofred * (sqrt(ldwet+dt)-sqrt(ldwet))) / dt
        empreva = min(empreva,peva)
        ldwet = ldwet + dt
      elseif (swredu.eq.2) then
! ---   reduction with boesten and stroosnijder model ---
        if ((nrai+nird) .lt. peva) then
          spev = spev + (peva - (nrai+nird)) * dt
          saevm1 = saev
          if (spev .lt. cofred**2) then
            saev = spev
          else
            saev = cofred * sqrt(spev)
          endif
          empreva = ((nrai + nird)*dt + saev - saevm1) / dt
        else
          empreva = peva
          saev = max (0.0d0,saev - (nrai+nird-peva)*dt)
          if (saev .lt. (cofred**2)) then
            spev = saev
          else
            spev = (saev/cofred)**2
          endif
        endif
      endif

      return
      end
