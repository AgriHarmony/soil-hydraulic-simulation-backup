! File VersionID:
!   $Id: watstor.for 78 2008-01-08 08:56:16Z kroes006 $
! ----------------------------------------------------------------------
      subroutine watstor (volm1,volact,numnod,theta,dz,FrArMtrx)
! ----------------------------------------------------------------------
!     Date               : 29/9/99
!     Purpose            : calc. water storage in the soil profile        
!     Subroutines called : -                                           
!     Functions called   : -                                           
!     File usage         : -                                           
!     Differences SWAP/SWAPS: 
!     - SWAPS: extra parameters
! ----------------------------------------------------------------------
      IMPLICIT NONE
      Include 'arrays.fi'

! --- global
      INTEGER NUMNOD
      real*8  DZ(MACP), FrArMtrx(MaCp)
      real*8  THETA(MACP),VOLM1,VOLACT

! --- local
      INTEGER i
! ----------------------------------------------------------------------
! --- update soil profile water storage
      volm1 = volact
      volact = 0.0d0
      do 10 i = 1,numnod
        volact = volact+theta(i)*dz(i)*FrArMtrx(i)
 10   continue

      return
      end

