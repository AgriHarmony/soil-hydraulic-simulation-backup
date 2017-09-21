! File VersionID:
!   $Id: SharedExchange.for 140 2009-07-10 20:50:23Z kroes006 $
! ----------------------------------------------------------------------
      subroutine FromSwap(task) 
! ----------------------------------------------------------------------
!     Date               : July 2009
!     Purpose            : write data to shared files
! ----------------------------------------------------------------------
      use Variables
      implicit none

! --- global variables ------------------
      integer task
! --- local variables ------------------
!      save  unss     
! ----------------------------------------------------------------------

      goto (1000, 2000, 3000) task


1000  continue
!     section meant to initialize, open or access exchange file

      return


2000  continue
!     section meant to process intermediate SWAP results (variables/parameters) and writing to the exchange file

      return


3000  continue
!     section meant to terminate the exchnge process and close the exchange file

      return
      end
! ----------------------------------------------------------------------
      subroutine ToSwap(task) 
! ----------------------------------------------------------------------
!     Date               : July 2009
!     Purpose            : write data to shared files
! ----------------------------------------------------------------------
      use Variables
      implicit none

! --- global variables ------------------
      integer task
! --- local variables ------------------
!      save  unss     
! ----------------------------------------------------------------------

      goto (1000, 2000, 3000) task


1000  continue
!     section meant to initialize, open or access exchange file

      return


2000  continue
!     section meant to read from the exchange file and process the info to SWAP variables/parameters

      return


3000  continue
!     section meant to terminate the exchnge process and close the exchange file


      return
      end
