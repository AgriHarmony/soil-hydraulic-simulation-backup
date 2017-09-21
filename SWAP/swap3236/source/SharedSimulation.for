! File VersionID:
!   $Id: SharedSimulation.for 175 2009-08-27 10:22:16Z kroes006 $
! ----------------------------------------------------------------------
      subroutine SharedSimulation(task) 
! ----------------------------------------------------------------------
!     Date               : July 2009
!     Purpose            : open and write data to shared files
! ----------------------------------------------------------------------

      use Variables
! Preprocessor directive WINXPIVF for MS-WINdows XP and Intel Visual Fortran compiler
!D#ifdef WINXPIVF
!D     use ifport
!D#endif
! Preprocessor directive WINXPCVF for MS-WINdows XP and Compaq Visual Fortran compiler
!D#ifdef WINXPCVF
!D     use dflib
!D#endif
      implicit none

! --- global variables ------------------
      integer task
! --- local variables ------------------
      integer(2) PosArg,status
      integer(4) delay
      logical    flhold
      integer    unss, ID_Shared, getun, IDread
      character  strIDss*3, strFINA*80, messag*200

      save  unss     
! ----------------------------------------------------------------------
      data  delay/100/                       ! delay in milisecs


      goto (1000, 2000, 3000, 4000) task


1000  continue

      PosArg         = 2
      Call GetArg (PosArg,strIDss,status)
      if(status.ne.2) then
        messag = 'Argument of executable-call is not correct for '//
     &                    ' Shared simulation !'
        call fatalerr ('readswap',messag)
      endif

      read(strIDss,'(i3.3)')ID_Shared

      PosArg         = 3
      Call GetArg (PosArg,strFINA,status)

      unss  = getun (20,99)
      open(unit=unss,file=strFINA,status='unknown',
     &     action='READWRITE',share='DENYNONE',buffered='NO')


!     open shared data file
      call FromSwap(task)
      call ToSwap(task)


      return


2000  continue

!D     flhold = .true.
!D     do while (flhold)
!D#ifdef WINXPCVF
!D        call sleepqq(delay)             ! delay in milisecs
!D#endif
!D#ifdef WINXPIVF
!D        call sleepqq(delay)             ! delay in milisecs
!D#endif
!D        rewind(unss)
!D        read(unss,'(i4)')IDread
!D        if(IDread.eq.ID_Shared) flhold=.false.     
!D      end do

!     read New data

      call ToSwap(2)


      return



3000  continue

      rewind(unss)
      write(unss,'(i4)')-1*ID_Shared

!     write New data

      call FromSwap(2)

      return



4000  continue

! === close Shared Directive file ===========================

      call ToSwap(3)
      call FromSwap(3)

      close (unss)

      return
      end
