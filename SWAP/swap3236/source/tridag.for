! File VersionID:
!   $Id: tridag.for 137 2009-07-10 20:31:38Z kroes006 $
! ----------------------------------------------------------------------
      SUBROUTINE TRIDAG (N,A,B,C,R,U,ierror)
!***********************************************************************
!* Date      : 15/9/99                                    
!* Purpose:    Solves for a vector U a tridiagonal linear set.         *
!* References:                                                         *
!* Press, W.H., B.P. Flannery, S.A. Teukolsky & W.T. Vetterling, 1989. *
!* Numerical Recipes in FORTRAN. Cambridge University Press, New York. *
!* pp 40-41                                                            *
!***********************************************************************
!* Input:     N -      Number of equations                             *
!*            A,B,C -  Coefficients of the matrix                      *
!*            R -      known vector                                    *
!* Output:    U -      solved vector                                   *
!***********************************************************************
      implicit none
      include 'arrays.fi'

! --- global declarations
! --- (i.i) input
      integer   n, ierror
      real*8    a(n), b(n), c(n), r(n)
! --- output
      real*8    u(n)
! --- parameters
      real*8    small
      parameter (small = 0.3d-37)

! --- local declarations
      integer   i
      real*8    gamma(macp), beta
      character messag*200
! ----------------------------------------------------------------------

      ierror = 0

! --- (1) if b(1)=0 then rewrite the equations as a set of order n-1
!     to eliminate u(2)


      if (abs(b(1)).lt.small) then
        messag = 'During the numerical solution the factor b(1)'//
     &  ' became too small !'
        ierror = 1000
!        call fatalerr ('tridag',messag)
      else
! ---   (2) decomposition and forward substitution
        beta = b(1)
        u(1) = r(1) / beta
        do i = 2, n
          gamma(i) = c(i-1) / beta
          beta = b(i) - a(i)*gamma(i)

! ---     (2.1) if beta=0 then go to another algorithm including
! ---     elimination with pivoting
          if (abs(beta).lt.small) then
            messag = 'during the numerical solution the factor beta'//
     &      ' became too small !'
            ierror = 1000+i
!            call fatalerr ('tridag',messag)
            messag = messag ! for Forcheck
          else
            u(i) = (r(i) - a(i)*u(i-1)) / beta
            end if
        enddo

! ---   (3) back substitution
        do i = n-1, 1, -1
          u(i) = u(i) - gamma(i+1)*u(i+1)
        end do
      end if

      return
      end

      SUBROUTINE bandec(a,n,m1,m2,np,mp,al,mpl,indx,d)
! CHAPTER 2.4 Numerical Recipes
      implicit none
      INTEGER m1,m2,mp,mpl,n,np,indx(n)
      REAL*8 d,a(np,mp),al(np,mpl),TINY
      PARAMETER (TINY=1.d-20)
!     Given an n × n band diagonal matrix A with m1 subdiagonal rows and m2 superdiagonal
!     rows, compactly stored in the array a(1:n,1:m1+m2+1) as described in the comment for
!     routine banmul, this routine constructs an LU decomposition of a rowwise permutation
!     of A. The upper triangular matrix replaces a, while the lower triangular matrix is !     returned in al(1:n,1:m1). indx(1:n) is an output vector which records the row !    !     permutation effected by the partial pivoting; d is output as ±1 depending on whether !     the number of row interchanges was even or odd, respectively. This routine is used !     in combination with banbks to solve band-diagonal sets of equations.

      INTEGER i,j,k,l,mm
      REAL*8 dum
      character messag*200

      mm=m1+m2+1
!      if(mm.gt.mp.or.m1.gt.mpl.or.n.gt.np) stop 'bad args in bandec'
      if(mm.gt.mp.or.m1.gt.mpl.or.n.gt.np) then
         messag = 'bad args in bandec !'
         call fatalerr ('bandec',messag)
      endif
      l=m1
      do i=1,m1               !   Rearrange the storage a bit.
         do j=m1+2-i,mm
            a(i,j-l)=a(i,j)
         enddo
         l=l-1
         do j=mm-l,mm
            a(i,j)=0.0d0
         enddo
      enddo

      d=1.0d0
      l=m1
      do k=1,n                !  For each row...
         dum=a(k,1)
         i=k
         if(l.lt.n)l=l+1
         do j=k+1,l   !  Find the pivot element.
            if(abs(a(j,1)).gt.abs(dum))then
               dum=a(j,1)
               i=j
            endif
         enddo
         indx(k)=i
         if(abs(dum).lt.1.0d-20) a(k,1)=TINY

            !Matrix is algorithmically singular, but proceed anyway with TINY pivot 
            !(desirable in some applications).

         if(i.ne.k)then          !  Interchange rows.
            d=-d
            do j=1,mm
               dum=a(k,j)
               a(k,j)=a(i,j)
               a(i,j)=dum
            enddo
         endif
         do i=k+1,l           !  Do the elimination.
            dum=a(i,1)/a(k,1)
            al(k,i-k)=dum
            do j=2,mm
               a(i,j-1)=a(i,j)-dum*a(k,j)
            enddo
            a(i,mm)=0.0d0
         enddo
      enddo
      return
      END


      SUBROUTINE banbks(a,n,m1,m2,np,mp,al,mpl,indx,b)
! CHAPTER 2.4 Numerical Recipes
      implicit none
      INTEGER m1,m2,mp,mpl,n,np,indx(n)
      REAL*8 a(np,mp),al(np,mpl),b(n)
!     Given the arrays a, al, and indx as returned from bandec, and given a right-hand side
!     vector b(1:n), solves the band diagonal linear equations A · x = b. The solution    !     vector x overwrites b(1:n). The other input arrays are not modified, and can be left !     in place for successive calls with different right-hand sides.
      INTEGER i,k,l,mm
      REAL*8 dum
      character messag*200

      mm=m1+m2+1
!      if(mm.gt.mp.or.m1.gt.mpl.or.n.gt.np) stop 'bad args in banbks'
      if(mm.gt.mp.or.m1.gt.mpl.or.n.gt.np) then
         messag = 'bad args in banbks !'
         call fatalerr ('banbks',messag)
      endif
      l=m1
      do k=1,n    !  Forward substitution, unscrambling the permuted rows as we go
         i=indx(k)
         if(i.ne.k)then
            dum=b(k)
            b(k)=b(i)
            b(i)=dum
         endif
         if(l.lt.n)l=l+1
         do i=k+1,l
            b(i)=b(i)-al(k,i-k)*b(k)
         enddo
      enddo
      l=1
      do i=n,1,-1 ! Backsubstitution.
         dum=b(i)
         do k=2,l
            dum=dum-a(i,k)*b(k+i-1)
         enddo
         b(i)=dum/a(i,1)
         if(l.lt.mm) l=l+1
      enddo
      return
      END
