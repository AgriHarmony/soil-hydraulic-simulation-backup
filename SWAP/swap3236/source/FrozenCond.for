! File VersionID:
!   $Id: FrozenCond.for 184 2010-11-19 11:12:27Z kroes006 $
! ----------------------------------------------------------------------
      subroutine FrozenCond
! ----------------------------------------------------------------------
!     date               : sept 2005
!     purpose            : if Soil temperatures are simulated, determine 
!                          the reduction factors and frozen depth for 
!                          frozen conditions
! ----------------------------------------------------------------------
! global
      use variables
      implicit none

! local
      integer node
      logical flthaw

! ----------------------------------------------------------------------

!    reduction factor
      do node=1,numnod
         rfcp(node) = 1.0d0
         if (swfrost.eq.1)then
           if(tsoil(node).ge.tfroststa)then
              rfcp(node) = 1.0d0
           else if(tsoil(node).le.tfrostend) then
              rfcp(node) = 0.0d0
           else if(tsoil(node).lt.tfroststa .and. 
     &             tsoil(node).gt.tfrostend) then 
              rfcp(node) = (tsoil(node)-tfrostend)/(tfroststa-tfrostend)
           endif
         endif
      end do

 
!     frozen soil : frozen depth z and frozen node nr
      flthaw       = .true.
      nodfrostbot  = -1
      zfrostbot    = 0.0d0
!      nodfrosttop  = -1
      zfrosttop    = 0.0d0

      node = numnod
      do while (flthaw .and. node.gt.1)
         node = node - 1 
         if(tsoil(node) .le. tfrostend+1.0d-6)then
            zfrostbot = z(node+1) + (z(node) - z(node+1)) *
     &           (tfrostend-tsoil(node+1)) / (tsoil(node)-tsoil(node+1))
            flthaw      =.false.
            nodfrostbot = node
         endif
      end do

      if(.not.flthaw)then
         flthaw  = .true.
         node = 0
         do while (flthaw .and. node.lt.nodfrostbot)
            node = node + 1 
            if(tsoil(node) .le. tfrostend+1.0d-6)then
               if(node.eq.1) then
                  if(tetop.le.tfrostend) then
                     zfrosttop = 0.0d0
                  else
                     zfrosttop = z(node) - (z(node) - 0.0d0) *
     &             (tsoil(node)-tfrostend) / (tsoil(node)-tetop)
                  endif
               else
                  zfrosttop = z(node) - (z(node) - z(node-1)) *
     &             (tsoil(node)-tfrostend) / (tsoil(node)-tsoil(node-1))
               endif
               zfrosttop = min(0.0d0,zfrosttop)
               flthaw      =.false.
!               nodfrosttop = node
            endif
         end do
      end if

      return
      end
! ----------------------------------------------------------------------
      subroutine FrozenBounds
! ----------------------------------------------------------------------
!     date               : 20070206
!     purpose            : reduce or stop boundary (drainage and bottom) 
!                          fluxes under frost conditions
! ----------------------------------------------------------------------
!     Swap modules for data communication
      use variables
      implicit none

!     global - in
!      integer numnod,nodfrostbot,nodfrosttop,nrlevs,swnrsrf,layer(macp)
!      real*8  rfcp(macp),zfrostbot,zfrosttop,ksatfit(maho),thetas(macp)
!      real*8  dz(macp),cofani(maho),qbot,Zbotdr(Madr),L(Madr),theta(macp)
!      real*8  qbot_nonfrozen
!     global - out
!     global - in/out
!      real*8  qdra(Madr,macp),qdrain(Madr),qdrtot

!     local

      integer node,level,layercp(macp),leveldeepest

      real*8  volair,ksatcp(macp),cofanicp(macp),qdratot
      real*8  hconode_vsmall,zdeepest,ztop
      logical frozencomp
      data    hconode_vsmall  /1.0d-10/  ! Hydraulic conductivity for complete frozen soils

! ----------------------------------------------------------------------

!     initialize qbot
      qbot = qbot_nonfrozen


! --  verify available air volume

      node = numnod
      volair = 0.0d0
      frozencomp = .true.
      do while (frozencomp)
         volair = volair + (thetas(node)-theta(node))*dz(node)
         node = node - 1
         if(node.eq.0)then
            frozencomp = .false.
         else
            if(rfcp(node) .le. 0.01d0)then
               frozencomp = .false.
            end if
         end if 
      end do

! --  consider reduction when volair is very low
!     reduction of drainage only when systems are present
      if(swdra.eq.0) then
         if(nodfrostbot.gt.1 .and. volair.lt.0.01d0)then
            qbot = 0.0d0
         endif
      else
         if(nodfrostbot.gt.1 .and. volair.lt.0.01d0)then

            leveldeepest = 0
            zdeepest     = 0.0d0
            do level=1,nrlevs
               if(zbotdr(level).lt.zdeepest) then
                  leveldeepest = level
                  zdeepest     = zbotdr(level)
               endif
            enddo

            do node=1,numnod
               if(flksatexm)then
                 ksatcp(node)  = ksatexm(layer(node))*rfcp(node) + 
     &                      (1.0d0-rfcp(node))*hconode_vsmall
               else
                 ksatcp(node)  = ksatfit(layer(node))*rfcp(node) + 
     &                      (1.0d0-rfcp(node))*hconode_vsmall
               endif

               cofanicp(node) = cofani(layer(node))
               layercp(node) = node
               do level=1,nrlevs
                  if(zfrostbot.lt.zbotdr(level)) then
                     qdra(level,node) = 0.0d0
                     qdrain(level) = 0.0d0
                  endif
               enddo
            enddo

            qdratot = 0.0d0
            do level = 1,nrlevs
               qdratot = qdratot + qdrain(level)
            end do

            if(abs(qdratot).lt.1.0d-6  .and. 
     &                            zfrostbot.lt.zbotdr(leveldeepest))then
               qbot = 0.0d0
            else
               do level = 1,nrlevs
                  qdrain(level) = qdrain(level)/qdratot * qbot
               end do
            end if

            if (swdivd.eq.1) then
               ztop = min(gwl,zfrostbot)
               call divdra (numnod,nrlevs,dz,ksatcp,layercp,cofanicp,
     &                   ztop,L,qdrain,qdra,
     &                   Swdivdinf,Swnrsrf,SwTopnrsrf,Zbotdr,           !  Divdra, infiltration
     &                   dt,FacDpthInf,owltab,t1900)                    !  Divdra, infiltration
            endif
         else

            do level = 1,nrlevs
               qdrain(level) = 0.0d0
               do node = 1,numnod
                  qdra(level,node) = qdra(level,node)*rfcp(node)
                  qdrain(level) = qdrain(level) + qdra(level,node)
               end do
            end do

         endif

         qdrtot = 0.0d0
         do level=1,nrlevs
             qdrtot = qdrtot + qdrain(level)
         end do

      endif


!     write(981,'(i5,3('','',f16.10) , 99('','',f16.6:))') 
!    &      daycum, tcum, t1900, dt, zfrosttop, zfrostbot,gwl,qbot,
!    &      (qdrain(level),level=1,Madr)


      return
      end
