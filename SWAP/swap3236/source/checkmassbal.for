! File VersionID:
!   $Id: checkmassbal.for 162 2009-08-13 15:47:18Z kroes006 $
! ----------------------------------------------------------------------
      subroutine checkmassbal (DayCum,nrlevs,NumNodNew,
     &              SwMacro,outfil,pathwork,FlOpenFileDev,DZNew,
     &              CritDevMasBal,ievap,igird,igrai,
     &              igSnow,inird,inrai,inqdraNew,IQExcMtxDm1CpNew,
     &              IQExcMtxDm2CpNew,inqNew,IQOutDrRapCpNew,inqrotNew,
     &              IPondBeg,IQInTopPreDm1,IQInTopLatDm1,IQInTopPreDm2,
     &              IQInTopLatDm2,ISsnowBeg,IThetaBegNew,
     &              iruno,irunon,isnrai,iSubl,pond,Ssnow,thetaNew,
     &              IWaSrDm1Beg,IWaSrDm2Beg,WaSrDm1,WaSrDm2)
! ----------------------------------------------------------------------
!     Date               : 26-jun-2003
!     Purpose            : Checking of mass balance per period OutPer for
!                          ANIMO/PEARL output
!     Subroutines called : -              
!     Functions called   : -
!     File usage         : outfil
! ---------------------------------------------------------------------
      implicit none
      include 'arrays.fi'
! -   global
      integer DayCum, nrlevs, NumNodNew, SwMacro
      real*8  CritDevMasBal, DZNew(MaCp)
      real*8  ievap, igird, igrai, igSnow, inird,inrai
      real*8  inqdraNew(Madr,macp)
      real*8  IQExcMtxDm1CpNew(macp), IQExcMtxDm2CpNew(macp)
      real*8  inqNew(macp+1), IQOutDrRapCpNew(macp), inqrotNew(macp)
      real*8  IPondBeg, IQInTopPreDm1, IQInTopLatDm1, IQInTopPreDm2
      real*8  IQInTopLatDm2, ISsnowBeg, IThetaBegNew(MaCp), iruno,irunon
      real*8  isnrai, iSubl, pond, Ssnow, thetaNew(macp)
      real*8  IWaSrDm1Beg, IWaSrDm2Beg, WaSrDm1, WaSrDm2
      character outfil*(*),pathwork*(*)
      logical FlOpenFileDev

! -   local
      integer Dev, Level, getun, ic
      real*8  DevMasBalDm1,DevMasBalDm2, DevMasBalCmp(MaCp)
      real*8  DevMasBalPnd, DevMasBalPrf, IQExcMtxDm1
      real*8  IQExcMtxDm2,IQInTopLatDm,  IQInTopPreDm, IQOutDrRap
      real*8  Qdra(MaCp), QdraPrf, QrotPrf, SrDif
      real*8  WaSr(MaCp), WaSrBeg(MaCp), WaSrPrf, WaSrPrfBeg  
      character filnam*300
      logical FlWriteDevCmp(MaCp), FlWriteDev, FlWriteDevDm1 
      logical FlWriteDevDm2, FlWriteDevPnd, FlWriteDevPrf
      
!     save values of locals
      save    dev

! ----------------------------------------------------------------------
! --- Checking of mass balances of sub systems per period OutPer
      FlWriteDev= .false.
      FlWriteDevPnd = .false.
      FlWriteDevPrf = .false.
      do ic= 1, NumNodNew
         FlWriteDevCmp(ic) = .false.
      enddo
      FlWriteDevDm1 = .false.
      FlWriteDevDm2 = .false.

!   - 1) Ponding layer
      SrDif = IPondBeg-Pond + ISsnowBeg-Ssnow
      IQInTopPreDm= 0.d0
      IQInTopLatDm= 0.d0
      if (Swmacro.eq.1) then
         IQInTopPreDm= IQInTopPreDm1 + IQInTopPreDm2
         IQInTopLatDm= IQInTopLatDm1 + IQInTopLatDm2
      endif

!     - Deviation mass balance Ponding layer in cm
      DevMasBalPnd = igrai + igsnow + igird + irunon + inqNew(1) + SrDif
     &             - (igrai-inrai-isnrai + igird-inird + isubl + ievap + 
     &                iruno)- IQInTopPreDm - IQInTopLatDm
!     &             - (igrai-inrai-isnrai + igird-inird + isubl + ievap + iruno)
!     &             - IQInTopPreDm - IQInTopLatDm

!     - Check mass balance against criteria
      if (abs(DevMasBalPnd).gt.CritDevMasBal) then
         FlWriteDev = .true.
         FlWriteDevPnd = .true.
      endif

!   - 2) Total Soil Profile
      WaSrPrfBeg = 0.d0 
      WaSrPrf    = 0.d0 
      QrotPrf    = 0.d0 
      QdraPrf    = 0.d0 
      IQExcMtxDm1= 0.d0
      IQExcMtxDm2= 0.d0
      do ic = 1, numnodnew
         WaSrPrfBeg= WaSrPrfBeg + dzNew(ic)*IThetaBegNew(ic)
         WaSrPrf   = WaSrPrf    + dzNew(ic)*ThetaNew(ic)
         QrotPrf   = QrotPrf    + inqrotNew(ic)
         do level=1,nrlevs
            QdraPrf = QdraPrf + InqdraNew(level,ic)
         enddo
         if (Swmacro.eq.1) then
            IQExcMtxDm1= IQExcMtxDm1 + IQExcMtxDm1CpNew(ic)
            IQExcMtxDm2= IQExcMtxDm2 + IQExcMtxDm2CpNew(ic)
         endif
      enddo
      SrDif= WaSrPrfBeg - WaSrPrf

!     - Deviation mass balance total Profile in cm
      DevMasBalPrf = inqNew(NumNodNew+1) + SrDif + IQExcMtxDm1 + 
     &               IQExcMtxDm2 - (inqNew(1) + QrotPrf + QdraPrf) 

!     - Check mass balance against criteria
      if (abs(DevMasBalPrf).gt.CritDevMasBal) then
         FlWriteDev = .true.
         FlWriteDevPrf = .true.
      endif

!   - 3) Individual Soil Compartments
      do 100 ic = 1, numnodnew
         SrDif = 0.d0
         Qdra(ic)= 0.d0 
         WaSrBeg(ic)= dzNew(ic) * IThetaBegNew(ic)
         WaSr(ic)   = dzNew(ic) * ThetaNew(ic)
         SrDif = WaSrBeg(ic) - WaSr(ic) 
         do level=1,nrlevs
            Qdra(ic) = Qdra(ic) + inqdraNew(level,ic)
         enddo

!     - Deviation mass balance Soil Compartments in cm
         DevMasBalCmp(ic) = inqNew(ic+1) + SrDif
     &                    - (inqNew(ic) + inqrotNew(ic) + Qdra(ic))
         if (Swmacro.eq.1) DevMasBalCmp(ic) = DevMasBalCmp(ic) + 
     &                     IQExcMtxDm1CpNew(ic) + IQExcMtxDm2CpNew(ic)

!     - Check mass balance against criteria
         if (abs(DevMasBalCmp(ic)).gt.CritDevMasBal) then
            FlWriteDev = .true.
            FlWriteDevCmp(ic) = .true.
         endif
 100  continue

!   - 4) Macropore domains Dm1 and Dm2
      if (SwMacro.eq.1) then
         IQOutDrRap= 0.d0
         do ic = 1, numnodnew
            IQOutDrRap= IQOutDrRap + IQOutDrRapCpNew(ic)
         enddo

!     - Deviation mass balance Macropore Domains in cm
         SrDif = IWaSrDm1Beg - WaSrDm1
         DevMasBalDm1= IQInTopPreDm1 + IQInTopLatDm1 + SrDif -
     &                 (IQExcMtxDm1  + IQOutDrRap)

         SrDif = IWaSrDm2Beg - WaSrDm2
         DevMasBalDm2= IQInTopPreDm2 + IQInTopLatDm2 + SrDif -
     &                 IQExcMtxDm2

!     - Check mass balance against criteria
         if (abs(DevMasBalDm1).gt.CritDevMasBal) then
            FlWriteDev = .true.
            FlWriteDevDm1 = .true.
         endif
         if (abs(DevMasBalDm2).gt.CritDevMasBal) then
            FlWriteDev = .true.
            FlWriteDevDm2 = .true.
         endif
      endif

!
! --- In case of deviations of mass balance open file 'xxxxx.dwb.csv'

      if (FlWriteDev .and. .not.FlOpenFileDev) then
         filnam = trim(pathwork)//trim(outfil)//'.dwb'
         dev = getun (20,90)
         call fopens(dev,filnam,'new','del')
         write(dev,1)
         if (SwMacro.eq.1) write(dev,2)
         FlOpenFileDev = .true.
      endif

!   - Write deviations of water balance Top system
      if (FlWriteDevPnd) write(dev,3) daycum, DevMasBalPnd, 
     &    igrai, igsnow, igird, irunon, isnrai, igrai-inrai,igird-inird,
     &    isubl,ievap, iruno, inqNew(1), Pond, IPondBeg, Ssnow, 
     &    ISsnowBeg,IQInTopPreDm, IQInTopLatDm

!   - Write deviations of water balance whole Profile
      if (FlWriteDevPrf) write(dev,4) daycum, DevMasBalPrf, 
     &    inqNew(1), inqNew(NumNodNew+1), QrotPrf, QdraPrf, WaSrPrf, 
     &    WaSrPrfBeg, IQExcMtxDm1, IQExcMtxDm2

!   - Write deviations of water balance of Individual Soil Compartments
      do ic= 1, numnodnew
         if (FlWriteDevCmp(ic)) write(dev,5) daycum,ic,DevMasBalCmp(ic),
     &      inqNew(ic), inqNew(ic+1), inqrotNew(ic), Qdra(ic), WaSr(ic),
     &      WaSrBeg(ic), IQExcMtxDm1CpNew(ic), IQExcMtxDm2CpNew(ic)
      enddo

!   - Write deviations of water balance Macropore Domains
      if (FlWriteDevDm1) write(dev,6) daycum, DevMasBalDm1,
     &   IQInTopPreDm1, IQInTopLatDm1, IQExcMtxDm1, WaSrDm1, 
     &   IWaSrDm1Beg, IQOutDrRap
      if (FlWriteDevDm2) write(dev,7) daycum, DevMasBalDm2,
     &   IQInTopPreDm2, IQInTopLatDm2, IQExcMtxDm2, WaSrDm2, 
     &   IWaSrDm2Beg
!
    1 format(' DEVIATIONS WATERBALANCE for different subsystems: 1. Pond
     &.layer; 2. Whole profile; 3. Compartment; (optional: Macrop. Dom.:
     & 4. Dom1; 5. Dom2)',/,
     &' Relevant terms of waterbalance per subsystem',
     &' (all terms in cm):',//,
     &' DayCum, 1. PONDLAY., DevMasBalAbs, IgRai, IgSnow, IgIrd, IRunon, 
     & SnowFall,IntcpRai, IntcpIrd, ISubl, IEvap, IRuno, InQTop,
     & Pond, IPondBeg, Ssnow, ISsnowBeg, IQInTopPreDm, IQInTopLatDm',/,
     &' , 2. PROFILE, DevMasBalPrf, InQTop, InQBot, QrotPrf, QdraPrf,
     & WaSrPrf, WaSrPrfBeg, InQExcMtxDm1, InQExcMtxDm2',/,
     &' , 3. COMPno, DevMasBalCmp, InQNew(top), InQNew(bot),
     & InQrotNew, Qdra, WaSr, WaSrBeg, InQExcMtxDm1CpNew, 
     & InQExcMtxDm2CpNew')
    2 format(' , 4. MPDOM1, DevMasBalDm1, IQInTopPreDm1,
     & IQInTopLatDm1, InQExcMtxDm1, WaSrDm1, IWaSrDm1Beg, InQOutDrRap'/
     &' , 5. MPDOM2, DevMasBalDm2, IQInTopPreDm2, IQInTopLatDm2,
     & InQExcMtxDm2, WaSrDm2, IWaSrDm2Beg')
    3 format(i5,',',' Pondlay. : ',18(',',f12.8))
    4 format(i5,',',' Profile : ',9(',',f12.8))
    5 format(i5,',',' Comp',i3,': ',9(',',f12.8))
    6 format(i5,',',' MpDom1 : ',7(',',f12.8))
    7 format(i5,',',' MpDom2 : ',6(',',f12.8))

      return
      end
