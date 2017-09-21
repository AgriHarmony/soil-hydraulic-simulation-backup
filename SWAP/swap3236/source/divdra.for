! File VersionID:
!   $Id: divdra.for 186 2010-11-24 12:13:39Z kroes006 $
! ----------------------------------------------------------------------
      SUBROUTINE DIVDRA( NumComp, NumDrain,ThickComp,ksatfit,layer,
     &                   cofani, gwlev, DistDrain, FluxDr,FluxDrComp,
     &                   Swdivdinf,Swnrsrf,SwTopnrsrf,Zbotdr,           !  Divdra, infiltration
     &                   dt,FacDpthInf,owltab,t1900)                    !  Divdra, infiltration
! ----------------------------------------------------------------------
!     Date               : 29/9/99
!     Purpose            : Simulation of lateral waterfluxes in the
!                          saturated zone. In two steps:
!                          1. Calculate bottom-boundaries of
!                             model_discharge_layers;
!                          2. Distribute drainage fluxes over each
!                             model_discharge_layer.
!                          
!     Subroutines called : Lev2Comp                                          
!     Functions called   : -                                     
!     File usage         : -
!     Differences SWAP/SWAPS: None
!     Remarks            : Joop Kroes (July 3,2003): COFANI = Khor/Kvert
! ----------------------------------------------------------------------
      IMPLICIT NONE
      Include 'arrays.fi'

      INTEGER DrainSequence(Madr),Icomp,idr,iidr, i
      INTEGER idum,jdr,NumComWatLev,NumComBotDislay(Madr),NumActDrain
      INTEGER NumDrain,NumComp,LAYER(MACP)
      INTEGER Swdivdinf, Swnrsrf, SwTopnrsrf, NumDrHlp, NumComBotIntflw !  Divdra, infiltration

      real*8  FacAniso,CondSatHorAv,CondSatVerAV,BotDisLay(Madr)
      real*8  ThickCum
      real*8  CumThickCondHor,CumThickCondVer,DistDrain(Madr),Dum1,Dum2
      real*8  FluxDr(Madr),FluxDrComp(Madr,MACP),FlowDrDisch(Madr)
      real*8  HelpFl(Madr)
      real*8  HelpTh,HelpTr,MaxDepthDislay(Madr),CondSatHor(MACP)
      real*8  CondSatVer(MACP),Small,ThickComp(MACP),ThickCompSatWatLev
      real*8  ThickCompBotDislay(Madr),Transmissivity(Madr)
      real*8  WatLevAv,GWLEV
      real*8  COFANI(MAHO), ThickDislay

      real*8  ksatfit(MAHO), Zbotdr(Madr)
      real*8  ThickCompBotIntflw, ThickCompUndIntflw, TransmisIntflw 

      LOGICAL NonExistFluxDr

!   - Declarations Divdra Infiltration
      real*8  dt, FacDpthInf, owltab(Madr,2*maowl), t1900

      integer  NumComDpthInf, NumComSrfLev
      real*8   afgen, CumKD, DpthInflay, FDisInf(Madr), FDisInfmin
      real*8   FluxComWatLev,KD(Macp), RQmax, SurfLev, temptab(2*maowl)
      real*8   ThickCompAbvDpthInf, ThickCompBlwSrfLev
      real*8   ThickCompUnsWatLev, Transmissiv, TransmisSat, TransmisTot
      real*8   TransmisUns

      LOGICAL flDivdInf

      DATA    Small /1.0d-10/

! ----------------------------------------------------------------------
! --- paragraph 1 
! --- initial calculations
!
! --- groundwater level converted to cm below surface level 
      WatLevAv = -1.0d0*MIN(GWLEV,0.0d0)

! --- calculation of CondSatHor and CondSatVer
      do 10 icomp = 1,NumComp
        CondSatHor(icomp) = ksatfit(LAYER(icomp))*COFANI(LAYER(icomp))
        CondSatVer(icomp) = ksatfit(LAYER(icomp))
 10   continue

! --- Initialisation and test whether any drainflux exists
      NonExistFluxDr = .true.
      flDivdInf      = .false.                                          !  Divdra, infiltration
      do idr=1,NumDrain
         if( abs( FluxDr(idr) ) .gt. Small ) NonExistFluxDr = .false.
         do icomp = 1, NumComp
            FluxDrComp(idr,icomp) = 0.0d0
         enddo
         DrainSequence(idr) = 0
!   - infiltration flux exists and Swdivdinf = 1?  Yes, then distribution of 
!     infiltration fluxes is separate from distribution discharge fluxes
         if(Swdivdinf.eq.1 .and. FluxDr(idr).lt.-Small) flDivdInf=.true. !  Divdra, infiltration
      enddo      
      if( NonExistFluxDr ) return

! --- paragraph 2 
! --- Search for compartment with groundwaterlevel: NumComWatLev
! --- saturated part of compartment with waterlevel: ThickCompSatWatLev
      Call Lev2Comp(WatLevAv,ThickComp,         NumComWatLev,
     &              ThickCum,ThickCompUnsWatLev,ThickCompSatWatLev)

! --- paragraph 3 
! --- RobHen 30-09-2004; in case of interflow: the bottom of the highest order 
! --- drainage system (-Zbotdr(NumDrain)) represents max depth of the interflow 
! --- discharge layer and top of all other model_discharge_layers 
      NumDrHlp = NumDrain
!     allow disabling of option
      if( (Swnrsrf.eq.1.or.Swnrsrf.eq.2) .and. Swtopnrsrf.eq.1) then
        NumDrHlp = NumDrain-1
        if ( FluxDr(NumDrain).gt.Small ) then
! --- 3a determine bottom and total transmissivity of the interflow 
! --- discharge layer
           NumComBotIntflw = NumComWatLev    
           TransmisIntflw  = CondSatHor(NumComBotIntflw) *
     &                       ThickCompSatWatLev
           do while (-ZBotDr(NumDrain).gt.ThickCum) 
             NumComBotIntflw = NumComBotIntflw + 1
             ThickCum        = ThickCum + ThickComp(NumComBotIntflw)
             TransmisIntflw  = TransmisIntflw + 
     &                        CondSatHor(NumComBotIntflw) *
     &                        ThickComp(NumComBotIntflw)
           enddo
           ThickCompUndIntflw = ThickCum - (-ZBotDr(NumDrain))
           ThickCompBotIntflw = ThickComp(NumComBotIntflw) - 
     &                        ThickCompUndIntflw
           TransmisIntflw     = TransmisIntflw - 
     &                        CondSatHor(NumComBotIntflw) *
     &                        ThickCompUndIntflw
           if (NumComBotIntflw.eq.NumComWatLev) then
            ThickCompSatWatLev = ThickCompSatWatLev - ThickCompUndIntflw
            ThickCompBotIntflw  = ThickCompSatWatLev
           endif

! --- 3b distribute interflow fluxes as lateral fluxes over compartments
! --- of the interflow discharge layer
           idr = NumDrain
           FluxDrComp(idr,NumComWatLev) = FluxDr(idr) * 
     &      ThickCompSatWatLev*CondSatHor(NumComWatLev) / TransmisIntflw
           do icomp = NumComWatLev+1, NumComBotIntflw-1
            FluxDrComp(idr,icomp) = FluxDr(idr) * ThickComp(icomp) *
     &                              CondSatHor(icomp) / TransmisIntflw
           enddo
           FluxDrComp(idr,NumComBotIntflw) = FluxDr(idr) * 
     &                      ThickCompBotIntflw * 
     &                      CondSatHor(NumComBotIntflw) / TransmisIntflw

! --- 3c reset variables for determinig top of all other model_discharge_layers 
          WatLevAv = -ZBotDr(NumDrain)
          NumComWatLev = NumComBotIntflw
          ThickCompSatWatLev = ThickCompUndIntflw
        endif
! --- no other drainage levels besides interflow -> leave DIVDRA
        if (NumDrHlp.eq.0) return
      endif

! --- paragraph 4
! --- Calculate overall anisotropic factor model profile and 
! --- cumulative transmissivity as a function of depth
      CumThickCondHor = ThickCompSatWatLev * CondSatHor(NumComWatLev)
      CumThickCondVer = ThickCompSatWatLev / CondSatVer(NumComWatLev)
      ThickCum        = ThickCompSatWatLev

      do icomp=NumComWatLev+1,NumComp
         CumThickCondHor = CumThickCondHor + ThickComp(icomp) * 
     &                       CondSatHor(icomp)
         CumThickCondVer = CumThickCondVer + ThickComp(icomp) /
     &                       CondSatVer(icomp)
         ThickCum        = ThickCum + ThickComp(icomp)
      enddo
      
      CondSatHorAv      = CumThickCondHor / ThickCum
      CondSatVerAv      = ThickCum / CumThickCondVer
      FacAniso          = sqrt( CondSatVerAv / CondSatHorAv )

! --- paragraph 5      
! --- Calculate maximum depth per drainage system and discharge 
! --- flow rate per drainage system 
      do idr=1,NumDrHlp
!                                                                       !   Divdra, infiltration
! --- In case of infiltration and switch for seperate infiltration flux distribution
!     on: set factor for adapting depth of discharge layer to depth of infiltration layer
         if (flDivdInf .and. FluxDr(idr).lt.-small) then               
!   - Constrain infiltration depth to minimum = bottom compartment with groundwaterlevel
            FDisInfmin   = ThickCompSatWatLev / 
     &                                  (0.25d0*DistDrain(idr)*FacAniso)
            FDisInf(idr) = Max (FacDpthInf,FDisInfmin)
         else
            FDisInf(idr) = 1.d0
         endif                                                          !  Divdra, infiltration
!
         MaxDepthDislay(idr) = FDisInf(idr) *                           !  Divdra, infiltration
     &                           0.25d0*DistDrain(idr)*FacAniso+WatLevAv
!
! --- Constrain to SWAP profile thickness
         MaxDepthDislay(idr) = MIN (MaxDepthDislay(idr),
     &                              Thickcum+WatLevAv)
      enddo

! --- paragraph 6      
! --- determine sequence of order of drainage systems 

      NumActDrain = 0
      do idr=1,NumDrHlp
         if ( abs(FluxDr(idr)) .gt. small ) then
            NumActDrain = NumActDrain+1
            DrainSequence(NumActDrain) = idr
            HelpFl(NumActDrain)        = FDisInf(idr)*DistDrain(idr)    !  Divdra, infiltration
         end if 
      end do
      
      do idr=1,NumActDrain-1
         do iidr = idr+1,NumActDrain
            if ( HelpFl(idr) .lt. HelpFl(iidr) )then
             dum1                = HelpFl(iidr)
             HelpFl(iidr)        = HelpFl(idr)
             HelpFl(idr)         = dum1
             idum                = DrainSequence(iidr)
             DrainSequence(iidr) = DrainSequence(idr)
             DrainSequence(idr)  = idum
            endif
         enddo
      enddo       

      idr = DrainSequence(NumActDrain)
      FlowDrDisch(idr) = FDisInf(idr)*abs(FluxDr(idr)*DistDrain(idr))   !  Divdra, infiltration
      do iidr=NumActDrain-1,1,-1 
         idr = DrainSequence(iidr)
         jdr = DrainSequence(iidr+1)
         FlowDrDisch(idr) = FlowDrDisch(jdr) + 
     &                      FDisInf(idr)*abs(FluxDr(idr)*DistDrain(idr)) !  Divdra, infiltration
      enddo

! --- paragraph 7
! --- Bottom of 1st order model_discharge_layer

      idr = DrainSequence(1)
      Transmissivity(idr)     = CumThickCondHor
      BotDisLay(idr)          = ThickCum + WatLevAv
      NumComBotDislay(idr)    = NumComp
      ThickCompBotDislay(idr) = ThickComp(NumComp)           
      
! --- correction of BotDisLay(1) if D1 < 0.25 L \/(kv/kh) 
      if (abs(FluxDr(idr)).gt.Small) then
         if (BotDisLay(idr) .gt. MaxDepthDislay(idr) )then
            BotDisLay(idr) = MaxDepthDislay(idr)
! ---       determine adjusted transmissivity and compartment 
! ---       number which contains bottom of discharge layer
            NumComBotDislay(idr) = NumComWatLev
            HelpTh               = ThickCompSatWatLev
            Transmissivity(idr)  = ThickCompSatWatLev * 
     &                             CondSatHor(NumComWatLev)
            ThickDislay = BotDisLay(idr)-WatlevAv
            do while ( ThickDislay .gt. HelpTh )      
               NumComBotDislay(idr) = NumComBotDislay(idr) + 1       
               HelpTh               = HelpTh + 
     &                                ThickComp(NumComBotDislay(idr))
               Transmissivity(idr)  = Transmissivity(idr) + 
     &                                ThickComp(NumComBotDislay(idr)) * 
     &                                CondSatHor(NumComBotDislay(idr))              
            enddo

            Transmissivity(idr) = Transmissivity(idr) - 
     &                          (HelpTh - ThickDislay) * 
     &                          CondSatHor(NumComBotDislay(idr))              
! ---       thickness of part of bottom compartment
            ThickCompBotDislay(idr) = ThickComp(NumComBotDislay(idr)) -
     &                              (HelpTh - ThickDislay)
         endif      
      else 
         BotDisLay(idr) = WatLevAv
      endif

! --- paragraph 8
! --- Bottom of 2nd and higher order model_discharge_layers 
! --- for drainage system of orders 2 to NumDrain (number of drains)         
      do iidr=2,NumActDrain
         idr = DrainSequence(iidr) 
         jdr = DrainSequence(iidr-1)
         Transmissivity(idr) = Transmissivity(jdr) * 
     &                         FlowDrDisch(idr) / FlowDrDisch(jdr) 

! --- bottom of discharge layer of order i
!     and thickness of bottom compartment, and
!     number of bottom compartment
         icomp  = NumComWatLev
         HelpTr = ThickCompSatWatLev * CondSatHor(icomp)
         HelpTh = ThickCompSatWatLev
         do while ( HelpTr .lt. transmissivity(idr) )      
            icomp  = icomp + 1       
            HelpTr = HelpTr + ThickComp(icomp)*CondSatHor(icomp)      
            HelpTh = HelpTh + ThickComp(icomp)       
         enddo
         NumComBotDislay(idr)    = icomp
         ThickCompBotDislay(idr) = ThickComp(icomp) - 
     &             ( HelpTr - Transmissivity(idr)) / CondSatHor(icomp)
            BotDisLay(idr)      = WatLevAv + HelpTh +
     &             ( Transmissivity(idr) - HelpTr) / CondSatHor(icomp)

! --- paragraph 9
!     correction of BotDisLay(idr) if D(idr) < 0.25 L(idr) \/(kv/kh) 
         if (BotDisLay(idr).gt. MaxDepthDislay(idr)) then
            BotDisLay(idr) = MaxDepthDislay(idr)

! --- transmissivity of order i, thickness 
!     of bottom compartment, and number 
!     of bottom compartment
            NumComBotDislay(idr) = NumComWatLev
            HelpTh               = ThickCompSatWatLev
            Transmissivity(idr)  = ThickCompSatWatLev * 
     &                             CondSatHor(NumComWatLev)
            ThickDislay = BotDisLay(idr)-WatlevAv
            do while ( ThickDislay .gt. HelpTh )      
               NumComBotDislay(idr) = NumComBotDislay(idr) + 1       
               HelpTh = HelpTh + ThickComp(NumComBotDislay(idr))       
               Transmissivity(idr)  = Transmissivity(idr) + 
     &                               ThickComp(NumComBotDislay(idr)) * 
     &                               CondSatHor(NumComBotDislay(idr))
            enddo
            Transmissivity(idr) = Transmissivity(idr) - 
     &                            (HelpTh - ThickDislay) * 
     &                            CondSatHor(NumComBotDislay(idr))              
            ThickCompBotDislay(idr) = ThickComp(NumComBotDislay(idr)) -
     &                            (HelpTh - ThickDislay)
         endif   
      enddo

! --- paragraph 10
!     Distribute drainage fluxes as lateral fluxes over 
!     ith order model discharge layers
      do iidr=1,NumActDrain
         idr = DrainSequence(iidr) 

         if (.not.flDivdInf .or. FluxDr(idr).gt.0.d0) then              !  Divdra, infiltration
           FluxDrComp(idr,NumComWatLev)= FluxDr(idr)*ThickCompSatWatLev*
     &                               CondSatHor(NumComWatLev) / 
     &                               Transmissivity(idr)
           do icomp=NumComWatLev+1,NumComBotDislay(idr)-1
              FluxDrComp(idr,icomp) = FluxDr(idr)*ThickComp(icomp)*
     &                                CondSatHor(icomp) / 
     &                                Transmissivity(idr)
           enddo
           FluxDrComp(idr,NumComBotDislay(idr)) = FluxDr(idr)*
     &                                ThickCompBotDislay(idr)*
     &                                CondSatHor(NumComBotDislay(idr)) / 
     &                                Transmissivity(idr)
           if (NumComWatLev .eq. NumComBotDislay(idr))
     &         FluxDrComp(idr,NumComBotDislay(idr)) = FluxDr(idr)
         endif
      enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Divdra, Infiltration !!!!!!!!!!!!!!!!!!!!!!!!
!
!     If flDivdInf = true, then distribution of infiltration fluxes is separate
!     from distribution discharge fluxes. 
      if (flDivdInf) then    

        do idr = 1, NumActDrain
           if (FluxDr(idr).lt.-small) then                             
                              
! --- Find surface water level
!     - first copy to 1-dimensional table temptab
              do i = 1,2*maowl 
                 temptab(i) = owltab(idr,i)
              end do
              SurfLev = afgen(temptab,2*maowl,t1900+dt-1.d0)
! --- Search for compartment with surface water level: NumComSrfLev
!   - part of NumComSrfLev below SurfLev
              call Lev2Comp(-1.0d0*Min(SurfLev,0.0d0),ThickComp, 
     &                      NumComSrfLev,Dum1,Dum2,ThickCompBlwSrfLev)

! --- Depth of Infiltration layer: DepthInflay
              DpthInflay = MaxDepthDislay(idr)
! --- Search for compartment with depth Infiltration layer: NumComDepthInf
!   - part of NumComDepthInf Above DepthInflay
              call Lev2Comp(DpthInflay,ThickComp, 
     &                      NumComDpthInf,Dum1,ThickCompAbvDpthInf,Dum2)
! --- Special case: DpthInflay = bottom compartment with groundwaterlevel
              if (NumComDpthInf.eq.NumComWatLev) then
                 ThickCompAbvDpthInf = ThickCompSatWatLev
              endif

! --- Determine transmissivity of total infiltration zone, of its 
!     unsaturated part and saturated part, and of the compartments within
              KD(NumComDpthInf) = ThickCompAbvDpthInf * 
     &                            CondSatHor(NumComDpthInf)
              TransmisTot = KD(NumComDpthInf)
              TransmisSat = KD(NumComDpthInf)
              do icomp = NumComDpthInf-1, NumComSrfLev+1, -1
                 KD(icomp) = ThickComp(icomp) * CondSatHor(icomp)
                 TransmisTot = TransmisTot + KD(icomp)
                 if (icomp.gt.NumComWatLev) then
                    TransmisSat = TransmisSat + KD(icomp)
                 endif 
              enddo
              KD(NumComSrfLev) = ThickCompBlwSrfLev * 
     &                           CondSatHor(NumComSrfLev)
              if (NumComDpthInf.eq.NumComWatLev) then
!   - special case: DpthInflay = bottom compartment with groundwaterlevel
                 if (NumComSrfLev.eq.NumComWatLev) then
                    KD(NumComSrfLev) = KD(NumComSrfLev) - TransmisTot
                 else
                    TransmisTot = TransmisTot + ThickCompUnsWatLev * 
     &                            CondSatHor(NumComWatLev)
                 endif
              endif
!
              TransmisTot = TransmisTot + KD(NumComSrfLev)
              KD(NumComWatLev) = ThickCompSatWatLev *
     &                           CondSatHor(NumComWatLev)
              if (NumComDpthInf.gt.NumComWatLev) TransmisSat = 
     &                                    TransmisSat + KD(NumComWatLev)
              TransmisUns = TransmisTot - TransmisSat

! --- RQmax = max flux per unit of transmissivity
              RQmax = 2.d0*FluxDr(idr) / TransmisTot

! --- Distribute infiltration fluxes as lateral fluxes over unsaturated compartments
              if (NumComSrfLev.eq.NumComWatLev) then
                 FluxComWatLev = RQmax *
     &               0.5d0 * (ThickCompBlwSrfLev - ThickCompSatWatLev) *
     &               CondSatHor(NumComWatLev) 
              else
                 CumKD = 0.d0
                 do icomp = NumComSrfLev, NumComWatLev-1
                    FluxDrComp(idr,icomp) = RQmax / TransmisUns * 0.5d0* 
     &                         ( (CumKD+KD(icomp))**2.d0 - CumKD**2.d0 )
                    CumKD = CumKD + KD(icomp)
                 enddo
                 FluxComWatLev = RQmax / TransmisUns * 0.5d0 * ( (CumKD+
     &              ThickCompUnsWatLev*CondSatHor(NumComWatLev))**2.d0 - 
     &                                                     CumKD**2.d0 )
              endif

! --- Distribute infiltration fluxes as lateral fluxes over saturated compartments
              CumKD = 0.d0
              do icomp = NumComDpthInf, NumComWatLev, -1
                 FluxDrComp(idr,icomp) = RQmax / TransmisSat *
     &                 0.5d0 * ( (CumKD+KD(icomp))**2.d0 - CumKD**2.d0 )
                 CumKD = CumKD + KD(icomp)
              enddo
! --- Add unsaturated flux to infiltration flux of compartment containing groundwater level 
              FluxDrComp(idr,NumComWatLev) = 
     &                      FluxDrComp(idr,NumComWatLev) + FluxComWatLev

           endif                                                      
        enddo

      endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Divdra, Infiltration !!!!!!!!!!!!!!!!!!!!!!!!

      return
      end

! File VersionID:
!   $Id: divdra.for 186 2010-11-24 12:13:39Z kroes006 $
! ----------------------------------------------------------------------
      SUBROUTINE Lev2Comp(Level,ThickComp,         NumCom2Lev, 
     &                    ThickCum,ThickCompAbvLev,ThickCompBlwLev)

! ----------------------------------------------------------------------
!     Date               : 29/10/10
!     Purpose            : Find Compartment with Level and its part
!                          Above and Under this Level  
!     Subroutines called : -                                           
!     Functions called   : -                                     
!     File usage         : -
! ----------------------------------------------------------------------
      IMPLICIT NONE
      Include 'arrays.fi'

      integer NumCom2Lev

      real*8  Level, ThickComp(Macp), ThickCompAbvLev, ThickCompBlwLev
      real*8  ThickCum

! --- Find number Compartment containing Level
      NumCom2Lev = 1
      ThickCum = ThickComp(1)
      do while (Level.gt.ThickCum+1.d-10)
         NumCom2Lev = NumCom2Lev + 1
         ThickCum   = ThickCum + ThickComp(NumCom2Lev)
      enddo
! --- part of compartment Below level
      ThickCompBlwLev = ThickCum - Level

! --- part of compartment Above level
      ThickCompAbvLev = ThickComp(NumCom2Lev) - ThickCompBlwLev

      return
      end