!=======================================================================
!     SUBROUTINE CohortComp calculates the composition 
!       of leaf and stem cohorts
!     Currently used only for PRFRM model
!=======================================================================
      SUBROUTINE CohortComp(DTX, ADF_daily, NDF_daily)

      USE COHORTS_MOD
      USE IPCOHO_MOD
      IMPLICIT NONE
      SAVE

      REAL, INTENT(IN) :: DTX
      REAL, INTENT(OUT) :: ADF_daily, NDF_daily

      INTEGER I
      REAL ADFmass, NDFmass, TotalMass
      REAL, DIMENSION(1:LCMax) :: ADF_Lf_c, NDF_Lf_c
      REAL, DIMENSION(1:LCMax) :: ADF_St_c, NDF_St_c

!-----------------------------------------------------------------------
!     If species parameters are not available, skip quality calcs
      IF (.NOT. CalcQual) THEN
        ADF_daily = -99.
        NDF_daily = -99.
        RETURN
      ENDIF

!-----------------------------------------------------------------------
      ADFmass = 0.0
      NDFmass = 0.0

      DO I = 1, NLC
        IF (LFDM(I) > 0.0) THEN     
          IF (I ==  NLC) THEN
!           Initialize composition in newest cohort from species parameters
            LeafCellFrac(I) = CELLFI
            LeafHemiFrac(I) = HCLLFI
            LeafLigFrac (I) = LIGLFI
          ELSE
!           Adjust leaf composition based on slope and cohort age
            LeafCellFrac(I) = LeafCellFrac(I) + CELLFS * DTX
            LeafHemiFrac(I) = LeafHemiFrac(I) + HCLLFS * DTX
            LeafLigFrac (I) = LeafLigFrac(I)  + LIGLFS * DTX

            LeafCellFrac(I) = MIN(LeafCellFrac(I), CELLFX)
            LeafHemiFrac(I) = MIN(LeafHemiFrac(I), HCLLFX)
            LeafLigFrac (I) = MIN(LeafLigFrac (I), LIGLFX)

            LeafCellFrac(I) = MAX(LeafCellFrac(I), CELLFN)
            LeafHemiFrac(I) = MAX(LeafHemiFrac(I), HCLLFN)
            LeafLigFrac (I) = MAX(LeafLigFrac (I), LIGLFN)
          ENDIF

        ELSE
          LeafCellFrac(I)  = 0.0
          LeafHemiFrac(I)  = 0.0
          LeafLigFrac (I)  = 0.0
        ENDIF

        IF (STDM(I) > 0.0) THEN
          IF (I ==  NLC) THEN
!           Initialize composition in newest cohort from species parameters
            StemCellFrac(I) = CELSTI
            StemHemiFrac(I) = HCLSTI
            StemLigFrac (I) = LIGSTI
          ELSE
!           Adjust stem composition based on slope and cohort age
            StemCellFrac(I) = StemCellFrac(I) + CELSTS * DTX
            StemHemiFrac(I) = StemHemiFrac(I) + HCLSTS * DTX
            StemLigFrac (I) = StemLigFrac (I) + LIGSTS * DTX

            StemCellFrac(I) = MIN(StemCellFrac(I), CELSTX)
            StemHemiFrac(I) = MIN(StemHemiFrac(I), HCLSTX)
            StemLigFrac (I) = MIN(StemLigFrac (I), LIGSTX)

            StemCellFrac(I) = MAX(StemCellFrac(I), CELSTN)
            StemHemiFrac(I) = MAX(StemHemiFrac(I), HCLSTN)
            StemLigFrac (I) = MAX(StemLigFrac (I), LIGSTN)
          ENDIF

        ELSE
          StemCellFrac(I)  = 0.0
          StemHemiFrac(I)  = 0.0
          StemLigFrac (I)  = 0.0
        ENDIF

!       Calculate quality of leaf and stem for each cohort
!       ADF and NDF are in fraction here
        ADF_Lf_c(I) = LeafCellFrac(I) + LeafLigFrac(I)
        NDF_Lf_c(I) = LeafCellFrac(I) + LeafHemiFrac(I) + 
     &    LeafLigFrac(I)

        ADFmass = ADFmass + ADF_Lf_c(I) * LFDM(I) 
        NDFmass = NDFmass + NDF_Lf_c(I) * LFDM(I) 

        ADF_St_c(I) = StemCellFrac(I) + StemLigFrac(I)
        NDF_St_c(I) = StemCellFrac(I) + StemHemiFrac(I) + 
     &    StemLigFrac(I)

        ADFmass = ADFmass + ADF_St_c(I) * STDM(I)
        NDFmass = NDFmass + NDF_St_c(I) * STDM(I)

!       temp chp
!       Print cohort composition over time for selected cohorts.
        select case(i)
        case(1)
          IF (LFDM(I) > 0) THEN
            WRITE(5001,'(2I5,15F10.3)') NLC, I, CohortAge(I),
     &      LFDM(I), STDM(I), 
     &      LeafCellFrac(I), LeafHemiFrac(I), LeafLigFrac (I), 
     &      StemCellFrac(I), StemHemiFrac(I), StemLigFrac (I), 
     &      PCNLeaf(I), PCNStem(I)
          ENDIF
        case(10)
          IF (LFDM(I) > 0) THEN
            WRITE(5010,'(2I5,15F10.3)') NLC, I, CohortAge(I), 
     &      LFDM(I), STDM(I),
     &      LeafCellFrac(I), LeafHemiFrac(I), LeafLigFrac (I), 
     &      StemCellFrac(I), StemHemiFrac(I), StemLigFrac (I),
     &      PCNLeaf(I), PCNStem(I)
          ENDIF
        case(100)
          IF (LFDM(I) > 0) THEN
            WRITE(5100,'(2I5,15F10.3)') NLC, I, CohortAge(I), 
     &      LFDM(I), STDM(I),
     &      LeafCellFrac(I), LeafHemiFrac(I), LeafLigFrac (I), 
     &      StemCellFrac(I), StemHemiFrac(I), StemLigFrac (I),
     &      PCNLeaf(I), PCNStem(I)
          ENDIF
        case(500)
          IF (LFDM(I) > 0) THEN
            WRITE(5500,'(2I5,15F10.3)') NLC, I, CohortAge(I), 
     &      LFDM(I), STDM(I),
     &      LeafCellFrac(I), LeafHemiFrac(I), LeafLigFrac (I), 
     &      StemCellFrac(I), StemHemiFrac(I), StemLigFrac (I),
     &      PCNLeaf(I), PCNStem(I)
         ENDIF
        end select
!       END TEMP CHP

      ENDDO

!     Divide thru by total mass to get weighted average and convert to %
      TotalMass = SUM(LFDM) + SUM(STDM)
      IF (TotalMass > 0.0) THEN
        ADF_daily = ADFmass / TotalMass * 100.
        NDF_daily = NDFmass / TotalMass * 100.
      ELSE
        ADF_daily = 0.
        NDF_daily = 0.
      ENDIF

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE CohortComp
!=======================================================================

!=======================================================================
! Subroutine LossAdjust calculates the adjustment rate for additions to 
!     leaf and stem cohorts due to freeze, pest, and senescence.
! Also, ensures that mass decreases do not exceed mass of leaf or stem cohortS.

!  This subroutine potentially modifies the values of:
!       LFFRZ or STFRZ            !tissue damaged by freeze g/m2
!       LFPST or STPST            !tissue damaged by pest g/m2
!       LeafTotSen or StemTotSen  !total senesced tissue g/m2
!       LFWSSN or STWSSN          !water senescence g/m2
!       LFNMNSN or STNMNSN        !leaf N mining senescence g/m2
!       LFCAD or STCAD            !addition of CH2O to reserves g/m2
!       LFNAD or STNAD            !addition of N to reserved g/m2

      SUBROUTINE LossAdjust (LeafOrStem, MODEL, MassDecrease)

      USE COHORTS_MOD
      IMPLICIT NONE

      CHARACTER (len=4), INTENT(IN) :: LeafOrStem
      CHARACTER (len=8), INTENT(IN) :: MODEL
      REAL, DIMENSION(1:LCMax), INTENT(OUT) :: MassDecrease

      INTEGER I
      REAL Excess, LossFactor, SenFrac
      REAL, DIMENSION(1:LCMax) :: CAdd, Freeze, Mass, NAdd
      REAL, DIMENSION(1:LCMax) :: NMinSen, Pest, TotalSen, WatSen

!-----------------------------------------------------------------------
      SELECT CASE(LeafOrStem)
      CASE ("LEAF")
        Mass     = LFDM
        Freeze   = LFFRZ
        Pest     = LFPST
        TotalSen = LeafTotSen
        WatSen   = LFWSSN
        NMinSen  = LFNMNSN
        CAdd     = LFCAD
        Nadd     = LFNAD

      CASE ("STEM")
        Mass     = STDM
        Freeze   = STFRZ
        Pest     = STPST
        TotalSen = StemTotSen
        WatSen   = STWSSN
        NMinSen  = STNMNSN
        CAdd     = STCAD
        Nadd     = STNAD
      END SELECT

!------------------------------
!     LEAF and STEM LOSSES
!     Ensure that losses don't exceed mass, adjust as necessary
!------------------------------
!     Calculate the loss of tissue per cohort
      MassDecrease = 0.0
      LossFactor = 1.0

      DO I = 1, NLC
!       Leaf or stem mass decrease 
        MassDecrease(I) = Freeze(I) + Pest(I) + TotalSen(I)

!       Check that loss is no greater than cohort mass 
        IF (MassDecrease(I) > Mass(I)) THEN
          MassDecrease(I) = Mass(I)

!         Freeze damage occurs first
          IF (Freeze(I) > Mass(I)) THEN
            Freeze(I) = Mass(I)
            Excess = 0.0
          ELSE
            Excess = Mass(I) - Freeze(I)
          ENDIF

!         Next, pests get their bit
          IF (Pest(I) > Excess) THEN
            Pest(I) = Excess
            Excess = 0.0
          ELSE
            Excess = Excess - Pest(I)
          ENDIF

!         Anything leftover gets taken by senescence
          IF (Excess > 0.0) THEN
            SenFrac = Excess / TotalSen(I)
            TotalSen(I) = Excess
            NMinSen(I) = NMinSen(I) * SenFrac
            WatSen(I) = WatSen(I) * SenFrac
          ELSE
            TotalSen(I) = 0.0
            NMinSen(I) = 0.0
            WatSen(I) = 0.0
          ENDIF
        ENDIF

        SELECT CASE (MODEL(1:5))
        CASE ('CRGRO')
          IF (Mass(I) > 0.0) THEN
            LossFactor = (1. - MIN(1.0, MassDecrease(I) / Mass(I)))
          ELSE
            LossFactor = 0.0
          ENDIF

        CASE ('PRFRM')
          IF (Mass(I) - TotalSen(I) > 0.0) THEN
!           CHP: This is how it's done in for_grow, but I'm not convinced it's correct.
!           If it is correct, we should do the same thing for CRGRO 
            LossFactor = (1. - MIN(1.0, (Pest(I) + Freeze(I)) / 
     &                                  (Mass(I) - TotalSen(I))))
          ELSE
            LossFactor = 0.0
          ENDIF
        END SELECT

        CAdd(I) = CAdd(I) * LossFactor
        Nadd(I) = Nadd(I) * LossFactor
      ENDDO

      SELECT CASE(LeafOrStem)
      CASE ("LEAF")
        LFFRZ = Freeze
        LFPST = Pest
        LeafTotSen = TotalSen
        LFWSSN = WatSen
        LFNMNSN = NMinSen
        LFCAD = CAdd
        LFNAD = Nadd

      CASE ("STEM")
        STFRZ = Freeze
        STPST = Pest
        StemTotSen = TotalSen
        STWSSN = WatSen
        STNMNSN = NMinSen
        STCAD = CAdd
        STNAD = Nadd
      END SELECT
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE LossAdjust
!=======================================================================

!=======================================================================
!     Subroutine COFF calculates daily change in non-structural CH2O 
!     for each leaf and stem cohort

      SUBROUTINE COFF(LeafOrStem, MODEL,
     &  MassDecrease, PCHO_F, RHO, SENC_V,        !Input
     &  WRC_DT)                                   !Output

      USE COHORTS_MOD
      IMPLICIT NONE

      CHARACTER (len=4), INTENT(IN) :: LeafOrStem
      CHARACTER (len=8), INTENT(IN) :: MODEL
      REAL, INTENT(IN) :: PCHO_F, SENC_V
      REAL, DIMENSION(1:LCMax), INTENT(IN) :: MassDecrease, RHO
      REAL, DIMENSION(1:LCMax), INTENT(OUT) :: WRC_DT

      INTEGER I
      REAL C_OFF
      REAL, DIMENSION(1:LCMax) :: CAdd, CMine, Freeze, LitSen, Mass,  
     &  NSC, Pest, S_MDOT, SENWT, WatSen

!-----------------------------------------------------------------------
      SELECT CASE(LeafOrStem)
      CASE ("LEAF")
        Mass     = LFDM
        NSC      = LFNSC !Not used for leaf, only stem (but why?)
        Cadd     = LFCAD
        CMine    = LFCMN
        Freeze   = LFFRZ
        Pest     = LFPST
        WatSen   = LFWSSN
        LitSen   = LTSEN_c
        S_MDOT   = SLMDOT_c
        SENWT    = LFSENWT_c

      CASE ("STEM")
        Mass     = STDM
        NSC      = STNSC
        Cadd     = STCAD
        CMine    = STCMN
        Freeze   = STFRZ
        Pest     = STPST
        WatSen   = STWSSN
        LitSen   = STLTSEN_c
        S_MDOT   = SSMDOT_c
        SENWT    = STSENWT_c
      END SELECT

!     Mobile, non-structural CH2O 
      WRC_DT = 0.0

      DO I = 1, NLC
!       ---------------------------
        IF (Mass(I) .GT. 0.0) THEN
          SELECT CASE (MODEL(1:5))
          CASE ('CRGRO')
            WRC_DT(I) = 
     &        - CMine(I)      !mined CH2O
     &        + CAdd(I)       !new reserves
            SELECT CASE(LeafOrStem)
            CASE ("LEAF")
              WRC_DT(I) = WRC_DT(I) 
     &          - (WatSen(I) + Pest(I) + Freeze(I)) * RHO(I)
             CASE ("STEM")
              WRC_DT(I) = WRC_DT(I) 
     &          - WatSen(I) * RHO(I)   !Water senescence
     &          - NSC(I) / Mass(I) * MassDecrease(I)
             END SELECT

          CASE ('PRFRM')
              C_OFF = (S_MDOT(I) + LitSen(I) + SENWT(I)) * 
     &                (SENC_V * (RHO(I) - PCHO_F) + PCHO_F) 
     &              + (WatSen(I) + Pest(I) + Freeze(I)) * RHO(I)

              IF (C_OFF. LT. 0.0) THEN
                C_OFF = 0.0
              ENDIF
              WRC_DT(I) = -CMine(I) - C_OFF + CAdd(I)

!             TEMP CHP
              IF (LeafOrStem == 'STEM') THEN
                WRITE(3111,'(A,a7,i7,3F15.8)') 
     &            'COHORTS  ',' ',I, C_OFF, CMine(I), CAdd(I)
              ENDIF

          END SELECT
        ENDIF
      ENDDO
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE COFF
!=======================================================================

!=======================================================================
! Subroutine NOFF calculates the net loss of N for leaf and stem
!     cohorts. 

      SUBROUTINE NOFF (
     &  LeafOrStem, MODEL, PRO_F, SENN_V,         !Input
     &  PCN, NTot,                                !Input
     &  N_DOT)                                    !Output

      USE COHORTS_MOD
      IMPLICIT NONE

      CHARACTER (len=4), INTENT(IN) :: LeafOrStem
      CHARACTER (len=8), INTENT(IN) :: MODEL
      REAL, INTENT(IN) :: PRO_F, SENN_V
      REAL, DIMENSION(1:LCMax), INTENT(OUT) :: N_DOT

      INTEGER I
      REAL, DIMENSION(1:LCMax) :: Freeze, LitSen, Mass, N_OFF, 
     &  NAdd, NatSen, NMine, Ntot, PCN, 
     &  Pest, SENWT, TotalSen, WatSen

!-----------------------------------------------------------------------
      SELECT CASE(LeafOrStem)
      CASE ("LEAF")
        Mass     = LFDM
        Freeze   = LFFRZ
        Pest     = LFPST
        TotalSen = LeafTotSen
        WatSen   = LFWSSN
        Nadd     = LFNAD
        SENWT    = LFSENWT_c
        NMine    = LFNMN
        NatSen   = LFNSEN_c
        LitSen   = LTSEN_c

      CASE ("STEM")
        Mass     = STDM
        Freeze   = STFRZ
        Pest     = STPST
        TotalSen = StemTotSen
        WatSen   = STWSSN
        Nadd     = STNAD
        SENWT    = STSENWT_c
        NMine    = STNMN
        NatSen   = SSMDOT_c
        LitSen   = STLTSEN_c
      END SELECT

      N_DOT = 0.0
      N_OFF = 0.0

      DO I = 1, NLC
        IF (Mass(I) > 0.0) THEN
!         ---------------------------
          SELECT CASE (MODEL(1:5))
          CASE ('CRGRO')
!             N loss due to senescence, freeze, pest
              N_OFF(I) = 
     &          + (WatSen(I) + Pest(I) + Freeze(I)) * PCN(I) / 100.
     &          + (TotalSen(I) - WatSen(I)) * PRO_F * 0.16
              N_OFF(I) = MIN(N_OFF(I), NTot(I))

!             Net N gain today for cohort I
              N_DOT(I) = - N_OFF(I) - NMine(I) + Nadd(I) 
              N_DOT(I) = MAX(N_DOT(I), -NTot(I))
          
          CASE ('PRFRM')
            N_OFF(I) = NatSen(I) * 
     &       (SENN_V * (PCN(I)/100. - PRO_F*0.16) + PRO_F*0.16)
     &       + (LitSen(I) + SENWT(I)) * PRO_F *0.16
     &       + (WatSen(I) + Pest(I) + Freeze(I)) * PCN(I)/100.
          
            N_OFF(I) = MIN(N_OFF(I), NTot(I))

!           Net N gain today for cohort I
            N_DOT(I) = - N_OFF(I) - NMine(I) + Nadd(I)
            N_DOT(I) = MAX(N_DOT(I), -NTot(I))
          END SELECT
        ENDIF
      ENDDO

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE NOFF
!=======================================================================
