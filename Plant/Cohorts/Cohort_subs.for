!=======================================================================
!     SUBROUTINE CohortComp calculates the composition 
!       of leaf and stem cohorts
      SUBROUTINE CohortComp()

      USE COHORTS_MOD
      IMPLICIT NONE
      SAVE

      INTEGER I

!-----------------------------------------------------------------------
!     Values in g/m2
      DO I = 1, NLC
!!!!    REPLACE THESE EQUATIONS WITH SOMETHING THAT MAKES SENSE !!!
        LeafLignin(I)    = LFDM(I) * 0.25
        LeafCellulose(I) = LFDM(I) * 0.25
        LeafHemicell(I)  = LFDM(I) * 0.25
        StemLignin(I)    = STDM(I) * 0.25
        StemCellulose(I) = STDM(I) * 0.25
        StemHemicell(I)  = STDM(I) * 0.25
      ENDDO

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

!     temp chp
      REAL RHO_calc, NSC_calc, Mass_calc

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

!     ---------------------------------
!     temp chp
!     use average RHO instead of cohort value
      NSC_calc = SUM(NSC)
      Mass_calc = SUM(Mass)
!     RHOS(I) = STNSC(I) / STDM(I)
      if (Mass_calc > 0.0) then
        RHO_calc = NSC_calc / Mass_calc
      else
        RHO_calc = 0.0
      endif
!     ---------------------------------

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
!    &          - (WatSen(I) + Pest(I) + Freeze(I)) * RHO_calc !temp chp
             CASE ("STEM")
              WRC_DT(I) = WRC_DT(I) 
     &          - WatSen(I) * RHO(I)   !Water senescence
!    &          - WatSen(I) * RHO_calc !temp chp   !Water senescence
     &          - NSC(I) / Mass(I) * MassDecrease(I)
             END SELECT

          CASE ('PRFRM')
              C_OFF = (S_MDOT(I) + LitSen(I) + SENWT(I)) * 
     &                (SENC_V * (RHO(I) - PCHO_F) + PCHO_F) 
!    &                (SENC_V * (RHO_calc - PCHO_F) + PCHO_F) !temp chp
     &              + (WatSen(I) + Pest(I) + Freeze(I)) * RHO(I)
!    &              + (WatSen(I) + Pest(I) + Freeze(I)) * RHO_calc !temp chp

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
