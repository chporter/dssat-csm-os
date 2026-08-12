!=======================================================================
!     SUBROUTINE HarvestCohorts calculates the harvested amounts of leaf 
!       and stem cohorts and the quality of the harvest (ADF and NDF)

!     These variables are potentially modified by this routine
!       LFDM,     STDM     Leaf and stem dry mass g/m2
!       LFNSC,    STNSC    Leaf and stem mobile CH2O g/m2
!       LeafNTot, StemNTot Leaf and stem total N g/m2
!       LFSN,     STSN     Leaf and stem non-mobile N  g/m2
!       LFNSN,    STNSN    Leaf and stem mobile N  g/m2
!       LFAREA             Leaf area 
!       LFSLA              Specific leaf area
!       FHLEAF_c, FHSTEM_c Harvested mass of leaf and stem g/m2

      SUBROUTINE HarvestCohorts(YRDOY,
     &  FHLEAF, FHSTEM)               !Input/Output

!     &  PCNLeaf, PROLFF, RHOL,                    !Input
!     &  PCNStem, PROSTF, RHOS,                    !Input
!     &  LeafNTot, LFSN, LFAREA, LFSLA,            !Input/Output
!     &  StemNTot, STSN,                           !Input/Output
!     &  ADF, NDF)                                 !Output

      USE COHORTS_MOD
      USE IPCOHO_MOD
      USE ModuleData
      IMPLICIT NONE
      SAVE

      REAL, INTENT(INOUT) :: FHLEAF, FHSTEM

!     REAL, DIMENSION(1:LCMax) :: RHOL, RHOS

      INTEGER I, YRDOY
      REAL WTLF, STMWT
      REAL ADF, NDF
      REAL ADF_Leaf, NDF_Leaf
      REAL ADF_Stem, NDF_Stem
      REAL TotHarvested, FHTOT

!     temp chp
      REAL RHOL_calc, RHOS_calc

!-----------------------------------------------------------------------
      TotHarvested = 0.0
      FHLEAF_c = 0.0
      FHSTEM_c = 0.0
      ADF = 0.0
      NDF = 0.0

      IF (FHLEAF > 0.0 .OR. FHSTEM > 0.0) THEN

!       For initial testing, reduce each cohort by the proportion of whole leaf lost
!       Eventually, we want to remove new (top) growth
        WTLF = SUM(LFDM)
        IF (FHLEAF > 0.0) THEN
          DO I = 1, NLC
            FHLEAF_c(I) = FHLEAF * LFDM(I) / WTLF
            FHLEAF_c(I) = MAX(0.0, FHLEAF_c(I))
            FHLEAF_c(I) = MIN(LFDM(I), FHLEAF_c(I))
          ENDDO
        ENDIF

        STMWT = SUM(STDM)
        IF (FHSTEM > 0.0) THEN
          DO I = 1, NLC
            FHSTEM_c(I) = FHSTEM * STDM(I) / STMWT
            FHSTEM_c(I) = MAX(0.0, FHSTEM_c(I))
            FHSTEM_c(I) = MIN(STDM(I), FHSTEM_c(I))
          ENDDO
        ENDIF

!       NEWEST FIRST HARVEST (UNTESTED)
!        WTLF = SUM(LFDM)
!        IF (FHLEAF > 0.0) THEN
!          TotalRemoved = 0.0
!          DO I = NLC, 1, -1
!            TotalRemoved = TotalRemoved + LFDM(I)
!            FHLEAF_c(I) = LFDM(I)
!            IF (TotalRemoved >= FHLEAF) THEN
!              FHLEAF_c(I) = TotalRemoved - FHLEAF
!              EXIT
!            ELSE
!              CYCLE
!            ENDIF
!          ENDDO
        
!!       Remove newest cohorts for mow (UNTESTED)
!        STMWT = SUM(STDM)
!        IF (FHSTEM > 0.0) THEN
!          TotalRemoved = 0.0
!          DO I = NLC, 1, -1
!            TotalRemoved = TotalRemoved + STDM(I)
!            FHSTEM_c(I) = STDM(I)
!            IF (TotalRemoved >= FHSTEM) THEN
!              FHSTEM_c(I) = TotalRemoved - FHSTEM
!              EXIT
!            ELSE
!              CYCLE
!            ENDIF
!          ENDDO
!        ENDIF

        DO I = 1, NLC
!         Calculate quality of pre-harvested leaf and stem for each cohort
!!!!      REPLACE THESE EQUATIONS WITH SOMETHING THAT MAKES SENSE !!!
          ADF_Leaf = (LeafLignin(I)+LeafCellulose(I)+LeafHemicell(I))/3.
          NDF_Leaf = (LeafLignin(I)+LeafCellulose(I)+LeafHemicell(I))/4.

          ADF_Stem = (StemLignin(I)+StemCellulose(I)+StemHemicell(I))/3.
          NDF_Stem = (StemLignin(I)+StemCellulose(I)+StemHemicell(I))/4.

!         ---------------------------------
!         temp chp
          RHOL_calc = SUM(LFNSC) / WTLF
!         ---------------------------------

!         Update leaf and stem composition
          LFDM(I) = LFDM(I) - FHLEAF_c(I)
          IF (LFDM(I) > 0.0) THEN
            LFAREA(I) = LFAREA(I) - FHLEAF_c(I) * LFSLA(I)
            LFSLA(I) = LFAREA(I) / LFDM(I)
            LFNSC(I) = LFNSC(I) - FHLEAF_c(I) * RHOL_c(I)
!           LFNSC(I) = LFNSC(I) - FHLEAF_c(I) * RHOL_calc  !temp chp
            LeafNTot(I) = LeafNTot(I) - FHLEAF_c(I) * PCNLeaf(i)/100.
            LFSN(I) = MIN(LeafNTot(I),PROLFF*0.16 * (LFDM(I) -LFNSC(I)))
            LFNSN(I) = LeafNTot(I) - LFSN(I)
          ELSE
            FHLEAF_c(I) = LFDM(I)
            LFDM(I) = 0.0
            LFAREA(I) = 0.0
            LFSLA(I) = 0.0
            LFNSC(I) = 0.0
            LeafNTot(I) = 0.0
            LFSN(I) = 0.0
            LFNSN(I) = 0.0
          ENDIF

!         ---------------------------------
!         temp chp
!         use average RHOS instead of cohort value
          RHOS_calc = SUM(STNSC) / STMWT
!         ---------------------------------

          STDM(I) = STDM(I) - FHSTEM_c(I)
          IF (STDM(I) > 0.0) THEN
            STNSC(I) = STNSC(I) - FHSTEM_c(I) * RHOS_c(I)
!           STNSC(I) = STNSC(I) - FHSTEM_c(I) * RHOS_calc  !temp chp
            StemNTot(I) = StemNTot(I) - FHSTEM_c(I) * PCNStem(I)/100.
            STSN(I) = MIN(StemNTot(I),PROSTF*0.16 * (STDM(I) -STNSC(I)))
            STNSN(I) = StemNTot(I) - STSN(I)
          ELSE
            FHSTEM_c(I) = STDM(I)
            STDM(I) = 0.0
            STNSC(I) = 0.0
            StemNTot(I) = 0.0
            STSN(I) = 0.0
            STNSN(I) = 0.0
          ENDIF

!         Weighted average of ADF and NDF
          TotHarvested = TotHarvested + FHLEAF_c(I) + FHSTEM_c(I)
          ADF = ADF_Leaf * FHLEAF_c(I) + ADF_Stem * FHSTEM_c(I)
          NDF = NDF_Leaf * FHLEAF_c(I) + NDF_Stem * FHSTEM_c(I)
        ENDDO

!       Divide thru by total mass to get weighted average
        ADF = ADF / TotHarvested * 100.
        NDF = NDF / TotHarvested * 100.

!       Cohort harvest values may have been adjusted, recalculate totals
        FHLEAF = SUM(FHLEAF_c)
        FHSTEM = SUM(FHSTEM_c)
        FHTOT = FHLEAF + FHSTEM

!       temp chp
        WRITE(4353,'(I8, 10F10.2)') 
     &    YRDOY, WTLF, STMWT, FHLEAF, FHSTEM, FHTOT
      ENDIF

      CALL PUT('MHARVEST','ADF', ADF)
      CALL PUT('MHARVEST','NDF', NDF)

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE HarvestCohorts
!=======================================================================
