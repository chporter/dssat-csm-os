!=======================================================================
!     SUBROUTINE HarvestCohorts calculates the harvested amounts of leaf 
!       and stem cohorts and the quality of the harvest (ADF and NDF)

!     Called from the for_harv.for subroutine and only used for PRFRM model.

!     These variables are potentially modified by this routine
!       LFDM,     STDM     Leaf and stem dry mass g/m2
!       LFNSC,    STNSC    Leaf and stem mobile CH2O g/m2
!       LeafNTot, StemNTot Leaf and stem total N g/m2
!       LFSN,     STSN     Leaf and stem non-mobile N  g/m2
!       LFNSN,    STNSN    Leaf and stem mobile N  g/m2
!       LFAREA             Leaf area 
!       LFSLA              Specific leaf area
!       FHLEAF_c, FHSTEM_c Harvested mass of leaf and stem g/m2

      SUBROUTINE HarvestCohorts(FHLEAF, FHSTEM)   !Input/Output

!     These variables are available throught USE statements
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

      INTEGER I
      REAL WTLF, STMWT
      REAL ADF, NDF, ADFmass, NDFmass
      REAL, DIMENSION(1:LCMax) :: ADF_Lf_c, NDF_Lf_c
      REAL, DIMENSION(1:LCMax) :: ADF_St_c, NDF_St_c
      REAL TotHarvested, FHTOT

!     temp chp
      REAL RHOL_calc, RHOS_calc

!-----------------------------------------------------------------------
      TotHarvested = 0.0
      FHLEAF_c = 0.0
      FHSTEM_c = 0.0
      ADF = 0.0
      NDF = 0.0
      ADFmass = 0.0
      NDFmass = 0.0
      ADF_Lf_c = 0.0; NDF_Lf_c = 0.0
      ADF_St_c = 0.0; NDF_St_c = 0.0

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

!!       Remove newest leaf cohorts first (UNTESTED)
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

!!       Remove newest stem cohorts first (UNTESTED)
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

!         Calculate quality of pre-harvested leaf and stem for each cohort
!         ADF and NDF are in fraction here
          IF (FHLEAF_c(I) > 0.0) THEN
            ADF_Lf_c(I) = LeafCellFrac(I) + LeafLigFrac(I)
            NDF_Lf_c(I) = LeafCellFrac(I) + LeafHemiFrac(I) + 
     &        LeafLigFrac(I)

            ADFmass = ADFmass + ADF_Lf_c(I) * FHLEAF_c(I) 
            NDFmass = NDFmass + NDF_Lf_c(I) * FHLEAF_c(I) 
          ENDIF

          IF (FHSTEM_c(I) > 0.0) THEN
            ADF_St_c(I) = StemCellFrac(I) + StemLigFrac(I)
            NDF_St_c(I) = StemCellFrac(I) + StemHemiFrac(I) + 
     &        StemLigFrac(I)

            ADFmass = ADFmass + ADF_St_c(I) * FHSTEM_c(I)
            NDFmass = NDFmass + NDF_St_c(I) * FHSTEM_c(I)
          ENDIF

          TotHarvested = TotHarvested + FHLEAF_c(I) + FHSTEM_c(I)
        ENDDO

!       Divide thru by total mass to get weighted average and convert to %
        ADF = ADFmass / TotHarvested * 100.
        NDF = NDFmass / TotHarvested * 100.

!       Cohort harvest values may have been adjusted, recalculate totals
        FHLEAF = SUM(FHLEAF_c)
        FHSTEM = SUM(FHSTEM_c)
        FHTOT = FHLEAF + FHSTEM
      ENDIF

      CALL PUT('MHARVEST','ADF', ADF)
      CALL PUT('MHARVEST','NDF', NDF)

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE HarvestCohorts
!=======================================================================
