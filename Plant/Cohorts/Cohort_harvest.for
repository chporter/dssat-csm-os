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

      SUBROUTINE HarvestCohorts(FHLEAF, FHSTEM,   !Input/Output
     &    fhlfn, fhstn, fhcrlf, fhcrst, fhplig         !OUTPUT
     &  )
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
      REAL, INTENT(OUT) :: fhlfn, fhstn, fhcrlf, fhcrst, fhplig

      INTEGER I
      REAL WTLF, STMWT
      REAL ADF_harvest, NDF_harvest, ADFmass, NDFmass, LigHarvested
      REAL, DIMENSION(1:LCMax) :: ADF_Lf_c, NDF_Lf_c
      REAL, DIMENSION(1:LCMax) :: ADF_St_c, NDF_St_c
      REAL, DIMENSION(1:LCMax) :: fhlfn_c, fhstn_c, fhcrlf_c, fhcrst_c
      REAL TotHarvested, FHTOT, TotalRemoved

!-----------------------------------------------------------------------
      TotHarvested = 0.0
      FHLEAF_c = 0.0
      FHSTEM_c = 0.0
      ADF_harvest = 0.0
      NDF_harvest = 0.0
      ADFmass = 0.0
      NDFmass = 0.0
      ADF_Lf_c = 0.0; NDF_Lf_c = 0.0
      ADF_St_c = 0.0; NDF_St_c = 0.0
      ADF_harvest = -99.
      NDF_harvest = -99.
      LigHarvested = 0.0

      IF (FHLEAF > 0.0 .OR. FHSTEM > 0.0) THEN

!!       For initial testing, reduce each cohort by the proportion of whole leaf lost
!!       Eventually, we want to remove new (top) growth
!        WTLF = SUM(LFDM)
!        IF (FHLEAF > 0.0) THEN
!          DO I = 1, NLC
!            FHLEAF_c(I) = FHLEAF * LFDM(I) / WTLF
!            FHLEAF_c(I) = MAX(0.0, FHLEAF_c(I))
!            FHLEAF_c(I) = MIN(LFDM(I), FHLEAF_c(I))
!          ENDDO
!        ENDIF
!
!        STMWT = SUM(STDM)
!        IF (FHSTEM > 0.0) THEN
!          DO I = 1, NLC
!            FHSTEM_c(I) = FHSTEM * STDM(I) / STMWT
!            FHSTEM_c(I) = MAX(0.0, FHSTEM_c(I))
!            FHSTEM_c(I) = MIN(STDM(I), FHSTEM_c(I))
!          ENDDO
!        ENDIF

!       Remove newest leaf cohorts first
        WTLF = SUM(LFDM)
        IF (FHLEAF > 0.0) THEN
          TotalRemoved = 0.0
          DO I = NLC, 1, -1
            TotalRemoved = TotalRemoved + LFDM(I)
            FHLEAF_c(I) = LFDM(I)
            IF (TotalRemoved >= FHLEAF) THEN
              FHLEAF_c(I) = TotalRemoved - FHLEAF
              EXIT
            ELSE
              CYCLE
            ENDIF
          ENDDO
        ENDIF

!       Remove newest stem cohorts first
        STMWT = SUM(STDM)
        IF (FHSTEM > 0.0) THEN
          TotalRemoved = 0.0
          DO I = NLC, 1, -1
            TotalRemoved = TotalRemoved + STDM(I)
            FHSTEM_c(I) = STDM(I)
            IF (TotalRemoved >= FHSTEM) THEN
              FHSTEM_c(I) = TotalRemoved - FHSTEM
              EXIT
            ELSE
              CYCLE
            ENDIF
          ENDDO
        ENDIF

        DO I = 1, NLC
!         Update leaf and stem composition
          LFDM(I) = LFDM(I) - FHLEAF_c(I)
          IF (LFDM(I) > 0.0) THEN
            LFAREA(I) = LFAREA(I) - FHLEAF_c(I) * LFSLA(I)
            LFSLA(I) = LFAREA(I) / LFDM(I)

!           fhcrlf = fhleaf*rhol      !leaf CH2O harvested g/m2
            fhcrlf_c(I) = FHLEAF_c(I) * RHOL_c(I)
            LFNSC(I) = LFNSC(I) - fhcrlf_c(I)

!           fhlfn = fhleaf*pcnl/100.  !harvested leaf N %
            fhlfn_c(I) = FHLEAF_c(I) * PCNLeaf(i)/100.  !harvested leaf N g/m2
            LeafNTot(I) = LeafNTot(I) - fhlfn_c(I)
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

          STDM(I) = STDM(I) - FHSTEM_c(I)
          IF (STDM(I) > 0.0) THEN

!           fhcrst = fhstem*rhos      !stem CH2O harvested g/m2
            fhcrst_c(I) = FHSTEM_c(I) * RHOS_c(I)
            STNSC(I) = STNSC(I) - fhcrst_c(I)

!           fhstn = fhstem*pcnst/100. !harvested stem N %
            fhstn_c(I) = FHSTEM_c(I) * PCNStem(I)/100. !harvested stem N %
            StemNTot(I) = StemNTot(I) - fhstn_c(I)
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
        ENDDO


!===============================================================================
!     repeated code from for_harv.for
!       fhlfn = fhleaf*pcnl/100.  !harvested leaf N g/m2
        fhlfn = sum(fhlfn_c)

!       fhstn = fhstem*pcnst/100. !harvested stem N g/m2
        fhstn = sum(fhstn_c)

!       fhtotn = fhlfn+fhstn      !total N harvested

!       fhcrlf = fhleaf*rhol      !leaf CH2O harvested g/m2
        fhcrlf = sum(fhcrlf_c)

!       fhcrst = fhstem*rhos      !stem CH2O harvested g/m2
        fhcrst = sum(fhcrst_c)

!       fhpctn = fhtotn/fhtot*100 !%N of harvested material
!       fhplig = (fhleaf*pliglf+fhstem*pligst)/fhtot*100  !lignin of harvested material %
!       fhpcho = (fhcrlf+fhcrst)/fhtot*100  !CH2O of harvested material %
!       fhpctlf = fhleaf/fhtot*100          !% leaf in harvested material

!       WTLF = WTLF - FHLEAF      !update leaf mass
!       STMWT = STMWT - FHSTEM    !update stem mass
!       TOPWT = TOPWT - FHLEAF - FHSTEM
!       TOTWT = TOTWT - FHLEAF - FHSTEM

!!     Stopped cohort integration here. Need to coordinate the rest with cohorts module
!
!        WCRLF = WTLF*RHOL         !update leaf carbohydrate
!        WCRST = STMWT*RHOS        !update stem carbohydrate
!        WTNLF = WTLF*PCNL/100.    !update leaf N %
!        WTNST = STMWT*PCNST/100.  !update stem N %
!        WTNCAN = WTNCAN - FHLEAF*PCNL/100. - FHSTEM*PCNST/100.
!
!        IF ((WTLF - WCRLF) .GT. 0.0) THEN !mobile N in leaf
!          WNRLF = MAX (WTNLF - PROLFF*0.16*(WTLF-WCRLF), 0.0)
!        ELSE
!          WNRLF = 0.0
!        ENDIF
!
!        IF ((STMWT - WCRST) .GT. 0.0) THEN  !!mobile N in stem
!          WNRST = MAX (WTNST - PROSTF*0.16*(STMWT-WCRST), 0.0)
!        ELSE
!          WNRST = 0.0
!        ENDIF
!
!        AREALF = WTLF*SLA
!        XLAI = AREALF/10000.
!        XHLAI = XLAI
!!     end repeated code from for_harv.for
!===============================================================================

!       Harvest quality calculations
        IF (CalcQual) THEN
          DO I = 1, NLC
!           Calculate quality of pre-harvested leaf and stem for each cohort
!           ADF and NDF are in fraction here
            IF (FHLEAF_c(I) > 0.0) THEN
              ADF_Lf_c(I) = LeafCellFrac(I) + LeafLigFrac(I)
              NDF_Lf_c(I) = LeafCellFrac(I) + LeafHemiFrac(I) + 
     &          LeafLigFrac(I)
              ADFmass = ADFmass + ADF_Lf_c(I) * FHLEAF_c(I) 
              NDFmass = NDFmass + NDF_Lf_c(I) * FHLEAF_c(I) 
            ENDIF

            IF (FHSTEM_c(I) > 0.0) THEN
              ADF_St_c(I) = StemCellFrac(I) + StemLigFrac(I)
              NDF_St_c(I) = StemCellFrac(I) + StemHemiFrac(I) + 
     &          StemLigFrac(I)
              ADFmass = ADFmass + ADF_St_c(I) * FHSTEM_c(I)
              NDFmass = NDFmass + NDF_St_c(I) * FHSTEM_c(I)
            ENDIF

            TotHarvested = TotHarvested + FHLEAF_c(I) + FHSTEM_c(I)
            LigHarvested = LigHarvested + LeafLigFrac(I) * FHLEAF_c(I) 
     &                                  + StemLigFrac(I) * FHSTEM_c(I)
          ENDDO

!         Divide thru by total mass to get weighted average and convert to %
          ADF_harvest = ADFmass / TotHarvested * 100.
          NDF_harvest = NDFmass / TotHarvested * 100.
          fhplig = LigHarvested / TotHarvested * 100.
        ELSE
!         Quality parameters not available
          ADF_harvest = -99.
          NDF_harvest = -99.
        ENDIF

!       Cohort harvest values may have been adjusted, recalculate totals
        FHLEAF = SUM(FHLEAF_c)
        FHSTEM = SUM(FHSTEM_c)
        FHTOT = FHLEAF + FHSTEM
      ENDIF

      CALL PUT('MHARVEST','ADF', ADF_harvest)
      CALL PUT('MHARVEST','NDF', NDF_harvest)

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE HarvestCohorts
!=======================================================================
