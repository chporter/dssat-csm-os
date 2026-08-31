!=======================================================================
      MODULE COHORTS_MOD
!=======================================================================
      INTEGER, PARAMETER :: LCMax = 10000 !max # of leaf/stem cohorts
      INTEGER NLC     !current number of leaf (and stem) cohorts

!     STATE VARIABLES: Leaf and stem cohorts
      REAL, DIMENSION(LCMax) ::  
     &  CohortAge     !Leaf/stem age for (thermal days)

!     STATE VARIABLES: Leaf cohorts
      REAL, DIMENSION(LCMax) ::  
     &  LFDM,         !Leaf dry matter (g[leaf]/m2) = WTLF
     &  CumLeafDM,    !Cumulative leaf growth (g[leaf]/m2) = CLW
     &  LFNSC,        !Leaf non-structural (mobile) CH2O (g/m2) = WCRLF
     &  LeafNTot,     !Leaf total N g/m2
     &  LFSN,         !Leaf structural N g/m2
     &  LFNSN,        !Leaf non-structural (mobile) N (g/m2) = WNRLF
     &  PCNLeaf,      !Leaf N%
     &  LFAREA,       !Leaf area (m2)
     &  LFSLA,        !Leaf SLA
     &  RHOL_c        !Fraction of mobile CH2O in leaf

!     STATE VARIABLES: Stem cohorts
      REAL, DIMENSION(LCMax) ::  
     &  STDM,         !Stem dry matter (g[stem]/m2) = WTLF
     &  STNSC,        !Stem non-structural (mobile) CH2O (g/m2) = WCRLF
     &  StemNTot,     !Stem total N g/m2
     &  STSN,         !Stem structural N g/m2
     &  STNSN,        !Stem non-structural (mobile) N (g/m2) = WNRLF
     &  PCNStem,      !Stem N%
     &  RHOS_c        !Fraction of mobile CH2O in stem

!     RATE VARIABLES: Leaf and stem cohorts
      REAL, DIMENSION(LCMax) ::  
!       calculated in FREEZE, for_freeze
     &  LFFRZ,        !leaf mass frozen today (g[leaf]/m2) = WLFDOT
     &  STFRZ,        !stem mass frozen today (g[stem]/m2) = WSFDOT

!       calculated in VEGGR, for_veggr
     &  LFCMN,        !leaf non-struc CH2O mined (g[CH2O]/m2) = CRUSLF
     &  STCMN,        !Stem non-struc CH2O mined (g[CH2O]/m2) = CRUSLF

!       Calculated LeafCohortPest
     &  LFPST,        !leaf pest damage today (g[leaf]/m2) = WLIDOT
     &  STPST,        !Stem pest damage today (g[stem]/m2) = WLIDOT

!       calculated in MOBIL, for_mobil, for_veggr
     &  LFNMN,        !Leaf non-struc N mined today (g[N]]/m2) = NRUSLF
     &  STNMN,        !Stem non-struc N mined today (g[N]]/m2) = NRUSLF

!       calculated in SENES, for_senmob
     &  LeafTotSen,   !Total leaf senescense today (g[leaf]/m2) = SLDOT
     &  LFNMNSN,      !Leaf senescence due to N mining (g[leaf]/m2)
     &  LFWSSN,       !leaf water stress senescence today (g[leaf]/m2)

     &  StemTotSen,   !Total Stem senescense today (g[stem]/m2) = SSDOT
     &  STNMNSN,      !Stem senescence due to N mining (g[stem]/m2)
     &  STWSSN,       !Stem water stress senescence today (g[stem]/m2)

!       calculated in GROW and for_grow, adjusted in COHORTS
     &  LFCAD,        !leaf non-struc CH2O stored  (g[CH2O]/m2) = CADLF
     &  LFNAD,        !Leaf non-struc N stored today (g[N]]/m2) = NADLF

     &  STCAD,        !Stem non-struc CH2O stored  (g[CH2O]/m2) = CADST
     &  STNAD,        !Stem non-struc N stored today (g[N]]/m2) = NADST

!       calculated in for_senmob
     &  LFCMINE_c,    !Max potential CH2O mining today
     &  LFSNMOB_c,    !Leaf N mobilized by natural senescence (g[N]/m2)
     &  LTSEN_c,      !Low light senescence
!       is LFSENWT_c the same as LFNMNSN?
     &  LFSENWT_c,    !Leaf senescence due to N mobilization
     &  LFNSEN_c,     !natural senescence
     &  SLMDOT_c,     !Leaf senescence with N mobilization 

     &  STCMINE_c,    !Max potential CH2O mining today
     &  STSNMOB_c,    !Stem N mobilized by natural senescence (g[N]/m2)
!       is LFSENWT_c the same as STNMNSN?
     &  STSENWT_c,    !Stem senescence due to N mobilization
     &  STNSEN_c,     !natural senescence
     &  SSMDOT_c,     !Stem senescence with N mobilization 
     &  STLTSEN_c,    !Stem low light senescence

!       calculated in for_harv
     &  FHLEAF_c,     !Forage harvest
     &  FHSTEM_c      !Forage harvest

!     Cohort composition - Values in g/m2
      REAL, DIMENSION(1:LCMax) :: 
     &  LeafLignin, LeafCellulose, LeafHemicell,
     &  StemLignin, StemCellulose, StemHemicell

      CONTAINS
C=======================================================================
C  COHORTS, Subroutine, K.J. Boote, P. Alderman
C-----------------------------------------------------------------------
C  Daily leaf and stem cohorts
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1853 (?) KJB, PA Written
C  11/--/2025 GH, CHP  Revised
C  06/07/2026 CHP added stem cohorts
C=======================================================================

      SUBROUTINE COHORTS(DYNAMIC, 
     &  DTX, F, FILECC, NGRLF, NGRST,         !Input
     &  WLDOTN, WSDOTN,                       !Input
     &  YRPLT)                                !Input
!    &  SLA, SLAAD, LAIMX, XLAI,              !Output (eventually)
!    &  WTLF, WCRLF, WNRLF, WTNLF,            !Output (eventually)
!    &  STMWT, WCRST, WNRST, WTNST)           !Output (eventually)

      USE ModuleData
      USE IPCOHO_MOD

      IMPLICIT NONE
      SAVE
      EXTERNAL YR_DOY, GETLUN, HEADER, TIMDIF, OPCOHORTS
      EXTERNAL LossAdjust, NOFF, COFF, CohortComp
      EXTERNAL WARNING, ERROR

      INTEGER, INTENT(IN) :: DYNAMIC
      REAL, INTENT(IN) :: DTX, F, NGRLF, NGRST, WLDOTN, WSDOTN
      INTEGER, INTENT(IN) :: YRPLT
      CHARACTER*92, INTENT(IN) :: FILECC

!     Eventually, these will be output variables.
      REAL SLA, SLAAD, LAIMX, XLAI
      REAL WTLF, WCRLF, WNRLF, WTNLF
      REAL STMWT, WCRST, WNRST, WTNST

      REAL WLDOT_calc, WSDOT_calc

!     temp chp
      REAL LFWT_MAX, LFWT_MIN
      REAL STMWT_MAX, STMWT_MIN

      CHARACTER (len=6), PARAMETER :: ERRKEY = 'COHORT'
      CHARACTER (len=6)  LCMax_txt
      CHARACTER (len=8)  MODEL
      CHARACTER (len=78) MSG(2)

      INTEGER YRDOY, YEAR, DOY, DAS, DAP, TIMDIF
      INTEGER I, FirstCohortDAS

      INTEGER, DIMENSION(LCMax) :: CohortAgeDays

!     Leaf Cohort state variables, not exported (yet?)
      REAL, DIMENSION(LCMax) :: 
!     &  LeafNTot,     !Leaf N total (g[N]]/m2) = WTNLF
!     &  LFSN,         !Leaf structural (non-mobile) N (g[N]]/m2)
!     &  LFAREA,       !Leaf area (cm2[leaf]/m2)
     &  LFAREAH       !Healthy leaf area (cm2[leaf]/m2)
!     &  LFSLA         !Specific leaf area (cm2/g)

!     Stem Cohort state variables, not exported (yet?)
!      REAL, DIMENSION(LCMax) :: 
!     &  StemNTot,      !stem N total (g[N]]/m2) = WTNLF
!     &  STSN           !stem structural (non-mobile) N (g[N]]/m2)

      REAL, DIMENSION(LCMax) :: LeafMassDecrease, NLDOT_c, 
     &    WRCLDT_c !, RHOL
      REAL, DIMENSION(LCMax) :: StemMassDecrease, NSDOT_c, 
     &    WRCSDT_c !, RHOS

      REAL WLDOT_cohort
      REAL WSDOT_cohort

!!     Variables read from species file:
!      REAL ALPHL, ALPHS, PROLFF, PROLFI, PROSTF, PROSTI
!!     These variables are no longer used in this routine.
!!     Need to remove from species file read routine.
!      REAL NVSMOB, SENDAY, TCMP
!      REAL XSENMX(4),SENMAX(4)
!!     Not currently used, but will be needed for shading 
!!     Keep here or move to MOBIL?
!      REAL ICMP, MAXNMINE, NMOBMX
!      REAL SENCLV, SENNLV, PCHOLFF !forage species file
!      REAL SENCSV, SENNSV, PCHOSTF !forage species file

!     Date info for output files
      TYPE (ControlType) CONTROL
      CALL GET (CONTROL)
      DAS   = CONTROL % DAS
      YRDOY = CONTROL % YRDOY
      CALL YR_DOY(YRDOY, YEAR, DOY) 
      DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
      IF (DAP > DAS) DAP = 0

      MODEL = CONTROL % MODEL

!***********************************************************************
!***********************************************************************
!     Run initialization - run once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
      CALL  IPCOHO(
     &  FILECC, MODEL)                            !Input

      CALL OpCohorts(DYNAMIC, YRPLT,          !Input
!     &  LFSN, LFAREA, LeafNTot, PCNLeaf, 
     &  WRCLDT_c, WRCSDT_c,   !TEMP CHP
     &  WLDOTN, ALPHL, WSDOTN, ALPHS,  !TEMP CHP
     &  WLDOT_CALC, WSDOT_CALC, !TEMP CHP
     &  LFWT_MIN, LFWT_MAX, STMWT_MIN, STMWT_MAX, !TEMP CHP
     &  LAIMX, SLA, SLAAD, XLAI,              !Output
     &  WTLF, WCRLF, WNRLF, WTNLF,            !Output
     &  STMWT, WCRST, WNRST, WTNST)           !Output

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      NLC       = 0   !Number of cohorts
      CohortAge = 0.0 !Cohort age for (photo-thermal days)
      CohortAgeDays = 0.0 !Cohort age (days)

      LFDM      = 0.0; STDM      = 0.0 !dry matter (g/m2)
      LFNSC     = 0.0; STNSC     = 0.0 !Mobile CH2O (g/m2)
      LeafNTot  = 0.0; StemNTot  = 0.0 !N total (g[N]]/m2)
      LFNSN     = 0.0; STNSN     = 0.0 !Mobile N (g/m2)
      LFSN      = 0.0; STSN      = 0.0 !Structural N (g[N]]/m2)
      FHLEAF_c  = 0.0; FHSTEM_c  = 0.0 !harvested mass
      CumLeafDM = 0.0 !Cumulative leaf growth (g[leaf]/m2) = CLW
      LFAREA    = 0.0 !Leaf area (cm2[leaf]/m2)
      LFAREAH   = 0.0 !healthy leaf area (cm2[leaf]/m2)
      LeafLignin    = 0.0; StemLignin    = 0.0 !fraction 
      LeafCellulose = 0.0; StemCellulose = 0.0 !fraction 
      LeafHemicell  = 0.0; StemHemicell  = 0.0 !fraction 

      LFFRZ = 0.0      ; STFRZ = 0.0
      LFCMN = 0.0      ; STCMN = 0.0
      LFPST = 0.0      ; STPST = 0.0
      LFNMN = 0.0      ; STNMN = 0.0
      LeafTotSen = 0.0 ; StemTotSen = 0.0
      LFNMNSN = 0.0    ; STNMNSN = 0.0
      LFWSSN = 0.0     ; STWSSN = 0.0
      LFCAD = 0.0      ; STCAD = 0.0
      LFNAD = 0.0      ; STNAD = 0.0
      LFCMINE_c = 0.0  ; STCMINE_c = 0.0
      LFSNMOB_c = 0.0  ; STSNMOB_c = 0.0
      LTSEN_c = 0.0    ; STLTSEN_c  = 0.0
      LFSENWT_c = 0.0  ; STSENWT_c = 0.0
      LFNSEN_c = 0.0   ; STNSEN_c = 0.0
      SLMDOT_c = 0.0   ; SSMDOT_c = 0.0
      FHLEAF_c = 0.0   ; FHSTEM_c  = 0.0
      WLDOT_cohort = 0.0;WSDOT_cohort = 0.0

      CALL OpCohorts(DYNAMIC, YRPLT,          !Input
!     &  LFSN, LFAREA, LeafNTot, PCNLeaf, 
     &  WRCLDT_c, WRCSDT_c,   !TEMP CHP
     &  WLDOTN, ALPHL, WSDOTN, ALPHS,  !TEMP CHP
     &  WLDOT_CALC, WSDOT_CALC, !TEMP CHP
     &  LFWT_MIN, LFWT_MAX, STMWT_MIN, STMWT_MAX, !TEMP CHP
     &  LAIMX, SLA, SLAAD, XLAI,              !Output
     &  WTLF, WCRLF, WNRLF, WTNLF,            !Output
     &  STMWT, WCRST, WNRST, WTNST)           !Output

!***********************************************************************
!***********************************************************************
!     EMERGENCE CALCULATIONS - Performed once per season upon emergence
!         or transplanting of plants
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. EMERG) THEN
!-----------------------------------------------------------------------
!     Initialize first cohort upon emergence
!-------------------------------------
!     Cohorts
      NLC = 1                             !Number of cohorts
      CohortAge(1) = DTX                  !age (ptd)
      FirstCohortDAS = DAS                !DAS on day of first cohort
      CohortAgeDays(1) = 1                !Cohort age in days

!     Leaf Cohorts
      LFDM(1)  = WLDOTN                   !dry matter (g/m2)
      CumLeafDM(1) = WLDOTN               !cumulative addition (g/m2)
      LFNSC(1) = WLDOTN * ALPHL           !mobile CH2O (g/m2)
      LeafNTot(1) = NGRLF                 !total leaf N (g/m2)
!     structural N (g/m2) based on structural C
      LFSN(1)  = PROLFI * 0.16 * (LFDM(1) - LFNSC(1))  
      LFNSN(1) = NGRLF - LFSN(1)          !mobile N (g/m2)
      IF (LFDM(1) > 0.0) THEN
        PCNLeaf(1) = LeafNTot(1) / LFDM(1) * 100.  ! % N
      ELSE
        PCNLeaf(1) = 0.0
      ENDIF

!     Leaf area
      LFAREA(1)  = WLDOTN * F              !leaf area (cm2/m2)
      LFAREAH(1) = LFAREA(1)              !healthy leaf area
      IF (LFDM(1) > 0.0) THEN
        LFSLA(1)   = LFAREA(1) / LFDM(1)
      ELSE
        LFSLA(1)   = 0.0
      ENDIF

!     Stem Cohorts
      STDM(1)  = WSDOTN                   !dry matter (g/m2)
      STNSC(1) = WSDOTN * ALPHS           !mobile CH2O (g/m2)
      StemNTot(1) = NGRST                 !total stem N (g/m2)
!     structural N (g/m2) based on structural C
      STSN(1)  = PROSTI * 0.16 * (STDM(1) - STNSC(1)) 
      STNSN(1) = NGRST - STSN(1)          !mobile N (g/m2) 
      IF (STDM(1) > 0.0) THEN
        PCNStem(1) = StemNTot(1) / STDM(1) * 100.  ! % N
      ELSE
        PCNStem(1) = 0.0
      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!------------------------------
!     LEAF and STEM LOSSES
!     Ensure that losses don't exceed mass, adjust as necessary
!------------------------------
      CALL LossAdjust ("LEAF", MODEL, LeafMassDecrease)
      CALL LossAdjust ("STEM", MODEL, StemMassDecrease)

!---------------------------------------------
!     Integration of leaf and stem mass
!---------------------------------------------
!     Calculate total change to leaf mass per cohort
      WLDOT_calc = WLDOTN
      WSDOT_calc = WSDOTN
!     WLDOTN, WSDOTN = total new growth today (added to new cohort below)

      DO I = 1, NLC
!     ---------------------------------------------------------
!       Leaf dry matter increase (WLDOT in GROW)
        WLDOT_cohort = 
     &      LFCAD(I)            !Reserve C = LCADD
     &    - LFCMN(I)            !C mined = CRUSLF
     &    + LFNAD(I)/0.16       !Reserve N = LNADD
     &    - LFNMN(I)/0.16       !N mined = NRUSLF/0.16 
     &    - LeafMassDecrease(I) !freez, pst, senes=SLDOT+WLIDOT+WLFDOT

!       Leaf dry matter (WTLF in GROW)
        IF (LFDM(I) + WLDOT_cohort >= 1.E-10) THEN
          LFDM(I) = LFDM(I) + WLDOT_cohort
        ELSE
          WLDOT_cohort = LFDM(I)
          LFDM(I) = 0.0
        ENDIF

!       Stem dry matter increase (WSDOT in GROW)
        WSDOT_cohort = 
     &      STCAD(I)            !Reserve C = LCADD
     &    + STNAD(I)/0.16       !Reserve N = LNADD
     &    - STNMN(I)/0.16       !N mined = NRUSST/0.16 
     &    - STCMN(I)            !C mined = CRUSST
     &    - StemMassDecrease(I) !freez, pst, senes=SSDOT+WSIDOT+WSFDOT

!       Stem dry matter (STMWT in GROW)
        IF (STDM(I) + WSDOT_cohort >= 1.E-10) THEN
          STDM(I) = STDM(I) + WSDOT_cohort
        ELSE
          WSDOT_cohort = STDM(I)
          STDM(I) = 0.0
        ENDIF

!       Keep track of total leaf and stem mass addition today
        WLDOT_calc = WLDOT_calc + WLDOT_cohort
        WSDOT_calc = WSDOT_calc + WSDOT_cohort

!---------------------------------------------
!     Integration of leaf area
!---------------------------------------------
        IF (LFDM(I) .GT. 0.0) THEN
!     ---------------------------------------------------------
!         Leaf area
!         For now, use whole leaf SLA for each cohort. But this
!           should be replaced by a cohort SLA when everything is working
!           as it is in GROW.
          LFAREA(I) = LFAREA(I) 
     &      - LeafMassDecrease(I) * LFSLA(I)
     &      - LFNMN(I) / 0.16 * LFSLA(I)
     &      + LFNAD(I) / 0.16 * LFSLA(I)
          LFSLA(I) = LFAREA(I) / LFDM(I)
        ELSE
          LFAREA(I)   = 0.0
          LFSLA(I)    = 0.0
        ENDIF
      ENDDO

!     TEMP CHP
      LFWT_MIN = LFDM(1)
      LFWT_MAX = LFDM(1)
      STMWT_MIN = STDM(1)
      STMWT_MAX = STDM(1)

      DO I = 2, NLC
        IF (LFDM(I) > LFWT_MAX) LFWT_MAX = LFDM(I)
        IF (LFDM(I) < LFWT_MIN) LFWT_MIN = LFDM(I)
        IF (STDM(I) > STMWT_MAX) STMWT_MAX = STDM(I)
        IF (STDM(I) < STMWT_MIN) STMWT_MIN = STDM(I)
      ENDDO

!---------------------------------------------
!     Integration of leaf and stem CH2O
!---------------------------------------------
!     Mobile, non-structural CH2O (WCRLF, WCRST in GROW)
!     ---------------------------
      CALL COFF("LEAF", MODEL,
     &  LeafMassDecrease, PCHOLFF, RHOL_c, SENCLV,  !Input
     &  WRCLDT_c)                                   !Output

      CALL COFF("STEM", MODEL,
     &  StemMassDecrease, PCHOSTF, RHOS_c, SENCSV,  !Input
     &  WRCSDT_c)                                   !Output

      DO I = 1, NLC
        IF (LFDM(I) > 0.0) THEN
!         Update mobile CH2O in leaf 
          LFNSC(I) = LFNSC(I) + WRCLDT_c(I)
          IF (LFNSC(I) < 0.0) LFNSC(I) = 0.0
          RHOL_c(I) = LFNSC(I) / LFDM(I)
!!         Limit mobile CH2O for each cohort to 1.5 * ALPHL(need to ask Ken!!)
!          IF (RHOL_c(I) > 1.5 * ALPHL) THEN
!            LFNSC(I) = LFDM(I) * ALPHL * 1.5  
!            RHOL_c(I) = LFNSC(I) / LFDM(I)
!          ENDIF
        ELSE  !LFDM(I) <= 0.0
          LFNSC(I) = 0.0
          RHOL_c(I)  = 0.0
        ENDIF

        IF (STDM(I) .GT. 0.0) THEN
!         Update mobile CH2O in stem
          STNSC(I) = STNSC(I) + WRCSDT_c(I)
          IF (STNSC(I) < 0.0) STNSC(I) = 0.0
          RHOS_c(I) = STNSC(I) / STDM(I)
!!         Limit mobile CH2O for each cohort to 1.5 * ALPHS (need to ask Ken!!)
!          IF (RHOS_c(I) > 1.5 * ALPHS) THEN
!            STNSC(I) = STDM(I) * ALPHS * 1.5
!            RHOS_c(I) = STNSC(I) / STDM(I)
!          ENDIF
        ELSE  !STDM(I) <= 0.0
          STNSC(I)    = 0.0
          RHOS_c(I) = 0.0
        ENDIF
      ENDDO

!---------------------------------------------
!     Integration of leaf and stem N
!---------------------------------------------
      CALL NOFF (
     &  "LEAF", MODEL, PROLFF, SENNLV,            !Input
     &  PCNLeaf, LeafNTot,                        !Input
     &  NLDOT_c)                                  !Output

      CALL NOFF (
     &  "STEM", MODEL, PROSTF, SENNSV,            !Input
     &  PCNStem, StemNTot,                        !Input
     &  NSDOT_c)                                  !Output

      DO I = 1, NLC
        IF (LFDM(I) > 0.0) THEN
!         Leaf N
          LeafNTot(I) = LeafNTot(I) + NLDOT_c(I) 

!         Structural N (WTNLF minus WNRLF in GROW)
          LFSN(I) = MIN(LeafNTot(I), PROLFF*0.16 * (LFDM(I) - LFNSC(I)))

!         Non-structural N (WNRLF in GROW)
          LFNSN(I) = LeafNTot(I) - LFSN(I)

          PCNLeaf(I) = LeafNTot(I) / LFDM(I) * 100.  ! % N 

        ELSE  !LFDM(I) <= 0.0
          LeafNTot(I) = 0.0
          LFSN(I)     = 0.0
          LFNSN(I)    = 0.0
          PCNLeaf(I)  = 0.0
        ENDIF

!       ---------------------------
!       Stem cohort
        IF (STDM(I) .GT. 0.0) THEN
!         Stem N
          StemNTot(I) = StemNTot(I) + NSDOT_c(I) 
          PCNStem(I) = StemNTot(I) / STDM(I) * 100.  ! % N 

!         Structural N (WTNST minus WNRST in GROW)
          STSN(I) = MIN(StemNTot(I), PROSTF*0.16 * (STDM(I) - STNSC(I)))

!         Non-structural N (WNRLF in GROW)
          STNSN(I) = StemNTot(I) - STSN(I)

        ELSE  !STDM(I) <= 0.0
          StemNTot(I) = 0.0
          STSN(I)     = 0.0
          STNSN(I)    = 0.0
          PCNStem(I)  = 0.0
        ENDIF
      ENDDO

!-------------------------------------------------------------------
!     Today's new cohort
!-------------------------------------------------------------------
!     Check for max number of cohorts.
!     End the simulation if this new cohort exceeds the limit.
      IF (NLC == LCMax) THEN
        WRITE(LCMax_txt,'(G0)') LCMax
        WRITE(MSG(1),'(A,A,A)')
     &  "Maximum number of cohorts (",TRIM(ADJUSTL(LCMax_txt)),
     &  ") reached." 
        MSG(2) = 
     &  "Contact DSSAT developers if you need longer simulations."
        CALL WARNING(2,ERRKEY,MSG)
        CALL ERROR(ERRKEY,75," ",0)
      ENDIF

!     If there is any new leaf OR new stem mass today, add a new cohort
      IF (WLDOTN > 0.0 .OR. WSDOTN > 0.0) THEN
        NLC = NLC + 1  !today's new cohort

!       New growth for today's leaf cohort
        LFDM(NLC)   = WLDOTN             !leaf dry mass
        CumLeafDM(NLC) = WLDOTN          !cum leaf mass added
        LFAREA(NLC) = WLDOTN * F         !leaf area
        LFNSC(NLC)  = WLDOTN * ALPHL     !non-struct CH2O
        LeafNTot(NLC) = NGRLF            !total leaf N (g/m2)
        LFSN(NLC)   = PROLFF * 0.16 * (WLDOTN - LFNSC(NLC))  !struc N
        LFNSN(NLC)  = NGRLF - LFSN(NLC)  !non-struct N (mobile)
        IF (LFDM(NLC) > 0.0) THEN
          PCNLeaf(NLC) = LeafNTot(NLC) / LFDM(NLC) * 100.  ! % N
        ELSE
          PCNLeaf(NLC) = 0.0
        ENDIF

!       New growth for today's stem cohort
        STDM(NLC)  = WSDOTN             !stem dry mass
        STNSC(NLC) = WSDOTN * ALPHS     !non-struct CH2O
        StemNTot(NLC) = NGRST           !total stem N (g/m2)
        STSN(NLC)  = PROSTF * 0.16 * (WSDOTN - STNSC(NLC))  !struc N
        STNSN(NLC) = NGRST - STSN(NLC)  !non-struct N (mobile)
        IF (STDM(NLC) > 0.0) THEN
          PCNStem(NLC) = StemNTot(NLC) / STDM(NLC) * 100.  ! % N
        ELSE
          PCNStem(NLC) = 0.0
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

!***********************************************************************
!***********************************************************************
!     Sum over all cohorts for SEASINIT and for INTEGR
!***********************************************************************
      IF (DYNAMIC .EQ. EMERG .OR.DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      DO I = 1, NLC
        CohortAge(I) = CohortAge(I) + DTX  !cohort age in p-t-d
        CohortAgeDays(I) = CohortAgeDays(I) + 1

        LeafNTot(I) = LFNSN(I) + LFSN(I)
        IF (LFDM(I) > 0.0) THEN
          PCNLeaf(I) = LeafNTot(I) / LFDM(I) * 100.  ! % N 
          RHOL_c(I) = LFNSC(I) / LFDM(I)
        ELSE
          PCNLeaf(I) = 0.0
          RHOL_c(I) = 0.0
        ENDIF

        StemNTot(I) = STNSN(I) + STSN(I)
        IF (STDM(I) > 0.0) THEN
          PCNStem(I) = StemNTot(I) / STDM(I) * 100.  ! % N 
          RHOS_c(I) = STNSC(I) / STDM(I)
        ELSE
          PCNStem(I) = 0.0
          RHOS_c(I) = 0.0
        ENDIF
      ENDDO

      CALL CohortComp()

!***********************************************************************
!***********************************************************************
!     OUTPUT section 
!***********************************************************************
!-----------------------------------------------------------------------
      ELSE IF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      CALL OpCohorts(DYNAMIC, YRPLT,          !Input
!     &  LFSN, LFAREA, LeafNTot, PCNLeaf, 
     &  WRCLDT_c, WRCSDT_c,   !TEMP CHP
     &  WLDOTN, ALPHL, WSDOTN, ALPHS,  !TEMP CHP
     &  WLDOT_CALC, WSDOT_CALC, !TEMP CHP
     &  LFWT_MIN, LFWT_MAX, STMWT_MIN, STMWT_MAX, !TEMP CHP
     &  LAIMX, SLA, SLAAD, XLAI,              !Output
     &  WTLF, WCRLF, WNRLF, WTNLF,            !Output
     &  STMWT, WCRST, WNRST, WTNST)           !Output

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE COHORTS
!=======================================================================

!=======================================================================
!***********************************************************************
!     Variable listing for COHORTS subroutine (updated 20 April 2009)
!***********************************************************************
! CHRTIN       Logical unit number of cohort parameter input file
! CHRTOUT      Logical unit number of cohort output file
! COHORTOUT    File name of cohort output file
! COHORTIN     File name of cohort parameter input file
! CUMAREA      Sum of leaf area to cohort I, used to calculate SHADEFAC(I)
!                (m2[leaf]/m2[ground])
! CUMLFDM      Cumulative leaf dry matter for the season (g[leaf]/m2[ground])
! DOY          Day of year of simulation (DDD)
! DTX          Thermal time that occurs in a real day based on vegetative 
!                development temperature function (thermal days / day)
! F            Specific leaf area of new leaf tissue growth, including N
!                (cm2[leaf] / g[leaf])
! ICMP         Light compensation point for senescence (moles[quanta]/m2-d)
! KCAN         Canopy light extinction coefficient
! LFDM(I)      Leaf dry matter for cohort I (g[leaf]/m2[ground])
! LFNSC(I)     Leaf non-structural (mobile) CH2O for cohort I
!                (g[leaf CH2O]/m2[ground]) = leaf CH2O reserves = WCRLF
! LFNSN(I)     Leaf non-structural (mobile) N for cohort I
!                (g[leaf N]/m2[ground])
! LFSC(I)      Leaf structural (non-mobile) CH2O for cohort I
!                (g[leaf CH2O]/m2[ground])
! LFSN(I)      Leaf structural (non-mobile) N for cohort I
!                (g[leaf N]/m2[ground])
! LFAREA(I)    Leaf area for cohort I (cm2[leaf]/m2[ground]
! CohortAge(I) Age for cohort I (thermal days)
! LFDMSN(I)    Total leaf dry matter senescence for today for cohort I,
!                includes LFWSSN(I) and LFNMNSN(I) (g[leaf]/m2[ground]/d)
! LFFRZ(I)     Leaf dry matter lost due to freeze damage today for
!                cohort I (g[leaf]/m2[ground]/d)
! LFNMN(I)     Leaf N mining rate for today for cohort I
!                (g[leaf N]/m2[ground]/d)
! LFNMNSN(I)   Leaf dry matter senescence due to N mining for today for
!                cohort I (g[leaf]/m2[ground]/d)
! LFNMNR(I)    Maximum leaf N mining rate for cohort I
!                (g[leaf N]/m2[ground]/d)
! LFPST(I)     Leaf dry matter lost to due to pest damage today for
!                cohort I (g[leaf]/m2[ground]/d)
! LFWSSN(I)    Leaf senescence due to water stress for today for cohort I
!                (g[leaf]/m2[ground]/d)
! MAXNMINE     Maximum N mining rate (fraction/day)
! NGRLF        Maximum N demand for leaf growth (g[leaf N] / m2[ground] / d)
! NMOBMX       Maximum N mobilization rate (fraction/day)
! NMOBR        Stage-dependent potential N mining rate expressed as a
!                fraction of the maximum rate (NMOBMX) 
! NRUSLF       N actually mobilized from leaves in a day (g[N]/m2-d)
! NVSMOB       Relative rate of N mining during vegetative stage to that in 
!                reproductive stage 
! PAR          Daily photosynthetically active radiation or photon flux 
!                density (moles[quanta]/m2-d)
! PCNLeaf      Actual cohort N concentration %
! PLEAFN_calc  Leaf N concentration averaged over all cohorts today (percent)
! PORLFT       Proportion of leaf weight grown which will have been senesced 
!                if no water stress has occurred prior to this V-stage 
! PROLFF       Minimum leaf protein composition after N mining
!                (g[protein] / g[leaf])
! RATTP        Factor used in determining senescence due to water 
!               stress
! SENDAY       Maximum rate of leaf abscission due to water stress
!                (fraction/day)
! SENMAX(I)    Maximum proportion of total leaf weight as a function of 
!                V-stage (XSENMX(I)) which can be senesced due to water stress. 
! SHADEFAC(I)  Factor used to increase N mobilization of cohort I due to
!                shading
! SLDOT        Defoliation due to daily leaf senescence (g/m2/day)
! SLNDOT       Leaf senescence due to water stress (g/m2/day)
! SUMLFDM      Sum of leaf dry matter for all cohorts today
!                (g[leaf]/m2[ground])
! SUMLFNSN     Sum of leaf non-structural N for all cohorts today
!                (g[leaf N]/m2[ground])
! SUMLFAREA    Sum of leaf area for all cohorts today
!                (cm2[leaf]/m2[ground])
! SUMLFN       Sum of N (structural and non-structural for all cohorts today
!                (g[leaf N]/m2[ground])
! SWFAC        Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!                0.0=max stress
! TCMP         Time constant for low light senescence (days)
! VSTAGE       Number of nodes on main stem of plant 
! WLDOTN       Dry weight growth rate of new leaf tissue including N but not
!                C reserves (g[leaf] / m2[ground]/d)
! WSLOSS       Leaf senescence due to water stress for today (g/m2/day)
! LCMP         LAI at which today's light compensation (ICMP) is reached
!                (m2[leaf] / m2[ground])
! WNRLF        N available for mobilization from leaves above lower limit of 
!                mining (g[N] / m2)
! WTLF         Dry mass of leaf tissue including C and N
!                (g[leaf] / m2[ground])
! WTNLF        Mass of N in leaves (g[leaf N] / m2[ground])
! XLAI         Leaf area (one side) per unit of ground area
!                (m2[leaf] / m2[ground])
! XSENMX(I)    V-stage at which maximum fraction of cumulative leaf growth 
!                vulnerable to loss due to water stress is SENMAX(I).
!                (# leaf nodes)
! YEAR         Year of simulation (YYYY)
! YRDOY        Current day of simulation (YYYYDDD)
!***********************************************************************

C=======================================================================
      END MODULE COHORTS_MOD
C=======================================================================

!     SHADING CODE FROM ORIGINAL BOOTE / ALDERMAN VERSION
!!-------------------------------------------
!! INCREASED N MINING FROM SHADING (SHADEFAC)
!!-------------------------------------------
!     This part is not handled (yet) in VEGGR  ************* <<<--- SHADEFAC NOT HANDLED YET
!      IF (PAR .GT. 0.) THEN
!        LCMP = -(1. / KCAN) * ALOG(ICMP / PAR)
!      ENDIF
!
!      SHADEFAC = 1.0
!      CUMAREA=0.0
!      DO I=1,NLC
!        CUMAREA=CUMAREA+LFAREA(I)/10000
!        IF (CUMAREA/LCMP.GE.1)THEN
!          SHADEFAC(I)=CUMAREA/LCMP
!        ELSE
!          SHADEFAC(I)=1
!        ENDIF
!      ENDDO
!
! This is now done in MOBIL, but does not include shadefac  ************* <<<--- MOBIL

!    DO  I=1,NLC
!        IF (LFNSN(I) .LE. 0.0 .OR. MAXNMINE .LE. 0.0 
!     &                        .OR. NMOBMX .LE. 0.0) THEN
!          LFNMN(I)=0.0
!        ELSE
!          LFNMN(I)=SHADEFAC(I)*(NMINER/NMOBMX)*MAXNMINE*LFNSN(I)
!          LFNMN(I)=MIN(LFNMN(I),LFNSN(I))
!          IF ((LFNSN(I)-LFNMN(I)).LE.0.00001)THEN
!            LFNMN(I)=LFNSN(I)
!          ENDIF
!        ENDIF
!     END DO

