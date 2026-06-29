C=======================================================================
      MODULE COHORTS_MOD
C=======================================================================
      INTEGER, PARAMETER :: LCMax = 10000 !max # of leaf/stem cohorts
      INTEGER NLC     !current number of leaf (and stem) cohorts

!     STATE VARIABLES: Leaf cohorts
      REAL, DIMENSION(LCMax) ::  
     &  LFDM,         !Leaf dry matter (g[leaf]/m2) = WTLF
     &  CumLeafDM,    !Cumulative leaf growth (g[leaf]/m2) = CLW
     &  LFNSC,        !Leaf non-structural (mobile) CH2O (g/m2) = WCRLF
     &  LFNSN         !Leaf non-structural (mobile) N (g/m2) = WNRLF
!    &  PCNLeaf,      !Leaf N%

!     STATE VARIABLES: Stem cohorts
      REAL, DIMENSION(LCMax) ::  
     &  STDM,         !Stem dry matter (g[stem]/m2) = WTLF
     &  STNSC,        !Stem non-structural (mobile) CH2O (g/m2) = WCRLF
     &  STNSN         !Stem non-structural (mobile) N (g/m2) = WNRLF
!    &  PCNStem       !Stem N%

!     STATE VARIABLES: Leaf and stem cohorts
      REAL, DIMENSION(LCMax) ::  
     &  CohortAge     !Leaf/stem age for (thermal days)

!     RATE VARIABLES: Leaf and stem cohorts
      REAL, DIMENSION(LCMax) ::  
!       calculated in FREEZE, for_freeze
     &  LFFRZ,      !leaf mass frozen today (g[leaf]/m2) = WLFDOT
     &  STFRZ,      !stem mass frozen today (g[stem]/m2) = WSFDOT

!       calculated in VEGGR, for_veggr
     &  LFCMN,      !leaf non-struc CH2O mined (g[CH2O]/m2) = CRUSLF
     &  STCMN,      !Stem non-struc CH2O mined (g[CH2O]/m2) = CRUSLF

!       Calculated LeafCohortPest
     &  LFPST,      !leaf pest damage today (g[leaf]/m2) = WLIDOT
     &  STPST,      !Stem pest damage today (g[stem]/m2) = WLIDOT

!       calculated in MOBIL, for_mobil, for_veggr
     &  LFNMN,      !Leaf non-struc N mined today (g[N]]/m2) = NRUSLF
     &  STNMN,      !Stem non-struc N mined today (g[N]]/m2) = NRUSLF

!       calculated in SENES, for_senmob
     &  LeafTotSen, !Total leaf senescense today (g[leaf]/m2) = SLDOT
     &  LFNMNSN,    !Leaf senescence due to N mining (g[leaf]/m2)
     &  LFWSSN,     !leaf water stress senescence today (g[leaf]/m2)

     &  StemTotSen, !Total Stem senescense today (g[stem]/m2) = SSDOT
     &  STNMNSN,    !Stem senescence due to N mining (g[stem]/m2)
     &  STWSSN,     !Stem water stress senescence today (g[stem]/m2)

!       calculated in GROW and for_grow, adjusted in COHORTS
     &  LFCAD,      !leaf non-struc CH2O stored  (g[CH2O]/m2) = CADLF
     &  LFNAD,      !Leaf non-struc N stored today (g[N]]/m2) = NADLF

     &  STCAD,      !Stem non-struc CH2O stored  (g[CH2O]/m2) = CADST
     &  STNAD,      !Stem non-struc N stored today (g[N]]/m2) = NADST

!       calculated in for_senmob
     &  LFCMINE_c,  !Max potential CH2O mining today
     &  LFSNMOB_c,  !Leaf N mobilized by natural senescence (g[N]/m2)
     &  LTSEN_c,    !Low light senescence
!       is LFSENWT_c the same as LFNMNSN?
     &  LFSENWT_c,  !Leaf senescence due to N mobilization
     &  LFNSEN_c,   !natural senescence
     &  SLMDOT_c,   !Leaf senescence with N mobilization 

     &  STCMINE_c,  !Max potential CH2O mining today
     &  STSNMOB_c,  !Stem N mobilized by natural senescence (g[N]/m2)
!       is LFSENWT_c the same as STNMNSN?
     &  STSENWT_c,  !Stem senescence due to N mobilization
     &  STNSEN_c,   !natural senescence
     &  SSMDOT_c,   !Stem senescence with N mobilization 
     &  STLTSEN_c,  !Stem low light senescence

!       calculated in for_harv
     &  FHLEAF_c,   !Forage harvest
     &  FHSTEM_c    !Forage harvest

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
     &  YRPLT,                                !Input
     &  PCNLeaf, PCNStem,                     !Output
     &  WTLF, WCRLF, WNRLF, WTNLF, XLAI,      !OUTPUT (eventually)
     &  STMWT, WCRST, WNRST, WTNST)           !OUTPUT (eventually)

      USE ModuleData
      IMPLICIT NONE
      SAVE
      EXTERNAL YR_DOY, GETLUN, HEADER, TIMDIF, IPCOHO, OPCOHORTS
      EXTERNAL LossAdjust, NOFF, COFF

      INTEGER, INTENT(IN) :: DYNAMIC
      REAL, INTENT(IN) :: DTX, F, NGRLF, NGRST, WLDOTN, WSDOTN
      INTEGER, INTENT(IN) :: YRPLT
      CHARACTER*92, INTENT(IN) :: FILECC

      REAL, DIMENSION(LCMax) :: PCNLeaf, PCNStem

!     Eventually, these will be output variables.
      REAL, INTENT(IN) :: WTLF, WCRLF, WNRLF, WTNLF, XLAI
      REAL, INTENT(IN) :: STMWT, WCRST, WNRST, WTNST

      REAL AREALF_calc, SLA_calc, SLAAD_calc, LAIMX_calc

      REAL WTLF_calc, WNRLF_calc, WCRLF_calc, XLAI_calc, 
     &     WTNLF_calc, PLEAFN_calc

      REAL WLDOT_calc, SLDOT_calc, WLFDOT_calc, NRUSLF_calc, 
     &  CRUSLF_calc, WLIDOT_calc, WatSen_calc, LfMineSen_calc, 
     &  LCADD_calc, LNADD_calc, NLDOT_calc, NLOFF_calc,
     &  LFSN_calc, FHLEAF_calc

      REAL WSDOT_calc, SSDOT_calc, NRUSST_calc, 
     &  CRUSST_calc, WSIDOT_calc, STMineSen_calc, 
     &  SCADD_calc, SNADD_calc, NSDOT_calc, NSOFF_calc,
     &  STSN_calc, FHSTEM_calc

      REAL WTST_calc, PStemN_calc, WCRST_calc, 
     &  WTNST_calc, WNRST_calc, WSFDOT_calc, 
     &  WatSenStem_calc, WRCLDT_calc

      CHARACTER (len=8) MODEL

      INTEGER YRDOY, YEAR, DOY, DAS, DAP, TIMDIF
      INTEGER I, FirstCohortDAS

      INTEGER, DIMENSION(LCMax) :: CohortAgeDays

!     Leaf Cohort state variables, not exported (yet?)
      REAL, DIMENSION(LCMax) :: 
     &  LeafNTot,     !Leaf N total (g[N]]/m2) = WTNLF
     &  LFSN,         !Leaf structural (non-mobile) N (g[N]]/m2)
     &  LFAREA,       !Leaf area (cm2[leaf]/m2)
     &  LFAREAH,      !Healthy leaf area (cm2[leaf]/m2)
     &  LFSLA         !Specific leaf area (cm2/g)

!     Stem Cohort state variables, not exported (yet?)
      REAL, DIMENSION(LCMax) :: 
     &  StemNTot,      !stem N total (g[N]]/m2) = WTNLF
     &  STSN           !stem structural (non-mobile) N (g[N]]/m2)

!!       Composition and quality
!     &  LFLIGNIN,     !Lignin content %
!     &  LFCELLUL,     !Cellulose content %
!     &  LFHEMICEL     !Hemicellulose content %

      REAL, DIMENSION(LCMax) :: LeafMassDecrease, NLDOT_c, 
     &    NLOFF_c, WRCLDT_c, RHOL
      REAL, DIMENSION(LCMax) :: StemMassDecrease, NSDOT_c, 
     &    NSOFF_c, WRCSDT_c, RHOS

      REAL CUMLFDM, WLDOT_cohort
      REAL WSDOT_cohort

!     TEMP CHP
      REAL Percent_harvested

!     Variables read from species file:
      REAL ALPHL, ALPHS, PROLFF, PROLFI, PROSTF, PROSTI
!     These variables are no longer used in this routine.
!     Need to remove from species file read routine.
      REAL NVSMOB, SENDAY, TCMP
      REAL XSENMX(4),SENMAX(4)
!     Not currently used, but will be needed for shading 
!     Keep here or move to MOBIL?
      REAL ICMP, MAXNMINE, NMOBMX
      REAL SENCLV, SENNLV, PCHOLFF !forage species file
      REAL SENCSV, SENNSV, PCHOSTF !forage species file

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
      CALL OpCohorts(DYNAMIC, YRPLT, 
!       Leaf output:
     &  WTLF_calc, XLAI_calc, WCRLF_calc, PLEAFN_calc, 
     &  WTNLF_calc, WNRLF_calc, LFSN_calc, 
     &  WLDOT_calc, LCADD_calc, LNADD_calc, 
     &  CRUSLF_calc, NRUSLF_calc, 
     &  WLIDOT_calc, WLFDOT_calc, SLDOT_calc, 
     &  WatSen_calc, LfMineSen_calc, 
     &  NLOFF_calc, NLDOT_calc, FHLEAF_calc,
     &  WRCLDT_calc,
!       Stem output:
     &  WTST_calc, WCRST_calc, PStemN_calc,
     &  WTNST_calc, WNRST_calc, STSN_calc, 
     &  WSDOT_calc, SCADD_calc, SNADD_calc, 
     &  CRUSST_calc, NRUSST_calc, 
     &  WSIDOT_calc, WSFDOT_calc, SSDOT_calc, 
     &  WatSenStem_calc, STMineSen_calc, 
     &  NSOFF_calc, NSDOT_calc, FHSTEM_calc)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      NLC       = 0   !Number of cohorts
      CohortAge = 0.0 !Cohort age for (photo-thermal days)
      CohortAgeDays = 0.0 !Cohort age (days)

      LFDM      = 0.0 !Leaf dry matter (g[leaf]/m2) = WTLF
      CumLeafDM = 0.0 !Cumulative leaf growth (g[leaf]/m2) = CLW
      LFNSC     = 0.0 !Leaf non-structural (mobile) CH2O (g/m2) = WCRLF
      LeafNTot  = 0.0 !Leaf N total (g[N]]/m2) = WTNLF
      LFNSN     = 0.0 !Leaf non-structural (mobile) N (g/m2) = WNRLF
      LFSN      = 0.0 !Leaf structural (non-mobile) N (g[N]]/m2)
      LFAREA    = 0.0 !Leaf area (cm2[leaf]/m2)
      LFAREAH   = 0.0 !healthy leaf area (cm2[leaf]/m2)
      FHLEAF_c  = 0.0 !harvested leaf mass

      CUMLFDM   = 0.0 !Not used by could be compared with CumLeafDM

      STDM      = 0.0 !stem dry matter (g[stem]/m2) = WTLF
      STNSC     = 0.0 !stem non-structural (mobile) CH2O (g/m2) = WCRLF
      StemNTot  = 0.0 !stem N total (g[N]]/m2) = WTNLF
      STNSN     = 0.0 !stem non-structural (mobile) N (g/m2) = WNRLF
      LFSN      = 0.0 !stem structural (non-mobile) N (g[N]]/m2)
      LFAREA    = 0.0 !stem area (cm2[stem]/m2)
      LFAREAH   = 0.0 !healthy stem area (cm2[stem]/m2)
      FHSTEM_c  = 0.0 !harvested stem mass

!      LFLIGNIN  = 0.0 !Lignin content %
!      LFCELLUL  = 0.0 !Cellulose content %
!      LFHEMICEL = 0.0 !Hemicellulose content %

!     Zero out rate arrays
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
      WTLF_calc = 0.0  ; WTST_calc = 0.0
      WNRLF_calc = 0.0 ; WNRST_calc = 0.0
      WCRLF_calc = 0.0 ; WCRST_calc = 0.0
      XLAI_calc = 0.0
      WTNLF_calc = 0.0 ; WTNST_calc = 0.0
      WLDOT_cohort = 0.0; WSDOT_cohort = 0.0

!     Read parameters from species file
      CALL IPCOHO(
     &  FILECC, MODEL,                            !Input
     &  ALPHL, ALPHS, ICMP, MAXNMINE,             !Output
     &  NMOBMX, NVSMOB,                           !Output
     &  PCHOLFF, PCHOSTF, PROLFI, PROLFF, PROSTI, !Output
     &  PROSTF, SENDAY, SENMAX, SENCLV, SENCSV,   !Output
     &  SENNLV, SENNSV, TCMP, XSENMX)             !Output

      CALL OpCohorts(DYNAMIC, YRPLT, 
!       Leaf output:
     &  WTLF_calc, XLAI_calc, WCRLF_calc, PLEAFN_calc, 
     &  WTNLF_calc, WNRLF_calc, LFSN_calc, 
     &  WLDOT_calc, LCADD_calc, LNADD_calc, 
     &  CRUSLF_calc, NRUSLF_calc, 
     &  WLIDOT_calc, WLFDOT_calc, SLDOT_calc, 
     &  WatSen_calc, LfMineSen_calc, 
     &  NLOFF_calc, NLDOT_calc, FHLEAF_calc,
     &  WRCLDT_calc,
!       Stem output:
     &  WTST_calc, WCRST_calc, PStemN_calc,
     &  WTNST_calc, WNRST_calc, STSN_calc, 
     &  WSDOT_calc, SCADD_calc, SNADD_calc, 
     &  CRUSST_calc, NRUSST_calc, 
     &  WSIDOT_calc, WSFDOT_calc, SSDOT_calc, 
     &  WatSenStem_calc, STMineSen_calc, 
     &  NSOFF_calc, NSDOT_calc, FHSTEM_calc)

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

!!     Composition
!      LFLIGNIN  = function of ??
!      LFCELLUL  = function of ??
!      LFHEMICEL = function of ??

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

!!     Composition
!      STLIGNIN  = function of ??
!      STCELLUL  = function of ??
!      STHEMICEL = function of ??

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
     &    + LFNAD(I)/0.16       !Reserve N = LNADD
     &    - LFNMN(I)/0.16       !N mined = NRUSLF/0.16 
     &    - LFCMN(I)            !C mined = CRUSLF
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
      ENDDO

!---------------------------------------------
!     Integration of leaf area
!---------------------------------------------
      DO I = 1, NLC
        IF (LFDM(I) .GT. 0.0) THEN
!     ---------------------------------------------------------
!         Leaf area
!         For now, use whole leaf SLA for each cohort. But this
!           should be replaced by a cohort SLA when everything is working
!           as it is in GROW.
          LFAREA(I) = LFAREA(I) 
     &      - LeafMassDecrease(I) * SLA_calc
     &      - LFNMN(I) / 0.16 * SLA_calc
     &      + LFNAD(I) / 0.16 * SLA_calc
          LFSLA(I) = LFAREA(I) / LFDM(I)
        ELSE
          LFAREA(I)   = 0.0
          LFSLA(I)    = 0.0
        ENDIF
      ENDDO

!---------------------------------------------
!     Integration of leaf and stem CH2O
!---------------------------------------------
!     Mobile, non-structural CH2O (WCRLF, WCRST in GROW)
!     ---------------------------
      CALL COFF("LEAF", MODEL,
     &  LeafMassDecrease, PCHOLFF, RHOL, SENCLV,  !Input
     &  WRCLDT_c)                                 !Output

      CALL COFF("STEM", MODEL,
     &  StemMassDecrease, PCHOSTF, RHOS, SENCSV,  !Input
     &  WRCSDT_c)                                 !Output

      DO I = 1, NLC
        IF (LFDM(I) > 0.0) THEN
!         Update mobile CH2O in leaf 
          LFNSC(I) = LFNSC(I) + WRCLDT_c(I)
          IF (LFNSC(I) < 0.0) LFNSC(I) = 0.0
          RHOL(I) = LFNSC(I) / LFDM(I)
        ELSE  !LFDM(I) <= 0.0
          LFNSC(I) = 0.0
          RHOL(I)  = 0.0
        ENDIF

        IF (STDM(I) .GT. 0.0) THEN
!         Update mobile CH2O in stem
          STNSC(I) = STNSC(I) + WRCSDT_c(I)
          IF (STNSC(I) < 0.0) STNSC(I) = 0.0
          RHOS(I) = STNSC(I) / STDM(I)
        ELSE  !STDM(I) <= 0.0
          STNSC(I)    = 0.0
          RHOS(I) = 0.0
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
        PCNLeaf(NLC) = LeafNTot(NLC) / LFDM(NLC) * 100.  ! % N

!       New growth for today's stem cohort
        STDM(NLC)  = WSDOTN             !stem dry mass
        STNSC(NLC) = WSDOTN * ALPHS     !non-struct CH2O
        StemNTot(NLC) = NGRST           !total stem N (g/m2)
        STSN(NLC)  = PROSTF * 0.16 * (WSDOTN - STNSC(NLC))  !struc N
        STNSN(NLC) = NGRST - STSN(NLC)  !non-struct N (mobile)
        PCNStem(NLC) = StemNTot(NLC) / STDM(NLC) * 100.  ! % N
      ENDIF

!-------------------------------------------------------------------
!     Handle harvested leaf
!     Update LFDM and STDM here so the harvested values do not affect
!       calculations of WRCLDT_c and WRCSDT_c
      FHLEAF_calc = SUM(FHLEAF_c)            !harvested
      IF (FHLEAF_calc > 0.0) THEN

!       TEMP CHP
!       FORCE NEW COHORT TO BE HARVESTED AT SAME RATE
        WTLF_calc   = SUM(LFDM) - WLDOTN                 !Leaf mass g/m2
        Percent_harvested = FHLEAF_calc / WTLF_calc
        FHLEAF_c(NLC) = LFDM(NLC) * Percent_harvested

!       Harvest today
        DO I = 1, NLC
          IF (FHLEAF_c(I) < LFDM(I)) THEN
            LFDM(I) = LFDM(I) - FHLEAF_c(I)
            LFAREA(I) = LFAREA(I) - FHLEAF_c(I) * SLA_calc
            LFSLA(I) = LFAREA(I) / LFDM(I)
            LFNSC(I) = LFNSC(I) -FHLEAF_c(I) * RHOL(I)
            LeafNTot(I) = LeafNTot(I) -FHLEAF_c(I) * PCNLeaf(i)/100.
            LFSN(I) = MIN(LeafNTot(I),PROLFF*0.16 * (LFDM(I) -LFNSC(I)))
            LFNSN(I) = LeafNTot(I) - LFSN(I)
          ELSE
            FHLEAF_c(I) = LFDM(I)
            LFDM(I)     = 0.0
            LFAREA(I)   = 0.0
            LFSLA(I)    = 0.0
            LFNSC(I)    = 0.0
            LeafNTot(I) = 0.0
            LFSN(I)     = 0.0
            LFNSN(I)    = 0.0
          ENDIF
        ENDDO
      ENDIF

!-------------------------------------------------------------------
!     Handle harvested stem
      FHSTEM_calc = SUM(FHSTEM_c)            !harvested
      WTST_calc   = SUM(STDM)                !Stem mass g/m2
      IF (FHSTEM_calc > 0.0) THEN
!       Harvest today
        DO I = 1, NLC
          IF (FHSTEM_c(I) < STDM(I)) THEN
            STDM(I) = STDM(I) - FHSTEM_c(I)
            STNSC(I) = STNSC(I) -FHSTEM_c(I) * RHOS(I)
            StemNTot(I) = StemNTot(I) -FHSTEM_c(I) * PCNStem(I)/100.
            STSN(I) = MIN(StemNTot(I),PROSTF*0.16 * (STDM(I) -STNSC(I)))
            STNSN(I) = StemNTot(I) - STSN(I)
          ELSE
            FHSTEM_c(I) = STDM(I)
            STDM(I)     = 0.0
            STNSC(I)    = 0.0
            StemNTot(I) = 0.0
            STSN(I)     = 0.0
            STNSN(I)    = 0.0
          ENDIF
        ENDDO
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
!-------------------------------------------------------------------
      DO I = 1, NLC
        CohortAge(I) = CohortAge(I) + DTX  !cohort age in p-t-d
        CohortAgeDays(I) = CohortAgeDays(I) + 1

        LeafNTot(I) = LFNSN(I) + LFSN(I)
        IF (LFDM(I) > 0.0) THEN
          PCNLeaf(I) = LeafNTot(I) / LFDM(I) * 100.  ! % N 
          RHOL(I) = LFNSC(I) / LFDM(I)
        ELSE
          PCNLeaf(I) = 0.0
          RHOL(I) = 0.0
        ENDIF

        StemNTot(I) = STNSN(I) + STSN(I)
        IF (STDM(I) > 0.0) THEN
          PCNStem(I) = StemNTot(I) / STDM(I) * 100.  ! % N 
          RHOS(I) = STNSC(I) / STDM(I)
        ELSE
          PCNStem(I) = 0.0
          RHOS(I) = 0.0
        ENDIF

!!        Composition
!         LFLIGNIN = function of ??
!         LFCELLUL = function of ??
!         LFHEMICEL = function of ??
!         ADF = function of LFLIGNIN, LFCELLUL, LFHEMICEL
!         NDF = function of LFLIGNIN, LFCELLUL, LFHEMICEL
      ENDDO

!     Total rates over all leaf cohorts
      CRUSLF_calc = SUM(LFCMN(1:LCMax))      !CH2O mined in VEGGR
      NRUSLF_calc = SUM(LFNMN(1:LCMax)) / 0.16 !N mined in MOBIL
      WLFDOT_calc = SUM(LFFRZ(1:LCMax))      !Freeze in FREEZE
      SLDOT_calc  = SUM(LeafTotSen(1:LCMax)) !Total senes in SENES
      WatSen_calc = SUM(LFWSSN(1:LCMax))     !Water senes in SENES
      LfMineSen_calc = SUM(LFNMNSN(1:LCMax)) !N mining senes 
      LCADD_calc  = SUM(LFCAD(1:LCMax))      !mobile CH2O in GROW
      LNADD_calc  = SUM(LFNAD(1:LCMax))      !mobile N in GROW
      NLOFF_calc  = SUM(NLOFF_c(1:LCMax))    !N loss senes,freez,pest
      NLDOT_calc  = SUM(NLDOT_c(1:LCMax)) + NGRLF !Total N added today
      FHLEAF_calc = SUM(FHLEAF_c)            !harvested
      WRCLDT_calc = SUM(WRCLDT_c)            !change to mobile CH2O

!     Total states over all leaf cohorts
      WTLF_calc  = SUM(LFDM(1:LCMax))     !Leaf mass g/m2
      AREALF_calc= SUM(LFAREA(1:LCMax))   !Lf area index
      WCRLF_calc = SUM(LFNSC(1:LCMax))    !CH2O reserves
      WTNLF_calc = SUM(LeafNTot(1:LCMax)) !Leaf N
      WNRLF_calc = SUM(LFNSN(1:LCMax))    !Non-structural N
      LFSN_calc  = SUM(LFSN(1:LCMax))     !Structural N

!------------------------------------
!     Total rates over all stem cohorts
      CRUSST_calc = SUM(STCMN(1:LCMax))      !CH2O mined in VEGGR
      NRUSST_calc = SUM(STNMN(1:LCMax)) / 0.16 !N mined in MOBIL
      WSFDOT_calc = SUM(STFRZ(1:LCMax))      !Freeze in FREEZE
      SSDOT_calc  = SUM(StemTotSen(1:LCMax)) !Total senes in SENES
      WatSenStem_calc = SUM(STWSSN(1:LCMax)) !Water senes in SENES
      StMineSen_calc = SUM(STNMNSN(1:LCMax)) !N mining senes 
      SCADD_calc  = SUM(STCAD(1:LCMax))      !mobile CH2O in GROW
      SNADD_calc  = SUM(STNAD(1:LCMax))      !mobile N in GROW
      NSOFF_calc  = SUM(NSOFF_c(1:LCMax))    !N loss senes,freez,pest
      NSDOT_calc  = SUM(NSDOT_c(1:LCMax)) + NGRST !Total N added today
      FHSTEM_calc = SUM(FHSTEM_c)            !harvested

!     Total states over all stem cohorts
      WTST_calc  = SUM(STDM(1:LCMax))     !Stem mass g/m2
      WCRST_calc = SUM(STNSC(1:LCMax))    !CH2O reserves
      WTNST_calc = SUM(StemNTot(1:LCMax)) !Stem N
      WNRST_calc = SUM(STNSN(1:LCMax))    !Non-structural N
      STSN_calc  = SUM(STSN(1:LCMax))     !Structural N
!------------------------------------

      IF (WTLF_calc > 0.0) THEN
        PLEAFN_calc = WTNLF_calc / WTLF_calc * 100.
      ELSE
        PLEAFN_calc = 0.0
      ENDIF

      IF (STMWT > 0.0) THEN
        PStemN_calc = WTNST_calc / STMWT * 100.
      ELSE
        PStemN_calc = 0.0
      ENDIF

!     Export a single SLA, XLAI, XHLAI, LAIMX for all leaves
      XLAI_calc  = AREALF_calc / 10000. !Leaf area index (m2/m2)
      IF (WTLF_calc > 0.0) THEN
        SLA_calc    = AREALF_calc / WTLF_calc
        SLAAD_calc  = AREALF_calc / (WTLF_calc - WCRLF_calc)
      ELSE
        SLA_calc    = 0.0
        SLAAD_calc  = 0.0
      ENDIF
      LAIMX_calc = MAX(LAIMX_calc, XLAI_calc)

!***********************************************************************
!***********************************************************************
!     OUTPUT section 
!***********************************************************************
!-----------------------------------------------------------------------
      ELSE IF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      CALL OpCohorts(DYNAMIC, YRPLT, 
!       Leaf output:
     &  WTLF_calc, XLAI_calc, WCRLF_calc, PLEAFN_calc, 
     &  WTNLF_calc, WNRLF_calc, LFSN_calc, 
     &  WLDOT_calc, LCADD_calc, LNADD_calc, 
     &  CRUSLF_calc, NRUSLF_calc, 
     &  WLIDOT_calc, WLFDOT_calc, SLDOT_calc, 
     &  WatSen_calc, LfMineSen_calc, 
     &  NLOFF_calc, NLDOT_calc, FHLEAF_calc,
     &  (WRCLDT_calc + WLDOTN * ALPHL),
!       Stem output:
     &  WTST_calc, WCRST_calc, PStemN_calc,
     &  WTNST_calc, WNRST_calc, STSN_calc, 
     &  WSDOT_calc, SCADD_calc, SNADD_calc, 
     &  CRUSST_calc, NRUSST_calc, 
     &  WSIDOT_calc, WSFDOT_calc, SSDOT_calc, 
     &  WatSenStem_calc, STMineSen_calc, 
     &  NSOFF_calc, NSDOT_calc, FHSTEM_calc)

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

