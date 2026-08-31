C=======================================================================
C  OpCohorts, Subroutine, CHPorter
C-----------------------------------------------------------------------
C  Daily output for leaf and stem cohorts
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  06/07/2026 CHP written
C=======================================================================

      SUBROUTINE OpCohorts(DYNAMIC, YRPLT,    !Input
!     &  LFSN, LFAREA, LeafNTot, PCNLeaf, 
     &  WRCLDT_c, WRCSDT_c,   !TEMP CHP
     &  WLDOTN, ALPHL, WSDOTN, ALPHS,  !TEMP CHP
     &  WLDOT_CALC, WSDOT_CALC, !TEMP CHP
     &  LFWT_MIN, LFWT_MAX, STMWT_MIN, STMWT_MAX, !TEMP CHP
     &  LAIMX, SLA, SLAAD, XLAI,              !Output
     &  WTLF, WCRLF, WNRLF, WTNLF,            !Output
     &  STMWT, WCRST, WNRST, WTNST)           !Output

      USE ModuleDefs
      USE ModuleData
      USE COHORTS_MOD
      IMPLICIT NONE
      SAVE
      EXTERNAL YR_DOY, GETLUN, HEADER, TIMDIF, CohortComp

      INTEGER, INTENT(IN) :: DYNAMIC, YRPLT

      REAL, INTENT(OUT) :: SLA, SLAAD, LAIMX
      REAL, INTENT(OUT) :: WTLF, WCRLF, WNRLF, WTNLF, XLAI
      REAL, INTENT(OUT) :: STMWT, WCRST, WNRST, WTNST

!     TEMP CHP
      REAL, DIMENSION(1:LCMax), INTENT(IN) :: 
!     &  LFSN, LFAREA, LeafNTot, PCNLeaf
     & WRCLDT_c, WRCSDT_c   !TEMP CHP
      REAL WRCLDT_calc, WRCSDT_calc

      REAL WTLF_calc, WNRLF_calc, WCRLF_calc, XLAI_calc, 
     &  WTNLF_calc, PLEAFN_calc,
     &  SLDOT_calc, WLFDOT_calc, NRUSLF_calc, 
     &  CRUSLF_calc, WLIDOT_calc, WatSen_calc, LfMineSen_calc, 
     &  LCADD_calc, LNADD_calc, SLAAD_calc, LAIMX_calc, SLA_calc,
     &  LFSN_calc, FHLEAF_calc

      REAL  WTST_calc, WNRST_calc, WCRST_calc, 
     &  WTNST_calc, PStemN_calc, 
     &  SSDOT_calc, WSFDOT_calc, NRUSST_calc, 
     &  CRUSST_calc, WSIDOT_calc, WatSenStem_calc, STMineSen_calc, 
     &  SCADD_calc, SNADD_calc, 
     &  STSN_calc, FHSTEM_calc

      REAL AREALF_calc

!     TEMP CHP
      REAL RHOL_CALC, RHOL_MIN, RHOL_MAX, RHOS_CALC, RHOS_MIN, RHOS_MAX
      REAL WLDOTN, ALPHL, WSDOTN, ALPHS, WLDOT_calc, WSDOT_calc
      REAL LFWT_MAX, LFWT_MIN
      REAL STMWT_MAX, STMWT_MIN

      CHARACTER (len=8) MODEL
      CHARACTER*15 LCOUT, SCOUT
      CHARACTER*16 LCOUT1, LCOUT2
      LOGICAL FEXIST

      INTEGER YRDOY, YEAR, DOY, DAS, DAP, TIMDIF
      INTEGER I, ERRNUM
      INTEGER LCLUN, LCLUN1, LCLUN2
      INTEGER SCLUN

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
      LCOUT = 'LeafCohorts.OUT'
      CALL GETLUN('LCOUT',  LCLUN)

      LCOUT1 = "LeafCohorts1.OUT"
      CALL GETLUN('LCOUT1', LCLUN1)

      LCOUT2 = "LeafCohorts2.OUT"
      CALL GETLUN('LCOUT2', LCLUN2)

      SCOUT = 'StemCohorts.OUT'
      CALL GETLUN('STCOHO', SCLUN)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      WTLF_calc = 0.0  ; WTST_calc = 0.0
      WNRLF_calc = 0.0 ; WNRST_calc = 0.0
      WCRLF_calc = 0.0 ; WCRST_calc = 0.0
      XLAI_calc = 0.0
      WTNLF_calc = 0.0 ; WTNST_calc = 0.0

!     Initialize main leaf cohort output file
!     Contains daily aggregated leaf variables from cohort model for
!       comparison to GROW.OUT or grow_for.OUT files.
      INQUIRE (FILE = LCOUT, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LCLUN, FILE = LCOUT, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = LCLUN, FILE = LCOUT, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(LCLUN,'("*Leaf cohort output file")')
      ENDIF

!     Write headers
      CALL HEADER(SEASINIT, LCLUN, CONTROL % RUN)
      WRITE (LCLUN,200)
  200 FORMAT('@YEAR DOY   DAS   DAP',
     &  '       LWADc       LAIDc      WCRLFc      LeafNc',
     &  '      WTNLFc      WNRLFc       LFSNc',
     &  '      LCADDc      LNADDc',
     &  '     CRUSLFc     NRUSLFc',
     &  '     WLIDOTc     WLFDOTc      SLDOTc',
     &  '      WatSen      NMinSn',
     &  '     FHLEAFc       RHOLc      RHOLmn      RHOLmx',
     &  '     WRCLDTc      WLDOTc',
     &  '      LFMINc      LFMAXc')

!-----------------------------------------------------------------------
!     Initialize 2nd leaf cohort output file
      INQUIRE (FILE = LCOUT1, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LCLUN1, FILE = LCOUT1, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = LCLUN1, FILE = LCOUT1, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(LCLUN1,'("*COHORT OUTPUT FILE1")') 
      ENDIF

!-----------------------------------------------------------------------
!     Initialize 3rd leaf cohort output file
!     Contains daily leaf mass for first 50 cohorts.
      INQUIRE (FILE = LCOUT2, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LCLUN2, FILE = LCOUT2, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = LCLUN2, FILE = LCOUT2, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(LCLUN2,'(A)') "*COHORT OUTPUT FILE2"
      ENDIF
      WRITE(LCLUN2,'("@YEAR DOY",50(A7,I2.2))') ((" LFWT",I),I=1,50)

!-----------------------------------------------------------------------
!     Initialize main stem cohort output file
!     Contains daily aggregated leaf variables from cohort model for
!       comparison to GROW.OUT or grow_for.OUT files.
      INQUIRE (FILE = SCOUT, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = SCLUN, FILE = SCOUT, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = SCLUN, FILE = SCOUT, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(SCLUN,'("*Stem cohort output file")')
      ENDIF

!     Write headers
      CALL HEADER(SEASINIT, SCLUN, CONTROL % RUN)
      WRITE (SCLUN,210)
  210 FORMAT('@YEAR DOY   DAS   DAP',
     &  '      STMWTc      WCRSTc      StemNc',
     &  '      WTNSTc      WNRSTc       STSNc',
     &  '      SCADDc      SNADDc',
     &  '     CRUSSTc     NRUSSTc',
     &  '     WSIDOTc     WSFDOTc      SSDOTc',
     &  '    WatSenST      NMinSn',
     &  '     FHSTEMc       RHOSc      RHOSmn      RHOSmx',
     &  '     WRCSDTc      WSDOTc',
     &  '      STMINc      STMAXc')

!***********************************************************************
!***********************************************************************
!     OUTPUT section 
!***********************************************************************
!-----------------------------------------------------------------------
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
!     Total rates over all leaf cohorts
      CRUSLF_calc = SUM(LFCMN)        !CH2O mined in VEGGR
      NRUSLF_calc = SUM(LFNMN) / 0.16 !N mined in MOBIL
      WLFDOT_calc = SUM(LFFRZ)        !Freeze in FREEZE
      SLDOT_calc  = SUM(LeafTotSen)   !Total senes in SENES
      WatSen_calc = SUM(LFWSSN)       !Water senes in SENES
      LfMineSen_calc = SUM(LFNMNSN)   !N mining senes 
      LCADD_calc  = SUM(LFCAD)        !mobile CH2O in GROW
      LNADD_calc  = SUM(LFNAD)        !mobile N in GROW
      FHLEAF_calc = SUM(FHLEAF_c)     !harvested
      WRCLDT_calc = SUM(WRCLDT_c) + WLDOTN * ALPHL

!     Total states over all leaf cohorts
      WTLF_calc  = SUM(LFDM)          !Leaf mass g/m2
      AREALF_calc= SUM(LFAREA)        !Lf area index
      WCRLF_calc = SUM(LFNSC)         !CH2O reserves
      WTNLF_calc = SUM(LeafNTot)      !Leaf N
      WNRLF_calc = SUM(LFNSN)         !Non-structural N
      LFSN_calc  = SUM(LFSN)          !Structural N

!------------------------------------
!     Total rates over all stem cohorts
      CRUSST_calc = SUM(STCMN)        !CH2O mined in VEGGR
      NRUSST_calc = SUM(STNMN) / 0.16 !N mined in MOBIL
      WSFDOT_calc = SUM(STFRZ)        !Freeze in FREEZE
      SSDOT_calc  = SUM(StemTotSen)   !Total senes in SENES
      WatSenStem_calc = SUM(STWSSN)   !Water senes in SENES
      StMineSen_calc = SUM(STNMNSN)   !N mining senes 
      SCADD_calc  = SUM(STCAD)        !mobile CH2O in GROW
      SNADD_calc  = SUM(STNAD)        !mobile N in GROW
      FHSTEM_calc = SUM(FHSTEM_c)     !harvested
      WRCSDT_calc = SUM(WRCSDT_c) + WSDOTN * ALPHS

!     Total states over all stem cohorts
      WTST_calc  = SUM(STDM)          !Stem mass g/m2
      WCRST_calc = SUM(STNSC)         !CH2O reserves
      WTNST_calc = SUM(StemNTot)      !Stem N
      WNRST_calc = SUM(STNSN)         !Non-structural N
      STSN_calc  = SUM(STSN)          !Structural N
!------------------------------------

      IF (WTLF_calc > 0.0) THEN
        PLEAFN_calc = WTNLF_calc / WTLF_calc * 100.
      ELSE
        PLEAFN_calc = 0.0
      ENDIF

      IF (WTST_calc > 0.0) THEN
        PStemN_calc = WTNST_calc / WTST_calc * 100.
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

      CALL CohortComp()

!     TEMP CHP
      if (wtlf_calc > 0.0) then
        RHOL_CALC = WCRLF_calc / WTLF_calc
      else
        RHOL_CALC = 0.0
      endif
      RHOL_MIN = RHOL_c(1)
      RHOL_MAX = RHOL_c(1)

      if (wtst_calc > 0.0) then
        RHOS_CALC = WCRST_calc / WTST_calc
      else
        RHOS_CALC = 0.0
      endif
      RHOS_MIN = RHOS_c(1)
      RHOS_MAX = RHOS_c(1)
      DO I = 2, NLC
        RHOL_MIN = MIN(RHOL_c(I), RHOL_MIN)
        RHOL_MAX = MAX(RHOL_c(I), RHOL_MAX)
        RHOS_MIN = MIN(RHOS_c(I), RHOS_MIN)
        RHOS_MAX = MAX(RHOS_c(I), RHOS_MAX)
      ENDDO

      WRITE (LCLUN,310) YEAR, DOY, DAS, DAP,
!       State
     &  WTLF_calc, XLAI_calc, WCRLF_calc, PLEAFN_calc, 
     &  WTNLF_calc, WNRLF_calc, LFSN_calc, 
!       Rate
     &  LCADD_calc, LNADD_calc / 0.16, 
     &  CRUSLF_calc, NRUSLF_calc, 
     &  WLIDOT_calc, WLFDOT_calc, SLDOT_calc, 
     &  WatSen_calc, LfMineSen_calc, 
     &  FHLEAF_calc, RHOL_CALC, RHOL_MIN, RHOL_MAX, WRCLDT_calc,
     &  WLDOT_calc, LFWT_MIN, LFWT_MAX

310   FORMAT (1X,I4, 1X,I3, 2I6, 30F12.6)

      write (LCLUN1,320) YEAR,DOY,CohortAge(1:50)
320   format (1X,I4,1X,I3,50F6.1)

      write (LCLUN2,330) YEAR,DOY,LFDM(1:50)
330   format (1X,I4,1X,I3,50F7.2)

      WRITE (SCLUN,410) YEAR, DOY, DAS, DAP,
!       State
     &  WTST_calc, WCRST_calc, PStemN_calc, 
     &  WTNST_calc, WNRST_calc, STSN_calc, 
!       Rate
     &  SCADD_calc, SNADD_calc / 0.16, 
     &  CRUSST_calc, NRUSST_calc, 
     &  WSIDOT_calc, WSFDOT_calc, SSDOT_calc, 
     &  WatSenStem_calc, StMineSen_calc, 
     &  FHSTEM_calc, RHOS_CALC, RHOS_MIN, RHOS_MAX, WRCSDT_calc,
     &  WSDOT_calc, STMWT_MIN, STMWT_MAX

410   FORMAT (1X,I4, 1X,I3, 2I6, 30F12.6)

!     Transfer cohort totals for output
      WTLF  = WTLF_calc
      WCRLF = WCRLF_calc
      WNRLF = WNRLF_calc
      WTNLF = WTNLF_calc
      XLAI  = XLAI_calc
      STMWT = WTST_calc
      WCRST = WCRST_calc
      WNRST = WNRST_calc
      WTNST = WTNST_calc
      LAIMX = LAIMX_calc
      SLAAD = SLAAD_calc
      SLA   = SLA_calc

!***********************************************************************
!***********************************************************************
!     SEASON END section
!***********************************************************************
!-----------------------------------------------------------------------
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      CLOSE(LCLUN)
      CLOSE(LCLUN1)
      CLOSE(LCLUN2)
      CLOSE(SCLUN)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE OpCohorts
!=======================================================================

