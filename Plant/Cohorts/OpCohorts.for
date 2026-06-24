C=======================================================================
C  OpCohorts, Subroutine, CHPorter
C-----------------------------------------------------------------------
C  Daily output for leaf and stem cohorts
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  06/07/2026 CHP written
C=======================================================================

      SUBROUTINE OpCohorts(DYNAMIC, YRPLT, 
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

      USE ModuleDefs
      USE ModuleData
      USE COHORTS_MOD
      IMPLICIT NONE
      SAVE
      EXTERNAL YR_DOY, GETLUN, HEADER, TIMDIF

      INTEGER, INTENT(IN) :: DYNAMIC, YRPLT

      REAL, INTENT(IN) :: WTLF_calc, WNRLF_calc, WCRLF_calc, XLAI_calc, 
     &  WTNLF_calc, PLEAFN_calc,
     &  WLDOT_calc, SLDOT_calc, WLFDOT_calc, NRUSLF_calc, 
     &  CRUSLF_calc, WLIDOT_calc, WatSen_calc, LfMineSen_calc, 
     &  LCADD_calc, LNADD_calc, NLDOT_calc, NLOFF_calc,
     &  LFSN_calc, FHLEAF_calc, WRCLDT_calc

      REAL, INTENT(IN) ::  WTST_calc, WNRST_calc, WCRST_calc, 
     &  WTNST_calc, PStemN_calc, 
     &  WSDOT_calc, SSDOT_calc, WSFDOT_calc, NRUSST_calc, 
     &  CRUSST_calc, WSIDOT_calc, WatSenStem_calc, STMineSen_calc, 
     &  SCADD_calc, SNADD_calc, NSDOT_calc, NSOFF_calc,
     &  STSN_calc, FHSTEM_calc

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
     &  '      WLDOTc      LCADDc      LNADDc',
     &  '     CRUSLFc     NRUSLFc',
     &  '     WLIDOTc     WLFDOTc      SLDOTc',
     &  '      WatSen      NMinSn',
     &  '      NLOFFc      NLDOTc     FHLEAFc',
     &  '     WRCLDTc')

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
     &  '      WSDOTc      SCADDc      SNADDc',
     &  '     CRUSSTc     NRUSSTc',
     &  '     WSIDOTc     WSFDOTc      SSDOTc',
     &  '    WatSenST      NMinSn',
     &  '      NSOFFc      NSDOTc     FHSTEMc')

!***********************************************************************
!***********************************************************************
!     OUTPUT section 
!***********************************************************************
!-----------------------------------------------------------------------
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      WRITE (LCLUN,310) YEAR, DOY, DAS, DAP,
!       State
     &  WTLF_calc, XLAI_calc, WCRLF_calc, PLEAFN_calc, 
     &  WTNLF_calc, WNRLF_calc, LFSN_calc, 
!       Rate
     &  WLDOT_calc, LCADD_calc, LNADD_calc / 0.16, 
     &  CRUSLF_calc, NRUSLF_calc, 
     &  WLIDOT_calc, WLFDOT_calc, SLDOT_calc, 
     &  WatSen_calc, LfMineSen_calc, 
     &  NLOFF_calc, NLDOT_calc, FHLEAF_calc, WRCLDT_calc

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
     &  WSDOT_calc, SCADD_calc, SNADD_calc / 0.16, 
     &  CRUSST_calc, NRUSST_calc, 
     &  WSIDOT_calc, WSFDOT_calc, SSDOT_calc, 
     &  WatSenStem_calc, StMineSen_calc, 
     &  NsOFF_calc, NsDOT_calc, FHSTEM_calc

410   FORMAT (1X,I4, 1X,I3, 2I6, 30F12.6)

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

