C=======================================================================
      MODULE COHORTS_MOD
C=======================================================================
      INTEGER, PARAMETER :: LCMax = 1000 !maximum number of leaf cohorts
      INTEGER NLC     !current number of leaf cohorts

!     Leaf cohort state variables used in other routines
      REAL, DIMENSION(LCMax) ::  
     &  LFDM,         !Leaf dry matter (g[leaf]/m2) = WTLF
     &  CumLeafDM,    !Cumulative leaf growth (g[leaf]/m2) = CLW
     &  CohortAge,    !Leaf age for (thermal days)
     &  LFNSC,        !Leaf non-structural (mobile) CH2O (g/m2) = WCRLF
     &  LFNSN         !Leaf non-structural (mobile) N (g/m2) = WNRLF

!     Leaf cohort processes, calculated by other routines
      REAL, DIMENSION(LCMax) ::  

!       calculated in FREEZE
     &  LFFRZ,      !leaf mass frozen today (g[leaf]/m2) = WLFDOT

!       calculated in VEGGR
     &  LFCMN,      !leaf non-struc CH2O mined (g[CH2O]/m2) = CRUSLF

!       Calculated LeafCohortPest
     &  LFPST,      !leaf pest damage today (g[leaf]/m2) = WLIDOT

!       calculated in MOBIL
     &  LFNMN,      !Leaf non-struc N mined today (g[N]]/m2) = NRUSLF

!       calculated in SENES
     &  LeafTotSen, !Total leaf senescense today (g[leaf]/m2) = SLDOT
     &  LFNMNSN,    !Leaf senescence due to N mining (g[leaf]/m2)
     &  LFWSSN,     !leaf water stress senescence today (g[leaf]/m2)

!       calculated in GROW, adjusted in COHORTS
     &  LFCAD,      !leaf non-struc CH2O stored  (g[CH2O]/m2) = CADLF
     &  LFNAD       !Leaf non-struc N stored today (g[N]]/m2) = NADLF


      CONTAINS
C=======================================================================
C  COHORTS, Subroutine, K.J. Boote, P. Alderman
C-----------------------------------------------------------------------
C  Daily leaf cohorts
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1853 (?) KJB, PA Written
C  11/--/2025 GH, CHP  Revised.
C=======================================================================

      SUBROUTINE COHORTS(DYNAMIC, 
     &  DTX, F, FILECC, NGRLF,                !Input
     &  WLDOTN,                               !Input
     &  YRPLT,                                !Input
     &  WTLF, WCRLF, WNRLF, WTNLF, XLAI)      !OUTPUT (eventually)

      USE ModuleData
      IMPLICIT NONE
      SAVE
      EXTERNAL YR_DOY, GETLUN, HEADER, TIMDIF

      REAL, INTENT(IN) :: DTX, F, NGRLF, WLDOTN
      INTEGER, INTENT(IN) :: YRPLT
      CHARACTER*92, INTENT(IN) :: FILECC

!     Eventually, these will be output variables.
      REAL, INTENT(IN) :: WTLF, WCRLF, WNRLF, WTNLF, XLAI
      REAL AREALF_calc, SLA_calc, SLAAD_calc, LAIMX_calc
C-GH 08/19/2025
      REAL WTLF_calc, WNRLF_calc, WCRLF_calc, XLAI_calc, 
     &     WTNLF_calc, PLEAFN_calc
CHP 2025-11-20
      REAL WLDOT_calc, SLDOT_calc, WLFDOT_calc, NRUSLF_calc, 
     &  CRUSLF_calc, WLIDOT_calc, WatSen_calc, LfMineSen_calc, 
     &  LCADD_calc, LNADD_calc, NLDOT_calc, NLOFF_calc,
     &  LFSN_calc

      CHARACTER*11 COHORTOUT
      character*12 COHORTOUT1, COHORTOUT2
      LOGICAL FEXIST

      INTEGER DYNAMIC
      INTEGER YRDOY, YEAR, DOY, DAS, DAP, TIMDIF
      INTEGER I, ERRNUM
      INTEGER CHRTOUT, CHRTOUT1, CHRTOUT2

!     Cohort state variables, not exported (yet?)
      REAL, DIMENSION(LCMax) :: 
     &  LeafNTot,     !Leaf N total (g[N]]/m2) = WTNLF
     &  LFSN,         !Leaf structural (non-mobile) N (g[N]]/m2)
     &  PCNLeaf,      !Leaf N %
     &  LFAREA,       !Leaf area (cm2[leaf]/m2)
     &  LFAREAH,      !Healthy leaf area (cm2[leaf]/m2)
     &  LFSLA         !Specific leaf area (cm2/g)
!!       Composition and quality
!     &  LFLIGNIN,     !Lignin content %
!     &  LFCELLUL,     !Cellulose content %
!     &  LFHEMICEL     !Hemicellulose content %

      REAL, DIMENSION(LCMax) :: LeafMassDecrease, NLDOT_c, 
     &    NLOFF_c

      REAL CUMLFDM, Excess, SenFrac, WLDOT_cohort, Loss_adjust

!     Variables read from species file:
      REAL ALPHL, PROLFF
!     These variables are no longer used in this routine.
!     Need to remove from species file read routine.
      REAL NVSMOB, SENDAY, TCMP
      REAL XSENMX(4),SENMAX(4)
!     Not currently used, but will be needed for shading 
!     Keep here or move to MOBIL?
      REAL ICMP, MAXNMINE, NMOBMX

!     Date info for output files
      TYPE (ControlType) CONTROL
      CALL GET (CONTROL)
      DAS   = CONTROL % DAS
      YRDOY = CONTROL % YRDOY
      CALL YR_DOY(YRDOY, YEAR, DOY) 
      DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
      IF (DAP > DAS) DAP = 0

!***********************************************************************
!***********************************************************************
!     Run initialization - run once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
      COHORTOUT = 'COHORTS.OUT'
      CALL GETLUN('COHORTOUT',  CHRTOUT)

      COHORTOUT1 = "COHORTS1.OUT"
      CALL GETLUN('COHORTOUT1', CHRTOUT1)

      COHORTOUT2 = "COHORTS2.OUT"
      CALL GETLUN('COHORTOUT2', CHRTOUT2)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      NLC       = 0   !Number of leaf cohorts
      CohortAge = 0.0 !Leaf age for (thermal days)
      LFDM      = 0.0 !Leaf dry matter (g[leaf]/m2) = WTLF
      CumLeafDM = 0.0 !Cumulative leaf growth (g[leaf]/m2) = CLW
      LFNSC     = 0.0 !Leaf non-structural (mobile) CH2O (g/m2) = WCRLF
      LeafNTot  = 0.0 !Leaf N total (g[N]]/m2) = WTNLF
      LFNSN     = 0.0 !Leaf non-structural (mobile) N (g/m2) = WNRLF
      LFSN      = 0.0 !Leaf structural (non-mobile) N (g[N]]/m2)
      LFAREA    = 0.0 !Leaf area (cm2[leaf]/m2)
      LFAREAH   = 0.0 !healthy leaf area (cm2[leaf]/m2)

      CUMLFDM = 0.0 !Not used by could be compared with CumLeafDM

!      LFLIGNIN  = 0.0 !Lignin content %
!      LFCELLUL  = 0.0 !Cellulose content %
!      LFHEMICEL = 0.0 !Hemicellulose content %

C-GH 08/19/2025
      WTLF_calc = 0.0
      WNRLF_calc = 0.0
      WCRLF_calc = 0.0
      XLAI_calc = 0.0
      WTNLF_calc = 0.0

!     Read parameters from species file
      CALL IPCOHO(
     &  FILECC,                                 !Input
     &  ALPHL, ICMP, MAXNMINE, NMOBMX, NVSMOB,  !Output
     &  PROLFF, SENDAY, SENMAX, TCMP, XSENMX)   !Output

!     Initialize COHORT.OUT file
      INQUIRE (FILE = COHORTOUT, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = CHRTOUT, FILE = COHORTOUT, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = CHRTOUT, FILE = COHORTOUT, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(CHRTOUT,'("*Leaf cohort output file")')
      ENDIF

!     Write headers
      CALL HEADER(SEASINIT, CHRTOUT, CONTROL % RUN)
      WRITE (CHRTOUT,200)
  200 FORMAT('@YEAR DOY   DAS   DAP',
     &  '       LWADc       LAIDc      WCRLFc      LeafNc',
     &  '      WTNLFc      WNRLFc       LFSNc',
     &  '      WLDOTc      LCADDc      LNADDc',
     &  '     CRUSLFc     NRUSLFc',
     &  '     WLIDOTc     WLFDOTc      SLDOTc',
     &  '      WatSen      NMinSn',
     &  '      NLOFFc      NLDOTc',
     &  '       NGRLF      WLDOTN')

!     Initialize 2nd cohort output file
      INQUIRE (FILE = COHORTOUT1, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = CHRTOUT1, FILE = COHORTOUT1, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = CHRTOUT1, FILE = COHORTOUT1, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(CHRTOUT1,'("*COHORT OUTPUT FILE1")') 
      ENDIF

!     Initialize 3RD cohort output file
      INQUIRE (FILE = COHORTOUT2, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = CHRTOUT2, FILE = COHORTOUT2, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = CHRTOUT2, FILE = COHORTOUT2, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(CHRTOUT2,'(A19)') "*COHORT OUTPUT FILE2"
      ENDIF
      WRITE(CHRTOUT2,'("@YEAR DOY   LFWT            ")')

!***********************************************************************
!***********************************************************************
!     EMERGENCE CALCULATIONS - Performed once per season upon emergence
!         or transplanting of plants
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. EMERG) THEN
!-----------------------------------------------------------------------
!     Initialize first cohort upon emergence
!-------------------------------------
      NLC = 1                             !Number of leaf cohorts
      CohortAge(1) = DTX                  !age (ptd)
      LFDM(1)  = WLDOTN                   !dry matter (g/m2)
      CumLeafDM(1) = WLDOTN               !cumulative addition (g/m2)
      LFNSC(1) = WLDOTN * ALPHL           !mobile CH2O (g/m2)
      LeafNTot(1) = NGRLF                 !total leaf N (g/m2)
!     structural N (g/m2) based on structural C
      LFSN(1)  = PROLFF * 0.16 * (LFDM(1) - LFNSC(1))  
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

!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
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

!------------------
! LEAF LOSSES
!------------------      
!     Calculate the loss of leaf tissue per cohort
      DO I = 1, NLC
!       Leaf mass decrease (WLIDOT + WLFDOT + SLDOT in GROW)
        LeafMassDecrease(I) = LFFRZ(I) + LFPST(I) + LeafTotSen(I)

!       Check that loss is no greater than leaf cohort mass 
        IF (LeafMassDecrease(I) > LFDM(I)) THEN
          LeafMassDecrease(I) = LFDM(I)

!         Freeze damage occurs first
          IF (LFFRZ(I) > LFDM(I)) THEN
            LFFRZ(I) = LFDM(I)
            Excess = 0.0
          ELSE
            Excess = LFDM(I) - LFFRZ(I)
          ENDIF

!         Next, pests get their bit
          IF (LFPST(I) > Excess) THEN
            LFPST(I) = Excess
            Excess = 0.0
          ELSE
            Excess = Excess - LFPST(I)
          ENDIF

!         Anything leftover gets taken by senescence
          IF (Excess > 0.0) THEN
            SenFrac = Excess / LeafTotSen(I)
            LeafTotSen(I) = Excess
            LFNMNSN(I) = LFNMNSN(I) * SenFrac
            LFWSSN(I) = LFWSSN(I) * SenFrac
          ELSE
            LeafTotSen(I) = 0.0
            LFNMNSN(I) = 0.0   
            LFWSSN(I) = 0.0    
          ENDIF
        ENDIF
      ENDDO

!------------------
! LEAF ADDITIONS
!------------------      
!     Adjust new reserves to account for leaf losses just calculated.
      DO I = 1, NLC
        IF (LFDM(I) > 0.0) THEN
          Loss_adjust = (1. - MIN(1.0, LeafMassDecrease(I) / LFDM(I)))
          LFCAD(I) = LFCAD(I) * Loss_adjust
          LFNAD(I) = LFNAD(I) * Loss_adjust
        ENDIF
      ENDDO

!------------------
! LEAF N CHANGES
!------------------      
      NLOFF_c = 0.0
      NLDOT_c = 0.0
      DO I = 1, NLC
        IF (LFDM(I) .GT. 0.0) THEN
!         N loss due to senescence, freeze, pest
          NLOFF_c(I) = 
     &      + (LFWSSN(I) + LFPST(I) + LFFRZ(I)) * PCNLeaf(I) / 100.
     &      + (LeafTotSen(I) - LFWSSN(I)) * PROLFF * 0.16
          NLOFF_c(I) = MIN(NLOFF_c(I), LeafNTot(I))

!         Net N gain today for cohort I
          NLDOT_c(I) = - NLOFF_c(I) - LFNMN(I) + LFNAD(I) 
          NLDOT_c(I) = MAX(NLDOT_c(I), -LeafNTot(I))
        ENDIF
      ENDDO

!     Notes on leaf N changes, NLDOT_c:
!     - New N, NGRLF, is added to today's new cohort, not distributed 
!       to cohorts and so does not show up here.
!     - Leaf N mining: LFNMN in COHORTS ~ NRUSLF in GROW
!     - LFNAD (new N reserves) has already been reduced by freeze, 
!       pest, and senescence above.

!---------------------------------------------
! INTEGRATION OF ALL STATE VARIABLES
!---------------------------------------------
!     Calculate total change to leaf mass per cohort
      WLDOT_calc = WLDOTN
!     WLDOTN = total new growth today (added to new cohort below)

      DO I = 1, NLC
        IF (LFDM(I) .GT. 0.0) THEN
!     ---------------------------------------------------------
!         Leaf dry matter increase (WLDOT in GROW)
          WLDOT_cohort = 
     &        LFCAD(I)            !Reserve C = LCADD
     &      + LFNAD(I)/0.16       !Reserve N = LNADD
     &      - LFNMN(I)/0.16       !N mined = NRUSLF/0.16 
     &      - LFCMN(I)            !C mined = CRUSLF
     &      - LeafMassDecrease(I) !freez, pst, senes=SLDOT+WLIDOT+WLFDOT

!         Leaf dry matter (WTLF in GROW)
          LFDM(I) = LFDM(I) + WLDOT_cohort

!         Keep track of total leaf mass addition today
          WLDOT_calc = WLDOT_calc + WLDOT_cohort

!     ---------------------------------------------------------
!         Leaf area
!         For now, use whole leaf SLA for each cohort. But I think this
!           should be replaced by a cohort SLA when everything is working
!           as it is in GROW.
          LFAREA(I) = LFAREA(I) 
     &      - LeafMassDecrease(I) * SLA_calc
     &      - LFNMN(I) / 0.16 * SLA_calc
     &      + LFNAD(I) / 0.16 * SLA_calc

          LFSLA(I) = LFAREA(I) / LFDM(I)

!     ---------------------------------------------------------
!         Non-structural CH2O (~WCRLF in GROW)
          LFNSC(I) = LFNSC(I) 
     &      - LFCMN(I)      !~ CRUSLF, mined CH2O
     &      + LFCAD(I)      !new reserves
!             leaf mass losses:
     &      - LFNSC(I) / LFDM(I) * LeafMassDecrease(I)
          IF (LFNSC(I) < 0.0) THEN
            LFNSC(I) = 0.0
          ENDIF

!     ---------------------------------------------------------
!         Leaf N
          LeafNTot(I) = LeafNTot(I) + NLDOT_c(I) 

!         Structural N (WTNLF minus WNRLF in GROW)
          LFSN(I) = MIN(LeafNTot(I), PROLFF*0.16 * (LFDM(I) - LFNSC(I)))

!         Non-structural N (WNRLF in GROW)
          LFNSN(I) = LeafNTot(I) - LFSN(I)

!!        Composition
!         LFLIGNIN = function of ??
!         LFCELLUL = function of ??
!         LFHEMICEL = function of ??
!         ADF = function of LFLIGNIN, LFCELLUL, LFHEMICEL
!         NDF = function of LFLIGNIN, LFCELLUL, LFHEMICEL

        ENDIF
      ENDDO

!-------------------------------------------------------------------
!     Today's new cohort
!-------------------------------------------------------------------
      IF (WLDOTN > 0.0) THEN
        NLC = NLC + 1  !today's new cohort

!       New growth for today's cohort
        LFDM(NLC)  = WLDOTN             !leaf dry mass
        CumLeafDM(NLC) = WLDOTN         !cum leaf mass added
        LFAREA(NLC)= WLDOTN * F         !leaf area
        LFNSC(NLC) = WLDOTN * ALPHL     !non-struct CH2O

!       struct N (non-mobile):
        LFSN(NLC)  = PROLFF * 0.16 * (WLDOTN - LFNSC(NLC))  
        LFNSN(NLC) = NGRLF - LFSN(NLC)  !non-struct N (mobile)

!!        Composition
!         LFLIGNIN = function of ??
!         LFCELLUL = function of ??
!         LFHEMICEL = function of ??
      ENDIF

!-------------------------------------------------------------------
      DO I = 1, NLC
        CohortAge(I) = CohortAge(I) + DTX  !cohort age in p-t-d
        LeafNTot(I) = LFNSN(I) + LFSN(I)
        IF (LFDM(I) > 0.0) THEN
          PCNLeaf(I) = LeafNTot(I) / LFDM(I) * 100.  ! % N 
        ELSE
          PCNLeaf(I) = 0.0
        ENDIF
      ENDDO

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

!***********************************************************************
!***********************************************************************
!     Sum over all cohorts for SEASINIT and for INTEGR
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT .OR. DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!     Total rates over all cohorts
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

!------------------------------------
!     Total states over all cohorts
      WTLF_calc  = SUM(LFDM(1:LCMax))   !Leaf mass g/m2
      AREALF_calc= SUM(LFAREA(1:LCMax)) !Lf area (cm2[leaf]/m2[ground])
      WCRLF_calc = SUM(LFNSC(1:LCMax))  !CH2O reserves
      WNRLF_calc = SUM(LFNSN(1:LCMax))  !Non-structural N
      LFSN_calc  = SUM(LFSN(1:LCMax))   !Structural N

!     Leaf N is sum of structural and non-structural N
      WTNLF_calc = LFSN_calc + WNRLF_calc

      IF (WTLF_calc > 0.0) THEN
        PLEAFN_calc = WTNLF_calc / WTLF_calc * 100.
      ELSE
        PLEAFN_calc = 0.0
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
!     OUTPUT section - DO THIS FOR SEASINIT AND INTEGR (for now)
!***********************************************************************
!-----------------------------------------------------------------------
!     ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      WRITE (CHRTOUT,310) YEAR, DOY, DAS, DAP,
!       State
     &  WTLF_calc, XLAI_calc, WCRLF_calc, PLEAFN_calc, 
     &  WTNLF_calc, WNRLF_calc, LFSN_calc, 
!       Rate
     &  WLDOT_calc, LCADD_calc, LNADD_calc / 0.16, 
     &  CRUSLF_calc, NRUSLF_calc, 
     &  WLIDOT_calc, WLFDOT_calc, SLDOT_calc, 
     &  WatSen_calc, LfMineSen_calc, 
     &  NLOFF_calc, NLDOT_calc, NGRLF, WLDOTN

310   FORMAT (1X,I4, 1X,I3, 2I6, 30F12.6)

      write (CHRTOUT1,320) YEAR,DOY,CohortAge(1:50)
320   format (1X,I4,1X,I3,50F6.1)

      write (CHRTOUT2,330) YEAR,DOY,LFDM(1:50)
330   format (1X,I4,1X,I3,50F6.1)

!***********************************************************************
!***********************************************************************
!     SEASON END section
!***********************************************************************
!-----------------------------------------------------------------------
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      CLOSE(CHRTOUT)
      close(CHRTOUT1)
      close(CHRTOUT2)
      
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
!  LeafCohortPest, Subroutine, C.H. Porter
!-----------------------------------------------------------------------
!  Determines pest damage per leaf cohort.
!  This should eventually be moved to the pest module with the type of 
!     damage to cohorts specified in the pest coupling file.
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  03-24-2026 CHP Adapted from COHORTS
!=======================================================================
      SUBROUTINE LeafCohortPest(
     &  WLIDOT, WTLF)   !Input
!       Output is LFPST, which is available through the COHORTS_mod module
!-----------------------------------------------------------------------
      IMPLICIT NONE

      REAL WLIDOT_calc, WLIDOT, WTLF
      INTEGER I

      WLIDOT_calc = 0.0
      LFPST = 0.0

      IF (WLIDOT .GT. 0.0) THEN
! FOR PROPORTIONAL DISTRIBUTION OF PEST DAMAGE:
        DO I = 1, NLC
          IF (LFDM(I) .GT. 0.0) THEN
            LFPST(I) = LFDM(I) / WTLF * WLIDOT
            WLIDOT_calc = WLIDOT_calc + LFPST(I)
          ELSE
            LFPST(I) = 0.0
          ENDIF
        ENDDO

! FOR PEST DAMAGE TO AFFECT OLD TISSUE FIRST:
!        LFPSTDM = WLIDOT
!        DO I = 1, NLC
!          IF (LFDM(I) .GT. 0.0) THEN
!            LFPST(I) = MIN(LFDM(I), LFPSTDM)
!            LFPST(I) = MAX(LFPST(I), 0.0)
!            LFPSTDM = LFPSTDM - LFPST(I)
!          ENDIF
!        ENDDO

! FOR PEST DAMAGE TO AFFECT NEW TISSUE FIRST:
!        LFPSTDM = WLIDOT
!        DO I = NLC, 1, -1
!          IF (LFDM(I) .GT. 0.0) THEN
!            LFPST(I) = MIN(LFDM(I), LFPSTDM)
!            LFPST(I) = MAX(LFPST(I), 0.0)
!            LFPSTDM = LFPSTDM - LFPST(I)
!          ENDIF
!        ENDDO

      ELSE
!       No pest damage
        LFPST = 0.0
      ENDIF

!     from GROW:
! NEED TO HANDLE THIS FOR COHORTS.
!C-----------------------------------------------------------------------
!C     Calculate "Healthy" or Non-Diseased Leaf Area Index
!C-----------------------------------------------------------------------
!!     AREAH  = AREALF - 2. * DISLA
!!     KJB Remove 2. factor
!      AREAH  = AREALF - DISLA
!      AREAH  = MAX(0.,AREAH)
!      XHLAI  = AREAH / 10000.

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE LeafCohortPest
!=======================================================================


!=======================================================================
!  IPCOHO, Subroutine, C.H. Porter
!-----------------------------------------------------------------------
!  Reads input data for COHORTS subroutine
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  11/18/2025 CHP Adapted from IPDMND.
!=======================================================================
      SUBROUTINE IPCOHO(
     &  FILECC,                                 !Input
     &  ALPHL, ICMP, MAXNMINE, NMOBMX, NVSMOB,  !Output
     &  PROLFF, SENDAY, SENMAX, TCMP, XSENMX)   !Output

!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL GETLUN, ERROR, FIND, IGNORE, WARNING
!-----------------------------------------------------------------------
      CHARACTER*92, INTENT(IN) :: FILECC
      REAL, INTENT(OUT) :: ALPHL, ICMP, MAXNMINE, NMOBMX, 
     &     NVSMOB, PROLFF, SENDAY, TCMP
      REAL, INTENT(OUT) :: SENMAX(4), XSENMX(4)

      CHARACTER*6   ERRKEY
      PARAMETER (ERRKEY = 'IPCOHO')
      CHARACTER*6   SECTION
      CHARACTER*80  C80

      INTEGER LUNCRP,  ERR, LINC, LNUM, FOUND, ISECT
      INTEGER II

!-----------------------------------------------------------------------
!     Read in values from species file
!-----------------------------------------------------------------------
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      LNUM = 0
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!-----------------------------------------------------------------------
!    Find and Read Plant Composition Section
!-----------------------------------------------------------------------
      SECTION = '!*PLAN'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!       READ(C80,'(F6.0,6X,2F6.0,6X,F6.0)',IOSTAT=ERR)
!    &          PROLFI, PROLFF, PROSTI, PROSTF
        READ(C80,'(12X,F6.0)',IOSTAT=ERR) PROLFF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

!-----------------------------------------------------------------------
!    Find and Read Carbon and Nitrogen Mining Section
!-----------------------------------------------------------------------
      SECTION = '!*CARB'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!       READ(C80,'(18X,3F6.0)',IOSTAT=ERR) NMOBMX, NVSMOB, NRCVR
        READ(C80,'(18X,2F6.0)',IOSTAT=ERR) NMOBMX, NVSMOB
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  !Skip the next line
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(F6.0)',IOSTAT=ERR) ALPHL
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  
        READ(C80,'(F6.0)',IOSTAT=ERR) MAXNMINE
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

!-----------------------------------------------------------------------
!    Find and Read Senescence Section
!    NOTE: First search for Section finds '!*LEAF GROWTH PARAMETERS'
!          Second search finds '!*LEAF SENESCENCE FACTORS'
!-----------------------------------------------------------------------
      SECTION = '!*LEAF'
      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ENDIF

      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  
        READ(C80,'(12X,F6.0)',IOSTAT=ERR) SENDAY
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(2F6.0)',IOSTAT=ERR) ICMP, TCMP
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(24X,4F6.0)',IOSTAT=ERR)
     &      (XSENMX(II),II=1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(24X,4F6.0)',IOSTAT=ERR)
     &      (SENMAX(II),II=1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF

!-----------------------------------------------------------------------
      CLOSE(LUNCRP)

!-----------------------------------------------------------------------
      RETURN
!-----------------------------------------------------------------------
      END  SUBROUTINE IPCOHO
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
