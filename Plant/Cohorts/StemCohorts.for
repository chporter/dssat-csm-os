C=======================================================================
      MODULE StemCohorts_MOD
C=======================================================================
      INTEGER, PARAMETER :: SCMax = 10000 !maximum # of Stem cohorts
      INTEGER NSC     !current number of Stem cohorts

!     Stem cohort state variables used in other routines
      REAL, DIMENSION(SCMax) ::  
     &  STDM,         !Stem dry matter (g[stem]/m2) = WTLF
     &  CumStemDM,    !Cumulative stem growth (g[stem]/m2) = CLW
     &  StemCohortAge,!Stem age (thermal days)
     &  STNSC,        !Stem non-structural (mobile) CH2O (g/m2) = WCRLF
     &  STNSN         !Stem non-structural (mobile) N (g/m2) = WNRLF
!    &  PCNStem       !Stem N%

!     Stem cohort processes, calculated by other routines
      REAL, DIMENSION(SCMax) ::  

!       calculated in FREEZE, for_freeze
     &  STFRZ,      !Stem mass frozen today (g[stem]/m2) = WLFDOT

!       calculated in VEGGR, for_veggr
     &  STCMN,      !Stem non-struc CH2O mined (g[CH2O]/m2) = CRUSLF

!       Calculated StemCohortPest
     &  STPST,      !Stem pest damage today (g[stem]/m2) = WLIDOT

!       calculated in MOBIL, for_mobil, for_veggr
     &  STNMN,      !Stem non-struc N mined today (g[N]]/m2) = NRUSLF

!       calculated in SENES, for_senmob
     &  StemTotSen, !Total Stem senescense today (g[stem]/m2) = SLDOT
     &  STNMNSN,    !Stem senescence due to N mining (g[stem]/m2)
     &  STWSSN,     !Stem water stress senescence today (g[stem]/m2)

!       calculated in GROW and for_grow, adjusted in COHORTS
     &  STCAD,      !Stem non-struc CH2O stored  (g[CH2O]/m2) = CADLF
     &  STNAD,      !Stem non-struc N stored today (g[N]]/m2) = NADLF

!       calculated in for_senmob
     &  STCMINE_c,  !Max potential CH2O mining today
     &  STSNMOB_c,  !Stem N mobilized by natural senescence (g[N]/m2)
     &  STSEN_c,    !Low light senescence
!       is LFSENWT_c the same as STNMNSN?
     &  STSENWT_c,  !Stem senescence due to N mobilization
     &  STNSEN_c,   !natural senescence
     &  SSMDOT_c,   !Stem senescence with N mobilization 

!       calculated in for_harv
     &  FHSTEM_c    !Forage harvest

      CONTAINS
C=======================================================================
C  StemCohorts, Subroutine, C.H. Porter
C-----------------------------------------------------------------------
C  Daily stem cohorts
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  06/04/2026     CHP Written, based on LeafCohorts
C=======================================================================

      SUBROUTINE StemCohorts(DYNAMIC, 
     &  DTX, F, FILECC, NGRLF,                !Input
     &  WSDOTN,                               !Input
     &  YRPLT,                                !Input
     &  PCNStem,                              !Output
     &  WTLF, WCRLF, WNRLF, WTNLF, XLAI)      !OUTPUT (eventually)

      USE ModuleData
      IMPLICIT NONE
      SAVE
      EXTERNAL YR_DOY, GETLUN, HEADER, TIMDIF

      REAL, INTENT(IN) :: DTX, F, NGRLF, WSDOTN
      INTEGER, INTENT(IN) :: YRPLT
      CHARACTER*92, INTENT(IN) :: FILECC

      REAL, DIMENSION(SCMax) :: PCNStem

!     Eventually, these will be output variables.
      REAL, INTENT(IN) :: WTLF, WCRLF, WNRLF, WTNLF, XLAI
      REAL AREALF_calc, SLA_calc, SLAAD_calc, LAIMX_calc
C-GH 08/19/2025
      REAL WTLF_calc, WNRLF_calc, WCRLF_calc, XLAI_calc, 
     &     WTNLF_calc, PLEAFN_calc
CHP 2025-11-20
      REAL WSDOT_calc, SSDOT_calc, WLFDOT_calc, NRUSLF_calc, 
     &  CRUSLF_calc, WLIDOT_calc, WatSen_calc, LfMineSen_calc, 
     &  LCADD_calc, LNADD_calc, NLDOT_calc, NLOFF_calc,
     &  LFSN_calc

      CHARACTER (len=8) MODEL
      CHARACTER*15 COHORTOUT
      character*16 COHORTOUT1, COHORTOUT2
      LOGICAL FEXIST

      INTEGER DYNAMIC
      INTEGER YRDOY, YEAR, DOY, DAS, DAP, TIMDIF
      INTEGER I, ERRNUM
      INTEGER CHRTOUT, CHRTOUT1, CHRTOUT2

!     Cohort state variables, not exported (yet?)
      REAL, DIMENSION(SCMax) :: 
     &  LeafNTot,     !stem N total (g[N]]/m2) = WTNLF
     &  LFSN,         !stem structural (non-mobile) N (g[N]]/m2)
     &  LFAREA,       !stem area (cm2[stem]/m2)
     &  LFAREAH,      !Healthy stem area (cm2[stem]/m2)
     &  LFSLA         !Specific stem area (cm2/g)
!!       Composition and quality
!     &  LFLIGNIN,     !Lignin content %
!     &  LFCELLUL,     !Cellulose content %
!     &  LFHEMICEL     !Hemicellulose content %

      REAL, DIMENSION(SCMax) :: StemMassDecrease, NLDOT_c, 
     &    NLOFF_c, WRCLDT_c

      REAL CUMLFDM, Excess, SenFrac, WLDOT_cohort, Loss_adjust
      REAL RHOL, CLOFF

!     TEMP CHP
      REAL CLOFF_sum, LFNSEN_sum, LTSEN_sum, LFSENWT_sum, WLIDOT_sum

!     Variables read from species file:
      REAL ALPHL, PROLFF
!     These variables are no longer used in this routine.
!     Need to remove from species file read routine.
      REAL NVSMOB, SENDAY, TCMP
      REAL XSENMX(4),SENMAX(4)
!     Not currently used, but will be needed for shading 
!     Keep here or move to MOBIL?
      REAL ICMP, MAXNMINE, NMOBMX
      REAL SENCLV, SENNLV, PCHOLFF !forage species file

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
      COHORTOUT = 'StemCohorts.OUT'
      CALL GETLUN('STCOHO', CHRTOUT)

      COHORTOUT1 = "StemCohorts1.OUT"
      CALL GETLUN('STCOHO1', CHRTOUT1)

      COHORTOUT2 = "StemCohorts2.OUT"
      CALL GETLUN('STCOHO2', CHRTOUT2)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      NSC       = 0   !Number of stem cohorts
      StemCohortAge = 0.0 !stem age for (thermal days)
      STDM      = 0.0 !stem dry matter (g[stem]/m2) = WTLF
      CumStemDM = 0.0 !Cumulative stem growth (g[stem]/m2) = CLW
      STNSC     = 0.0 !stem non-structural (mobile) CH2O (g/m2) = WCRLF
      LeafNTot  = 0.0 !stem N total (g[N]]/m2) = WTNLF
      STNSN     = 0.0 !stem non-structural (mobile) N (g/m2) = WNRLF
      LFSN      = 0.0 !stem structural (non-mobile) N (g[N]]/m2)
      LFAREA    = 0.0 !stem area (cm2[stem]/m2)
      LFAREAH   = 0.0 !healthy stem area (cm2[stem]/m2)
      FHSTEM_c  = 0.0 !harvested stem mass

      CUMLFDM = 0.0 !Not used by could be compared with CumStemDM

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
     &  FILECC, MODEL,                          !Input
     &  ALPHL, ICMP, MAXNMINE, NMOBMX, NVSMOB,  !Output
     &  PCHOLFF, PROLFF, SENDAY, SENMAX,        !Output
     &  SENCLV, SENNLV, TCMP, XSENMX)           !Output

!     Initialize COHORT.OUT file
      INQUIRE (FILE = COHORTOUT, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = CHRTOUT, FILE = COHORTOUT, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = CHRTOUT, FILE = COHORTOUT, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(CHRTOUT,'("*stem cohort output file")')
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
     &  '       NGRLF      WSDOTN')

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
      NSC = 1                             !Number of stem cohorts
      StemCohortAge(1) = DTX                  !age (ptd)
      STDM(1)  = WSDOTN                   !dry matter (g/m2)
      CumStemDM(1) = WSDOTN               !cumulative addition (g/m2)
      STNSC(1) = WSDOTN * ALPHL           !mobile CH2O (g/m2)
      LeafNTot(1) = NGRLF                 !total stem N (g/m2)
!     structural N (g/m2) based on structural C
      LFSN(1)  = PROLFF * 0.16 * (STDM(1) - STNSC(1))  
      STNSN(1) = NGRLF - LFSN(1)          !mobile N (g/m2)
      IF (STDM(1) > 0.0) THEN
        PCNStem(1) = LeafNTot(1) / STDM(1) * 100.  ! % N
      ELSE
        PCNStem(1) = 0.0
      ENDIF

!!     Composition
!      LFLIGNIN  = function of ??
!      LFCELLUL  = function of ??
!      LFHEMICEL = function of ??

!     stem area
      LFAREA(1)  = WSDOTN * F              !stem area (cm2/m2)
      LFAREAH(1) = LFAREA(1)              !healthy stem area
      IF (STDM(1) > 0.0) THEN
        LFSLA(1)   = LFAREA(1) / STDM(1)
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
!      DO I=1,NSC
!        CUMAREA=CUMAREA+LFAREA(I)/10000
!        IF (CUMAREA/LCMP.GE.1)THEN
!          SHADEFAC(I)=CUMAREA/LCMP
!        ELSE
!          SHADEFAC(I)=1
!        ENDIF
!      ENDDO
!
! This is now done in MOBIL, but does not include shadefac  ************* <<<--- MOBIL

!    DO  I=1,NSC
!        IF (STNSN(I) .LE. 0.0 .OR. MAXNMINE .LE. 0.0 
!     &                        .OR. NMOBMX .LE. 0.0) THEN
!          STNMN(I)=0.0
!        ELSE
!          STNMN(I)=SHADEFAC(I)*(NMINER/NMOBMX)*MAXNMINE*STNSN(I)
!          STNMN(I)=MIN(STNMN(I),STNSN(I))
!          IF ((STNSN(I)-STNMN(I)).LE.0.00001)THEN
!            STNMN(I)=STNSN(I)
!          ENDIF
!        ENDIF
!     END DO

!------------------
! stem LOSSES
!------------------
!     This might be wrong (or unneccessary) for forages - need to check
      IF (SUM(FHSTEM_c) <= 0.0) THEN
!       Calculate the loss of stem tissue per cohort
        DO I = 1, NSC
!         stem mass decrease (SSDOT+WSIDOT+WSFDOT in GROW_FOR)
          StemMassDecrease(I) = STFRZ(I) + STPST(I) + StemTotSen(I)

!         Check that loss is no greater than stem cohort mass 
          IF (StemMassDecrease(I) > STDM(I)) THEN
            StemMassDecrease(I) = STDM(I)

!           Freeze damage occurs first
            IF (STFRZ(I) > STDM(I)) THEN
              STFRZ(I) = STDM(I)
              Excess = 0.0
            ELSE
              Excess = STDM(I) - STFRZ(I)
            ENDIF

!           Next, pests get their bit
            IF (STPST(I) > Excess) THEN
              STPST(I) = Excess
              Excess = 0.0
            ELSE
              Excess = Excess - STPST(I)
            ENDIF

!           Anything leftover gets taken by senescence
            IF (Excess > 0.0) THEN
              SenFrac = Excess / StemTotSen(I)
              StemTotSen(I) = Excess
              STNMNSN(I) = STNMNSN(I) * SenFrac
              STWSSN(I) = STWSSN(I) * SenFrac
            ELSE
              StemTotSen(I) = 0.0
              STNMNSN(I) = 0.0   
              STWSSN(I) = 0.0    
            ENDIF
          ENDIF
        ENDDO
      ELSE
!       On days with an interrim forage harvest, what should be done here?
      ENDIF

!------------------
! stem ADDITIONS
!------------------
!     Adjust new reserves to account for stem losses just calculated.
      DO I = 1, NSC
        SELECT CASE (MODEL(1:5))
        CASE ('CRGRO')
          IF (STDM(I) > 0.0) THEN
            Loss_adjust = (1. - MIN(1.0, StemMassDecrease(I) / STDM(I)))
          ENDIF

        CASE ('PRFRM')
          IF (STDM(I) -  StemTotSen(I) > 0.0) THEN
!           CHP: This is how it's done in for_grow, but I'm not convinced it's correct.
!           If it is correct, we should do the same thing for CRGRO 
            Loss_adjust = (1. - MIN(1.0, (STPST(I) + STFRZ(I)) / 
     &                                   (STDM(I) - StemTotSen(I))))
          ENDIF
        END SELECT

        STCAD(I) = STCAD(I) * Loss_adjust
        STNAD(I) = STNAD(I) * Loss_adjust
      ENDDO

!------------------
! stem CH2O CHANGES
!------------------
!     Non-structural CH2O (~WCRLF in GROW)
      WRCLDT_c = 0.0

      SELECT CASE (MODEL(1:5))
      CASE ('CRGRO')
        DO I = 1, NSC
          IF (STDM(I) > 0.) THEN
            WRCLDT_c(I) = 
     &        - STCMN(I)      !~ CRUSLF, mined CH2O
     &        + STCAD(I)      !new reserves
!               stem mass losses:
     &        - STNSC(I) / STDM(I) * StemMassDecrease(I)
          ENDIF
        ENDDO
        
      CASE ('PRFRM')
        CLOFF_sum = 0.0 !temp chp
        DO I = 1, NSC
          IF (STDM(I) > 0.) THEN
!           RHOL =  WCRLF/WTLF
            RHOL = STNSC(I) / STDM(I)
!           CLOFF = (SLMDOT + LTSEN + LFSENWT) *  
            CLOFF = (SLMDOT_c(I) + LTSEN_c(I) + LFSENWT_c(I)) * 
!    &              (SENCLV * (RHOL - PCHOLFF) + PCHOLFF) 
     &              (SENCLV * (RHOL - PCHOLFF) + PCHOLFF) 
!    &            + (SLNDOT + WLIDOT + WLFDOT) * RHOL
     &            + (STWSSN(I) + STPST(I) + STFRZ(I)) * RHOL

            IF (FHSTEM_c(I) .GT. 0.0) THEN
!             CLOFF = CLOFF + FHLEAF * RHOL
              CLOFF = CLOFF + FHSTEM_c(I) * RHOL
            ENDIF

            IF (CLOFF. LT. 0.0) CLOFF = 0.0

!           TEMP CHP
            CLOFF_SUM = CLOFF_SUM + CLOFF

!           WRCLDT=WSDOTN*ALPHL-CRUSLF-CLOFF
            WRCLDT_c(I) = -STCMN(I) - CLOFF

            IF (STDM(I) .GT. 0.0.AND.FHSTEM_c(I) .EQ. 0.0) THEN
!             WRCLDT = WRCLDT + CADLF - LFCADDM
              WRCLDT_c(I) = WRCLDT_c(I) + STCAD(I)
            ENDIF
          ENDIF
        ENDDO
      END SELECT

!------------------
! stem N CHANGES
!------------------
      NLOFF_c = 0.0
      NLDOT_c = 0.0

      SELECT CASE (MODEL(1:5))
      CASE ('CRGRO')
        DO I = 1, NSC
          IF (STDM(I) .GT. 0.0) THEN
!           N loss due to senescence, freeze, pest
            NLOFF_c(I) = 
     &        + (STWSSN(I) + STPST(I) + STFRZ(I)) * PCNStem(I) / 100.
     &        + (StemTotSen(I) - STWSSN(I)) * PROLFF * 0.16
            NLOFF_c(I) = MIN(NLOFF_c(I), LeafNTot(I))

!           Net N gain today for cohort I
            NLDOT_c(I) = - NLOFF_c(I) - STNMN(I) + STNAD(I) 
            NLDOT_c(I) = MAX(NLDOT_c(I), -LeafNTot(I))
          ENDIF
        ENDDO

      CASE ('PRFRM')
        DO I = 1, NSC
          IF (STDM(I) .GT. 0.0) THEN
!           NLOFF      = SLMDOT *    
            NLOFF_c(I) = LFNSEN_c(I) * 
!    &       (SENNLV * (PCNL/100 - PROLFF * 0.16) + PROLFF * 0.16) 
     &       (SENNLV * (PCNStem(I)/100. - PROLFF*0.16) + PROLFF*0.16)
!    &       + (LTSEN + LFSENWT) * PROLFF *0.16
     &       + (LTSEN_c(I) + LFSENWT_c(I)) * PROLFF *0.16
!    &       + (SLNDOT + WLIDOT + WLFDOT) * PCNL/100  
     &       + (STWSSN(I) + STPST(I) + STFRZ(I)) * PCNStem(I)/100.

            IF (FHSTEM_c(I) .GT. 0.0) THEN
!             Harvest event today
!             NLOFF = NLOFF + FHLEAF * PCNL/100.
              NLOFF_c(I) = NLOFF_c(I) + FHSTEM_c(I) * PCNStem(I)/100.
            ENDIF

            NLOFF_c(I) = MIN(NLOFF_c(I), LeafNTot(I))

!           Net N gain today for cohort I
!           NLDOT=NGRLF-NRUSLF-NLOFF
            NLDOT_c(I) = - NLOFF_c(I) - STNMN(I)
!           
!           IF (WTLF .GT. 0.0.AND.FHLEAF.EQ.0) THEN
            IF (STDM(I) > 0.0 .AND. FHSTEM_c(I) == 0.0) THEN
!             NLDOT = NLDOT + NADLF - LFNADDM
              NLDOT_c(I) = NLDOT_c(I) + STNAD(I)
            ENDIF

            NLDOT_c(I) = MAX(NLDOT_c(I), -LeafNTot(I))
          ENDIF
        ENDDO
      END SELECT

!     Notes on stem N changes, NLDOT_c:
!     - New N, NGRLF, is added to today's new cohort, not distributed 
!       to cohorts and so does not show up here.
!     - stem N mining: STNMN in COHORTS ~ NRUSLF in GROW
!     - STNAD (new N reserves) has already been reduced by freeze, 
!       pest, and senescence above.

!---------------------------------------------
! INTEGRATION OF ALL STATE VARIABLES
!---------------------------------------------
!     Calculate total change to stem mass per cohort
      WSDOT_calc = WSDOTN
!     WSDOTN = total new growth today (added to new cohort below)

!from grow_for.for
!     WSDOT = WSDOTN - CRUSST - NRUSST/0.16-
!    &    SSDOT  - WSIDOT - WSFDOT

      DO I = 1, NSC
!     ---------------------------------------------------------
!       stem dry matter increase (WLDOT in GROW)
        WLDOT_cohort = 
     &      STCAD(I)            !Reserve C = LCADD ???
     &    + STNAD(I)/0.16       !Reserve N = LNADD ???
     &    - STNMN(I)/0.16       !N mined = NRUSST/0.16
     &    - STCMN(I)            !C mined = CRUSST
     &    - StemMassDecrease(I) !freez, pst, senes=SSDOT+WSIDOT+WSFDOT
     &    - STLEAF_c(I)         !harvest

!       stem dry matter (WTLF in GROW)
        IF (STDM(I) + WLDOT_cohort >= 1.E-10) THEN
          STDM(I) = STDM(I) + WLDOT_cohort
        ELSE
          WLDOT_cohort = STDM(I)
          STDM(I) = 0.0
        ENDIF

!       Keep track of total stem mass addition today
        WSDOT_calc = WSDOT_calc + WLDOT_cohort
      ENDDO

      DO I = 1, NSC
        IF (STDM(I) .GT. 0.0) THEN
!     ---------------------------------------------------------
!         stem area
!         For now, use whole stem SLA for each cohort. But this
!           should be replaced by a cohort SLA when everything is working
!           as it is in GROW.
          LFAREA(I) = LFAREA(I) 
     &      - StemMassDecrease(I) * SLA_calc
     &      - STNMN(I) / 0.16 * SLA_calc
     &      + STNAD(I) / 0.16 * SLA_calc
     &      - FHSTEM_c(I) * SLA_calc

          LFSLA(I) = LFAREA(I) / STDM(I)

!     ---------------------------------------------------------
!         Non-structural mobile CH2O (~WCRLF in GROW)
          STNSC(I) = STNSC(I) + WRCLDT_c(I)
          IF (STNSC(I) < 0.0) STNSC(I) = 0.0

!     ---------------------------------------------------------
!         stem N
          LeafNTot(I) = LeafNTot(I) + NLDOT_c(I) 

!         Structural N (WTNLF minus WNRLF in GROW)
          LFSN(I) = MIN(LeafNTot(I), PROLFF*0.16 * (STDM(I) - STNSC(I)))

!         Non-structural N (WNRLF in GROW)
          STNSN(I) = LeafNTot(I) - LFSN(I)

!!        Composition
!         LFLIGNIN = function of ??
!         LFCELLUL = function of ??
!         LFHEMICEL = function of ??
!         ADF = function of LFLIGNIN, LFCELLUL, LFHEMICEL
!         NDF = function of LFLIGNIN, LFCELLUL, LFHEMICEL

        ELSE
          LFAREA(I)   = 0.0
          LFSLA(I)    = 0.0
          STNSC(I)    = 0.0
          LeafNTot(I) = 0.0
          LFSN(I)     = 0.0
          STNSN(I)    = 0.0
        ENDIF
      ENDDO

!-------------------------------------------------------------------
!     Today's new cohort
!-------------------------------------------------------------------
      IF (WSDOTN > 0.0) THEN
        NSC = NSC + 1  !today's new cohort

!       New growth for today's cohort
        STDM(NSC)  = WSDOTN             !stem dry mass
        CumStemDM(NSC) = WSDOTN         !cum stem mass added
        LFAREA(NSC)= WSDOTN * F         !stem area
        STNSC(NSC) = WSDOTN * ALPHL     !non-struct CH2O

!       struct N (non-mobile):
        LFSN(NSC)  = PROLFF * 0.16 * (WSDOTN - STNSC(NSC))  
        STNSN(NSC) = NGRLF - LFSN(NSC)  !non-struct N (mobile)

!!        Composition
!         LFLIGNIN = function of ??
!         LFCELLUL = function of ??
!         LFHEMICEL = function of ??
      ENDIF

!-------------------------------------------------------------------
      DO I = 1, NSC
        StemCohortAge(I) = StemCohortAge(I) + DTX  !cohort age in p-t-d
        LeafNTot(I) = STNSN(I) + LFSN(I)
        IF (STDM(I) > 0.0) THEN
          PCNStem(I) = LeafNTot(I) / STDM(I) * 100.  ! % N 
        ELSE
          PCNStem(I) = 0.0
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
      CRUSLF_calc = SUM(STCMN(1:SCMax))      !CH2O mined in VEGGR
      NRUSLF_calc = SUM(STNMN(1:SCMax)) / 0.16 !N mined in MOBIL
      WLFDOT_calc = SUM(STFRZ(1:SCMax))      !Freeze in FREEZE
      SSDOT_calc  = SUM(StemTotSen(1:SCMax)) !Total senes in SENES
      WatSen_calc = SUM(STWSSN(1:SCMax))     !Water senes in SENES
      LfMineSen_calc = SUM(STNMNSN(1:SCMax)) !N mining senes 
      LCADD_calc  = SUM(STCAD(1:SCMax))      !mobile CH2O in GROW
      LNADD_calc  = SUM(STNAD(1:SCMax))      !mobile N in GROW
      NLOFF_calc  = SUM(NLOFF_c(1:SCMax))    !N loss senes,freez,pest
      NLDOT_calc  = SUM(NLDOT_c(1:SCMax)) + NGRLF !Total N added today

!------------------------------------
!     Total states over all cohorts
      WTLF_calc  = SUM(STDM(1:SCMax))     !stem mass g/m2
      AREALF_calc= SUM(LFAREA(1:SCMax))   !Lf area index
      WCRLF_calc = SUM(STNSC(1:SCMax))    !CH2O reserves
      WTNLF_calc = SUM(LeafNTot(1:SCMax)) !stem N
      WNRLF_calc = SUM(STNSN(1:SCMax))    !Non-structural N
      LFSN_calc  = SUM(LFSN(1:SCMax))     !Structural N

!!     stem N is sum of structural and non-structural N
!      WTNLF_calc = LFSN_calc + WNRLF_calc

      !IF (WTLF_calc > 0.0) THEN
      !  PLEAFN_calc = WTNLF_calc / WTLF_calc * 100.
      !ELSE
      !  PLEAFN_calc = 0.0
      ENDIF

!!     Export a single SLA, XLAI, XHLAI, LAIMX for all leaves
!      XLAI_calc  = AREALF_calc / 10000. !stem area index (m2/m2)
!      IF (WTLF_calc > 0.0) THEN
!        SLA_calc    = AREALF_calc / WTLF_calc
!        SLAAD_calc  = AREALF_calc / (WTLF_calc - WCRLF_calc)
!      ELSE
!        SLA_calc    = 0.0
!        SLAAD_calc  = 0.0
!      ENDIF
!      LAIMX_calc = MAX(LAIMX_calc, XLAI_calc)



!!     temp chp
!      LFNSEN_sum = SUM(LFNSEN_c)
!      LTSEN_sum = SUM(LTSEN_c)
!      LFSENWT_sum = SUM(LFSENWT_c)
!      if (wtlf_calc > 0) then
!        RHOL = WCRLF_calc / WTLF_calc
!      else
!        RHOL = 0.0
!      endif
!      WLIDOT_sum  = SUM(STPST)

!     write(5567,'(I7,50F10.4)') YRDOY, CLOFF, SLMDOT, LTSEN, LFSENWT, 
      write(5568,'(I7,50F10.4)') YRDOY, CLOFF_SUM, LFNSEN_sum, 
     &    LTSEN_sum, LFSENWT_sum, 
!    &    SENCLV, RHOL, PCHOLFF, SLNDOT, WLIDOT, WLFDOT
     &    SENCLV, RHOL, PCHOLFF, WatSen_calc, WLIDOT_sum, WLFDOT_calc

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
     &  WSDOT_calc, LCADD_calc, LNADD_calc / 0.16, 
     &  CRUSLF_calc, NRUSLF_calc, 
     &  WLIDOT_calc, WLFDOT_calc, SSDOT_calc, 
     &  WatSen_calc, LfMineSen_calc, 
     &  NLOFF_calc, NLDOT_calc, NGRLF, WSDOTN

310   FORMAT (1X,I4, 1X,I3, 2I6, 30F12.6)

      write (CHRTOUT1,320) YEAR,DOY,StemCohortAge(1:50)
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
      END SUBROUTINE StemCohorts
!=======================================================================


!=======================================================================
!  StemCohortPest, Subroutine, C.H. Porter
!-----------------------------------------------------------------------
!  Determines pest damage per stem cohort.
!  This should eventually be moved to the pest module with the type of 
!     damage to cohorts specified in the pest coupling file.
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  03-24-2026 CHP Adapted from COHORTS
!=======================================================================
      SUBROUTINE StemCohortPest(
     &  WLIDOT, WTLF)   !Input
!       Output is STPST, which is available through the COHORTS_mod module
!-----------------------------------------------------------------------
      IMPLICIT NONE

      REAL WLIDOT_calc, WLIDOT, WTLF
      INTEGER I

      WLIDOT_calc = 0.0
      STPST = 0.0

      IF (WLIDOT .GT. 0.0) THEN
! FOR PROPORTIONAL DISTRIBUTION OF PEST DAMAGE:
        DO I = 1, NSC
          IF (LFDM(I) .GT. 0.0) THEN
            STPST(I) = LFDM(I) / WTLF * WLIDOT
            WLIDOT_calc = WLIDOT_calc + STPST(I)
          ELSE
            STPST(I) = 0.0
          ENDIF
        ENDDO

! FOR PEST DAMAGE TO AFFECT OLD TISSUE FIRST:
!        LFPSTDM = WLIDOT
!        DO I = 1, NSC
!          IF (LFDM(I) .GT. 0.0) THEN
!            STPST(I) = MIN(LFDM(I), LFPSTDM)
!            STPST(I) = MAX(STPST(I), 0.0)
!            LFPSTDM = LFPSTDM - STPST(I)
!          ENDIF
!        ENDDO

! FOR PEST DAMAGE TO AFFECT NEW TISSUE FIRST:
!        LFPSTDM = WLIDOT
!        DO I = NSC, 1, -1
!          IF (LFDM(I) .GT. 0.0) THEN
!            STPST(I) = MIN(LFDM(I), LFPSTDM)
!            STPST(I) = MAX(STPST(I), 0.0)
!            LFPSTDM = LFPSTDM - STPST(I)
!          ENDIF
!        ENDDO

      ELSE
!       No pest damage
        STPST = 0.0
      ENDIF

!     from GROW:
! NEED TO HANDLE THIS FOR COHORTS.
!C-----------------------------------------------------------------------
!C     Calculate "Healthy" or Non-Diseased stem Area Index
!C-----------------------------------------------------------------------
!!     AREAH  = AREALF - 2. * DISLA
!!     KJB Remove 2. factor
!      AREAH  = AREALF - DISLA
!      AREAH  = MAX(0.,AREAH)
!      XHLAI  = AREAH / 10000.

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE StemCohortPest
!=======================================================================


!!=======================================================================
!!  IPCOHO, Subroutine, C.H. Porter
!!-----------------------------------------------------------------------
!!  Reads input data for COHORTS subroutine
!!-----------------------------------------------------------------------
!!  REVISION       HISTORY
!!  11/18/2025 CHP Adapted from IPDMND.
!!=======================================================================
!      SUBROUTINE IPCOHO(
!     &  FILECC, MODEL,                          !Input
!     &  ALPHL, ICMP, MAXNMINE, NMOBMX, NVSMOB,  !Output
!     &  PCHOLFF, PROLFF, SENDAY, SENMAX,        !Output
!     &  SENCLV, SENNLV, TCMP, XSENMX)           !Output
!
!!-----------------------------------------------------------------------
!      IMPLICIT NONE
!      EXTERNAL GETLUN, ERROR, FIND, IGNORE, WARNING
!!-----------------------------------------------------------------------
!      CHARACTER*92, INTENT(IN) :: FILECC
!      REAL, INTENT(OUT) :: ALPHL, PCHOLFF, ICMP, MAXNMINE, NMOBMX, 
!     &     NVSMOB, PROLFF, SENDAY, SENCLV, SENNLV, TCMP
!      REAL, INTENT(OUT) :: SENMAX(4), XSENMX(4)
!
!      CHARACTER*6   ERRKEY
!      PARAMETER (ERRKEY = 'IPCOHO')
!      CHARACTER*6   SECTION
!      CHARACTER*8   MODEL
!      CHARACTER*80  C80
!
!      INTEGER LUNCRP,  ERR, LINC, LNUM, FOUND, ISECT
!      INTEGER II, I
!
!!-----------------------------------------------------------------------
!!     Read in values from species file
!!-----------------------------------------------------------------------
!      CALL GETLUN('FILEC', LUNCRP)
!      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
!      LNUM = 0
!      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!!-----------------------------------------------------------------------
!!    Find and Read Plant Composition Section
!!-----------------------------------------------------------------------
!      SECTION = '!*PLAN'
!      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(SECTION, 42, FILECC, LNUM)
!      ELSE
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(12X,F6.0)',IOSTAT=ERR) PROLFF
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!
!        IF (MODEL(1:5) == 'PRFRM') THEN
!          DO I = 1, 11
!            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!          ENDDO
!          READ(C80,'(F6.0)',IOSTAT=ERR) PCHOLFF
!          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!        ENDIF
!      ENDIF
!
!!-----------------------------------------------------------------------
!!    Find and Read Carbon and Nitrogen Mining Section
!!-----------------------------------------------------------------------
!      SECTION = '!*CARB'
!      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(SECTION, 42, FILECC, LNUM)
!      ELSE
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(18X,2F6.0)',IOSTAT=ERR) NMOBMX, NVSMOB
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  !Skip the next line
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(F6.0)',IOSTAT=ERR) ALPHL
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!
!        SELECT CASE (MODEL(1:5))
!        CASE ('CRGRO')
!          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  
!          READ(C80,'(F6.0)',IOSTAT=ERR) MAXNMINE
!          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!
!        CASE ('PRFRM')
!          DO I = 1, 7
!            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  
!          ENDDO
!          READ(C80,'(2F6.0)',IOSTAT=ERR) SENNLV, SENCLV
!          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!
!          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  
!          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  
!          READ(C80,'(F6.0)',IOSTAT=ERR) MAXNMINE
!          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!        END SELECT
!      ENDIF
!
!!-----------------------------------------------------------------------
!!    Find and Read Senescence Section
!!    NOTE: First search for Section finds '!*stem GROWTH PARAMETERS'
!!          Second search finds '!*stem SENESCENCE FACTORS'
!!-----------------------------------------------------------------------
!      SECTION = '!*stem'
!      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(SECTION, 42, FILECC, LNUM)
!      ENDIF
!
!      CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
!      IF (FOUND .EQ. 0) THEN
!        CALL ERROR(SECTION, 42, FILECC, LNUM)
!      ELSE
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  
!        READ(C80,'(12X,F6.0)',IOSTAT=ERR) SENDAY
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(2F6.0)',IOSTAT=ERR) ICMP, TCMP
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(24X,4F6.0)',IOSTAT=ERR)
!     &      (XSENMX(II),II=1,4)
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!
!        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!        READ(C80,'(24X,4F6.0)',IOSTAT=ERR)
!     &      (SENMAX(II),II=1,4)
!        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!
!      ENDIF
!
!!-----------------------------------------------------------------------
!      CLOSE(LUNCRP)
!
!!-----------------------------------------------------------------------
!      RETURN
!!-----------------------------------------------------------------------
!      END  SUBROUTINE IPCOHO
!!=======================================================================

!***********************************************************************
!     Variable listing for COHORTS subroutine (updated 20 April 2009)
!***********************************************************************
! CHRTIN       Logical unit number of cohort parameter input file
! CHRTOUT      Logical unit number of cohort output file
! COHORTOUT    File name of cohort output file
! COHORTIN     File name of cohort parameter input file
! CUMAREA      Sum of stem area to cohort I, used to calculate SHADEFAC(I)
!                (m2[stem]/m2[ground])
! CUMLFDM      Cumulative stem dry matter for the season (g[stem]/m2[ground])
! DOY          Day of year of simulation (DDD)
! DTX          Thermal time that occurs in a real day based on vegetative 
!                development temperature function (thermal days / day)
! F            Specific stem area of new stem tissue growth, including N
!                (cm2[stem] / g[stem])
! ICMP         Light compensation point for senescence (moles[quanta]/m2-d)
! KCAN         Canopy light extinction coefficient
! LFDM(I)      stem dry matter for cohort I (g[stem]/m2[ground])
! STNSC(I)     stem non-structural (mobile) CH2O for cohort I
!                (g[stem CH2O]/m2[ground]) = stem CH2O reserves = WCRLF
! STNSN(I)     stem non-structural (mobile) N for cohort I
!                (g[stem N]/m2[ground])
! LFSC(I)      stem structural (non-mobile) CH2O for cohort I
!                (g[stem CH2O]/m2[ground])
! LFSN(I)      stem structural (non-mobile) N for cohort I
!                (g[stem N]/m2[ground])
! LFAREA(I)    stem area for cohort I (cm2[stem]/m2[ground]
! StemCohortAge(I) Age for cohort I (thermal days)
! LFDMSN(I)    Total stem dry matter senescence for today for cohort I,
!                includes STWSSN(I) and STNMNSN(I) (g[stem]/m2[ground]/d)
! STFRZ(I)     stem dry matter lost due to freeze damage today for
!                cohort I (g[stem]/m2[ground]/d)
! STNMN(I)     stem N mining rate for today for cohort I
!                (g[stem N]/m2[ground]/d)
! STNMNSN(I)   stem dry matter senescence due to N mining for today for
!                cohort I (g[stem]/m2[ground]/d)
! LFNMNR(I)    Maximum stem N mining rate for cohort I
!                (g[stem N]/m2[ground]/d)
! STPST(I)     stem dry matter lost to due to pest damage today for
!                cohort I (g[stem]/m2[ground]/d)
! STWSSN(I)    stem senescence due to water stress for today for cohort I
!                (g[stem]/m2[ground]/d)
! MAXNMINE     Maximum N mining rate (fraction/day)
! NGRLF        Maximum N demand for stem growth (g[stem N] / m2[ground] / d)
! NMOBMX       Maximum N mobilization rate (fraction/day)
! NMOBR        Stage-dependent potential N mining rate expressed as a
!                fraction of the maximum rate (NMOBMX) 
! NRUSLF       N actually mobilized from leaves in a day (g[N]/m2-d)
! NVSMOB       Relative rate of N mining during vegetative stage to that in 
!                reproductive stage 
! PAR          Daily photosynthetically active radiation or photon flux 
!                density (moles[quanta]/m2-d)
! PCNStem      Actual cohort N concentration %
! PLEAFN_calc  stem N concentration averaged over all cohorts today (percent)
! PORLFT       Proportion of stem weight grown which will have been senesced 
!                if no water stress has occurred prior to this V-stage 
! PROLFF       Minimum stem protein composition after N mining
!                (g[protein] / g[stem])
! RATTP        Factor used in determining senescence due to water 
!               stress
! SENDAY       Maximum rate of stem abscission due to water stress
!                (fraction/day)
! SENMAX(I)    Maximum proportion of total stem weight as a function of 
!                V-stage (XSENMX(I)) which can be senesced due to water stress. 
! SHADEFAC(I)  Factor used to increase N mobilization of cohort I due to
!                shading
! SLDOT        Defoliation due to daily stem senescence (g/m2/day)
! SLNDOT       stem senescence due to water stress (g/m2/day)
! SUMLFDM      Sum of stem dry matter for all cohorts today
!                (g[stem]/m2[ground])
! SUMLFNSN     Sum of stem non-structural N for all cohorts today
!                (g[stem N]/m2[ground])
! SUMLFAREA    Sum of stem area for all cohorts today
!                (cm2[stem]/m2[ground])
! SUMLFN       Sum of N (structural and non-structural for all cohorts today
!                (g[stem N]/m2[ground])
! SWFAC        Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!                0.0=max stress
! TCMP         Time constant for low light senescence (days)
! VSTAGE       Number of nodes on main stem of plant 
! WSDOTN       Dry weight growth rate of new stem tissue including N but not
!                C reserves (g[stem] / m2[ground]/d)
! WSLOSS       stem senescence due to water stress for today (g/m2/day)
! LCMP         LAI at which today's light compensation (ICMP) is reached
!                (m2[stem] / m2[ground])
! WNRLF        N available for mobilization from leaves above lower limit of 
!                mining (g[N] / m2)
! WTLF         Dry mass of stem tissue including C and N
!                (g[stem] / m2[ground])
! WTNLF        Mass of N in leaves (g[stem N] / m2[ground])
! XLAI         stem area (one side) per unit of ground area
!                (m2[stem] / m2[ground])
! XSENMX(I)    V-stage at which maximum fraction of cumulative stem growth 
!                vulnerable to loss due to water stress is SENMAX(I).
!                (# stem nodes)
! YEAR         Year of simulation (YYYY)
! YRDOY        Current day of simulation (YYYYDDD)
!***********************************************************************

C=======================================================================
      END MODULE StemCohorts_MOD
C=======================================================================
