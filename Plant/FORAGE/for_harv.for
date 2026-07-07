C=======================================================================
C  for_harv, Subroutine
C
C  Description
C-----------------------------------------------------------------------
C  Revision history
C
C  07/02/2003 KJB/SJR/PA?  Written.
C  05/07/2020 FO  Added new Y4K subroutine call to convert YRDOY
C  10/15/2020 FO  Fixed path issue for MOWFILE.
C  06/23/2021 FO  Update MOWFILE to handle paths with spaces.
C  01/28/2022 DP/FO/TF Added AutomaticMOW
C  01/28/2022 DP/TF  Added GDD option for AutomaticMOW
!  06/16/2026 CHP Added MOWED variable TRUE after any mowing event.
!  07/01/2026 CHP Subroutine MowFileRead handles all MOWFILE operations
!  07/01/2026 CHP Subroutine IPSPE_FORHAR reads species file
!  07/01/2026 CHP Subroutine OP_FORHARV handles output
C-----------------------------------------------------------------------
C  INPUT  : 
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called :
C
C  Calls  :
C=======================================================================
      SUBROUTINE forage_harvest(CONTROL,FILECC, ATMOW, ATTP,
     &  RHOL,RHOS,PCNL,PCNST,SLA,RTWT,STRWT,   !Input
     &  WTLF,STMWT,TOPWT,TOTWT,WCRLF,WCRST,    !Input/Output
     &  WTNLF,WTNST,WNRLF,WNRST,WTNCAN,        !Input/Output
     &  AREALF,XLAI,XHLAI,VSTAGE,vstagp,canht, !Input/Output
     &  fhtot,FHTOTN, fhpctlf,fhpctn,FREQ,
     &  MOWC,RSPLC,HMFRQ,HMGDD,HMCUT, HMMOW,HRSPL,
     &  DWTCO, DWTLO, DWTSO, PWTCO, PWTLO, PWTSO,
     &  HMVS, WTCO, WTLO, WTSO, TAVG, MOWGDD,
     &  MOWCOUNT, TGMIN, VTO1, VTB1, MOWREF, 
     &  RSREF, YFREQ, YRSREF, YCUTHT, YCHMOW,
     &  XCUTHT, XCHMOW, XFRGDD, XFREQ, CUTDAY,
     &  PROLFF, PROSTF, pliglf, pligst, 
     &  MOWED)                                  !Output

      USE MODULEDEFS
      USE ModuleData
      USE COHORTS_MOD

      IMPLICIT NONE
      SAVE
      EXTERNAL ERROR, TABEX
      EXTERNAL MowFileRead, IPSPE_FORHAR, OP_FORHARV, HarvestCohorts

      INTEGER YRDOY, DYNAMIC, MOWCOUNT

      LOGICAL MOWTODAY, MOWED

      REAL MOWa,RSPLFa,MVSa,rshta
      REAL FHLEAF,FHSTEM,FHVSTG
      REAL RHOL,RHOS,PCNL,PCNST,SLA
      REAL WTLF,STMWT,TOPWT,TOTWT,WCRLF,WCRST
      REAL WTNLF,WTNST,WNRLF,WNRST,WTNCAN,RTWT,STRWT
      REAL AREALF,XLAI,XHLAI,VSTAGE  !AREAH,
      REAL PROLFF,PROSTF,pliglf,pligst
      real canht,fhcrlf,fhcrst,fhtotn,fhtot,fhlfn,fhstn
      real fhpcho,fhpctlf,fhpctn,fhplig
      real vstagp,MOWC,RSPLC

      REAL DWTCO, DWTLO, DWTSO, PWTCO, PWTLO, PWTSO
      REAL WTCO, WTLO, WTSO
      REAL FREQ,MOWREF !CUHT
!     REAL YHT

      REAL TABEX  ! Function subroutine - Lookup utility
      REAL HMCUT, RSREF
!     INTEGER,dimension(6) :: IXFREQ
      REAL,dimension(6) :: XFREQ
      REAL,dimension(6) :: YFREQ
!     INTEGER,dimension(6) :: IXCUTHT
      REAL,dimension(6) :: XCUTHT
      REAL,dimension(6) :: YCUTHT
!     INTEGER,dimension(6) :: IXCHMOW
      REAL,dimension(6) :: XCHMOW
      REAL,dimension(6) :: YCHMOW
!     INTEGER,dimension(6) :: IXFRGDD
      REAL,dimension(6) :: XFRGDD
      REAL,dimension(6) :: YRSREF
      REAL GDD, MOWGDD
      INTEGER HMFRQ, HMGDD, CUTDAY, HMVS
      INTEGER HMMOW, HRSPL !TF 2022-01-31 Smart version AutoMOW
      INTEGER CUTNO !Count number of cuts for AutoMOW

      REAL TAVG, TGMIN
!     REAL TB(5), TO1(5) !, TO2(5) , TM(5)
      REAL VTO1, VTB1 !Vegetative coefficients

      CHARACTER(len=6)  ERRKEY
      CHARACTER*12 FILEX
      CHARACTER*80, INTENT(IN) :: FILECC
      LOGICAL ATMOW
      CHARACTER*1 ATTP

      TYPE(CONTROLTYPE) CONTROL

      PARAMETER  (ERRKEY = 'FRHARV')

      DYNAMIC  = CONTROL % DYNAMIC
      YRDOY = CONTROL % YRDOY

C***********************************************************************
C***********************************************************************
!     Run Initialization - Called once per simulation
C***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN

        CALL IPSPE_FORHAR(
     &  ATTP, FILECC,                             !Input
     &  PROLFF, PROSTF, pliglf, pligst,           !Output
     &  VTO1, VTB1, TGMIN,                        !Output
     &  MOWREF, RSREF, XFREQ, XFRGDD, YFREQ,      !Output
     &  YRSREF, XCUTHT, YCUTHT, XCHMOW, YCHMOW)   !Output

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
        MOWGDD = 0.0
        MOWCOUNT = 1
        MOWTODAY = .FALSE.
        MOWED = .FALSE. !set to TRUE after the first mow
        FILEX = CONTROL % FILEX

        CALL PUT('MHARVEST','ISH_date',-99)
        CALL PUT('MHARVEST','ISH_wt',  -99.)

        IF (.NOT. ATMOW) THEN
!         Reads MOW file
          CALL MowFileRead(CONTROL,           !Input
     &      MOWa, RSPLFa, MVSa, RSHTa)        !Output

        ELSE
          IF(ATTP .EQ. 'W' .AND. HMFRQ .LE. 0) THEN
            CALL ERROR (ERRKEY,3,FILEX,0)
          ENDIF
          IF(ATTP .EQ. 'X' .AND. HMGDD .LE. 0) THEN
            CALL ERROR (ERRKEY,4,FILEX,0)
          ENDIF        
          IF(ATTP .EQ. 'Y' .AND. HMFRQ .LE. 0) THEN
            CALL ERROR (ERRKEY,3,FILEX,0)
          ENDIF              
          IF(ATTP .EQ. 'Z' .AND. HMGDD .LE. 0) THEN
            CALL ERROR (ERRKEY,4,FILEX,0)
          ENDIF
          IF(HMCUT .LT. 0.0) CALL ERROR (ERRKEY,6,FILEX,0)
          IF(HMVS .LT. 0 .OR. HMVS .GT. 80) THEN
            CALL ERROR (ERRKEY,8,FILEX,0)
          ENDIF
          !HMMOW and HRSPL are used only for SmartMOW
          IF(ATTP .EQ. 'Y' .OR. ATTP .EQ. 'Z') THEN
            IF(HMMOW .LT. 0.0) CALL ERROR (ERRKEY,7,FILEX,0)
            IF(HRSPL .GT. 100 .OR. HRSPL .LT. 0) THEN
              CALL ERROR (ERRKEY,5,FILEX,0)
            ENDIF
          ENDIF
        ENDIF

        CALL OP_FORHARV(CONTROL, 
     &   CUTNO, topwt, wtlf, stmwt, strwt, rtwt,  !Input
     &   xlai, fhtot, fhtotn, fhpctn, fhpcho,     !Input
     &   fhplig, fhpctlf, MOWC, RSPLC)            !Input

!***********************************************************************
!***********************************************************************
!     Daily Rate Calculations
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
      MOWTODAY = .FALSE.
      MOWC = 0.0
      RSPLC = 0.0
      MOWa = 0.0
      RSPLFa = 0.0
      MVSa = 0.0
      RSHTa = 0.0

        IF(ATMOW) THEN
          IF(ATTP .EQ. 'W' .OR. ATTP .EQ. 'Y') THEN
            FREQ = HMFRQ
            CUTDAY = MOD(MOWCOUNT,HMFRQ)
            MOWGDD = 0 !It will not accumulate GDD if there is HMFRQ
          ENDIF
          IF(ATTP .EQ. 'Z' .OR. ATTP .EQ. 'X') THEN
            FREQ = HMGDD
            CUTDAY = 1
          ENDIF
        ELSE
          CALL MowFileRead(CONTROL,           !Input
     &      MOWa, RSPLFa, MVSa, RSHTa)        !Output
        ENDIF

        FHLEAF_c = 0.0
        FHSTEM_c = 0.0
        fhtot = 0
        fhlfn = 0
        fhstn = 0
        fhtotn = 0
        fhcrlf = 0
        fhcrst = 0
        fhpctn = 0
        fhplig = 0
        fhpcho = 0
        fhpctlf = 0

!***********************************************************************
!***********************************************************************
!     Daily Integration
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
!     Daily Senescence
      DWTCO = WTCO - PWTCO
      DWTLO = WTLO - PWTLO
      DWTSO = WTSO - PWTSO
      DWTSO = WTSO - PWTSO
      DWTSO = WTSO - PWTSO
      FHLEAF_c = 0.0
      FHSTEM_c = 0.0
!----------------------------------------------------------------------

      IF (.NOT. ATMOW .AND. MOWa > 1.E-6) THEN
        CUTNO = CUTNO + 1
        MOWTODAY = .TRUE.

        if(MOWa/10. < topwt) THEN
          FHLEAF=0.0
          FHSTEM=0.0
          FHVSTG=0.0
          IF(RSPLFa>=0)THEN
            FHLEAF=WTLF-(MOWa/10.)*RSPLFa/100.
            FHSTEM=STMWT-(MOWa/10.)*(1.0-RSPLFa/100.)
          ELSE
            IF (WTLF + STMWT > 0.0) THEN
              FHLEAF=WTLF-(MOWa/10)*WTLF/(WTLF+STMWT)
              FHSTEM=STMWT-(MOWa/10)*STMWT/(WTLF+STMWT)
            ENDIF
          END IF
          FHLEAF=MAX(FHLEAF,0.0)
          FHSTEM=MAX(FHSTEM,0.0)
          FHVSTG=MAX(MVSa,0.0)
          canht=max(rshta/100.,0.0)
!         canht=max(rshta,0.0)     !enter rsht in cm

          fhtot = fhleaf+fhstem
        ENDIF !MowAmount < TOPWT
      end if  !(MowAmount > 0.0)

!***********************************************************************
! AUTOMOW calculations (DP,KJB,WP,FO,TF)
!***********************************************************************
      ! DP/TF - 01/28/2022 Added degree days (GDD) option
      IF (ATMOW) THEN
        IF (CUTDAY .EQ. 0 .OR.
     &        (MOWGDD .GE. HMGDD .AND. HMGDD .GT. 0)) THEN
            !DP/TF 2022-01-31 Switch to complete version AutoMOW
          IF (ATTP .EQ. 'W' .OR. ATTP .EQ. 'X') THEN
            MOWC = (TABEX(YFREQ, XFREQ, FREQ, 6) * MOWREF) *
     &          (TABEX(YCUTHT, XCUTHT, HMCUT*100, 6)) *
     &          (TABEX(YCHMOW, XCHMOW, topwt, 6))
            RSPLC = (TABEX(YRSREF, XFREQ, FREQ, 6) * RSREF)
            !DP/TF 2022-01-31 Switch to simple version AutoMOW
          ELSEIF (ATTP .EQ. 'Y' .OR. ATTP .EQ. 'Z') THEN
            MOWC = MAX(HMMOW,0)
            RSPLC = MAX(HRSPL,0)
          ENDIF
          MOWGDD = 0.0
        ELSE
          MOWCOUNT = MOWCOUNT + 1
      !DP/TF 2022-01-31 GDD calculations as harvest frequency option
          IF(TAVG .GT. VTB1) THEN
            GDD = TAVG - VTB1
           !GDD = (((TMAX+TMIN)/2) - TB(1)) 
          ELSE
            GDD = 0.0
          ENDIF
          IF (GDD .GT. TGMIN) GDD = TGMIN
          GDD = MAX(GDD, 0.0)
          MOWGDD = MOWGDD + GDD
          RETURN
        ENDIF

        IF (MOWC .GE. 0.0) THEN
          MOWCOUNT = 1
          MOWTODAY = .TRUE.

          CUTNO = CUTNO + 1
          IF (MOWC/10. < topwt) THEN
            FHLEAF=0
            FHSTEM=0
            FHVSTG=0
            IF (RSPLC>=0) THEN
              FHLEAF=WTLF-(MOWC/10.)*RSPLC/100
              FHSTEM=STMWT-(MOWC/10.)*(1.0-RSPLC/100)
            ELSE
              FHLEAF=WTLF-(MOWC/10.)*WTLF/(WTLF+STMWT)
              FHSTEM=STMWT-(MOWC/10.)*STMWT/(WTLF+STMWT)
            END IF

            FHLEAF = MAX(FHLEAF,0.0)
            FHSTEM = MAX(FHSTEM,0.0)
            FHVSTG = HMVS
            canht  = max(HMCUT/100,0.0)
           !canht=max(rsht(i),0.0)     !enter rsht in cm

            fhtot = fhleaf+fhstem

          ENDIF
        ENDIF
      ENDIF

!     Summarize today's harvest
      if (fhtot > 0.0) then
        MOWED = .TRUE.
        fhlfn = fhleaf*pcnl/100
        fhstn = fhstem*pcnst/100
        fhtotn = fhlfn+fhstn
        
        fhcrlf = fhleaf*rhol
        fhcrst = fhstem*rhos

        fhpctn = fhtotn/fhtot*100
        fhplig = (fhleaf*pliglf+fhstem*pligst)/fhtot*100
        fhpcho = (fhcrlf+fhcrst)/fhtot*100
        fhpctlf = fhleaf/fhtot*100

        WTLF = WTLF - FHLEAF

        STMWT = STMWT - FHSTEM
        TOPWT = TOPWT - FHLEAF - FHSTEM
        TOTWT = TOTWT - FHLEAF - FHSTEM

        WCRLF = WTLF*RHOL
        WCRST = STMWT*RHOS

        WTNLF = WTLF*PCNL/100.
        WTNST = STMWT*PCNST/100.
        WTNCAN = WTNCAN - FHLEAF*PCNL/100. - FHSTEM*PCNST/100.

        IF ((WTLF - WCRLF) .GT. 0.0) THEN
          WNRLF = MAX (WTNLF - PROLFF*0.16*(WTLF-WCRLF), 0.0)
        ELSE
          WNRLF = 0.0
        ENDIF

        IF ((STMWT - WCRST) .GT. 0.0) THEN
          WNRST = MAX (WTNST - PROSTF*0.16*(STMWT-WCRST), 0.0)
        ELSE
          WNRST = 0.0
        ENDIF

        AREALF = WTLF*SLA
        XLAI = AREALF/10000.
        XHLAI = XLAI

        VSTAGE = FHVSTG
        vstagp = vstage

!       Send out amount harvested today for MgmtEvent.OUT file
        CALL PUT('MHARVEST','ISH_date',YRDOY)
        CALL PUT('MHARVEST','ISH_wt', fhtot*10.)

      ELSE
        FHLEAF = 0.0
        FHSTEM = 0.0
        fhtot = 0
        fhlfn = 0
        fhstn = 0
        fhtotn = 0
        fhcrlf = 0
        fhcrst = 0
        fhpctn = 0
        fhplig = 0
        fhpcho = 0
        fhpctlf = 0
      ENDIF

      IF(CUTDAY .EQ. 0) THEN
        PWTCO = WTCO
        PWTLO = WTLO
        PWTSO = WTSO
        DWTCO = WTCO - PWTCO
        DWTLO = WTLO - PWTLO
        DWTSO = WTSO - PWTSO
      ENDIF

      CALL HarvestCohorts(YRDOY, FHLEAF, FHSTEM)

!***********************************************************************
!***********************************************************************
!     End of Season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      IF (MOWTODAY) THEN
        CALL OP_FORHARV(CONTROL, 
     &   CUTNO, topwt, wtlf, stmwt, strwt, rtwt,  !Input
     &   xlai, fhtot, fhtotn, fhpctn, fhpcho,     !Input
     &   fhplig, fhpctlf, MOWC, RSPLC)            !Input
      ENDIF

!***********************************************************************
!***********************************************************************
!     End of Season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
        CALL OP_FORHARV(CONTROL, 
     &   CUTNO, topwt, wtlf, stmwt, strwt, rtwt,  !Input
     &   xlai, fhtot, fhtotn, fhpctn, fhpcho,     !Input
     &   fhplig, fhpctlf, MOWC, RSPLC)            !Input

        CALL MowFileRead(CONTROL,           !Input
     &      MOWa, RSPLFa, MVSa, RSHTa)      !Output

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE forage_harvest

!=======================================================================


!=======================================================================
!     SUBROUTINE MowFileRead
!     Reads mow file and sends back today's mowed amounts.
!-----------------------------------------------------------------------
!  Revision history
!
!  07/01/2026 CHP moved MOWFILE operations to this subroutine
!=======================================================================

      SUBROUTINE MowFileRead(CONTROL,     !Input
     &  MOWa, RSPLFa, MVSa, RSHTa)        !Output

      USE ModuleDefs
      IMPLICIT NONE
      SAVE

      EXTERNAL GETLUN, ERROR, WARNING, PARSE_HEADERS, Y4K_DOY

      TYPE (ControlType), INTENT(IN) :: CONTROL
      REAL, INTENT(OUT) :: MOWa, RSPLFa, MVSa, RSHTa

      CHARACTER(len=1), PARAMETER :: BLANK = ' '
      CHARACTER(len=6), PARAMETER :: ERRKEY = 'FRHARV'
      CHARACTER(len=6)  trtchar
      CHARACTER*12 MOWFILE, FILEX
      CHARACTER*78 MSG(2)
      CHARACTER*80 PATHEX, MOW80
      CHARACTER*92 FILEMOW
      LOGICAL FEXIST

      INTEGER, PARAMETER :: MAXCOL = 50
      CHARACTER*15  HEADER(MAXCOL)
      INTEGER COL(MAXCOL,2), C1, C2, COUNT
      INTEGER MOWLUN, DYNAMIC, PATHL, LNUM, ERR, ISECT

      INTEGER YRDOY, trtno, I, J
      INTEGER MOWCOUNT !# mow entries in MOW file

      INTEGER,ALLOCATABLE,DIMENSION(:) :: TRNO,DATE
      REAL,ALLOCATABLE,DIMENSION(:) :: MOW, RSPLF, MVS, rsht

      DYNAMIC = CONTROL % DYNAMIC
      YRDOY  = CONTROL % YRDOY

!***********************************************************************
!***********************************************************************
!     Run initialization - read all treatments in MOW file
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      FILEX  = CONTROL % FILEX
      PATHEX = CONTROL % PATHEX
      trtno  = control % trtnum

      MOWFILE = FILEX(1:8) // ".MOW"
      PATHL  = INDEX(PATHEX,BLANK)
      IF (PATHL .LE. 1) THEN
        FILEMOW = mowfile
      ELSE
        PATHL = LEN(TRIM(PATHEX))
        FILEMOW = PATHEX(1:(PATHL)) // mowfile
      ENDIF

      INQUIRE(FILE = MOWFILE, EXIST = FEXIST)
      IF (.NOT. FEXIST) THEN
        MSG(1) = "Mow file missing."
        MSG(2) = MOWFILE
        CALL WARNING(2, ERRKEY, MSG)
        CALL ERROR(ERRKEY,29,FILEMOW,LNUM)
      ENDIF

      IF (ALLOCATED(MOW)) THEN
        deallocate(mow,trno,date,rsplf,mvs,rsht)
      ENDIF
      CALL GETLUN('MOWFILE',MOWLUN)
      OPEN (UNIT=MOWLUN,FILE=FILEMOW,STATUS='OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,29,FILEMOW,LNUM)

      REWIND(MOWLUN)

      ISECT = 0
      MOWCOUNT = 0
      write(trtchar,'(i6)') trtno
      DO WHILE (ISECT.EQ.0)
        READ (MOWLUN,'(A80)',IOSTAT=ISECT) MOW80
        IF (MOW80(1:1).NE."@"
     &     .AND.MOW80(1:1).NE."!"
     &     .AND.MOW80(1:20).NE."                    "
     &     .and.mow80(1:6)==trtchar
     &     .AND.ISECT.EQ.0)THEN
           MOWCOUNT = MOWCOUNT + 1
        END IF
      END DO
      REWIND(MOWLUN)

      IF (MOWCOUNT.GT.0) THEN
        ALLOCATE(TRNO(MOWCOUNT),DATE(MOWCOUNT),MOW(MOWCOUNT))
        ALLOCATE(RSPLF(MOWCOUNT),MVS(MOWCOUNT),rsht(mowcount))
        TRNO = 0
        DATE = 0
        MOW = 0.0
        RSPLF = 0.0
        MVS = 0.0
        RSHT = 0.0
      ELSE
C       MOW file has no data for this treatment
        CALL ERROR(ERRKEY,2,MOWFILE,0)
!        ALLOCATE(MOW(1))
!        MOW (1) = -99.
!        RETURN
      END IF

      I = 0
      ISECT = 0
      DO WHILE (ISECT.EQ.0)
        READ (MOWLUN,'(A80)',IOSTAT=ISECT) MOW80
!       TF 05/22/2023 - Updated read method for mow file to handle 
!        dates in YYDDD and YYYYDDD format
        IF(MOW80(1:1).EQ."@") THEN
          CALL PARSE_HEADERS(MOW80, MAXCOL, HEADER, COUNT, COL)
        ENDIF
        IF (MOW80(1:1).NE."@"
     &     .AND.MOW80(1:1).NE."!"
     &     .AND.MOW80(1:20).NE."                    "
     &     .and.mow80(1:6)==trtchar
     &     .AND.ISECT.EQ.0)THEN
          I = I + 1
          DO J = 1, COUNT
            C1 = COL(J,1)
            C2 = COL(J,2)
            SELECT CASE (TRIM(HEADER(J)))
             CASE('TRNO');READ(MOW80(C1:C2+1),*,IOSTAT=ERR) TRNO(I)
             CASE('DATE');READ(MOW80(C1:C2),*,IOSTAT=ERR) DATE(I)
             CASE('MOW');READ(MOW80(C1:C2),*,IOSTAT=ERR) MOW(I)
             CASE('RSPLF');READ(MOW80(C1:C2),*,IOSTAT=ERR) RSPLF(I)
             CASE('MVS');READ(MOW80(C1:C2),*,IOSTAT=ERR) MVS(I)
             CASE('RSHT');READ(MOW80(C1:C2),*,IOSTAT=ERR) rsht(I)
            END SELECT
          END DO
          CALL Y4K_DOY(DATE(I),MOWFILE,I,ERRKEY,1)
        END IF
      END DO

!***********************************************************************
!***********************************************************************
!     Daily Rate Calculations
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
      MOWa = 0.0
      RSPLFa = 0.0
      MVSa = 0.0
      RSHTa = 0.0

      DO I=1,SIZE(MOW)
        if(date(i)==yrdoy) then
          IF (MOW(I).GE.0.and.trno(i)==trtno)then
            MOWa   = MOW(I)
            RSPLFa = RSPLF(I)
            MVSa   = MVS(I)
            RSHTa  = RSHT(I)
            EXIT
          ENDIF
        ENDIF
      ENDDO

!***********************************************************************
!***********************************************************************
!     End of season
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
        IF (ALLOCATED(MOW)) THEN
          DEALLOCATE(TRNO, DATE, MOW, RSPLF, MVS, rsht)
        ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE MowFileRead
!=======================================================================

!=======================================================================
!     SUBROUTINE IPSPE_FORHAR
!     Read from species file:
!       PROLFF, PROSTF, pliglf, pligst
!       TB(1), TO1(1)
!       MOWREF, RSREF, IXFREQ, IXFRGDD, YFREQ, YRSREF, 
!       IXCUTHT, YCUTHT, IXCHMOW, YCHMOW
      
!     Output for use in Forage harvest:
!       PROLFF, PROSTF, pliglf, pligst
!       VTO1, VTB1, TGMIN
!       MOWREF, RSREF, XFREQ, XFRGDD, YFREQ, YRSREF, 
!       XCUTHT, YCUTHT, XCHMOW, YCHMOW

!-----------------------------------------------------------------------
      SUBROUTINE IPSPE_FORHAR(
     &  ATTP, FILECC,                             !Input
     &  PROLFF, PROSTF, pliglf, pligst,           !Output
     &  VTO1, VTB1, TGMIN,                        !Output
     &  MOWREF, RSREF, XFREQ, XFRGDD, YFREQ,      !Output
     &  YRSREF, XCUTHT, YCUTHT, XCHMOW, YCHMOW)   !Output

      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE

      CHARACTER*1, INTENT(IN) ::  ATTP
      CHARACTER*80, INTENT(IN) :: FILECC
      REAL, INTENT(OUT) :: PROLFF, PROSTF, pliglf, pligst
      REAL, INTENT(OUT) :: VTO1, VTB1, TGMIN 
      REAL, INTENT(OUT) :: MOWREF, RSREF
      REAL, DIMENSION(6), INTENT(OUT) :: XFREQ, YFREQ, XFRGDD, YRSREF
      REAL, DIMENSION(6), INTENT(OUT) :: XCUTHT, YCUTHT, XCHMOW, YCHMOW

      CHARACTER(len=6) SECTION
      CHARACTER(len=6), PARAMETER :: ERRKEY = 'FRHARV'
      CHARACTER*80 MOW80
      CHARACTER*255 C255

      INTEGER LUNCRP, ERR, LNUM, FOUND, ISECT, I, J
      REAL TB(5), TO1(5) !, TO2(5) , TM(5)
      INTEGER,dimension(6) :: IXFREQ
      INTEGER,dimension(6) :: IXCUTHT
      INTEGER,dimension(6) :: IXCHMOW
      INTEGER,dimension(6) :: IXFRGDD

!-----------------------------------------------------------------------
!     OPEN AND READ SPECIES FILE
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

!     Read "!*PLANT COMPOSITION VALUES" section
      SECTION = '!*PLAN'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)

      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,MOW80)
        READ(MOW80,'(12X,F6.0,12X,F6.0)',IOSTAT=ERR) PROLFF, PROSTF
        do j=1,5; CALL IGNORE(LUNCRP,LNUM,ISECT,MOW80); end do
        READ(MOW80,'(2f6.0)',IOSTAT=ERR) pliglf, pligst
      ENDIF

!-----------------------------------------------------------------------
!       Find Phenology Section in FILEC and read cardinal temperatures
!       for GDD calculations as harvest frequency option
!-----------------------------------------------------------------------
!     Read "!*PHENOLOGY PARAMETERS" section
      SECTION = '!*PHEN'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)

      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,MOW80)
        READ(MOW80,'(4F6.1)') TB(1), TO1(1)
      ENDIF

      VTO1 = TO1(1)
      VTB1 = TB(1)
      TGMIN = VTO1 - VTB1

      IF(ATTP .EQ. 'W' .OR. ATTP .EQ. 'X') THEN
!       Read "!*STUBBLE MASS AND PERCENT LEAF FORAGE HARVEST" section
        SECTION = '!*STUB'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
          CALL ERROR(ERRKEY, 1, FILECC, LNUM)
        ELSE
          CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
          READ(C255,'(2F6.0)',IOSTAT=ERR)  MOWREF, RSREF
          CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
          READ(C255,'(6I6)',IOSTAT=ERR) (IXFREQ(I),I=1,6)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
          READ(C255,'(6I6)',IOSTAT=ERR) (IXFRGDD(I),I=1,6)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
          READ(C255,'(6F6.2)',IOSTAT=ERR) (YFREQ(I),I=1,6)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
          READ(C255,'(6F6.2)',IOSTAT=ERR) (YRSREF(I),I=1,6)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
          READ(C255,'(6I6)',IOSTAT=ERR) (IXCUTHT(I),I=1,6)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
          READ(C255,'(6F6.2)',IOSTAT=ERR) (YCUTHT(I),I=1,6)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
          READ(C255,'(6I6)',IOSTAT=ERR) (IXCHMOW(I),I=1,6)
          CALL IGNORE(LUNCRP,LNUM,ISECT,C255)
          READ(C255,'(6F6.2)',IOSTAT=ERR) (YCHMOW(I),I=1,6)
          
          XCUTHT = IXCUTHT
          XCHMOW = IXCHMOW
          XFRGDD = IXFRGDD
          IF(ATTP .EQ. 'W') THEN
            XFREQ = IXFREQ
          ELSEIF( ATTP .EQ. 'X') THEN
            XFREQ = IXFRGDD
          ENDIF
        ENDIF
      ENDIF

      CLOSE(LUNCRP)

      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE IPSPE_FORHAR
!=======================================================================

!=======================================================================
!     SUBROUTINE OP_FORHARV
!     Outputs forage harvest info on harvest days
!-----------------------------------------------------------------------
      SUBROUTINE OP_FORHARV(CONTROL, 
     &   CUTNO, topwt, wtlf, stmwt, strwt, rtwt,  !Input
     &   xlai, fhtot, fhtotn, fhpctn, fhpcho,     !Input
     &   fhplig, fhpctlf, MOWC, RSPLC)            !Input

      USE ModuleData
      IMPLICIT NONE
      EXTERNAL GETLUN, HEADER, yr_doy
      SAVE

      TYPE (ControlType), INTENT(IN) :: CONTROL
      INTEGER, INTENT(IN) :: CUTNO
      REAL, INTENT(IN) :: topwt, wtlf, stmwt, strwt, rtwt, xlai, 
     &   fhtot, fhtotn, fhpctn, fhpcho, fhplig, fhpctlf, MOWC, 
     &   RSPLC

      character(len=10),parameter :: fhout='FORAGE.OUT'
      INTEGER fhlun, ERR, trtno, RUN, YEAR, DOY, DYNAMIC, YRDOY
      LOGICAL FEXIST
      CHARACTER*12 FILEX
      CHARACTER*2  CROP

      REAL ADF, NDF

      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      FILEX = CONTROL % FILEX
      crop   = control % crop
      trtno  = control % trtnum
      run    = control % run

        CALL GETLUN('FORHARV', fhlun)

        INQUIRE(file=FHOUT,EXIST=FEXIST)
        IF (FEXIST) THEN
          OPEN(FILE=FHOUT,UNIT=FHLUN,STATUS = 'OLD',
     &      IOSTAT = ERR, POSITION = 'APPEND')
        ELSE
          OPEN(FILE=FHOUT,UNIT=FHLUN, STATUS = 'NEW',
     &      IOSTAT = ERR)
          WRITE(fhlun,'("*Forage Model Harvest Output")')
          CALL HEADER(SEASINIT, fhlun, CONTROL % RUN)
          WRITE(fhlun,'(a)')
     &     '@RUN FILEX    CR TRNO FHNO YEAR DOY'//
     &     ' RCWAH RLWAH RSWAH RSRWH RRTWH RLAIH'//
     &     ' FHWAH FHNAH FHN%H FHC%H FHLGH FHL%H'//
     &     '  MOWC RSPLC   ADF   NDF'
        ENDIF

!***********************************************************************
!***********************************************************************
!     Output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
!       Get quality of harvest calculated in Cohorts module
        CALL GET('MHARVEST','ADF', ADF)
        CALL GET('MHARVEST','NDF', NDF)

        call yr_doy(yrdoy,year,doy)
        WRITE(fhlun,1000)
     &       run,FILEX(1:8),crop,trtno,CUTNO,year,doy,
     &       Nint(topwt*10.),Nint(wtlf*10.),Nint(stmwt*10.),
     &       Nint(strwt*10.),Nint(rtwt*10.),xlai,
     &       Nint(fhtot*10.),Nint(fhtotn*10.),
     &       fhpctn,fhpcho,fhplig,fhpctlf,
     &       MOWC,RSPLC, ADF, NDF
 1000   FORMAT(i4,x,a8,a3,2(i5),i5,i4,
     &        5(i6),f6.2,2(i6),3(f6.2),f6.1,x,f5.0,F6.1, 2F6.1)

!***********************************************************************
!***********************************************************************
!     End of Season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
          close(fhlun)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OP_FORHARV
!=======================================================================
