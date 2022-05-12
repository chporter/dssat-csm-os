!=======================================================================
!  LowInput_daily, Subroutine
!
!  Generates output in tsv format for the Low Input systems model intercomparison 2021.
!  Variables must be available through ModuleData GET routines.
!  Variables are hard-wired in this code, but could be swapped out easily.
!-----------------------------------------------------------------------
!  Revision history
!
!  09/22/2008 CHP Written
!  06/09/2021 CHP Modified to compile output from various routines and
!                 to send to tsv generic output.
!  02/17/2022 CHP Adapted OPGENERIC for use by Low Input Systems study
!=======================================================================

      Subroutine LowInput_daily
!     Also used to screen values for use in summary output for sensitivity analyses.

      USE ModuleDefs
      USE ModuleData
      IMPLICIT  NONE
      SAVE

      CHARACTER*2 CROPID
      CHARACTER*6, PARAMETER :: ERRKEY = 'GENPRN'
      CHARACTER*11 SEASONID
      CHARACTER*19 OUTG2 
      CHARACTER*13, DIMENSION(12) :: HeaderTxt
      CHARACTER*10 DateText
      CHARACTER*78, MSG(10)
!     CHARACTER*220 FMT_STRING_A, FMT_STRING_C
!     CHARACTER*220 HDR_String_C  !, HDR_String_A, 
      INTEGER DAS, DOY, DYNAMIC, ERRNUM, LUN1, LUN2
      INTEGER NLayr, RUN, YEAR, YRDOY, NLayers, YRPLT, SEASON
      LOGICAL FEXIST, FIRST

      INTEGER NVars, I, L, TargetDOY, PDOY, PYRDOY, YR


!     Variables needed for computation of output variables:
      REAL, DIMENSION(NL) :: SON, SOC, SW   
      REAL, DIMENSION(NL) :: DS, DLAYR   
      REAL TSW, TDEP, QCO2hum, QCO2res, QNhum, QNres
      REAL SOCtop, SONtop, SumSOC, SumSON, SWplt
      REAL SumQCO2hum, SumQCO2res, SumQNhum, SumQNres

!     Site ID
      CHARACTER*4 SITEID, TRTNAME

      TYPE (ControlType) CONTROL
      TYPE (SoilType) SOILPROP

      DATA FIRST /.TRUE./

      CALL GET(CONTROL)

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      CALL YR_DOY(YRDOY, YEAR, DOY)

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!-----------------------------------------------------------------------
!       Use treatment name to get site ID and Low Input treatment
        SITEID  = CONTROL % TITLET(1:4)
!        TRTNAME = CONTROL % TITLET(13:16)
        SEASONID= CONTROL % TITLET(1:11)
        READ(SEASONID,'(10X,I1)') SEASON
!        CROPID  = CONTROL % TITLET(18:19)
!        IF (CROPID == 'FA') THEN
!          SEASONID = TRIM(SEASONID) // "_FA"
!        ENDIF

!        OUTG2 = SITEID // "_" // TRTNAME // "_daily.txt"

!        NVars = 7
!        HeaderTxt = '         '

!-----------------------------------------------------------------------
!!       Initialize headers and output formats, set everything to zero for now.
!        HeaderTxt(1) ='          SOC' 
!        HeaderTxt(2) ='          SON' 
!        HeaderTxt(3) ='      QCO2hum' 
!        HeaderTxt(4) ='      QCO2res' 
!        HeaderTxt(5) ='        QNhum' 
!        HeaderTxt(6) ='        QNres' 
!        HeaderTxt(7) =' SoilW.layer1' 

!!       Build the format strings and the header text for tsv
!        HDR_String_C = ADJUSTL(HeaderTxt(1)) !tsv header line
!        DO I = 2, NVars
!          HDR_String_C = TRIM(HDR_String_C) // achar(9) // 
!     &                   TRIM(ADJUSTL(HeaderTxt(I)))
!        ENDDO

!!       ----------------------------------------------------
!!       Open tab-delimited file and write headers
!        IF (FIRST) THEN
!          FIRST = .FALSE.
!          CALL GETLUN('GenericC', LUN2)
!          INQUIRE (FILE = OUTG2, EXIST = FEXIST)
!          IF (FEXIST) THEN
!            OPEN (UNIT = LUN2, FILE = OUTG2, STATUS = 'REPLACE',
!     &        IOSTAT = ERRNUM)
!          ELSE
!            OPEN (UNIT = LUN2, FILE = OUTG2, STATUS = 'NEW',
!     &        IOSTAT = ERRNUM)
!          ENDIF
!          WRITE(LUN2,'(20A)') 
!     &      "Model",achar(9),"id_site",achar(9),"id_season",achar(9),
!     &      "id_treatment",achar(9),"Date",achar(9),TRIM(HDR_String_C)
!        ENDIF

!       For Low input systems, the layer depth is dependent on location
! id_site	season	crop	cultivar	cul_note	pdate_doy	plant_population	row_spacing	planting_depth
!    ICGA	     1	maize	IRAT83	    hybrid	          103	           5	             80	             5
!    ICGA	     2	maize	IRAT83	    hybrid	          253	           5	             80	             5
!    KEEM	     1	maize	H513	    hybrid	           88	           3.92	             85.8	         5
!    KEEM	     2	maize	H513	    hybrid	          291	           3.92	             85.8	         5
!    KEMA	     1	maize	Kutamani	hybrid	           88	           5.32	             75	             5
!    KEMA	     2	maize	Kutamani	hybrid	          297	           5.32	             75	             5
!    ZIMU	     1	maize	SC525	    hybrid	          329	           4.4444            90	             5

        SELECT CASE(SITEID)
        CASE ('ICGA')
          NLayers = 3     !for SW, SOC, SON
          TargetDOY = 80  !for SOC, SON
          SELECT CASE (SEASON)
            CASE (1); PDOY = 103  ! for SW
            CASE (2); PDOY = 253
            CASE DEFAULT; PDOY = 0
          END SELECT
        CASE ('ZIMU')
          NLayers = 3
          TargetDOY = 305
          PDOY = 329
        CASE ('KEMA')
          NLayers = 2
          TargetDOY = 64
          SELECT CASE (SEASON)
            CASE (1); PDOY = 88
            CASE (2); PDOY = 297
            CASE DEFAULT; PDOY = 0
          END SELECT
        CASE ('KEEM')
          NLayers = 2
          TargetDOY = 66
          SELECT CASE (SEASON)
            CASE (1); PDOY = 88
            CASE (2); PDOY = 291
            CASE DEFAULT; PDOY = 0
          END SELECT
        CASE DEFAULT
          NLayers = 0
          PDOY = 0
          MSG(1) = "Wrong site ID."
          CALL WARNING(1, ERRKEY, MSG)
        END SELECT

        CALL YR_DOY(CONTROL % YRSIM, YR, DOY)
        PYRDOY = YR * 1000 + PDOY

        CALL GET(SOILPROP)
        NLayr = SOILPROP % NLayr
        DS    = SOILPROP % DS
        DLAYR = SOILPROP % DLAYR

        QCO2hum = 0.0
        QCO2res = 0.0
        QNhum = 0.0
        QNres = 0.0

        SumQCO2hum = 0.0
        SumQCO2res = 0.0
        SumQNhum   = 0.0
        SumQNres   = 0.0

!***********************************************************************
!***********************************************************************
!     Daily OUTPUT 
!-----------------------------------------------------------------------
      ELSE
!***********************************************************************
!     Today's date
      CALL Date_Text (YRDOY, DateText)

!     Get daily values, calculate cumulative values
      CALL GET('ORGC', 'QCO2hum', QCO2hum)
      CALL GET('ORGC', 'QCO2res', QCO2res)
      CALL GET('ORGC', 'QNhum', QNhum)
      CALL GET('ORGC', 'QNres', QNres)

      SumQCO2hum = SumQCO2hum + QCO2hum
      SumQCO2res = SumQCO2res + QCO2res
      SumQNhum   = SumQNhum   + QNhum
      SumQNres   = SumQNres   + QNres

!     ----------------------------------------------------
!     Extract soil water at specified depths on planting date.
!      CALL GET('PLANT', 'YRPLT', YRPLT)
 !     IF (YRDOY .EQ. YRPLT) THEN
      IF (YRDOY .EQ. PYRDOY) THEN
        CALL GET('WATER', 'SW', SW) 
!       Extract soil water at specified depths
        TSW    = 0.0
        DO L = 1, NLayers
          TSW = TSW + SW(L) * DLAYR(L) !cm
          TDEP = DS(L)
        ENDDO
        SWplt = TSW * 10.  !mm
        CALL PUT('WATER', 'SWplt', SWplt)
      ENDIF

!     ----------------------------------------------------
      IF (DOY .EQ. TargetDOY) THEN
        CALL GET('ORGC', 'SOC', SOC)
        CALL GET('ORGC', 'SON', SON)

!       Extract soil water at specified depths
        SOCtop = 0.0
        SONtop = 0.0
        DO L = 1, NLayers
          SOCtop = SOCtop + SOC(L)
          SONtop = SONtop + SON(L)
        ENDDO
        SumSOC = SOCtop / 1000.   !t
        SumSON = SONtop / 1000.   !t

        CALL PUT('ORGC', 'SumSOC', SumSOC)
        CALL PUT('ORGC', 'SumSON', SumSON)
      ENDIF

!!     ----------------------------------------------------
!!     TSV format output
!      WRITE(LUN2,
!     &  '(10A,F0.1,A,F0.2,A,F0.2,A,F0.2,A,F0.2,A,F0.2,A,F5.3)')
!     &  "CE1", achar(9), SITEID, achar(9), TRIM(SEASONID), achar(9), 
!     &  TRTNAME, achar(9), TRIM(DateText), achar(9), 
!     &  SOCtop, achar(9), SONtop, achar(9), QCO2hum, achar(9), 
!     &  QCO2res, achar(9), QNhum, achar(9), QNres, achar(9), SWAVG

      ENDIF
!***********************************************************************
!***********************************************************************
!     End of season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASEND) THEN
!***********************************************************************
        CLOSE (LUN1)
        CLOSE (LUN2)

        CALL PUT('ORGC', 'SumQCO2hum', SumQCO2hum)
        CALL PUT('ORGC', 'SumQCO2res', SumQCO2res)
        CALL PUT('ORGC', 'SumQNhum', SumQNhum)
        CALL PUT('ORGC', 'SumQNres', SumQNres)

!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE LowInput_daily
!=======================================================================
