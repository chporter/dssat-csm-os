!=======================================================================
!  OPGENERIC, Subroutine
!
!  Generates output for a mix of simulated data from various modules.
!  Variables must be available through ModuleData GET routines.
!  Variables are hard-wired in this code, but could be swapped out easily.
!-----------------------------------------------------------------------
!  Revision history
!
!  09/22/2008 CHP Written
!  06/09/2021 CHP Modified to compile output from various routines and
!                 to send to csv generic output.
!=======================================================================

      Subroutine OPGENERIC

      USE ModuleDefs
      USE ModuleData
      IMPLICIT  NONE
      SAVE

      CHARACTER*2 CROPID
      CHARACTER*6, PARAMETER :: ERRKEY = 'GENPRN'
      CHARACTER*10, DIMENSION(12) :: FormatTxt 
      CHARACTER*11 SEASONID
      CHARACTER*11, PARAMETER :: OUTG1 = 'Generic.OUT'
      CHARACTER*11, PARAMETER :: OUTG2 = 'Generic.CSV'
      CHARACTER*13, DIMENSION(12) :: HeaderTxt
      CHARACTER*13 DATETXT
      CHARACTER*78, MSG(10)
!     CHARACTER*220 FMT_STRING_A, FMT_STRING_C
      CHARACTER*220 HDR_String_C  !, HDR_String_A, 
      INTEGER DAS, DOY, DYNAMIC, ERRNUM, LUN1, LUN2
      INTEGER NLayr, INCDAT, RUN, YEAR, YRDOY, YRDOY0, NLayers
      LOGICAL FEXIST, FIRST

      INTEGER NVars, I, L, iMON, NDAY

!     Variables needed for computation of output variables:
      REAL, DIMENSION(NL) :: SON, SOC, SW   
      REAL, DIMENSION(NL) :: DS, DLAYR   
      REAL TSW, TDEP, SWAVG, QCO2hum, QCO2res, QNhum, QNres
      REAL SOCtop, SONtop

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
        NVars = 7
        HeaderTxt = '         '
        FormatTxt = '         '

!       Initialization values printed for seasinit represent end of previous day 
        YRDOY0 = INCDAT(YRDOY,-1)
        CALL YR_DOY(YRDOY0, YEAR, DOY)

!-----------------------------------------------------------------------
!       Initialize headers and output formats, set everything to zero for now.
        HeaderTxt(1) ='          SOC' ; FormatTxt(1) = 'F13.0'
        HeaderTxt(2) ='          SON' ; FormatTxt(2) = 'F13.1'
        HeaderTxt(3) ='      QCO2hum' ; FormatTxt(3) = 'F13.2'
        HeaderTxt(4) ='      QCO2res' ; FormatTxt(4) = 'F13.2'
        HeaderTxt(5) ='        QNhum' ; FormatTxt(5) = 'F13.2'
        HeaderTxt(6) ='        QNres' ; FormatTxt(6) = 'F13.2'
        HeaderTxt(7) =' SoilW.layer1' ; FormatTxt(7) = 'F13.3'

        QCO2hum = 0.0
        QCO2res = 0.0
        QNhum = 0.0
        QNres = 0.0

!       Build the format strings and the header text for ASCII and CSV
!        FMT_STRING_A = "(I5,I4,I6," // TRIM(FormatTxt(1)) 
!        HDR_String_A = HeaderTxt(1) !ASCII header line
        HDR_String_C = ADJUSTL(HeaderTxt(1)) !CSV header line
        DO I = 2, NVars
!          FMT_STRING_A = TRIM(FMT_STRING_A) // "," // TRIM(FormatTxt(I))
!          HDR_String_A = TRIM(HDR_String_A) // TRIM(HeaderTxt(I))
          HDR_String_C = TRIM(HDR_String_C) // "," // 
     &                   TRIM(ADJUSTL(HeaderTxt(I)))
        ENDDO
!        FMT_STRING_A = TRIM(FMT_STRING_A) // ")"
!        WRITE(FMT_STRING_C,'(A,I2,A)') "(", NVars+4, "(g0,','),F5.3)"

!!       ----------------------------------------------------
!!       Open ASCII file and write headers
!        CALL GETLUN('GenericA', LUN1)
!        INQUIRE (FILE = OUTG1, EXIST = FEXIST)
!        IF (FEXIST) THEN
!          OPEN (UNIT = LUN1, FILE = OUTG1, STATUS = 'OLD',
!     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
!        ELSE
!          OPEN (UNIT = LUN1, FILE = OUTG1, STATUS = 'NEW',
!     &      IOSTAT = ERRNUM)
!          WRITE(LUN1,'("*Generic daily output")')
!        ENDIF
!        CALL HEADER(SEASINIT, LUN1, RUN)
!        WRITE(LUN1,'(A,A)') "@YEAR DOY   DAS", TRIM(HDR_String_A)

!       ----------------------------------------------------
!       Open CSV file and write headers
        IF (FIRST) THEN
          FIRST = .FALSE.
          CALL GETLUN('GenericC', LUN2)
          INQUIRE (FILE = OUTG2, EXIST = FEXIST)
          IF (FEXIST) THEN
            OPEN (UNIT = LUN2, FILE = OUTG2, STATUS = 'REPLACE',
     &        IOSTAT = ERRNUM)
          ELSE
            OPEN (UNIT = LUN2, FILE = OUTG2, STATUS = 'NEW',
     &        IOSTAT = ERRNUM)
          ENDIF
          WRITE(LUN2,'(A,A)') 
     &      "Model,id_site,id_season,id_treatment,Date,",
     &      TRIM(HDR_String_C)
        ENDIF

!       Use treatment name to get site ID and Low Input treatment
        SITEID  = CONTROL % TITLET(1:4)
        TRTNAME = CONTROL % TITLET(13:16)
        SEASONID= CONTROL % TITLET(1:11)
        CROPID  = CONTROL % TITLET(18:19)
        IF (CROPID == 'FA') THEN
          SEASONID = TRIM(SEASONID) // "_FA"
        ENDIF

!       For Low input systems, the layer depth is dependent on location
        SELECT CASE(SITEID)
        CASE ('ICGA','ZIMU')
          NLayers = 3
        CASE ('KEMA','KEEM')
          NLayers = 2
        CASE DEFAULT
          NLayers = 0
          MSG(1) = "Wrong site ID."
          CALL WARNING(1, ERRKEY, MSG)
        END SELECT

        CALL GET(SOILPROP)
        NLayr = SOILPROP % NLayr
        DS    = SOILPROP % DS
        DLAYR = SOILPROP % DLAYR

!***********************************************************************
!***********************************************************************
!     Daily OUTPUT 
!-----------------------------------------------------------------------
      ELSE
!***********************************************************************
!     Today's date
      CALL ETAD_NAILUJ (DOY, YEAR, iMON, NDAY)
      WRITE(DATETXT, '(I4,"-",I2.2,"-",I2.2)') YEAR, iMON, NDAY

!     Get daily values
      CALL GET('ORGC', 'SOC', SOC)
      CALL GET('ORGC', 'SON', SON)
      CALL GET('ORGC', 'QCO2hum', QCO2hum)
      CALL GET('ORGC', 'QCO2res', QCO2res)
      CALL GET('ORGC', 'QNhum', QNhum)
      CALL GET('ORGC', 'QNres', QNres)
      CALL GET('WATER', 'SW', SW) 

!     Extract soil water at specified depths
      TSW    = 0.0
      SOCtop = 0.0
      SONtop = 0.0

      DO L = 1, NLayers
        TSW = TSW + SW(L) * DLAYR(L)
        TDEP = DS(L)
        SOCtop = SOCtop + SOC(L)
        SONtop = SONtop + SON(L)
      ENDDO

      SWAVG = TSW / TDEP
      SOCtop = SOCtop / 1000.   !t
      SONtop = SONtop / 1000.   !t

!     ----------------------------------------------------
!!     ASCII format output
!      WRITE(LUN1,TRIM(FMT_STRING_A)) YEAR, DOY, DAS, 
!     &    SOCtop, SONtop, QCO2hum, QCO2res, QNhum, QNres, SWAVG
     
!     ----------------------------------------------------
!     CSV format output
      WRITE(LUN2,
     &  '(A,",",A,",",A,",",A,",",A,",",
     &    F0.1,",",F0.2,",",F0.2,",",F0.2,",",F0.2,",",F0.2,",",F5.3)')
     &  "CE1", SITEID, TRIM(SEASONID), TRTNAME, TRIM(DATETXT),
     &  SOCtop, SONtop, QCO2hum, QCO2res, QNhum, QNres, SWAVG

      ENDIF
!***********************************************************************
!***********************************************************************
!     End of season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASEND) THEN
!***********************************************************************
        CLOSE (LUN1)
        CLOSE (LUN2)

!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPGENERIC
!=======================================================================
