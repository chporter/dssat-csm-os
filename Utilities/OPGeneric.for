!=======================================================================
!  OPGENERIC, Module
!
!  Generates output for simulated data, up to 12 variables
!  Variables must be available through ModuleData GET routines.
!  Variables are hard-wired in this code, but could be swapped out easily.
!-----------------------------------------------------------------------
!  Revision history
!
!  09/22/2008 CHP Written
!  06/09/2021 CHP Modified to compile output from various routines and
!                 to send to csv generic output.
!=======================================================================

      Module OPGENERIC (SOILPROP)

      USE ModuleDefs
      USE ModuleData
      IMPLICIT  NONE
      SAVE

      CHARACTER*10, DIMENSION(12) :: FormatTxt 
      CHARACTER*10, DIMENSION(12) :: HeaderTxt
      CHARACTER*11, PARAMETER :: OUTG = 'Generic.OUT'
      CHARACTER*120 FMT_STRING
      INTEGER DAS, DOY, DYNAMIC, ERRNUM, LUN
      INTEGER RUN, YEAR, YRDOY
      LOGICAL FEXIST

      INTEGER NVars
      REAL SoilDepth

!     List of variables to bring in:
      REAL LAID   !Leaf area index
      REAL NUPC   !N uptake, cumulative seasonal, kg/ha
      REAL ESAA   !Soil evaporation, daily, mm
      REAL EPAA   !Plant transpiration, daily, mm
      REAL NMNC   !Net N mineralization, cumulative, kg/ha
      REAL, DIMENSION(NL) :: SW
      REAL, DIMENSION(NL) :: SNO3
      REAL, DIMENSION(NL) :: SNH4
      REAL, DIMENSION(NL) :: SOC
      REAL, DIMENSION(NL) :: SON

      TYPE (ControlType) CONTROL
      TYPE (SoilType) SOILPROP

      CALL GET(CONTROL)

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!-----------------------------------------------------------------------
      HeaderTxt = '         '
      FormatTxt = '         '

!     Initialize headers and output formats
      HeaderTxt(1) = 'LAID'; FormatTxt(1) = 'F10.2'; LAID = 0.0
      HeaderTxt(2) = 'NUPC'; FormatTxt(2) = 'F10.2'; NUPC = 0.0
      HeaderTxt(3) = ''; FormatTxt = 'F10.2';  = 0.0
      HeaderTxt(4) = ''; FormatTxt = 'F10.2';  = 0.0
      HeaderTxt(5) = ''; FormatTxt = 'F10.2';  = 0.0
      HeaderTxt(6) = ''; FormatTxt = 'F10.2';  = 0.0
      HeaderTxt(7) = ''; FormatTxt = 'F10.2';  = 0.0
      HeaderTxt(8) = ''; FormatTxt = 'F10.2';  = 0.0
      HeaderTxt(9) = ''; FormatTxt = 'F10.2';  = 0.0
      HeaderTxt(10)= ''; FormatTxt = 'F10.2';  = 0.0
      HeaderTxt(11)= ''; FormatTxt = 'F10.2';  = 0.0
      HeaderTxt(12)= ''; FormatTxt = 'F10.2';  = 0.0
     
!     Depth to report soil variables
      SoilDepth = 30. !cm
      CALL GET(SOILPROP)
      


        CALL GETLUN('Generic', LUN)
      
        INQUIRE (FILE = OUTG, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUN, FILE = OUTG, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = LUN, FILE = OUTG, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(LUN,'("*Generic daily output")')
        ENDIF



!       Write headers
        CALL HEADER(SEASINIT, LUN, RUN)
        WRITE (LUN,'(A,A)') "@YEAR DOY   DAS", HeaderTxt(1:Width)

        FMT_STRING = "(1X,I4,1X,I3.3,1X,I5)"  

!***********************************************************************
!***********************************************************************
!     Daily OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC == OUTPUT .OR. 
     &        DYNAMIC == INTEGR .OR. 
     &        DYNAMIC == RATE) THEN

        CALL YR_DOY(YRDOY, YEAR, DOY)
        WRITE (LUN,FMT_STRING,ADVANCE='NO') YEAR, DOY, DAS
        SELECT CASE (NVars)
        CASE(1); WRITE(LUN,FormatTxt) VAR1
        CASE(2); WRITE(LUN,FormatTxt) VAR1,VAR2
        CASE(3); WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3
        CASE(4); WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3,VAR4
        CASE(5); WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3,VAR4,VAR5
        CASE(6); WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3,VAR4,VAR5,VAR6
        CASE(7); WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3,VAR4,VAR5,VAR6,VAR7
        CASE(8); WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3,VAR4,VAR5,VAR6,
     &                                VAR7,VAR8
        CASE(9); WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3,VAR4,VAR5,VAR6,
     &                                VAR7,VAR8,VAR9
        CASE(10);WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3,VAR4,VAR5,VAR6,
     &                                VAR7,VAR8,VAR9,VAR10
        CASE(11);WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3,VAR4,VAR5,VAR6,
     &                                VAR7,VAR8,VAR9,VAR10,VAR11
        CASE(12);WRITE(LUN,FormatTxt) VAR1,VAR2,VAR3,VAR4,VAR5,VAR6,
     &                                VAR7,VAR8,VAR9,VAR10,VAR11,VAR12
        END SELECT

!***********************************************************************
!***********************************************************************
!     End of season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASEND) THEN
!***********************************************************************
        CLOSE (LUN)

!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPGENERIC
!=======================================================================
