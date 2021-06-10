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

      Subroutine OPGENERIC

      USE ModuleDefs
      USE ModuleData
      IMPLICIT  NONE
      SAVE

      CHARACTER*10, DIMENSION(12) :: FormatTxt 
      CHARACTER*10, DIMENSION(12) :: HeaderTxt
      CHARACTER*11, PARAMETER :: OUTG1 = 'Generic.OUT'
      CHARACTER*11, PARAMETER :: OUTG2 = 'Generic.CSV'
      CHARACTER*220 FMT_STRING_A, FMT_STRING_C
      CHARACTER*220 HDR_String_A, HDR_String_C
      INTEGER DAS, DOY, DYNAMIC, ERRNUM, LUN1, LUN2
      INTEGER RUN, YEAR, YRDOY, NLayr
      LOGICAL FEXIST, FIRST

      INTEGER NVars, I, L
      REAL, DIMENSION(3) :: SoilDepth
      REAL RootingDepth, Fraction

!     List of variables to output:
      REAL LAID   !Leaf area index
      REAL NUPC   !N uptake, cumulative seasonal, kg/ha
      REAL ESAA   !Soil evaporation, daily, mm
      REAL EPAA   !Plant transpiration, daily, mm
      REAL NMNC   !Net N mineralization, cumulative, kg/ha
      REAL SWD1   !Soil water content at specified depth, mm3/mm3
      REAL SWD2   !Soil water content at specified depth, mm3/mm3
      REAL SWD3   !Soil water content at specified depth, mm3/mm3
      REAL SNO3D  !Soil nitrate content to rooting depth, kg/ha
      REAL SNH4D  !Soil ammonium content to rooting depth, kg/ha
      REAL SCTD   !Soil organic C, kg/ha
      REAL SNTD   !Soil organic N, kg/ha

!     Variables needed for computation:
      REAL, DIMENSION(NL) :: SW
      REAL, DIMENSION(NL) :: SNO3
      REAL, DIMENSION(NL) :: SNH4
      REAL, DIMENSION(NL) :: DS

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
        NVars = 12
        HeaderTxt = '         '
        FormatTxt = '         '
        
!       Depths to report soil variables
        SoilDepth(1) = 5.   !cm (SW1D)
        SoilDepth(2) = 30.  !cm (SW2D)
        SoilDepth(3) = 90.  !cm (SW3D)
        RootingDepth = 100. !cm (SNO3D and SNH4D)
        
!       Initialize headers and output formats, set everything to zero for now.
        HeaderTxt(1) ='      LAID' ; FormatTxt(1) = 'F10.2'; LAID  = 0.0
        HeaderTxt(2) ='      NUPC' ; FormatTxt(2) = 'F10.1'; NUPC  = 0.0
        HeaderTxt(3) ='      ESAA' ; FormatTxt(3) = 'F10.2'; ESAA  = 0.0
        HeaderTxt(4) ='      EPAA' ; FormatTxt(4) = 'F10.2'; EPAA  = 0.0
        HeaderTxt(5) ='      NMNC' ; FormatTxt(5) = 'F10.1'; NMNC  = 0.0
        HeaderTxt(6) ='      SWD1' ; FormatTxt(6) = 'F10.3'; SWD1  = 0.0
        HeaderTxt(7) ='      SWD2' ; FormatTxt(7) = 'F10.3'; SWD2  = 0.0
        HeaderTxt(8) ='      SWD3' ; FormatTxt(8) = 'F10.3'; SWD3  = 0.0
        HeaderTxt(9) ='     SNO3D' ; FormatTxt(9) = 'F10.2'; SNO3D = 0.0
        HeaderTxt(10)='     SNH4D' ; FormatTxt(10)= 'F10.2'; SNH4D = 0.0
        HeaderTxt(11)='      SNTD' ; FormatTxt(11)= 'F10.0'; SCTD  = 0.0
        HeaderTxt(12)='      SNTD' ; FormatTxt(12)= 'F10.0'; SNTD  = 0.0
     
!       Build the format string and the header text
        FMT_STRING_A = "(I5,I4,I6," // TRIM(FormatTxt(1))   !Format for outputs
!       FMT_STRING_C = "(I5,A1,I4,A1,I6,A1," // TRIM(FormatTxt(1))
        HDR_String_A = HeaderTxt(1) !ASCII header line
        HDR_String_C = ADJUSTL(HeaderTxt(1)) !CSV header line
        DO I = 2, NVars
          FMT_STRING_A = TRIM(FMT_STRING_A) // "," // TRIM(FormatTxt(I))
!         FMT_STRING_C = TRIM(FMT_STRING_C) //",A1,"//TRIM(FormatTxt(I))
          HDR_String_A = TRIM(HDR_String_A) // TRIM(HeaderTxt(I))
          HDR_String_C = TRIM(HDR_String_C) // "," // 
     &                   TRIM(ADJUSTL(HeaderTxt(I)))
        ENDDO
        FMT_STRING_A = TRIM(FMT_STRING_A) // ")"
!       FMT_STRING_C = TRIM(FMT_STRING_C) // ")"


        WRITE(FMT_STRING_C,'(A,I2,A)') "(", NVars+7, "(g0,','),g0)"

!       ----------------------------------------------------
!       ASCII format output
        CALL GETLUN('GenericA', LUN1)
      
        INQUIRE (FILE = OUTG1, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUN1, FILE = OUTG1, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = LUN1, FILE = OUTG1, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(LUN1,'("*Generic daily output")')
        ENDIF
        CALL HEADER(SEASINIT, LUN1, RUN)
        WRITE(LUN1,'(A,A)') "@YEAR DOY   DAS", TRIM(HDR_String_A)

!       ----------------------------------------------------
!       CSV format output
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
          WRITE(LUN2,'(A,A)') "YEAR,DOY,DAS,",TRIM(HDR_String_C)
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily OUTPUT
!***********************************************************************
!     Get daily values
      CALL GET(SOILPROP)
      NLayr = SOILPROP % NLayr
      DS    = SOILPROP % DS

      CALL GET('PLANT', 'LAID',    LAID) 
      CALL GET('PLANT', 'NUPTAKE', NUPC) 
      CALL GET('SPAM',  'ES',      ESAA) 
      CALL GET('SPAM',  'EP',      EPAA) 
      CALL GET('NITR',  'NET_MIN', NMNC) 
      CALL GET('NITR',  'SNO3',    SNO3)
      CALL GET('NITR',  'SNH4',    SNH4)
      CALL GET('WATER', 'SW',      SW) 
      CALL GET('ORGC',  'SCTD',    SCTD)
      CALL GET('ORGC',  'SNTD',    SNTD)
      
!     Extract soil water at specified depths
      SWD1 = SW(1)
      SWD2 = SW(1)
      SWD3 = SW(1)
      DO L = 2, NLayr
        IF (DS(L) > SoilDepth(1)) SWD1 = SW(L)
        IF (DS(L) > SoilDepth(2)) SWD2 = SW(L)
        IF (DS(L) > SoilDepth(3)) SWD3 = SW(L)
      ENDDO

!     Extract SNO3 and SNH4 through rooting depth
      SNO3D = SNO3(1)
      SNH4D = SNH4(1)
      DO L = 2, NLAYR
        IF (DS(L) < RootingDepth) THEN
          SNO3D = SNO3D + SNO3(L)
          SNH4D = SNH4D + SNH4(L)
        ELSEIF (DS(L-1) < RootingDepth) THEN
          Fraction = (RootingDepth - DS(L-1)) / (DS(L) - DS(L-1))
          SNO3D = SNO3D + SNO3(L) * Fraction
          SNH4D = SNH4D + SNH4(L) * Fraction
        ELSE
          EXIT
        ENDIF
      ENDDO

!     ----------------------------------------------------
!     ASCII format output
      WRITE(LUN1,TRIM(FMT_STRING_A)) YEAR, DOY, DAS, LAID, NUPC, ESAA, 
     &    EPAA, NMNC, SWD1, SWD2, SWD3, SNO3D, SNH4D, SCTD, SNTD
     
!     ----------------------------------------------------
!     CSV format output
!      WRITE(LUN2,FMT_STRING_C) YEAR, ",", DOY, ",", DAS, ",", LAID, 
!     &  ",", NUPC, ",", ESAA, ",", EPAA, ",", NMNC, ",", SWD1, ",", 
!     &  SWD2, ",", SWD3, ",", SNO3D, ",", SNH4D, ",", SCTD, ",", SNTD
      WRITE(LUN2,TRIM(FMT_STRING_C)) RUN, CONTROL%FILEX, CONTROL%TRTNUM,
     &    CONTROL%ROTNUM, CONTROL%REPNO, YEAR, DOY, DAS, 
     &    LAID, NUPC, ESAA, EPAA, NMNC, 
     &    SWD1, SWD2, SWD3, SNO3D, SNH4D, SCTD, SNTD

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
