!=================================================================
!  Read_methane, Subroutine, CHPorter
!  Reads methane parameters from METHAxxx.SDA file
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  01/01/2025 CHP, EHFMS Written
!-----------------------------------------------------------------------
!  Called by: Methane.for
!=======================================================================

      SUBROUTINE Read_methane(CONTROL, BRAD, WFPS_methane)
!-----------------------------------------------------------------------
      USE ModuleDefs   
      IMPLICIT NONE
      EXTERNAL FIND2, INFO, GETLUN, WARNING, IGNORE, PATH, Read_ERROR
      SAVE

      TYPE (ControlType), INTENT(IN) :: CONTROL
      REAL,               INTENT(OUT) :: BRAD
      REAL,               INTENT(OUT) :: WFPS_methane

      CHARACTER*6, PARAMETER :: ERRKEY = 'MethPar'
      CHARACTER*12 FILEX, EXPER, NAMEF
      CHARACTER*78 MSG(3)
      CHARACTER*80 CHARTEST, PATHSD
      CHARACTER*92 Filename

      INTEGER ERR, FOUND, ISECT
      INTEGER LNUM, LUNMETH, PFLAG
      LOGICAL FEXIST

      FILEX = CONTROL % FILEX

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     First look for file in data directory
      Filename = 'METHA' // ModelVerTxt // '.SDA'
      LNUM = 0
      INQUIRE (FILE = Filename, EXIST = FEXIST)

!     Look for file in StandardData, pathname from DSSATPRO
      IF (.NOT. FEXIST) THEN
        CALL PATH('STD',CONTROL%DSSATP,PATHSD,PFLAG,NAMEF)
        Filename = TRIM(PATHSD) // Filename
      ENDIF

!     Does the file exist in StandardData directory?
      INQUIRE (FILE = Filename, EXIST = FEXIST)
      IF (.NOT. FEXIST) THEN
        CALL Read_ERROR(10, Filename, BRAD, WFPS_methane)
      ENDIF

!     Request an unused file unit number
      CALL GETLUN('FILEMETH', LUNMETH)
      OPEN (LUNMETH,FILE=Filename,STATUS='OLD',IOSTAT=ERR)
      IF (ERR > 0) THEN
        CALL Read_ERROR(10, Filename, BRAD, WFPS_methane)
        RETURN
      ENDIF

!     Read the file
  100 CALL FIND2(LUNMETH,"@EXPER",LNUM,FOUND)
      IF (FOUND .LT. 1) CALL Read_ERROR(42, Filename, BRAD,WFPS_methane)

      EXPER = '            '
      ReadLoop: DO WHILE (.TRUE.)
        CALL IGNORE(LUNMETH,LNUM,ISECT,CHARTEST)
        SELECT CASE(ISECT)
          CASE(0,2)
            IF (FILEX == "DEFAULT     ") THEN
              CALL Read_ERROR(20, Filename, BRAD, WFPS_methane)
              EXIT ReadLoop
            ELSE
!             Use default values from this file.
              FILEX = "DEFAULT     "
              REWIND (LUNMETH)
              GO TO 100
            ENDIF

          CASE(1)
!           Read the experiment identifier and the methane parameters.
            READ(CHARTEST,'(A12,F8.0,F14.0)') EXPER, BRAD, WFPS_methane
!           Is this the correct experiment data?
            IF (EXPER == FILEX) THEN
!             Are values within range?
              IF ((BRAD .LT. 0.0) .OR. 
     &            (BRAD .GT. 1.0) .OR.
     &            (WFPS_methane .LT. 0.0) .OR. 
     &            (WFPS_methane .GT. 1.0)) THEN
!               Values out of range
                CALL Read_ERROR(22, Filename, BRAD, WFPS_methane)
                EXIT ReadLoop
              ELSE
!               Values look good.
                EXIT ReadLoop
              ENDIF
            ELSE
!             This isn't the right experiment, read the next one.
              CYCLE
            ENDIF
        END SELECT
      ENDDO ReadLoop

      MSG(1) = "Methane parameters:"
      WRITE(MSG(2),'(A,F8.3)') "  BRAD =         ", BRAD
      WRITE(MSG(3),'(A,F8.2)') "  WFPS_methane = ", WFPS_methane

      CALL INFO(3, ERRKEY, MSG)

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE Read_methane
!=======================================================================

!=======================================================================
      SUBROUTINE Read_ERROR(ERRNUM, Filename, BRAD, WFPS_methane)
!     Error handling routine for methane parameter input

      IMPLICIT NONE
      EXTERNAL WARNING, ERROR

      INTEGER, INTENT(IN) :: ERRNUM

      CHARACTER*6, PARAMETER :: ERRKEY = 'MethPar'
      CHARACTER*78 MSG(5)
      CHARACTER*92 Filename
      REAL BRAD, WFPS_methane

      SELECT CASE(ERRNUM)
      CASE(10); MSG(1) = 'Error opening methane parameter input file.'
      CASE(42); MSG(1) = 'Data section not found in file.'
      CASE(20); MSG(1) = 'DEFAULT data section not found in file.'
      CASE(22); MSG(1) = 'One or more parameters are out of range.'
      END SELECT

      MSG(2) = "File = " // Trim(Filename)
      WRITE(MSG(3),'(A)') "Default values will be used."
      BRAD = 0.07
      WFPS_methane = 0.70
      WRITE(MSG(4),'(A,F8.3)') "BRAD =         ", BRAD
      WRITE(MSG(5),'(A,F8.2)') "WFPS_methane = ", WFPS_methane
      CALL WARNING(5,ERRKEY,MSG)

      RETURN
      END SUBROUTINE Read_ERROR
!=======================================================================
!=======================================================================