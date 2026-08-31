C=======================================================================
C  FOR_SENMOB Subroutine, S.J. Rymph, K.J. Boote, J.W. Jones and G. Hoogenboom
C  Calculates leaf, stem, root, and STOR senescence only for tissues lost
C  at "final" N concentration (due to natural aging, light stress, and 
C  physiological maturity).  Also consolidates the calculation of 
C  potential mining of C and N in one location.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  09/23/2005 SJR Created from parts of CROPGRO, DEMAND, ROOTS, SENES 
!  05/06/2026 CHP Added leaf cohorts
!  06/08/2026 CHP Added stem cohorts
C-----------------------------------------------------------------------
C  Called : CROPGRO
C  Calls  : ERROR, FIND, IGNORE
C========================================================================
      SUBROUTINE FOR_SENMOB(
     &    FILECC, CLW, DLAYR, DTX, DUL, DXR57, FNINL,           !Input
     &    FNINR, FNINS, FNINSR, ISWWAT, LL, NLAYR, NR5,         !Input 
     &    NR7, NSTRES, PAR, PCNL, PCNRT, PCNSR, PCNST,          !Input
     &    PPMFAC, RLV, RTWT, SAT, SLAAD, STMWT,                 !Input
     &    STRWT, SW, SWFAC, TDUMX, TDUMX2, VSTAGE, WCRLF,       !Input
     &    WCRRT,WCRSH, WCRSR, WCRST, WNRLF, WNRRT, WNRSH,       !Input
     &    WNRSR,WNRST, WTLF, XLAI, XPOD,                        !Input
     &    YRDOY, YRSIM, TGRO,                                   !Input
     &    CMINELF, CMINEP, CMINERT, CMINESH, CMINESR,           !Output
     &    CMINEST, CMOBMX, CMOBSR, LAIMOBR, LFCMINE,            !Output
     &    LFSCMOB, LFSENWT, LFSNMOB, LTSEN, NMINELF,            !Output
     &    NMINEP, NMINERT, NMINESR, NMINEST, NMOBR,             !Output
     &    NMOBSR, PORPT, RLSEN, RTCMINE, RTSCMOB, RTSNMOB,      !Output
     &    SHCMINE, SHNMINE, SLDOT, SLMDOT, SRCMINE,             !Output
     &    SRDOT, SRMDOT, SRNDOT, SRSCMOB, SRSNMOB, SSMDOT,      !OutpuT
     &    SSNDOT, SSDOT, SSRDOT, SSRMDOT, SSRNDOT, STCMINE,     !Output
     &    STSCMOB, STSNMOB, STLTSEN, STSENWT, TSCMOB,           !Output
     &    TSNMOB, VNMOBR,                                       !Output
!=========================================================================
!     TEMP CHP Add printout for SENES variables
     &    YRPLT,  !temporary input
!     END TEMP CHP
!=========================================================================
     &    DYNAMIC)                                              !Control

C-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      USE COHORTS_MOD
      IMPLICIT NONE
      EXTERNAL GETLUN, FIND, ERROR, IGNORE, TIMDIF, TABEX, CURV
      SAVE
      
      CHARACTER*1 ISWWAT
      CHARACTER*6  ERRKEY, SECTION
      CHARACTER*80 CHAR
      CHARACTER*92 FILECC
      CHARACTER*3 TYPLMOB, TYPNMOB 

      PARAMETER (ERRKEY = 'SENMOB')

      INTEGER I, II, LUNCRP, ERR, LNUM, ISECT
      INTEGER DYNAMIC, TIMDIF
      INTEGER YRDOY, NR5, NR7, DAS, YRSIM
      INTEGER FOUND
      INTEGER NSWAB
      PARAMETER (NSWAB = 5)

      REAL DAYL_1, DAYL_2, LFSEN  !, SNSTAGE  DAYL, 
      REAL DTX, PAR, SLAAD  !RHOL, 
      REAL STMWT, VSTAGE, WTLF, XLAI
      REAL SLDOT, SSDOT, SRDOT
      REAL ICMP, KCAN, PORPT, SENDAY
      REAL SENRT2, SENRTE, TCMP
      REAL LCMP, LTSEN, PORLFT, TABEX
      REAL SENMAX(4), SENPOR(4), XSENMX(4), XSTAGE(4)
      REAL SWFCAB(NSWAB)
      REAL SENSR, STRWT

      REAL PCNL, PCNRT, PCNSR, PCNST
      REAL PCHOLFF, PCHORTF, PCHOSRF, PCHOSTF
      REAL PROLFF, PRORTF, PROSRF, PROSTF
      REAL WCRLF, WCRRT, WCRSH, WCRSR, WCRST
      REAL WNRLF, WNRRT, WNRSH, WNRSR, WNRST

      INTEGER L, L1, NLAYR
      REAL DLAYR(NL), RLV(NL), RLSEN(NL)
      REAL DUL(NL), ESW(NL), LL(NL), SAT(NL), SW(NL)
      REAL RNDOT(NL), RLNSEN(NL) 
      REAL RFAC1, RFAC3, RLDSM, RTSEN
      REAL TRTDY, TRLSEN, RTWT  !, WRDOTN

      REAL LFNSEN, SLMDOT, SRMDOT, SSMDOT, SSRMDOT 
      REAL LFSCMOB, RTSCMOB, SRSCMOB, STSCMOB, TSCMOB
      REAL LFSNMOB, RTSNMOB, SRSNMOB, STSNMOB, TSNMOB
      REAL STNSEN

      REAL CMINELF, CMINERT, CMINESH, CMINESR, CMINEST
      REAL NMINELF, NMINERT, NMINESR, NMINEST  !NMINESH, 

      REAL CMINEP, LFCMINE, RTCMINE, SHCMINE, SRCMINE, STCMINE
      REAL NMINEP, LFNMINE, RTNMINE, SHNMINE, SRNMINE, STNMINE
      REAL DXR57, CMOBMX, CMOBSR, NMOBR, NMOBSR, PPMFAC
      
      REAL CMOBSRN, CMOBSRX, FNINL, FNINR, FNINS, 
     &   FNINSR, LAIMOBR, NMOBMX, NMOBSRN, NMOBSRX, NSTRES, 
     &   NVSMOB, SWFAC, TDUMX, TDUMX2, VEGNCMX, VEGNCNT,  
     &   VNMOBR, VNSTAT, XPOD

      REAL LRMOB(4), NRMOB(4)

      REAL CLW, RATTP, WSLOSS
      REAL LFSENWT, SLNDOT, SSNDOT, 
     &    SSRDOT, SSRNDOT, STLTSEN, STSENWT

      REAL PORMIN, RTSDF, RTEXF, SRNDOT, TRLNSEN
      REAL RTSURV, SUMEX, SUMRL, SWDF, SWEXF 

      REAL SENCLV, SENCRV, SENCSV, SENCSRV
      REAL SENNLV, SENNRV, SENNSV, SENNSRV

      REAL CMINEO, NMINEO
      REAL CURV
      REAL MOBTEM,TGRO(TS)
*      INTEGER,dimension(6) :: IXFREQ
      REAL,dimension(6) :: XMOTEM
      REAL,dimension(6) :: YMOTEM
      REAL MOBSWF

      REAL,dimension(4) :: XMOSWF
      REAL,dimension(4) :: YMOSWF

!     Leaf and stem cohorts
      REAL, DIMENSION(LCMax) :: CMINELF_c, LFNMINE_c, 
     &    NMINELF_c, WaterSen_c
      REAL LFNMINE_sum, LFNSEN_sum, LFSNMOB_sum, LTSEN_sum, NMINELF_sum,
     &    SLMDOT_sum, WaterSen_sum, LeafTotSen_SUM, LFSENWT_sum,
     &    CMINELF_sum, LFCMINE_sum 
!      REAL, DIMENSION(LCMax) :: PCNLeaf !, PCNStem
      REAL WtLeaf

      REAL, DIMENSION(LCMax) :: STNMINE_c, CMINEST_c, 
     &    NMINEST_c, SSNDOT_c
      REAL STNMINE_sum, STSNMOB_sum, STLTSEN_sum, STNSEN_sum, 
     &    NMINEST_sum, StemTotSen_sum, SSMDOT_sum, SSNDOT_sum, 
     &    STSENWT_sum, CMINEST_sum, STCMINE_sum 

!=========================================================================
!    TEMP CHP Add printout for SENESMOB variables
      EXTERNAL YR_DOY, HEADER
      TYPE (ControlType) CONTROL
      CHARACTER*13 OUTLSN  !LeafSenes.OUT
      CHARACTER*13 OUTSSN  !StemSenes.OUT
      CHARACTER*10 OUTSN2  !SENES2.OUT - whole leaf only output
      INTEGER NOUTDGL, NOUTDGS, NOUTDG2, ERRNUM, YEAR, DOY, DAP, YRPLT
      LOGICAL FEXIST
      CALL GET(CONTROL)
      DAS   = CONTROL % DAS
      YRDOY = CONTROL % YRDOY
      CALL YR_DOY(YRDOY, YEAR, DOY) 
      DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
      IF (DAP > DAS) DAP = 0
!     end temp chp
!=========================================================================

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     Read in values from input file, which were previously input
!       in Subroutine IPCROP.
!-----------------------------------------------------------------------
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

!-----------------------------------------------------------------------
!    Find and Read Photosynthesis Section
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
      LNUM = 1
      SECTION = '!*PHOT'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        ISECT = 2
        DO WHILE (ISECT .NE. 1)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        IF (ISECT .EQ. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDDO
        READ(CHAR,'(12X,F6.0)',IOSTAT=ERR) KCAN
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

!-----------------------------------------------------------------------
!    Find and Read Plant Composition Section
!-----------------------------------------------------------------------
        SECTION = '!*PLAN'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
        ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(12X,F6.0,12X,F6.0)',IOSTAT=ERR)
     &    PROLFF,PROSTF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(12X,F6.0)',IOSTAT=ERR)
     &    PRORTF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(12X, F6.0)',IOSTAT=ERR)
     &    PROSRF 
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(4F6.0)',IOSTAT=ERR)
     &    PCHOLFF, PCHOSTF, PCHORTF, PCHOSRF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF

!-----------------------------------------------------------------------
!    Find and Read Carbon and Nitrogen Mining Section
!-----------------------------------------------------------------------
      SECTION = '!*CARB'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(F6.0,12X,2F6.0)',IOSTAT=ERR) CMOBMX,NMOBMX, NVSMOB
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(2F6.0,6X,2F6.0)',IOSTAT=ERR) CMOBSRN, CMOBSRX, 
     &    NMOBSRN, NMOBSRX
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(4(1X,F5.2),3X,A3)',IOSTAT=ERR)
     &    (LRMOB(II),II=1,4), TYPLMOB
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(4(1X,F5.2),3X,A3)',IOSTAT=ERR)
     &    (NRMOB(II),II=1,4), TYPNMOB
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(4(1X,F5.2))',IOSTAT=ERR)
     &    SENNLV, SENCLV, SENNRV, SENCRV
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(4(1X,F5.2))',IOSTAT=ERR)
     &    SENNSV, SENCSV, SENNSRV, SENCSRV
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

!-----------------------------------------------------------------------
!    Find and Read Partitioning Section
!-----------------------------------------------------------------------
      SECTION = '!*VEGE'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        DO I=1,4
        ISECT = 2
        DO WHILE (ISECT .NE. 1)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        ENDDO
        ENDDO
        READ(CHAR,'(6X,F6.0)',IOSTAT=ERR) PORPT
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

!-----------------------------------------------------------------------
!    Find and Read Senescence Section
!    NOTE: First search for Section finds '!*LEAF GROWTH PARAMETERS'
!          Second search finds '!*LEAF SENESCENCE FACTORS'
!-----------------------------------------------------------------------
      SECTION = '!*LEAF'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ENDIF

      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(3F6.0)',IOSTAT=ERR) SENRTE, SENRT2, SENDAY
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
!        READ(CHAR,'(3F6.0)',IOSTAT=ERR) ICMP, TCMP, LFSEN
        READ(CHAR,'(2F6.0,F6.3)',IOSTAT=ERR) ICMP, TCMP, LFSEN
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(8F6.0)',IOSTAT=ERR)
     &  (XSTAGE(II),II=1,4),(XSENMX(II),II=1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(8F6.0)',IOSTAT=ERR)
     &  (SENPOR(II),II=1,4),(SENMAX(II),II=1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

C-----------------------------------------------------------------------
C    Find and Read Roots section
!-----------------------------------------------------------------------
      LNUM = 1
      SECTION = '!*ROOT'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(6X,4F6.0)') RFAC1, RTSEN,RLDSM, RTSDF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(12X,2F6.0)',IOSTAT=ERR) PORMIN, RTEXF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

C-----------------------------------------------------------------------
C
C     ***** READ Storage organ senescence parameters *****
C
C-----------------------------------------------------------------------
        SECTION = '!*STOR'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
        CALL ERROR(ERRKEY, 1, FILECC, LNUM)
        ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(F6.0)',IOSTAT=ERR) SENSR
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF

C-----------------------------------------------------------------------
C    Find and Read Surviving section  Added by Diego
!-----------------------------------------------------------------------
        SECTION = '!*SURVIVI'
        CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
         CALL ERROR("FRSURV", 1, FILECC, LNUM)
        ELSE
         CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
         READ(CHAR,'(6F6.0)',IOSTAT=ERR) (XMOTEM(I),I=1,6)
         IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
         CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
         READ(CHAR,'(6F6.0)',IOSTAT=ERR) (YMOTEM(I),I=1,6)
         IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
         CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
         READ(CHAR,'(6F6.0)',IOSTAT=ERR) (XMOSWF(I),I=1,4)
         IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
         CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
         READ(CHAR,'(6F6.0)',IOSTAT=ERR) (YMOSWF(I),I=1,4)
         IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF

      CLOSE (LUNCRP)


!=========================================================================
!     TEMP CHP Add printout for for_senmob variables

          OUTLSN  = 'LeafSenes.OUT'
          CALL GETLUN('OUTLSN',  NOUTDGL)

          OUTSSN  = 'StemSenes.OUT'
          CALL GETLUN('OUTSSN',  NOUTDGS)

          OUTSN2  = 'SENES2.OUT'
          CALL GETLUN('OUTSN2',  NOUTDG2)

!     end temp chp
!=========================================================================

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      SRDOT = 0.0    
      SRNDOT = 0.0   
      RLV   = 0.0
!      RTDEP = 0.0       
!      SENRT = 0.0
      SUMEX = 0.0
      SUMRL = 0.0

      LFNSEN = 0.0

      LFSNMOB = 0.0
      RTSNMOB = 0.0
      SRSNMOB = 0.0
      STSNMOB = 0.0
      TSNMOB = 0.0

      CMINELF = 0.0
      CMINEP = 0.0 
      CMINERT = 0.0
      CMINESH = 0.0
      CMINESR = 0.0
      CMINEST = 0.0
      CMOBSR = 0.0
      LFCMINE = 0.0
      LFSCMOB = 0.0
      LFSNMOB = 0.0
      LTSEN = 0.0
      NMINELF = 0.0
      NMINEP = 0.0
      NMINERT = 0.0
      NMINESR = 0.0
      NMINEST = 0.0
      NMOBR = 0.0
      NMOBSR = 0.0
      RLSEN = 0.0
      RTCMINE = 0.0
      RTSCMOB = 0.0
      RTSNMOB = 0.0
      SHCMINE = 0.0
      SHNMINE = 0.0
      SLMDOT = 0.0
      SRCMINE = 0.0
      SRMDOT = 0.0
      SRSCMOB = 0.0
      SRSNMOB = 0.0
      SSMDOT = 0.0
      SSRMDOT = 0.0
      STCMINE = 0.0
      STSCMOB = 0.0
      STSNMOB = 0.0
      TSCMOB = 0.0
      TSNMOB = 0.0      

      SSDOT  = 0.0
      SLDOT  = 0.0
      SLNDOT = 0.0
      SSNDOT = 0.0
      RATTP  = 1.0
      SSRDOT = 0.0
      SSRNDOT = 0.0
      DAYL_1 = -1.0
      DAYL_2 = -2.0

      LeafTotSen = 0.0
      LFNMNSN = 0.0   
      LFWSSN = 0.0    

      DO I = 1,5
        SWFCAB(I) = 1.0
      ENDDO

!     Leaf cohorts
      CMINELF_c  = 0.0
      LeafTotSen = 0.0  !=SLDOT
      LFCMINE_c  = 0.0
      LFNMINE_c  = 0.0
      LFNMNSN    = 0.0
      LFNSEN_c   = 0.0
      LFSENWT_c  = 0.0
      LFSNMOB_c  = 0.0
      LFWSSN     = 0.0  !=SLNDOT
      LTSEN_c    = 0.0
      NMINELF_c  = 0.0
      SLMDOT_c   = 0.0
      WaterSen_c = 0.0

      LeafTotSen_SUM = 0.0
      LFNMINE_sum = 0.0
      LFNSEN_sum  = 0.0
      LFSNMOB_sum = 0.0
      LTSEN_sum   = 0.0
      NMINELF_sum = 0.0
      SLMDOT_sum  = 0.0
      WaterSen_sum= 0.0

!     Stem cohorts
      CMINEST_c  = 0.0
      StemTotSen = 0.0
      STCMINE_c  = 0.0
      STNMINE_c  = 0.0
      STNMNSN    = 0.0
      STNSEN_c   = 0.0
      STSENWT_c  = 0.0
      STSNMOB_c  = 0.0
      STWSSN     = 0.0
      STLTSEN_c  = 0.0
      NMINEST_c  = 0.0
      SSMDOT_c   = 0.0
      SSNDOT_c   = 0.0

      StemTotSen_SUM = 0.0
      STNMINE_sum = 0.0
      STNSEN_sum  = 0.0
      STSNMOB_sum = 0.0
      STLTSEN_sum   = 0.0
      NMINEST_sum = 0.0
      SSMDOT_sum  = 0.0
      SSNDOT_sum= 0.0

!=========================================================================
!     TEMP CHP Add printout for SENESMOB variables

!       Initialize daily Leaf senescence output file      
        INQUIRE (FILE = OUTLSN, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDGL, FILE = OUTLSN, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = NOUTDGL, FILE = OUTLSN, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDGL,'("*Leaf senmob OUTPUT FILE")')
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, NOUTDGL, CONTROL % RUN)

        WRITE (NOUTDGL,200)
  200   FORMAT('@YEAR DOY   DAS   DAP',
     &   '      TotSen      NatSen     NMobSen    LoLitSen',
     &   '    WaterSen      SLMDOT      Nmob_a      Nmob_p',
     &   '     Nmob_mp     Cmine_p    Cmine_mp',
     &   '    TotSen_c    NatSen_c    NMbSen_c    LitSen_c',
     &   '    WatSen_c    SLMDOT_c     Nmoba_c     Nmobp_c',
     &   '    Nmobmp_c    Cminep_c    Cminempc')

!       Initialize daily Stem senescence output file      
        INQUIRE (FILE = OUTSSN, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDGS, FILE = OUTSSN, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = NOUTDGS, FILE = OUTSSN, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDGS,'("*Stem senmob OUTPUT FILE")')
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, NOUTDGS, CONTROL % RUN)

        WRITE (NOUTDGS,205)
  205   FORMAT('@YEAR DOY   DAS   DAP',
     &   '      TotSen      NatSen     NMobSen    LoLitSen',
     &   '    WaterSen      SSMDOT      Nmob_a      Nmob_p',
     &   '     Nmob_mp     Cmine_p    Cmine_mp',
     &   '    TotSen_c    NatSen_c    NMbSen_c    LitSen_c',
     &   '    WatSen_c    SSMDOT_c     Nmoba_c     Nmobp_c',
     &   '    Nmobmp_c    Cminep_c    Cminempc')

!       Initialize daily Leaf senescence (whole leaf only) output file
        INQUIRE (FILE = OUTSN2, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDG2, FILE = OUTSN2, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = NOUTDG2, FILE = OUTSN2, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDG2,'("*SENESMOB OUTPUT FILE2")')
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, NOUTDG2, CONTROL % RUN)
        WRITE (NOUTDG2,210)
  210   FORMAT('@YEAR DOY   DAS   DAP',
     &   '     LTotSen     LNatSen       LNMob     LLitSen',
     &   '     LWatSen      SLMDOT     LNmob_a     LNmob_p',
     &   '    LNmob_mp    LCmine_p   LCmine_mp')

!     end temp chp

!=========================================================================

!***********************************************************************
!***********************************************************************
!     DAILY RATE/INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      DAS   = MAX(0,TIMDIF(YRSIM,YRDOY))

      !Update value of RATTP.
      DO I = NSWAB,2,-1
        SWFCAB(I) = SWFCAB(I-1)
      ENDDO
!      DO I=NSWAB,2,-1
!       WSWTLF(I)=WSWTLF(I-1)
!      END DO
      SWFCAB(1) = SWFAC
!      WSWTLF(1)=WTLF
      RATTP = SWFCAB(NSWAB)

      SSDOT = 0.0   !stem
      SLDOT = 0.0   !leaf
      SLNDOT = 0.0  !leaf N

      SSNDOT = 0.0

      SSRDOT = 0.0
      SSRNDOT = 0.0

      LFNSEN = 0.0
      SLMDOT = 0.0
      SRMDOT = 0.0
      SSRMDOT = 0.0
      SSMDOT = 0.0

      LFSCMOB = 0.0
      RTSCMOB = 0.0
      SRSCMOB = 0.0
      STSCMOB = 0.0
      TSCMOB = 0.0

      LFSNMOB = 0.0
      RTSNMOB = 0.0
      SRSNMOB = 0.0
      STSNMOB = 0.0
      TSNMOB = 0.0

      CMINELF = 0.0
      CMINERT = 0.0
      CMINESR = 0.0
      CMINEST = 0.0
      
      LFCMINE = 0.0
      RTCMINE = 0.0
      SHCMINE = 0.0
      SRCMINE = 0.0
      STCMINE = 0.0

      CMINEP = 0.0
      CMINEO = 0.0

      NMINELF = 0.0
      NMINERT = 0.0
      NMINESR = 0.0
      NMINEST = 0.0

      LFNMINE = 0.0
      RTNMINE = 0.0
      SHNMINE = 0.0
      SRNMINE = 0.0
      STNMINE = 0.0

      NMINEP = 0.0
      NMINEO = 0.0

!     Leaf cohorts
      CMINELF_c  = 0.0
      LeafTotSen = 0.0  !=SLDOT
      LFCMINE_c  = 0.0
      LFNMINE_c  = 0.0
      LFNMNSN    = 0.0
      LFNSEN_c   = 0.0
      LFSENWT_c  = 0.0
      LFSNMOB_c  = 0.0
      LFWSSN     = 0.0  !=SLNDOT
      LTSEN_c    = 0.0
      NMINELF_c  = 0.0
      SLMDOT_c   = 0.0
      WaterSen_c = 0.0

      LeafTotSen_SUM = 0.0
      LFNMINE_sum = 0.0
      LFNSEN_sum  = 0.0
      LFSNMOB_sum = 0.0
      LTSEN_sum   = 0.0
      NMINELF_sum = 0.0
      SLMDOT_sum  = 0.0
      WaterSen_sum= 0.0

!     Stem cohorts
      CMINEST_c  = 0.0
      StemTotSen = 0.0
      STCMINE_c  = 0.0
      STNMINE_c  = 0.0
      STNMNSN    = 0.0
      STNSEN_c   = 0.0
      STSENWT_c  = 0.0
      STSNMOB_c  = 0.0
      STWSSN     = 0.0
      STLTSEN_c  = 0.0
      NMINEST_c  = 0.0
      SSMDOT_c   = 0.0
      SSNDOT_c   = 0.0

      StemTotSen_SUM = 0.0
      STNMINE_sum = 0.0
      STNSEN_sum  = 0.0
      STSNMOB_sum = 0.0
      STLTSEN_sum   = 0.0
      NMINEST_sum = 0.0
      SSMDOT_sum  = 0.0
      SSNDOT_sum= 0.0

!=========================================================================

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     Calculate root senescence
C-----------------------------------------------------------------------
C     Calculate root length per cm2 soil and initiate growth,
C     respiration and senescence by layer
C-----------------------------------------------------------------------
      TRTDY = 0.0
      TRLSEN = 0.0
      TRLNSEN = 0.0

      DO L = 1,NLAYR
        L1 = L
        TRTDY = TRTDY + RLV(L) * DLAYR(L)
        RLSEN(L) = 0.0
        RNDOT(L) = 0.0
        RLNSEN(L) = 0.0
      ENDDO

      SRNDOT = 0.0
!      TRLDF  = 0.0
      SUMEX = 0.0
      SUMRL = 0.0

      IF (RTWT .GE. 0.0001) THEN
!       RFAC3 = TRTDY * 10000.0 / (RTWT - WRDOTN)
!       RTWT has not yet been updated today, so use yesterday's
!       value and don't subtract out today's growth - chp 11/13/00
        RFAC3 = TRTDY * 10000.0 / RTWT
      ELSE
      RFAC3 = RFAC1
      ENDIF

      DO L = 1,L1
        RLSEN(L) = RLV(L) * RTSEN * DTX
        SWDF = 1.0
        SWEXF = 1.0

C-----------------------------------------------------------------------
C     Calculate water-stress factors only when H2O optionis "on"      
C-----------------------------------------------------------------------
      IF (ISWWAT .EQ. 'Y') THEN

        IF (SAT(L)-SW(L) .LT. PORMIN) THEN
        SWEXF = (SAT(L) - SW(L)) / PORMIN
        SWEXF = MIN(SWEXF, 1.0)
        ENDIF

!        SUMEX  = SUMEX + DLAYR(L)*RLV(L)*(1.0 - SWEXF)
        SUMEX  = SUMEX + DLAYR(L)*(RLV(L) - RLSEN(L))*(1.0 - SWEXF)
!        SUMRL  = SUMRL + DLAYR(L)*RLV(L)
        SUMRL  = SUMRL + DLAYR(L)*(RLV(L) - RLSEN(L))

!     Need to calculate ESW where used. CHP 10/15/01
        ESW(L) = DUL(L) - LL(L)
        IF (SW(L) - LL(L) .LT. 0.25*ESW(L)) THEN
        SWDF = (SW(L) - LL(L)) / (0.25*ESW(L))
        SWDF = MAX(SWDF, 0.0)
        ENDIF
      ENDIF

C-----------------------------------------------------------------------
        RTSURV = MIN(1.0,(1.-RTSDF*(1.-SWDF)),(1.-RTEXF*(1.-SWEXF)))
C-----------------------------------------------------------------------
        IF ((RLV(L) - RLSEN(L)) .GT. RLDSM) THEN
!            RNDOT(L) = RLV(L) * (1 - RTSURV)
        RNDOT(L) = (RLV(L) - RLSEN(L)) * (1 - RTSURV)
!          RLV(L) = RLV(L) * RTSURV
        ENDIF
        RLNSEN(L) = RNDOT(L)
        TRLNSEN = TRLNSEN + RLNSEN(L) * DLAYR(L)

        TRLSEN = TRLSEN + RLSEN(L) * DLAYR(L)

      ENDDO

C-----------------------------------------------------------------------
C     Calculate root senescence, growth, maintenance and growth
C     respiration, and update root length density for each layer.
!-----------------------------------------------------------------------
!     SRDOT = (TRTDY + RLNEW - TRLV) * 10000.0 / RFAC3
!     Sum RLSEN for total root senescence today. chp 11/13/00  
      SRMDOT = TRLSEN / RFAC3 * 10000.     !g/m2
      SRNDOT = TRLNSEN / RFAC3 * 10000. !g/m2
      SRDOT = SRMDOT + SRNDOT     

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     Calculate STORAGE ORGAN senescence
C-----------------------------------------------------------------------
C     This section calculates natural senescence of storage organ tissue
C      Thought about moving this below the IF...Then line but did not
C      Don't want to senesce if seedling but do want to senesce mature
C      stand after it has been cut or frozen back
C-----------------------------------------------------------------------      
      SSRMDOT = STRWT *  SENSR * DTX 
      SSRMDOT = MIN(SSRMDOT,STRWT)
C-----------------------------------------------------------------------
C     Calculate STOR water-stress senescence.
C-----------------------------------------------------------------------
        SSRNDOT = 0.0
C-----------------------------------------------------------------------
C     Calculate STOR senescence.
C-----------------------------------------------------------------------
        SSRDOT = SSRMDOT + SSRNDOT
        SSRDOT = MIN(STRWT,SSRDOT)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     Calculate LEAF senescence
C-----------------------------------------------------------------------
      IF (DAS .LE. NR7 .AND. VSTAGE .GE. 1.0) THEN
C-----------------------------------------------------------------------
C     This section calculates natural senescence prior to the
C     beginning of seed growth
C-----------------------------------------------------------------------
!        IF (VSTAGE .GE. 5.0) THEN
!          PORLFT = 1.0 - TABEX(SENPOR,XSTAGE,VSTAGE,4)
!          IF ((WTLF * ( 1.0 - RHOL)) .GT. CLW*PORLFT) THEN
!            SLDOT = WTLF * ( 1.0 - RHOL) - CLW * PORLFT
!          ENDIF
!        ENDIF
C-----------------------------------------------------------------------
C     09/18/05 SJR - Modify for forages Short harvest cycles prevent
C     VSTAGE>5.0.  CLW gets too large relative to WTLF to be useful.
C      Revise and simplify using one rate all the time, adjusted for 
C      temperature.  Rate is now a proportion of tissue senesced 
C      per physiological day.
C-----------------------------------------------------------------------
!      IF (WTLF .GT. WTLF * (1 - RHOL) * LFSEN * DTX * 
!     &  (1-EXP(-KCAN * XLAI))) THEN
!            LFNSEN = WTLF * (1 - RHOL) * LFSEN * DTX * 
        IF (WTLF .GT. WTLF * LFSEN * DTX) THEN
          LFNSEN = WTLF * LFSEN * DTX
        ELSE
          LFNSEN = WTLF
        ENDIF

!       Handle leaf cohorts
        WtLeaf = SUM(LFDM)
        IF (WtLeaf > 0.0) THEN
          DO I = 1, NLC
            LFNSEN_c(I) = LFNSEN * LFDM(I) / WtLeaf
          ENDDO
        ELSE
          LFNSEN_c = 0.0
        ENDIF

        SLMDOT = LFNSEN  
        SLMDOT_c = LFNSEN_c
        LFNSEN_sum = SUM(LFNSEN_c)
        SLMDOT_sum = LFNSEN_sum

C-----------------------------------------------------------------------
C     This section calculates senescence due to low light in lower
C     canopy.  First compute LAI at which light compensation is reached
C     then allow LAI above this amount to be senesced over TCMP thermal
C     days.
C-----------------------------------------------------------------------
        LTSEN = 0.0
        IF (PAR .GT. 0.) THEN
          LCMP = -(1. / KCAN) * ALOG(ICMP / PAR)
          LTSEN = DTX * (XLAI - LCMP) / TCMP
          LTSEN = MAX(0.0, LTSEN)
        ENDIF
C-----------------------------------------------------------------------
C     8/3/05 SJR Change LTSEN from leaf area senesced to the equivalent
C      leaf mass senesced.  Moved conversion from SLDOT update equation.
C      For ease of use in calculating DM, CH2O, and N lost in GROW 
C      subroutine
C-----------------------------------------------------------------------
       LTSEN = LTSEN * 10000. / SLAAD 

!      Handle leaf cohorts
!      Probably want to modify this calculation to use age of cohorts
!      to estimate location in the canopy.
       IF (WtLeaf > 0.0) THEN
         DO I = 1, NLC
           LTSEN_c(I) = LTSEN * LFDM(I) / WtLeaf
         ENDDO
       ENDIF

       LTSEN_sum = SUM(LTSEN_c)

C-----------------------------------------------------------------------
C     Convert area loss to biomass(m2 *10000cm2/m2)/(cm2/g)=g/m2
C-----------------------------------------------------------------------
!        SLDOT = SLDOT + LTSEN * 10000. / SLAAD 
        SLDOT = LFNSEN + LTSEN 
        SLDOT = MIN(WTLF,SLMDOT)

!       Handle leaf cohorts
        DO I = 1, NLC
          LeafTotSen(I) = LFNSEN_c(I) + LTSEN_c(I)
          LeafTotSen(I) = MIN(LFDM(I), SLMDOT_c(I))
        ENDDO

C-----------------------------------------------------------------------
C     Calculate senescence due to water stress.
C-----------------------------------------------------------------------
!       chp 2026-05-25
!       removed the comment from WSLOSS calculation line
!       removed the CLW portion of WSLOSS modification
!        IF (WTLF .GE. WSWTLF(5)) THEN
           WSLOSS = SENDAY * (1. - RATTP) * WTLF
!        ELSEIF (SENDAY*(1.-RATTP) .GT. WSWTLF(5)-WTLF) THEN
!          WSLOSS=SENDAY*(1.-RATTP)*(WSWTLF(5)-WTLF/WSWTLF(5))
!        ELSE
!          WSLOSS=0
!        ENDIF

        IF (WSLOSS .GT. 0.0) THEN
          PORLFT = 1.0 - TABEX(SENMAX, XSENMX, VSTAGE, 4)
          WSLOSS = MIN(WSLOSS, WTLF) ! - CLW * PORLFT)
          WSLOSS = MAX(WSLOSS, 0.0)
          SLNDOT = WSLOSS

          WaterSen_sum = 0.0
          DO I = 1, NLC
            WaterSen_c(I) = SENDAY * (1. - RATTP) * LFDM(I)
            WaterSen_c(I) = MIN(WaterSen_c(I), LFDM(I))
            WaterSen_c(I) = MAX(WaterSen_c(I), 0.0)
            WaterSen_sum = WaterSen_sum + WaterSen_c(I)
          ENDDO
        ENDIF

        SLDOT = SLDOT + SLNDOT
        SLDOT = MIN(WTLF,SLDOT)
        LeafTotSen = LeafTotSen + WaterSen_c

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     Calculate Stem senescence.
C-----------------------------------------------------------------------
        SSMDOT = SLMDOT * PORPT
        SSMDOT = MIN(SSMDOT,0.1*STMWT)

!       Handle stem cohorts
        DO I = 1, NLC
          SSMDOT_c(I) = SLMDOT_c(I) * PORPT
          SSMDOT_c(I) = MIN(SSMDOT_c(I), 0.1 * STDM(I))
        ENDDO

C-----------------------------------------------------------------------
C     10/04/05 SJR Link to SENMOB where natural senescence was already 
C                         calculated.  Calculate SSDOT in same way as SLDOT.
C-----------------------------------------------------------------------
        SSDOT = SSMDOT
        StemTotSen = SSMDOT_c

        SSDOT = SSDOT + LFSENWT * PORPT
        SSDOT = MIN(SSDOT,0.1*STMWT)
        STSENWT = SSDOT - SSMDOT

!       Low light senescence of stems
        SSDOT = SSDOT + LTSEN * PORPT
        SSDOT = MIN(SSDOT,0.1*STMWT)
        STLTSEN = SSDOT - (SSMDOT + STSENWT)

        DO I = 1, NLC
          StemTotSen(I) = StemTotSen(I) + LTSEN_c(I) * PORPT
          StemTotSen(I) = MIN(StemTotSen(I), 0.1 * STDM(I))
          STLTSEN_c(I) = StemTotSen(I) - SSMDOT_c(I)
        ENDDO

!       Water stress senescence of stems
        SSNDOT = SLNDOT * PORPT
        SSDOT = SSDOT + SSNDOT
        SSDOT = MIN(SSDOT, 0.1 * STMWT)
        SSNDOT = SSDOT - (SSMDOT + STSENWT + STLTSEN)

        DO I = 1, NLC
          SSNDOT_c(I) = WaterSen_c(I) * PORPT
          StemTotSen(I) = StemTotSen(I) + SSNDOT_c(I)
          StemTotSen(I) = MIN(StemTotSen(I), 0.1 * STDM(I))
          SSNDOT_c(I) = StemTotSen(I) - (SSMDOT_c(I) + STLTSEN_c(I))
          SSNDOT_c(I) = MAX(0.0, SSNDOT_c(I))
        ENDDO

C-----------------------------------------------------------------------
C     This section calculates senescence of leaves and petioles
C     after R7.
C-----------------------------------------------------------------------
      ELSEIF (DAS .GT. NR7) THEN
        IF (WTLF .GT. 0.0001) THEN
!          SLMDOT = WTLF * SENRT2
!          SLNDOT = SLDOT
          SLMDOT = 0.0
          SLMDOT_c = 0.0
!          SSMDOT = SLMDOT * PORPT
!          SSNDOT = SSDOT
          SSMDOT = 0.0
          SSMDOT_c = 0.0
        ELSE
          SLMDOT = 0.0
          SLMDOT_c = 0.0
          SSMDOT = 0.0
          SSMDOT_c = 0.0
!          SLNDOT = 0.0
!          SSNDOT = 0.0
        ENDIF
!        IF (STMWT .LT. 0.0001) THEN
!          SLNDOT = 0.0
!          SSNDOT = 0.0
!        ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     N AND CH2O MOBILIZATION
C-----------------------------------------------------------------------
C     Calculate N available from today's senescence.
C      Only Age, low-light and N-mobilization-based senescece are lost at
C      less than current N and CH2O concentration.
C      Senesced tissues will contain N and CH2O concentrations between 
C      "final" and "current" levels - calculated as 
C      PROLFF * 0.16+( SENNxV * PCNL/100 - PROLFF * 0.16) 
C      so mobilization would be the difference between "current" N or C 
C      concentration and this calculated level at senescence.
C-----------------------------------------------------------------------
        LFSNMOB = SLMDOT * (PCNL/100.
     &    - (SENNLV * (PCNL / 100. - PROLFF*0.16) + PROLFF*0.16)) 
     &    + LTSEN * (PCNL / 100. - PROLFF * 0.16)

!       Handle leaf cohorts
        DO I = 1, NLC
          LFSNMOB_c(I) = SLMDOT_c(I) * (PCNLeaf(I)/100.
     &    - (SENNLV * (PCNLeaf(I) / 100. - PROLFF*0.16) + PROLFF*0.16)) 
     &    + LTSEN_c(I) * (PCNLeaf(I) / 100. - PROLFF * 0.16)
        ENDDO
        LFSNMOB_sum = SUM(LFSNMOB_c)

        STSNMOB = SSMDOT * (PCNST/100 - 
     &    (SENNSV * (PCNST / 100 - PROSTF*0.16) + PROSTF*0.16)) 
     &    + STLTSEN * (PCNST / 100 - PROSTF * 0.16)

!       Stem cohorts
        DO I = 1, NLC
          STSNMOB_c(I) = SSMDOT_c(I) * (PCNStem(I)/100.
     &    - (SENNSV * (PCNStem(I) / 100. - PROSTF*0.16) + PROSTF*0.16)) 
     &    + STLTSEN_c(I) * (PCNStem(I) / 100. - PROSTF * 0.16)
        ENDDO
        STSNMOB_sum = SUM(STSNMOB_c)

!       TEMP CHP
!        IF (ABS(STSNMOB_sum - STSNMOB) > 1.E-4) THEN
           WRITE(3224,'(I7,1X,A,1X,2F10.5)') 
     &       YRDOY, "STSNMOB", STSNMOB_sum, STSNMOB
!        ENDIF

        SRSNMOB = SSRMDOT * (PCNSR / 100 - 
     &    (SENNSRV * (PCNSR / 100 - PROSRF*0.16) + PROSRF*0.16))
        RTSNMOB = SRMDOT * (PCNRT / 100 - 
     &    (SENNRV * (PCNRT / 100 - PRORTF*0.16) + PRORTF*0.16))

C     TAKE OUT FROM HERE TO
!        LFSENWT = SENRTE * LFSNMOB / 0.16
!        LFSENWT = MIN(WTLF,LFSENWT)
!        SLDOT = SLDOT + LFSENWT
!        SLDOT = MIN(WTLF,SLDOT)

!        STSENWT = LFSENWT * PORPT
!        SSDOT = SSDOT + STSENWT
!        SSDOT = MIN(STMWT, SSDOT)

!            LFSNMOB = LFSNMOB + LFSENWT * (PCNL/100 - 
!     &              (SENNLV * (PCNL / 100 - PROLFF*0.16) + PROLFF*0.16)) 

!            STSNMOB = STSNMOB + STSENWT * (PCNST/100 - 
!     &              (SENNSV * (PCNST / 100 - PROSTF*0.16) + PROSTF*0.16)) 

C      HERE
!            TSNMOB = LFSNMOB + STSNMOB + SRSNMOB + RTSNMOB 

C-----------------------------------------------------------------------
C     Calculate CH2O available from today's senescence.
C-----------------------------------------------------------------------
!            LFSCMOB = (SLMDOT + LTSEN + LFSENWT) * (WCRLF / WTLF - 
!     &              (SENCLV * (WCRLF / WTLF - PCHOLFF) + PCHOLFF)) 
!            STSCMOB = (SSMDOT + STLTSEN + STSENWT) * (WCRST / STMWT - 
!     &              (SENCSV * (WCRST / STMWT - PCHOSTF) + PCHOSTF)) 
!            SRSCMOB = SSRMDOT * (WCRSR / STRWT - 
!     &              (SENCSRV * (WCRSR / STRWT - PCHOSRF) + PCHOSRF))
!            RTSCMOB = SRMDOT * (WCRRT / RTWT - 
!     &              (SENCRV * (WCRRT / RTWT - PCHORTF) + PCHORTF))

!            LFSCMOB = SLMDOT * ((WCRLF / WTLF) - PCHOLFF)
!            STSCMOB = SSMDOT * ((WCRST / STMWT) - PCHOSTF)
!            SRSCMOB = SSRMDOT * ((WCRSR / STRWT) - PCHOSRF)
!            RTSCMOB = SRMDOT * ((WCRRT / RTWT) - PCHORTF)

        TSCMOB = LFSCMOB + STSCMOB + SRSCMOB + RTSCMOB 

C-----------------------------------------------------------------------
C DSSAT4 code for CMINEP
!      CMINEP = CMOBMX * (DTX + DXR57) * (WCRST + WCRRT + WCRSH +WCRLF)
C-----------------------------------------------------------------------
C New code to deal with forages and dormancy
C Boosts mobilization from storage organs after harvests (LAI<CLAIT)
C Lowers mobilization during dormancy (when PPMFAC>0.0)
C-----------------------------------------------------------------------
!      IF (XLAI .LE. CLAIT) THEN
!      CMOBSR=CMOBSRX*(PPMFAC)
!      NMOBSR=NMOBSRX*(PPMFAC)
!      ELSE
!      CMOBSR=CMOBSRN*(PPMFAC)
!      NMOBSR=NMOBSRN*(PPMFAC)
!      ENDIF

      LAIMOBR = CURV(TYPLMOB,LRMOB(1),LRMOB(2),LRMOB(3),
     &    LRMOB(4), MIN(XLAI,LRMOB(4)))

!     chp note: ignore LAIMOBR for cohorts for now

C-----------------------------------------------------------------------
C      Increase mobilization from storage if N status of plant is high.
C-----------------------------------------------------------------------

      VEGNCNT = PCNL/100*WTLF + PCNST/100*STMWT +  
     &    PCNRT/100*RTWT + PCNSR/100*STRWT
      VEGNCMX = FNINL*WTLF + FNINS*STMWT + FNINR*RTWT + FNINSR*STRWT
      VNSTAT = MIN((VEGNCNT / VEGNCMX),  1.0)
      
      VNMOBR = CURV(TYPNMOB,NRMOB(1),NRMOB(2),NRMOB(3),NRMOB(4),VNSTAT)
C-----------------------------------------------------------------------
C      Set N mobilization rate from storage
C      Default to NMOBSRN under most conditions
C      set to NMOBSRX (max rate) after harvest or severe damage
C      Reduce from either level depending on degree of dormancy
C      Mobilization from storage is unaffected by water or N stress
C      but is accelerated by low N status.
C-----------------------------------------------------------------------
C      Tried various equations - a straight multiplicative equation 
C      doesn't work because LAIMOBR is usually 0.0 which forces 
C      mobilization to the minimum rate * PPMFAC.
C-----------------------------------------------------------------------
!      NMOBSR = (NMOBSRN + ((NMOBSRX-NMOBSRN)*VNMOBR * LAIMOBR)) * PPMFAC
C-----------------------------------------------------------------------
C      Additive equation would give the highest rate of mobilization.
C-----------------------------------------------------------------------
!      NMOBSR = (NMOBSRN + (NMOBSRX-NMOBSRN)*VNMOBR)
!      NMOBSR = (NMOBSR + (NMOBSRX-NMOBSR)*LAIMOBR)*PPMFAC
C-----------------------------------------------------------------------
C      Let the maximum of the two modifiers set the rate of mobilization.
C-----------------------------------------------------------------------
C      Modified to control mobilization to storage when low temperature
C      and drought occurs to prevent plants to die (new parameters in SPE.)
C      Diego Pequeno and Ken Boote (01-10-2024)
C-----------------------------------------------------------------------
      MOBSWF = TABEX (YMOSWF,XMOSWF,SWFAC,4)
*      write(*,*) YRDOY,'YRDOY',MOBSWF,'MOBSWF'
!       MOBSWF=1
!      write(*,*) MOBSWF,'MOBSWF',SWFAC,'SWFAC'
      MOBSWF = MAX(0.001,MOBSWF)
      MOBTEM = 0.0
      DO I = 1, 24
        MOBTEM = MOBTEM + TABEX(YMOTEM,XMOTEM,TGRO(I),6)
!     Cap negative values
*      MOBTEM = MAX(0.1,MOBTEM)
*      YMOTEM = MAX(0.1,YMOTEM)
      ENDDO
      MOBTEM = MOBTEM / 24.

      !     Cap negative values
      MOBTEM = MAX(0.001,MOBTEM)

      NMOBSR = (NMOBSRN + MAX(VNMOBR,LAIMOBR) *
!     &    (NMOBSRX - NMOBSRN)) * PPMFAC
     &    (NMOBSRX - NMOBSRN)) * PPMFAC * MOBTEM * MOBSWF
!     Cap negative values
!      NMOBSR = MAX(0.1,NMOBSR)
!      MOBTEM = MAX(0.1,MOBTEM)
!      MOBSWF = MAX(0.1,MOBSWF)

C-----------------------------------------------------------------------
C      Set C mobilization rate from storage
C      Default to CMOBSRN under most conditions
C      set to CMOBSRX (max rate) after harvest or severe damage
C      Reduce from either level depending on degree of dormancy
C      Mobilization from storage is unaffected by water or N stress
C-----------------------------------------------------------------------
!      CMOBSR = (CMOBSRN + ((CMOBSRX-CMOBSRN)*VNMOBR * LAIMOBR)) * PPMFAC

!      CMOBSR = (CMOBSRN + (CMOBSRX-CMOBSRN)*VNMOBR)
!      CMOBSR = (CMOBSR + (CMOBSRX-CMOBSR)*LAIMOBR)*PPMFAC

!      CMOBSR = (CMOBSRN + MAX(VNMOBR,LAIMOBR) *
!     &              (CMOBSRX - CMOBSRN)) * PPMFAC

      CMOBSR = (CMOBSRN + LAIMOBR *
!     &    (CMOBSRX - CMOBSRN)) * PPMFAC
     &    (CMOBSRX - CMOBSRN)) * PPMFAC * MOBTEM * MOBSWF
!     Cap negative values
      CMOBSR = MAX(0.001,CMOBSR)
!      MOBTEM = MAX(0.1,MOBTEM)
!      MOBSWF = MAX(0.1,MOBSWF)

C-----------------------------------------------------------------------
C      Calculate potential N mobilization for the day
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     Compute max N mining, NMINEP, based on stage-dependent mining
C     rate, NMOBR
C-----------------------------------------------------------------------
C     Assume that a Maximum Fraction (NMOBMX) of N can be Mobilized per Day
C     NVSMOB is the relative N mobil rate in veg stage, rel to reprod. stage
C-----------------------------------------------------------------------
C     9/27/95 ACCELERATE N MOBILIZATION AFTER R5, FUNCTION OF (1-SWFAC)
C     ALLOWS ACCELERATING BY 50% IF MAX DEFICIT.
C     2/6/96 SOMETIMES SEEDS FILL, XPOD IS LOW, THEN N MOBILIZATION SLOWS
C     I DON'T REALLY WANT THAT, LATE IN CYCLE.  KJB
C     NOW, DXR57 HITS CLOSE TO 1 AT MATURITY AND PREVENTS THAT
C-----------------------------------------------------------------------

      NMOBR  = NVSMOB * NMOBMX * TDUMX
      IF (DAS .GT. NR5) THEN
        NMOBR = NMOBMX * TDUMX2 * (1.0 + 0.5*(1.0 - SWFAC))
     &  * (1.0 + 0.3*(1.0 - NSTRES)) * (NVSMOB + (1. - NVSMOB)
     &  * MAX(XPOD,DXR57**2.))
      ENDIF

      NMINELF = NMOBR * WNRLF
      LFNMINE = LFSNMOB + NMINELF
      LFSNMOB = LFNMINE

!     --------------------------------------------
!     Handle leaf cohorts
      DO I = 1, NLC
        NMINELF_c(I) = NMOBR * LFNSN(I)
        LFNMINE_c(I) = LFSNMOB_c(I) + NMINELF_c(I)
      ENDDO
      LFSNMOB_c = LFNMINE_c

      NMINELF_sum = SUM(NMINELF_c)
      LFNMINE_sum = SUM(LFNMINE_c)
      LFSNMOB_sum = SUM(LFSNMOB_c)
!     --------------------------------------------

      NMINEST = NMOBR * WNRST
      STNMINE = STSNMOB + NMINEST
      STSNMOB = STNMINE

!     --------------------------------------------
!     Handle stem cohorts
      DO I = 1, NLC
        NMINEST_c(I) = NMOBR * STNSN(I)
        STNMINE_c(I) = STSNMOB_c(I) + NMINEST_c(I)
      ENDDO
      STSNMOB_c = STNMINE_c

      NMINEST_sum = SUM(NMINEST_c)
      STNMINE_sum = SUM(STNMINE_c)
      STSNMOB_sum = SUM(STSNMOB_c)
!     --------------------------------------------

      NMINERT = NMOBR * PPMFAC * WNRRT
      RTNMINE = RTSNMOB + NMINERT
      RTSNMOB = RTNMINE

      NMINESR = NMOBSR * WNRSR
      SRNMINE = SRSNMOB + NMINESR
      SRSNMOB = SRNMINE

      SHNMINE = NMOBR * WNRSH

      NMINEP = LFNMINE + STNMINE + RTNMINE + SRNMINE + SHNMINE
      NMINEO = NMINELF + NMINEST + NMINERT + NMINESR + SHNMINE

      TSNMOB = LFSNMOB + STSNMOB + SRSNMOB + RTSNMOB 
!      TSNMOB = LFNMINE + STNMINE + RTNMINE + SRNMINE + SHNMINE
C      ADDITIONAL DM LOSS DUE TO N MOBILIZATION? SENRTE

      LFSENWT = SENRTE * NMINELF / 0.16
      LFSENWT = MIN(WTLF,LFSENWT)
      SLDOT = SLDOT + LFSENWT
      SLDOT = MIN(WTLF,SLDOT)

!     --------------------------------------------
!     Handle leaf cohorts
      DO I = 1, NLC
        LFSENWT_c(I) = SENRTE * NMINELF_c(I) / 0.16
        LFSENWT_c(I) = MIN(LFDM(I) - LeafTotSen(I), LFSENWT_c(I))
      ENDDO
      LFSENWT_sum = SUM(LFSENWT_c)
      LeafTotSen = LeafTotSen + LFSENWT_c
!     --------------------------------------------

      STSENWT = LFSENWT * PORPT
      SSDOT = SSDOT + STSENWT
      SSDOT = MIN(STMWT, SSDOT)

!     --------------------------------------------
!     Handle stem cohorts
      DO I = 1, NLC
        STSENWT_c(I) = LFSENWT_c(I) * PORPT
        STSENWT_c(I) = MIN(STDM(I) - StemTotSen(I), STSENWT_c(I))
      ENDDO
      STSENWT_sum = SUM(STSENWT_c)
      StemTotSen = StemTotSen + STSENWT_c
!     --------------------------------------------

C-----------------------------------------------------------------------
C      Calculate potential CH2O mobilization for the day
C-----------------------------------------------------------------------
C    Adding cold temperature and water stress to reduce mobilization
C    1-12-2024 KJB and DP
      CMINELF = CMOBMX * (DTX + DXR57)* (WCRLF - WTLF * PCHOLFF)
     & * MOBTEM * MOBSWF
!     Cap negative values - SHOULD BE DONE WHERE THESE ARE CALCULATED, NOT AFTER THEY ARE USED
!     MOBTEM = MAX(0.0,MOBTEM)
!     MOBSWF = MAX(0.0,MOBSWF)

      LFCMINE = MAX(LFSCMOB, CMINELF)
!     LFSCMOB comes from for_veggr

!     --------------------------------------------
!     Handle leaf cohorts
      DO I = 1, NLC
        CMINELF_c(I) = CMOBMX * (DTX + DXR57) 
     &   * (LFNSC(I) - LFDM(I) * PCHOLFF) * MOBTEM * MOBSWF
!       LFSCMOB is always zero in current code (2026-05-11 chp)
        LFCMINE_c(I) = MAX(LFSCMOB, CMINELF_c(I))
      ENDDO
      CMINELF_sum = SUM(CMINELF_c)
      LFCMINE_sum = SUM(LFCMINE_c)
!     --------------------------------------------

      CMINEST = CMOBMX * (DTX + DXR57)* (WCRST - STMWT * PCHOSTF)
     & * MOBTEM * MOBSWF
!     Cap negative values
!     MOBTEM = MAX(0.0,MOBTEM)
      MOBSWF = MAX(0.0,MOBSWF)

      STCMINE = MAX(STSCMOB, CMINEST)

!     --------------------------------------------
!     Handle stem cohorts
      DO I = 1, NLC
        CMINEST_c(I) = CMOBMX * (DTX + DXR57) 
     &   * (STNSC(I) - STDM(I) * PCHOSTF) * MOBTEM * MOBSWF
!       STSCMOB is always zero in current code (2026-06-09 chp)
        STCMINE_c(I) = MAX(STSCMOB, CMINEST_c(I))
      ENDDO
      CMINEST_sum = SUM(CMINEST_c)
      STCMINE_sum = SUM(STCMINE_c)
!     --------------------------------------------

      CMINERT = CMOBMX * (DTX + DXR57)* PPMFAC *
     &    (WCRRT - RTWT * PCHORTF)
     & * MOBTEM * MOBSWF
!     Cap negative values
      MOBSWF = MAX(0.0,MOBSWF)

      RTCMINE = MAX(RTSCMOB, CMINERT)

      CMINESR = CMOBSR * (DTX + DXR57)* (WCRSR - STRWT * PCHOSRF)
      SRCMINE = MAX(SRSCMOB, CMINESR)

      SHCMINE = CMOBMX * (DTX + DXR57)* WCRSH
     & * MOBTEM * MOBSWF
!     Cap negative values
      MOBTEM = MAX(0.0,MOBTEM)
      MOBSWF = MAX(0.0,MOBSWF)

      CMINEP = LFCMINE + STCMINE + RTCMINE + SRCMINE + SHCMINE
      CMINEO = CMINELF + CMINEST + CMINERT + CMINESR + SHCMINE

      LFWSSN = WaterSen_c
      STWSSN = SSNDOT_c
      LeafTotSen_SUM = SUM(LeafTotSen)
      StemTotSen_sum = SUM(StemTotSen)

!=========================================================================
!     TEMP CHP Add printout for SENES variables

!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN

        WRITE (NOUTDGL,300)
     &   YEAR, DOY, DAS, DAP, 
     &   SLDOT, LFNSEN, LFSENWT, LTSEN, SLNDOT, SLMDOT,
     &   LFSNMOB, NMINELF, LFNMINE, CMINELF, LFCMINE, 
     &   LeafTotSen_sum, LFNSEN_sum, LFSENWT_sum, LTSEN_sum, 
     &   WaterSen_sum, SLMDOT_sum, 
     &   LFSNMOB_sum, NMINELF_sum, LFNMINE_sum, CMINELF_sum,LFCMINE_sum

  300   FORMAT (1X,I4,1X,I3.3,2(1X,I5)
     &    50F12.6)

        SSMDOT_SUM = SUM(SSMDOT_C)

        WRITE (NOUTDGS,300)
     &   YEAR, DOY, DAS, DAP, 
     &   SSDOT, STNSEN, STSENWT, STLTSEN, SSNDOT, SSMDOT,
     &   STSNMOB, NMINEST, STNMINE, CMINEST, STCMINE, 
     &   StemTotSen_sum, STNSEN_sum, STSENWT_sum, STLTSEN_sum, 
     &   SSNDOT_sum, SSMDOT_sum, 
     &   STSNMOB_sum, NMINEST_sum, STNMINE_sum, CMINEST_sum,STCMINE_sum

        WRITE (NOUTDG2,300)
     &   YEAR, DOY, DAS, DAP, 
     &   SLDOT, LFNSEN, LFSENWT, LTSEN, SLNDOT, SLMDOT,
     &   LFSNMOB, NMINELF, LFNMINE, CMINELF, LFCMINE 

!     still temp chp...

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN
C-----------------------------------------------------------------------
          CLOSE (NOUTDGL)
          CLOSE (NOUTDGS)
          CLOSE (NOUTDG2)

!     end temp chp
!=========================================================================

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE FOR_SENMOB
!***********************************************************************
!     SENES VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! CHAR      Contains the contents of last record read 
! CMINELF   Potential mobile CH2O avaialable today from leaf (g [CH2O] m-2)
! CMINEO        DSSAT4 potential CH2O mobilization from storage (g[CH2O] / m2 / d)
! CMINEP        Potential whole-plant CH2O mobilization from storage (g[CH2O] / m2 / d)
! CMINERT   Potential mobile CH2O avaialable today from root (g [CH2O] m-2)
! CMINESR   Potential mobile CH2O avaialable today from STOR (g [CH2O] m-2)
! CMINEST   Potential mobile CH2O avaialable today from stem (g [CH2O] m-2)
! DAS       Days after start of simulation (days)
! DAYL      Current daylength (hours)
! DAYL_1    Yesterdays daylength (hours)
! DAYL_1    Daylength two days ago (hours)
! DLAYR(L)  Soil Depth in layer L (cm)
! DTX       Thermal time that occurs in a real day based on vegetative 
!             development temperature function (thermal days / day)
! DYNAMIC   Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!             INTEGR, OUTPUT, or SEASEND 
! ERR       Error code for file operation 
! FILECC    Path plus filename for species file (*.spe) 
! FOUND     Indicator that good data was read from file by subroutine FIND 
!             (0 - End-of-file encountered, 1 - NAME was found) 
! ICMP      Light compensation point for senescence of lower leaves because 
!             of excessive self-shading by crop canopy (moles / m2 / day)
! ISECT     Data record code (0 - End of file encountered, 1 - Found a good 
!             line to read, 2 - End of Section in file encountered, denoted 
!             by * in column 1
! KCAN      Canopy light extinction coefficient for daily PAR, for 
!             equidistant plant spacing, modified when in-row and between 
!             row spacings are not equal 
! LCMP      LAI at which today's light compensation (ICMP) is reached
!             (m2[leaf] / m2[ground])
! LFCMINE        Today's maximum potential CH2O mobilization from leaf (g [CH2O] m-2)
! LFNMINE   Today's maximum potential N mobilization from leaf (g [N] m-2)
! LFNSEN        Defoliation from natural senescence (g [DM] m-2 d-1)
! LFSEN     Maximum rate of natural leaf senescence per physiological day
! LFSCMOB   Mass of leaf CH2O mobilized from tissue lost to natural 
!              senescence (g [CH2O] m-2 d-1)
! LFSNMOB   Mass of leaf N mobilized from tissue lost to natural  
!              senescence (g [N] m-2 d-1)
! LNUM      Current line number of input file 
! LTSEN     Senescence of lower leaves due to self-shading of canopy
!             (1/day) 8/3/05 now is g [DM] m-2 d-1
! LUNCRP    Logical unit number for FILEC (*.spe file) 
! NL        Maximum number of soil layers = 20 
! NLAYR     Number of soil layers 
! NMINELF   Potential mobile N avaialable today from leaf (g [N] m-2)
! NMINEO        DSSAT4 potential N mobilization from storage (g[N] / m2 / d)
! NMINEP        Potential whole-plant N mobilization from storage (g[N] / m2 / d)
! NMINERT   Potential mobile N avaialable today from root (g [N] m-2)
! NMINESR   Potential mobile N avaialable today from STOR (g [N] m-2)
! NMINEST   Potential mobile N avaialable today from stem (g [N] m-2)
! NR7       Day when 50% of plants first have yellowing or maturing pods
!             (days)
! PAR       Daily photosynthetically active radiation or photon flux 
!             density (moles[quanta]/m2-d)
! PORLFT    Proportion of leaf weight grown which will have been senesced 
!             if no water stress has occurred prior to this V-stage 
! PORPT     Ratio of petiole to leaf weight 
! RATTP     Factor used in determining increased senescence due to water 
!             stress 
! RFAC1     Root length per unit  root weight. (cm/g)
! RFAC3     Ratio of root length to root weight at the current time (cm/g)
! RHOL      Fraction of leaf which is carbohydrate (g [CH20] / g[leaf])
! RLV(L)    Root length density for soil layer L (cm[root] / cm3[soil])
! RTCMINE        Today's maximum potential CH2O mobilization from root (g [CH2O] m-2)
! RTNMINE   Today's maximum potential N mobilization from root (g [N] m-2)
! RTSEN     Fraction of existing root length which can be senesced per 
!             physiological day. (fraction / ptd)
! RTSCMOB   Mass of root CH2O mobilized from tissue lost to natural senescence 
!              (g [CH2O] m-2 d-1)
! RTSNMOB   Mass of root N mobilized from tissue lost to natural senescence 
!              (g [N] m-2 d-1)
! RTWT      Dry mass of root tissue, including C and N
!             (g[root] / m2[ground])
! SECTION   Section name in input file 
! SENDAY    Maximum fraction of existing leaf weight which can be senesced 
!             on day N as a function of severe water stress 4 days earlier. 
! SENMAX(I) Maximum proportion of total leaf weight as a function of 
!             V-stage (XSENMX(I)) which can be senesced due to water stress. 
! SENCLV     Proportion used to calculate amount of CHO mobilized from leaves lost to 
!               natural senescence, low-light senescence, and N-mobilization senescence
!               Is a fraction of the difference between PCNL (current N concentration and 
!               PROLFI*0.16 or "final" N concentration.
! SENCRV     Proportion used to calculate amount of CHO mobilized from leaves lost to 
!               natural senescence, low-light senescence, and N-mobilization senescence
!               Is a fraction of the difference between PCNL (current N concentration and 
!               PROLFI*0.16 or "final" N concentration.
! SENCSV     Proportion used to calculate amount of CHO mobilized from leaves lost to 
!               natural senescence, low-light senescence, and N-mobilization senescence
!               Is a fraction of the difference between PCNL (current N concentration and 
!               PROLFI*0.16 or "final" N concentration.
! SENCSRV     Proportion used to calculate amount of CHO mobilized from leaves lost to 
!               natural senescence, low-light senescence, and N-mobilization senescence
!               Is a fraction of the difference between PCNL (current N concentration and 
!               PROLFI*0.16 or "final" N concentration.
! SENNLV     Proportion used to calculate amount of N mobilized from leaves lost to 
!               natural senescence, low-light senescence, and N-mobilization senescence
!               Is a fraction of the difference between PCNL (current N concentration and 
!               PROLFI*0.16 or "final" N concentration.
! SENNRV     Proportion used to calculate amount of N mobilized from leaves lost to 
!               natural senescence, low-light senescence, and N-mobilization senescence
!               Is a fraction of the difference between PCNL (current N concentration and 
!               PROLFI*0.16 or "final" N concentration.
! SENNSV     Proportion used to calculate amount of N mobilized from leaves lost to 
!               natural senescence, low-light senescence, and N-mobilization senescence
!               Is a fraction of the difference between PCNL (current N concentration and 
!               PROLFI*0.16 or "final" N concentration.
! SENNSRV     Proportion used to calculate amount of N mobilized from leaves lost to 
!               natural senescence, low-light senescence, and N-mobilization senescence
!               Is a fraction of the difference between PCNL (current N concentration and 
!               PROLFI*0.16 or "final" N concentration.
! SENPOR(I) Proportion of leaf weight grown which will have been senesced 
!             by a given V- stage (XSTAGE(I)) if no water stress has 
!             occurred prior to this V-stage (XSTAGE(I)) -- normal 
!             vegetative senescence does not occur if prior water stress 
!             has already  reduced leaf  
! SENRT2    Factor by which leaf weight is multiplied to determine 
!             senescence each day after NR7 (g(leaf) / g(protein loss))
! SENRTE    Factor by which protein mined from leaves each day is 
!             multiplied to determine LEAF senescence.
!             (g(leaf) / g(protein loss))
! SENSR        Constant for senescence of storage organ tissue 
!                  (proportion of cumulative storage weight lost / physiological day)
! SENWT     Leaf senescence due to N mobilization (g[leaf] / m2[ground])
! SHCMINE   Potential mobile CH2O avaialable today from shell (g [CH2O] m-2)
! SHNMINE   Potential mobile N avaialable today from shell (g [N] m-2)
! SLAAD     Specific leaf area, excluding weight of C stored in leaves
!             (cm2[leaf] / g[leaf])
! SLMDOT    Defoliation due to daily leaf senescence that is lost at PROLFF, 
!              hence, some fraction of the N content is subject to mobilization (g/m2/day)
! SRCMINE        Today's maximum potential CH2O mobilization from STOR (g [CH2O] m-2)
! SRMDOT    Daily root senescence that is lost at PRORTF, 
!              hence, some fraction of the N content is subject to mobilization (g/m2/day)
! SRNMINE   Today's maximum potential N mobilization from STOR (g [N] m-2)
! SRSCMOB   Mass of STOR CH2O mobilized from tissue lost to natural senescence 
!              (g [CH2O] m-2 d-1)
! SRSNMOB   Mass of STOR N mobilized from tissue lost to natural senescence 
!              (g [N] m-2 d-1)
! SSMDOT    Daily petiole senescence that is lost at PROSTF, 
!              hence, some fraction of the N content is subject to mobilization (g/m2/day)
! SSRMDOT   Daily STOR senescence that is lost at PROSRF, 
!              hence, some fraction of the N content is subject to mobilization (g/m2/day)
! STCMINE        Today's maximum potential CH2O mobilization from stem (g [CH2O] m-2)
! STMWT     Dry mass of stem tissue, including C and N
!             (g[stem] / m2[ground)
! STNMINE   Today's maximum potential N mobilization from stem (g [N] m-2)
! STRWT     Dry mass of storage organ tissue, including C and N
! STSCMOB   Mass of petiole CH2O mobilized from tissue lost to natural senescence 
!              (g [CH2O] m-2 d-1)
! STSNMOB   Mass of petiole N mobilized from tissue lost to natural senescence 
!              (g [N] m-2 d-1)
! TABEX     Function subroutine - Lookup utility 
! TCMP      Time constant for senescence of lower leaves because of 
!             excessive self-shading by crop canopy (thermal days)
! TIMDIF    Integer function which calculates the number of days between 
!             two Julian dates (da)
! TRLSEN    Total root length density senesced today (cm[root]/ cm2[soil])
! TRTDY     Total root length per square cm soil yesterday 
!             (cm[root]/cm2[soil])
! TSCMOB    Total plant CH2O mobilized from tissue lost to natural and 
!              low-light senescence (g [CH2O] m-2 d-1)
! TSNMOB    Total plant N mobilized from tissue lost to natural and 
!              low-light senescence (g [N] m-2 d-1)
! VSTAGE    Number of nodes on main stem of plant 
! WRDOTN    Dry weight growth rate of new root tissue including N but not C 
!             reserves (g[root] / m2[ground]-d)
! WTLF      Dry mass of leaf tissue including C and N
!             (g[leaf] / m2[ground])
! XLAI      Leaf area (one side) per unit of ground area
!             (m2[leaf] / m2[ground])
! XSENMX(I) V-stage at which maximum fraction of cumulative leaf growth 
!             vulnerable to loss due to water stress is SENMAX(I).
!             (# leaf nodes)
! XSTAGE(I) V-stage at which SENPOR(I) fraction of cumulative leaf growth 
!             will have been senesced if no water stress occurred.
!             (# leaf nodes)
! YRDOY     Current day of simulation (YYDDD)
! YRSIM     Start of simulation date (YYDDD)
!-----------------------------------------------------------------------
!     END SUBROUTINE FOR_SENMOB
!-----------------------------------------------------------------------
