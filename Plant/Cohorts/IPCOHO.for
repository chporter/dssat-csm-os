!=======================================================================
!  IPCOHO, Subroutine, C.H. Porter
!-----------------------------------------------------------------------
!  Reads input data for COHORTS subroutine
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  11/18/2025 CHP Adapted from IPDMND.
!=======================================================================
      SUBROUTINE IPCOHO(
     &  FILECC, MODEL,                            !Input
     &  ALPHL, ALPHS, ICMP, MAXNMINE,             !Output
     &  NMOBMX, NVSMOB,                           !Output
     &  PCHOLFF, PCHOSTF, PROLFF, PROSTF,         !Output
     &  SENDAY, SENMAX, SENCLV, SENCSV,           !Output
     &  SENNLV, SENNSV, TCMP, XSENMX)             !Output

!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL GETLUN, ERROR, FIND, IGNORE, WARNING
!-----------------------------------------------------------------------
      CHARACTER*92, INTENT(IN) :: FILECC
      REAL, INTENT(OUT) :: ALPHL, ALPHS, PCHOLFF, PCHOSTF, ICMP, 
     &  MAXNMINE, NMOBMX, NVSMOB, 
     &  PROLFF, PROSTF, SENDAY, 
     &  SENCLV, SENCSV, SENNLV, SENNSV, TCMP
      REAL, INTENT(OUT) :: SENMAX(4), XSENMX(4)

      CHARACTER*6   ERRKEY
      PARAMETER (ERRKEY = 'IPCOHO')
      CHARACTER*6   SECTION
      CHARACTER*8   MODEL
      CHARACTER*80  C80

      INTEGER LUNCRP,  ERR, LINC, LNUM, FOUND, ISECT
      INTEGER II, I

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
        READ(C80,'(12X,F6.0,12X,F6.0)',IOSTAT=ERR) PROLFF, PROSTF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        IF (MODEL(1:5) == 'PRFRM') THEN
          DO I = 1, 11
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          ENDDO
          READ(C80,'(2F6.0)',IOSTAT=ERR) PCHOLFF, PCHOSTF
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDIF
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
        READ(C80,'(18X,2F6.0)',IOSTAT=ERR) NMOBMX, NVSMOB
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  !Skip the next line
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(2F6.0)',IOSTAT=ERR) ALPHL, ALPHS
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        SELECT CASE (MODEL(1:5))
        CASE ('CRGRO')
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  
          READ(C80,'(F6.0)',IOSTAT=ERR) MAXNMINE
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CASE ('PRFRM')
          DO I = 1, 7
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  
          ENDDO
          READ(C80,'(4F6.0)',IOSTAT=ERR) SENNLV, SENCLV, SENNSV, SENCSV
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)  
          READ(C80,'(F6.0)',IOSTAT=ERR) MAXNMINE
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        END SELECT
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

