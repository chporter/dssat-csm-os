!=======================================================================
      MODULE IPCOHO_MOD
!=======================================================================
      REAL ALPHL, ALPHS, PCHOLFF, PCHOSTF, ICMP, 
     &  MAXNMINE, NMOBMX, NVSMOB, 
     &  PROLFF, PROLFI, PROSTF, PROSTI, SENDAY, 
     &  SENCLV, SENCSV, SENNLV, SENNSV, TCMP
      REAL SENMAX(4), XSENMX(4)

!          Cellulose, hemicellulose, lignin
      REAL CELLFI, HCLLFI, LIGLFI,   !initial leaf
     &     CELSTI, HCLSTI, LIGSTI,   !initial stem
     &     CELLFS, HCLLFS, LIGLFS,   !slope leaf
     &     CELSTS, HCLSTS, LIGSTS,   !slope stem
     &     CELLFN, HCLLFN, LIGLFN,   !minimum leaf
     &     CELSTN, HCLSTN, LIGSTN,   !minimum stem
     &     CELLFX, HCLLFX, LIGLFX,   !maximum leaf
     &     CELSTX, HCLSTX, LIGSTX    !maximum stem

      CONTAINS

!=======================================================================
!  IPCOHO, Subroutine, C.H. Porter
!-----------------------------------------------------------------------
!  Reads input data for COHORTS subroutine
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  11/18/2025 CHP Adapted from IPDMND.
!=======================================================================
      SUBROUTINE IPCOHO(
     &  FILECC, MODEL)                            !Input

!     &  ALPHL, ALPHS, ICMP, MAXNMINE,             !Output
!     &  NMOBMX, NVSMOB,                           !Output
!     &  PCHOLFF, PCHOSTF, PROLFI, PROLFF, PROSTI, !Output
!     &  PROSTF, SENDAY, SENMAX, SENCLV, SENCSV,   !Output
!     &  SENNLV, SENNSV, TCMP, XSENMX)             !Output
!!       Cellulose, hemicellulose, lignin
!     &  CELLFI, HCLLFI, LIGLFI,   !initial leaf
!     &  CELSTI, HCLSTI, LIGSTI,   !initial stem
!     &  CELLFS, HCLLFS, LIGLFS,   !slope leaf
!     &  CELSTS, HCLSTS, LIGSTS    !slope stem

!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL GETLUN, ERROR, FIND, IGNORE, WARNING
!-----------------------------------------------------------------------
      CHARACTER*92, INTENT(IN) :: FILECC
      CHARACTER*8 , INTENT(IN) :: MODEL

      CHARACTER*6   ERRKEY
      PARAMETER (ERRKEY = 'IPCOHO')
      CHARACTER*6   SECTION
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
        READ(C80,'(F6.0,6X,2F6.0,6X,F6.0)',IOSTAT=ERR) 
     &    PROLFI, PROLFF, PROSTI, PROSTF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        IF (MODEL(1:5) == 'PRFRM') THEN
          DO I = 1, 11
            CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
          ENDDO
          READ(C80,'(2F6.0)',IOSTAT=ERR) PCHOLFF, PCHOSTF
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

!         New parameters for cellulose, hemicellulose, and lignin
          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!         Initial leaf cellulose, hemicellulose, lignin (fraction)
          READ(C80,'(6F6.0)',IOSTAT=ERR) CELLFI, HCLLFI, LIGLFI, 
!         Initial stem cellulose, hemicellulose, lignin (fraction)
     &                                   CELSTI, HCLSTI, LIGSTI
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!         Rate of change of leaf cellulose, hemicellulose, lignin (fraction/PTD)
          READ(C80,'(6F6.0)',IOSTAT=ERR) CELLFS, HCLLFS, LIGLFS, 
!         Rate of change of stem cellulose, hemicellulose, lignin (fraction/PTD)
     &                                   CELSTS, HCLSTS, LIGSTS
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!         Maximum leaf cellulose, hemicellulose, lignin (fraction)
          READ(C80,'(6F6.0)',IOSTAT=ERR) CELLFX, HCLLFX, LIGLFX, 
!         Minimum stem cellulose, hemicellulose, lignin (fraction)
     &                                   CELSTX, HCLSTX, LIGSTX
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

          CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
!         Minimum leaf cellulose, hemicellulose, lignin (fraction)
          READ(C80,'(6F6.0)',IOSTAT=ERR) CELLFN, HCLLFN, LIGLFN, 
!         Minimum stem cellulose, hemicellulose, lignin (fraction)
     &                                   CELSTN, HCLSTN, LIGSTN
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
      END MODULE IPCOHO_MOD
!=======================================================================

!***********************************************************************
!     Variable listing for COHORTS subroutine (updated 20 April 2009)
!***********************************************************************
! CELLFI       Initial leaf cellulose (fraction)
! CELLFN       Minimum leaf cellulose (fraction)
! CELLFS       Change per day leaf cellulose (fraction/PTD)
! CELLFX       Maximum leaf cellulose (fraction)
! CELSTI       Initial stem cellulose (fraction)
! CELSTN       Minimum stem cellulose (fraction)
! CELSTS       Change per day stem cellulose (fraction/PTD)
! CELSTX       Maximum stem cellulose (fraction)
! HCLLFI       Initial leaf hemicellulose (fraction)
! HCLLFN       Minimum leaf hemicellulose (fraction)
! HCLLFS       Change per day leaf hemicellulose (fraction/PTD)
! HCLLFX       Maximum leaf hemicellulose (fraction)
! HCLSTI       Initial stem hemicellulose (fraction)
! HCLSTN       Minimum stem hemicellulose (fraction)
! HCLSTS       Change per day stem hemicellulose (fraction/PTD)
! HCLSTX       Maximum stem hemicellulose (fraction)
! LIGLFI       Initial leaf lignin (fraction)
! LIGLFN       Minimum leaf lignin (fraction)
! LIGLFS,      Change per day leaf lignin (fraction/PTD)
! LIGLFX,      Maximum leaf lignin (fraction)
! LIGSTI,      Initial stem lignin (fraction)
! LIGSTN,      Minimum stem lignin (fraction)
! LIGSTS       Change per day stem lignin (fraction/PTD)
! LIGSTX       Maximum stem lignin (fraction)

! MAXNMINE     Maximum N mining rate (fraction/day)
! NGRLF        Maximum N demand for leaf growth (g[leaf N] / m2[ground] / d)
! NMOBMX       Maximum N mobilization rate (fraction/day)
! NMOBR        Stage-dependent potential N mining rate expressed as a
!                fraction of the maximum rate (NMOBMX) 
! NRUSLF       N actually mobilized from leaves in a day (g[N]/m2-d)
! NVSMOB       Relative rate of N mining during vegetative stage to that in 
!                reproductive stage 
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
! XSENMX(I)    V-stage at which maximum fraction of cumulative leaf growth 
!                vulnerable to loss due to water stress is SENMAX(I).
!                (# leaf nodes)
!***********************************************************************

