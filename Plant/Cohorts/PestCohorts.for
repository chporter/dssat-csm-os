!=======================================================================
!  PestCohorts, Subroutine, C.H. Porter
!-----------------------------------------------------------------------
!  Determines pest damage per leaf and stem cohort.
!  This should eventually be moved to the pest module with the type of 
!     damage to cohorts specified in the pest coupling file.
!  Output is LFPST and STPST which are available through the 
!     COHORTS_mod module
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  03-24-2026 CHP Adapted from COHORTS
!=======================================================================
      SUBROUTINE PestCohorts(
     &  WLIDOT, WSIDOT, WTLF, STMWT)   !Input
!-----------------------------------------------------------------------
      USE Cohorts_Mod
      IMPLICIT NONE

      REAL, INTENT(IN) :: WLIDOT, WSIDOT, WTLF, STMWT
      REAL WLIDOT_calc, WSIDOT_calc
      INTEGER I

      WLIDOT_calc = 0.0
      WSIDOT_calc = 0.0
      LFPST = 0.0
      STPST = 0.0

!     ------------------------------------------------------------
!     Leaf damage
      IF (WLIDOT .GT. 0.0) THEN
!       FOR PROPORTIONAL DISTRIBUTION OF PEST DAMAGE:
        DO I = 1, NLC
          IF (LFDM(I) .GT. 0.0) THEN
            LFPST(I) = LFDM(I) / WTLF * WLIDOT
            WLIDOT_calc = WLIDOT_calc + LFPST(I)
          ELSE
            LFPST(I) = 0.0
          ENDIF
        ENDDO

!       FOR PEST DAMAGE TO AFFECT OLD TISSUE FIRST:
!        LFPSTDM = WLIDOT
!        DO I = 1, NLC
!          IF (LFDM(I) .GT. 0.0) THEN
!            LFPST(I) = MIN(LFDM(I), LFPSTDM)
!            LFPST(I) = MAX(LFPST(I), 0.0)
!            LFPSTDM = LFPSTDM - LFPST(I)
!          ENDIF
!        ENDDO

!       FOR PEST DAMAGE TO AFFECT NEW TISSUE FIRST:
!        LFPSTDM = WLIDOT
!        DO I = NLC, 1, -1
!          IF (LFDM(I) .GT. 0.0) THEN
!            LFPST(I) = MIN(LFDM(I), LFPSTDM)
!            LFPST(I) = MAX(LFPST(I), 0.0)
!            LFPSTDM = LFPSTDM - LFPST(I)
!          ENDIF
!        ENDDO

      ELSE
!       No pest damage
        LFPST = 0.0
      ENDIF

!     ------------------------------------------------------------
!     from GROW:
! NEED TO HANDLE THIS FOR COHORTS.
!C-----------------------------------------------------------------------
!C     Calculate "Healthy" or Non-Diseased Leaf Area Index
!C-----------------------------------------------------------------------
!!     AREAH  = AREALF - 2. * DISLA
!!     KJB Remove 2. factor
!      AREAH  = AREALF - DISLA
!      AREAH  = MAX(0.,AREAH)
!      XHLAI  = AREAH / 10000.

!     ------------------------------------------------------------
!     Stem damage
      IF (WSIDOT .GT. 0.0) THEN
!       FOR PROPORTIONAL DISTRIBUTION OF PEST DAMAGE:
        DO I = 1, NLC
          IF (STDM(I) .GT. 0.0) THEN
            STPST(I) = STDM(I) / STMWT * WSIDOT
            WSIDOT_calc = WSIDOT_calc + STPST(I)
          ELSE
            STPST(I) = 0.0
          ENDIF
        ENDDO

!       FOR PEST DAMAGE TO AFFECT OLD TISSUE FIRST:
!        STPSTDM = WSIDOT
!        DO I = 1, NLC
!          IF (STDM(I) .GT. 0.0) THEN
!            STPST(I) = MIN(STDM(I), STPSTDM)
!            STPST(I) = MAX(STPST(I), 0.0)
!            STPSTDM = STPSTDM - STPST(I)
!          ENDIF
!        ENDDO

!       FOR PEST DAMAGE TO AFFECT NEW TISSUE FIRST:
!        STPSTDM = WSIDOT
!        DO I = NLC, 1, -1
!          IF (STDM(I) .GT. 0.0) THEN
!            STPST(I) = MIN(STDM(I), STPSTDM)
!            STPST(I) = MAX(STPST(I), 0.0)
!            STPSTDM = STPSTDM - STPST(I)
!          ENDIF
!        ENDDO

      ELSE
!       No pest damage
        STPST = 0.0
      ENDIF

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE PestCohorts
!=======================================================================


