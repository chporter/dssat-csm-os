!***********************************************************************
!  RootWU_2D_mod, module
!
!  Purpose: Determines root uptake at sub-daily time steps for 2D model. 
!  Two subroutines are used with some shared variables.
!  - ROOTWU_2D is called daily from SPAM.for and sends back the daily
!       accumulated values of TRWU, RWU_2D .
!  - RWUts_2D is called from WatBal_2D on a sub-daily time step. At each
!       time step, root water uptake is calculated for that time step. 

!  Revision history:
!  2025-08-11 CHP  Written

!***********************************************************************

      MODULE RootWU_2D_mod
      
      USE Cells_2D

!     These variables are shared between ROOTWU_2D (called daily by SPAM) 
!       and RWUts_2D (called sub-daily by WatBal_2D)
!      REAL, PROTECTED :: 
!      REAL, DIMENSION(NL), PROTECTED :: 
      REAL, DIMENSION(MaxRows,MaxCols) :: CellArea, ColFrac
      REAL, DIMENSION(MaxRows,MaxCols) :: RootWU_2D
      INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_Type
      LOGICAL, PROTECTED :: First_ts

      REAL, DIMENSION(MaxRows,MaxCols) :: LL, RLV_2D
      REAL, DIMENSION(MaxRows,MaxCols) :: SAT, SWCON2, Thick
      REAL, DIMENSION(MaxRows,MaxCols) :: TSS, TSS_last
      REAL PORMIN, RWUMX
      REAL Scale2Hour

      contains

!==========================================================================
!     DAILY ROUTINE CALLED FROM SPAM
!==========================================================================
      SUBROUTINE ROOTWU_2DA (DYNAMIC, CELLS,
     &      WEATHER, 
     &      RWU, TRWUP)                           !Output

!     ------------------------------------------------------------------
      USE ModuleData
      IMPLICIT  NONE
      SAVE
!     ------------------------------------------------------------------

      INTEGER, INTENT(IN) :: DYNAMIC
      TYPE (CellType), INTENT(IN) :: CELLS(MaxRows,MaxCols)
      TYPE (WeatherType), INTENT(IN) :: WEATHER
      INTEGER i,j
      REAL RWU, TRWUP

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      LL        = CELLS % STATE % LL
      RLV_2D    = CELLS % STATE % RLV
      SAT       = CELLS % STATE % SAT
      CellArea  = CELLS % STRUC % CellArea
      Cell_TYPE = CELLS % STRUC % Cell_Type
      Thick     = CELLS % Struc % Thick
      ColFrac   = BedDimension % ColFrac

      TSS       = 0.0
      TSS_LAST  = 0.0

!-----------------------------------------------------------------------
!     Compute SWCON2 for each soil layer.  Adjust SWCON2 for extremely
!     high LL to avoid water uptake limitations.
!-----------------------------------------------------------------------
      SWCON2    = 0.0
      DO i = 1, NRowsTot
        DO j = 1, NColsTot
          SWCON2(i,j) = 120. - 250. * LL(i,j)
          IF (LL(i,j) > 0.30) SWCON2(i,j) = 45.0
        ENDDO  
      ENDDO

      RWU   = 0.0
      TRWUP = 0.0

      CALL GET('PLANT', 'PORMIN', PORMIN)
      CALL GET('PLANT', 'RWUMX',  RWUMX)

!***********************************************************************
!***********************************************************************
!     DAILY RATE
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!     ------------------------------------------------------------------
!     This call from SPAM is done before WatBal_2D has called RWUts_2D
!       at a sub-daily time step. 

      RLV_2D = Cells % State % RLV
      Scale2Hour = 24. / WEATHER % DAYL

!***********************************************************************
!***********************************************************************
!     INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!     ------------------------------------------------------------------
!     This call from SPAM is done after WatBal_2D has called RWUts_2D
!       at a sub-daily time step. 

        RWU_2D = SumRWU_2D

!     ------------------------------------------------------------------
!     End of DYNAMIC IF block
      ENDIF
!     ------------------------------------------------------------------
      END SUBROUTINE ROOTWU_2DA
!==========================================================================

!==========================================================================
!     SUB-DAILY ROUTINE CALLED FROM WATBAL_2D
!==========================================================================
!  RWUts_2D, Subroutine, J.T. Ritchie
!  Calculates root water uptake rate for each soil layer and total rate.
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  01/01/1989 JR  Written
!  02/26/2009 CHP Modify for 2D
!  02/27/2009 CHP Change units to mm for TRWUP_calc and RWUP_2D_ts
!  08/21/2009 CHP Change to sub-hourly, variable time step. Remove MGAR.
!-----------------------------------------------------------------------
! Called by: SPAM_2D
! Calls:     None
!=======================================================================
      SUBROUTINE RWUts_2D(TimeIncr,            !Input 
     &    Cells, EOP_ts, SWV_avail,                       !Input 
     &    RWU_2D_ts, RWUP_2D_ts, TRWU_ts, TRWUP_ts)       !Output

!-----------------------------------------------------------------------
      USE Cells_2D
      Use ModuleData
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------
      INTEGER i, j
      INTEGER, DIMENSION(MaxRows,MaxCols) :: Cell_type
      Double Precision, DIMENSION(MaxRows,MaxCols) :: RWU_2D_ts
      Double Precision, DIMENSION(MaxRows,MaxCols) :: RWUP_2D_ts

      REAL EXPFAC, SWEXF, TimeIncr
      REAL RootLimit
      REAL SWCON1, SWCON3, WUF

      Double Precision EOP_ts, TRWU_ts,  TRWUP_ts
      Double Precision, DIMENSION(MaxRows,MaxCols) :: RWUP_vf
      Double Precision, DIMENSION(MaxRows,MaxCols) :: SWV_avail, SWV_D

      TYPE (CellType) CELLS(MaxRows,MaxCols)

      PARAMETER (SWCON1 = 1.32E-3)
      PARAMETER (SWCON3 = 7.01)

!-----------------------------------------------------------------------
      SWV_D = SWV_avail 
      TRWUP_ts  = 0.0
      TRWU_ts   = 0.0
      RWUP_2D_ts = 0.0
      RWU_2D_ts = 0.0

      IF (EOP_ts < 1.E-9) RETURN

      DO i = 1, NRowsTot
        DO j = 1, NColsTot

          SELECT CASE (Cell_type(i,j))
          CASE(3,4,5); CONTINUE
          CASE DEFAULT; CYCLE
          END SELECT

          IF (RLV_2D(i,j) < 1.E-5 .OR. SWV_D(i,j) <= LL(i,j)) THEN
            RWUP_2D_ts(i,j) = 0.
          ELSE
!           ------------------------------------------------------------
!           Soil limitation
            EXPFAC = MIN((SWCON2(i,j) * (SWV_D(i,j) - LL(i,j))), 40.)
            RWUP_2D_ts(i,j) = SWCON1 * 
     &                        EXP(EXPFAC) / (SWCON3 - ALOG(RLV_2D(i,j)))
!           RWUP_2D_ts in cm3[water]/cm[root]-d

!           ------------------------------------------------------------
!           Root limitation
!           Effects of saturated soil
            IF ((SAT(i,j) - SWV_D(i,j)) >= PORMIN) THEN
               TSS(i,j) = 0.
            ELSE
               TSS(i,j) = TSS(i,j) + TimeIncr   !minutes
            ENDIF
!           Delay of 2 days after soil layer is saturated before root
!           water uptake is affected
            IF (TSS(i,j) .GT. 2880.) THEN   !2880 minutes = 2 days
               SWEXF = (SAT(i,j) - SWV_D(i,j)) / PORMIN
               SWEXF = MAX(SWEXF,0.0)
            ELSE
               SWEXF = 1.0
            ENDIF
            SWEXF = MIN(SWEXF,1.0)

!           Root limitation should be scaled up based on max that can
!               be extracted by roots in an hour.  Daily value underestimates
!               hourly limit.  Soil water supply limitation is OK as-is.
            RootLimit = RWUMX * SWEXF * Scale2Hour

!           ------------------------------------------------------------
!           Actual potential root uptake is minimum of root limited rate
!               and soil limited rate
            RWUP_2D_ts(i,j) = MIN(RWUP_2D_ts(i,j), RootLimit)
!           RWUP_2D_ts in cm3[water]/cm[root]-d

!           Convert to volumetric fraction 
            RWUP_vf(i,j) = RWUP_2D_ts(i,j) * RLV_2D(i,j)
!           cm3[water]     cm3[water]   cm[root] 
!           -----------  = ---------- * --------- 
!           cm3[soil]-d    cm[root]-d   cm3[soil] 

!           With this formula, the RWUP can be added in a soil column to get the total for that column.
!           Aggregating across a row requires multiplying by ColFrac
            RWUP_2D_ts(i,j) = RWUP_vf(i,j) *Thick(i,j)
!               cm[water]     cm3[water]                          1 cm2[soil cell width x row length]
!               ---------   = -----------  * cm[soil thickness] * -----------------------------------
!                   d         cm3[soil]-d                         1 cm2[watercell width x row length]

!           Scale to time step.  Units are mm (i.e., total for this time step)
            RWUP_2D_ts(i,j) = RWUP_2D_ts(i,j) * 10./ 24. *(TimeIncr/60.)
!                              cm[water]        mm    d
!              mm[water]    =  ---------      * -- * -- * hr
!                                  d            cm   hr

            TRWUP_ts = TRWUP_ts + RWUP_2D_ts(i,j) *ColFrac(i,j)      !mm
          ENDIF
        ENDDO
      ENDDO

!-----------------------------------------------------------------------
!     Scale back root water extraction, if greater than demand 
      IF (EOP_ts .LT. TRWUP_ts .AND. TRWUP_ts > 1.E-9) THEN
        WUF = EOP_ts / TRWUP_ts
      ELSE
        WUF = 1.0
      ENDIF

      TRWU_ts = 0.0
      DO i = 1, NRowsTot
        DO j = 1, NColsTot
          RWU_2D_ts(i,j) = RWUP_2D_ts(i,j) * WUF
          SELECT CASE (Cells(i,j)%Struc%Cell_Type)
          CASE (3,4,5)
            TRWU_ts = TRWU_ts + RWU_2D_ts(i,j) * ColFrac(i,j)
          END SELECT
        ENDDO
      ENDDO

      RETURN
      END SUBROUTINE RWUts_2D

!-----------------------------------------------------------------------
!     RWUts_2D VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! DLAYR(i,j)  Soil thickness in layer L (cm)
! LL(i,j)     Volumetric soil water content in soil layer L at lower limit
!             (cm3/cm3)
! NL        Maximum number of soil layers = 20 
! PORMIN    Minimum pore space required for supplying oxygen to roots for 
!             optimal growth and function (cm3/cm3)
! RLV_2D(i,j)    Root length density for soil layer L ((cm root / cm3 soil))
! RWUP_2D_ts(i,j)    Root water uptake from soil layer L in current time step(cm/d)
! RWUMX     Maximum water uptake per unit root length, constrained by soil 
!             water (cm3[water] / cm [root])
! SAT(i,j)    Volumetric soil water content in layer L at saturation
!             (cm3 [water] / cm3 [soil])
! SWV_D(i,j)  Volumetric soil water content in layer L
!             (cm3 [water] / cm3 [soil])
! SWCON1    Constant used in determining root water uptake 
! SWCON2(i,j) Variable used in determining root water uptake, dependant on 
!             lower limit in layer L 
! SWCON3    Constant used in determining root water uptake 
! SWEXF     Excess water stress factor for layer with deepest roots (0-1) 
! TRWUP_calc     Total potential daily root water uptake (mm/d)
! TSS(i,j)    Number of days soil layer L has been saturated (d)
!-----------------------------------------------------------------------
!     END SUBROUTINE ROOTWU
!-----------------------------------------------------------------------

!==========================================================================
      END MODULE RootWU_2D_mod
!==========================================================================

