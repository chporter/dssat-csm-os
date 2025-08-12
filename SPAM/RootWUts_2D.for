!***********************************************************************
!  RootWU_2D_mod, module
!
!  Purpose: Determines root uptake at sub-daily time steps for 2D model. 
!  Two subroutines are used with some shared variables.
!  - ROOTWU_2D is called daily from SPAM.for and sends back the daily
!       accumulated values of RWU (1D), TRWU, TRWUP.
!  - RWUts_2D is called from WatBal_2D on a sub-daily time step. At each
!       time step, root water uptake is calculated for that time step. 

!  Revision history:
!  2025-08-11 CHP  Written

!***********************************************************************

      MODULE RootWU_2D_mod
      
      USE Cells_2D

!     These variables are shared between ROOTWU_2D (called daily by SPAM) 
!       and RWUts_2D (called sub-daily by WatBal_2D)
      REAL, DIMENSION(MaxRows,MaxCols) :: RLVcell, RWUcell
      LOGICAL, PROTECTED :: First_ts
      REAL, DIMENSION(MaxRows,MaxCols) :: LLcell, SATcell
      REAL, DIMENSION(MaxRows,MaxCols) :: SWCON2, ThickCell, ColumnFrac
      REAL, DIMENSION(MaxRows,MaxCols) :: TSS, TSS_last
      REAL PORMINts, RWUMXts
      REAL Scale2Hour, TRWU_day, TRWUP_day

      contains

!==========================================================================
!     DAILY ROUTINE CALLED FROM SPAM
!==========================================================================
      SUBROUTINE ROOTWU_2DA (DYNAMIC, CELLS, 
     &      DAYL, NLAYR, PORMIN, RWUMX,           !Input
     &      RWU, TRWU, TRWUP)                     !Output

!     ------------------------------------------------------------------
      USE ModuleData
      IMPLICIT  NONE
      SAVE
!     ------------------------------------------------------------------

      INTEGER, INTENT(IN) :: DYNAMIC, NLAYR
      REAL, INTENT(IN) :: DAYL, PORMIN, RWUMX
      TYPE (CellType), INTENT(INOUT) :: CELLS(MaxRows,MaxCols)
      REAL, INTENT(OUT) :: TRWU, TRWUP
      REAL, DIMENSION(NL), INTENT(OUT) :: RWU
      INTEGER i,j

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      LLcell        = CELLS % STATE % LL
      RLVcell    = CELLS % STATE % RLV
      SATcell       = CELLS % STATE % SAT
      ThickCell     = CELLS % Struc % Thick
      ColumnFrac = BedDimension % ColFrac

      TSS       = 0.0
      TSS_LAST  = 0.0

!-----------------------------------------------------------------------
!     Compute SWCON2 for each soil layer.  Adjust SWCON2 for extremely
!     high LL to avoid water uptake limitations.
!-----------------------------------------------------------------------
      SWCON2    = 0.0
      DO i = 1, NRowsTot
        DO j = 1, NColsTot
          SWCON2(i,j) = 120. - 250. * LLcell(i,j)
          IF (LLcell(i,j) > 0.30) SWCON2(i,j) = 45.0
        ENDDO  
      ENDDO

      RWU   = 0.0
      TRWUP = 0.0

      PORMINts = PORMIN
      RWUMXts  = RWUMX

!***********************************************************************
!***********************************************************************
!     DAILY RATE
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!     ------------------------------------------------------------------
!     This call from SPAM is done before WatBal_2D has called RWUts_2D
!       at a sub-daily time step. 

      RLVcell = Cells % State % RLV
      Scale2Hour = 24. / DAYL
      RWUcell = 0.0

!***********************************************************************
!***********************************************************************
!     INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!     ------------------------------------------------------------------
!     This call from SPAM is done after WatBal_2D has called RWUts_2D
!       at a sub-daily time step. 

!     Store 2D RWU in CELLS variable
      CELLS % Rate % EP_rate = RWUcell

!     Convert 2D RWU to 1D 
      CALL Cell2Layer_2D(
     &  RWUcell, CELLS%Struc, NLAYR,  !Input
     &  RWU)                           !Output

!     Convert units from mm to cm for DSSAT plant routines.
      TRWU = TRWU_day / 10.             !cm
      TRWUP = TRWUP_day / 10.           !cm

      CALL PUT('SPAM','TRWUP', TRWUP)
      CALL PUT('SPAM','TRWU',  TRWU)

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

          SELECT CASE (CELLS(i,j) % Struc % Cell_Type)
          CASE(3,4,5); CONTINUE
          CASE DEFAULT; CYCLE
          END SELECT

          IF (RLVcell(i,j) < 1.E-5 .OR. SWV_D(i,j) <= LLcell(i,j)) THEN
            RWUP_2D_ts(i,j) = 0.
          ELSE
!           ------------------------------------------------------------
!           Soil limitation
            EXPFAC = MIN((SWCON2(i,j) * (SWV_D(i,j) -LLcell(i,j))), 40.)
            RWUP_2D_ts(i,j) = SWCON1 * 
     &                      EXP(EXPFAC) / (SWCON3 - ALOG(RLVcell(i,j)))
!           RWUP_2D_ts in cm3[water]/cm[root]-d

!           ------------------------------------------------------------
!           Root limitation
!           Effects of saturated soil
            IF ((SATcell(i,j) - SWV_D(i,j)) >= PORMINts) THEN
               TSS(i,j) = 0.
            ELSE
               TSS(i,j) = TSS(i,j) + TimeIncr   !minutes
            ENDIF
!           Delay of 2 days after soil layer is saturated before root
!           water uptake is affected
            IF (TSS(i,j) .GT. 2880.) THEN   !2880 minutes = 2 days
               SWEXF = (SATcell(i,j) - SWV_D(i,j)) / PORMINts
               SWEXF = MAX(SWEXF,0.0)
            ELSE
               SWEXF = 1.0
            ENDIF
            SWEXF = MIN(SWEXF,1.0)

!           Root limitation should be scaled up based on max that can
!               be extracted by roots in an hour.  Daily value underestimates
!               hourly limit.  Soil water supply limitation is OK as-is.
            RootLimit = RWUMXts * SWEXF * Scale2Hour

!           ------------------------------------------------------------
!           Actual potential root uptake is minimum of root limited rate
!               and soil limited rate
            RWUP_2D_ts(i,j) = MIN(RWUP_2D_ts(i,j), RootLimit)
!           RWUP_2D_ts in cm3[water]/cm[root]-d

!           Convert to volumetric fraction 
            RWUP_vf(i,j) = RWUP_2D_ts(i,j) * RLVcell(i,j)
!           cm3[water]     cm3[water]   cm[root] 
!           -----------  = ---------- * --------- 
!           cm3[soil]-d    cm[root]-d   cm3[soil] 

!           With this formula, the RWUP can be added in a soil column to get the total for that column.
!           Aggregating across a row requires multiplying by ColFrac
            RWUP_2D_ts(i,j) = RWUP_vf(i,j) *ThickCell(i,j)
!               cm[water]     cm3[water]                          1 cm2[soil cell width x row length]
!               ---------   = -----------  * cm[soil thickness] * -----------------------------------
!                   d         cm3[soil]-d                         1 cm2[watercell width x row length]

!           Scale to time step.  Units are mm (i.e., total for this time step)
            RWUP_2D_ts(i,j) = RWUP_2D_ts(i,j) * 10./ 24. *(TimeIncr/60.)
!                              cm[water]        mm    d
!              mm[water]    =  ---------      * -- * -- * hr
!                                  d            cm   hr

            TRWUP_ts = TRWUP_ts 
     &             + RWUP_2D_ts(i,j) * ColumnFrac(i,j)   !mm
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
            TRWU_ts = TRWU_ts + RWU_2D_ts(i,j) * ColumnFrac(i,j)
          END SELECT
        ENDDO
      ENDDO

      TRWU_day  = TRWU_day  + TRWU_ts
      TRWUP_day = TRWUP_day + TRWUP_ts
      RWUcell = RWUcell + RWU_2D_ts

      RETURN
      END SUBROUTINE RWUts_2D

!-----------------------------------------------------------------------
!     RWUts_2D VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! DLAYR(i,j)  Soil thickness in layer L (cm)
! LLcell(i,j) Volumetric soil water content in soil cell i,j at lower limit
!             (cm3/cm3)
! NL        Maximum number of soil layers = 20 
! PORMIN    Minimum pore space required for supplying oxygen to roots for 
!             optimal growth and function (cm3/cm3)
! RLVcell(i,j) Root length density for soil layer L ((cm root / cm3 soil))
! RWUP_2D_ts(i,j) Root water uptake from soil layer L in current time step(cm/d)
! RWUMX     Maximum water uptake per unit root length, constrained by soil 
!             water (cm3[water] / cm [root])
! SATcell(i,j) Volumetric soil water content in cell i,j at saturation
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

