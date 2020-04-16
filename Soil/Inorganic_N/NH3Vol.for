C=======================================================================
C  NH3Vol, Subroutine, U. Singh, C.H.Porter
C  Determines ammonia volatilization in top soil layer
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  04/15/2020 US/CHP adapted from OXLAYER
C=======================================================================

      SUBROUTINE NH3Vol (CONTROL,
     &    ES, FLOODWAT, NSWITCH, SNH4, SNO3,              !Input
     &    SOILPROP, SRAD, ST, SW, TMAX, TMIN,             !Input
     &    UHreduce, UHYDR1,UREA, XHLAI,                   !Input
     &    DLTSNH4, DLTSNO3, DLTUREA, OXLAYR,              !I/O
     &    TOTAML)                                         !Output

      USE FloodModule
      USE FertType_mod
      IMPLICIT NONE
      SAVE

      CHARACTER*1 RNMODE

      INTEGER  NSWITCH, YRDOY, YRDRY, DYNAMIC
      INTEGER  RUN

      REAL    FPI, SW1        !, BD1
      REAL    ALGACT,AMLOSS,OXRNTRF
      REAL    ALI
!     REAL    OXLT,OXFAC,OXU,OXN3,OXN3C,OXH4,OXH4C
      REAL    OXNI        !OXN,
      REAL    TOTAML
      INTEGER IHDAY, IBP      !,LFD10
      REAL NH4C, NH3C, UHYDR1

!     UNINCO: Fertilizer not fully incorporated; if true, ignore oxlayer
!      LOGICAL DailyCalc,UNINCO
!      REAL    WFP
      REAL    OXMIN4  !OXMIN3, 
      REAL    KG2PPM(NL)
      REAL    SURCEC

!     Local Variables
      INTEGER  IST,IHR,I,K
      REAL     SURAD,OXALI,STI,SWI,AMPES,AK,HTMFAC,MF,TK
      REAL     STEMP,HES,PHSHIFT,XL,XL2,OXPH1 !TEMPFU,SWF,UALGCT,OXUHYDR
!     REAL     UHYDC, UHYDM, PHUHY, OXPH,OXMINC,OXH3C,OXH3,WIND
      REAL     UHYDC, UHYDM, PHUHY, OXPH,             SNH3,WIND
      REAL     ALOGHK,HK,OXH3M,OXH3P,AMLOS1,GLOS1    !,TFACTOR,OXNC,WF2
      REAL     ELAG       !PHFACT,RNTRFI,HOXRNT,

!     Passed Variables
!      INTEGER  FERTYPE
      REAL     UREA(NL),SNH4(NL),SNO3(NL),SRAD,MSALB, XHLAI
      REAL     SW(NL),SAT(NL),ST(NL),DUL(NL),ES,TMIN,TMAX
      REAL     LL(NL),PH(NL),DLAYR(NL)
      REAL     OC(NL)

!     Additions to oxidation layer from today's fertilizer:
!      REAL ADDOXH4, ADDOXN3, ADDOXU

      TYPE (ControlType)  CONTROL
      TYPE (SoilType)     SOILPROP
      TYPE (FloodWatType) FloodWat
      TYPE (OxLayerType)  OXLAYR
!      TYPE (FertType)     FERTDATA

      REAL DLTUREA(NL),  DLTSNO3(NL),  DLTSNH4(NL)
!      REAL DLTOXU, DLTOXH4, DLTOXN3
      REAL TMPUREA, TMPNH4,  TMPNO3
!      REAL TMPOXU, TMPOXH4, TMPOXN3
      REAL UHreduce

!-----------------------------------------------------------------------
      DYNAMIC = CONTROL % DYNAMIC
      RNMODE  = CONTROL % RNMODE 
      RUN     = CONTROL % RUN    
      YRDOY   = CONTROL % YRDOY  

      YRDRY = FLOODWAT % YRDRY

      DLAYR = SOILPROP % DLAYR
      SURCEC= SOILPROP % CEC(1)
      DUL   = SOILPROP % DUL
      KG2PPM= SOILPROP % KG2PPM
      LL    = SOILPROP % LL
      OC    = SOILPROP % OC
      PH    = SOILPROP % PH
!     SALB  = SOILPROP % SALB
      MSALB  = SOILPROP % MSALB
      SAT   = SOILPROP % SAT

      SW1 = SW(1)

      !DailyCalc   = OXLAYR % DailyCalc
      !DLTOXH4 = OXLAYR % DLTOXH4
      !DLTOXN3 = OXLAYR % DLTOXN3
      !DLTOXU  = OXLAYR % DLTOXU
!      OXLT    = OXLAYR % OXLT
!      OXH4    = OXLAYR % OXH4
!      OXN3    = OXLAYR % OXN3
!      OXU     = OXLAYR % OXU
!      OXMIN3  = OXLAYR % OXMIN3
!     OXMIN4  = OXLAYR % OXMIN4
      OXMIN4  = 0.0   !from FCHEM.for
      IBP     = OXLAYR % IBP

!      ADDOXH4 = FERTDATA % ADDOXH4
!      ADDOXN3 = FERTDATA % ADDOXN3
!      ADDOXU  = FERTDATA % ADDOXU
!      FERTYPE = FERTDATA % FERTYPE
!      LFD10   = FERTDATA % LFD10
!      UNINCO  = FERTDATA % UNINCO

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      FPI = 0.50

      !Initialize for non-sequenced runs
      IF (INDEX('FQ',RNMODE) .LE. 0 .OR. RUN .EQ. 1) THEN

! CHP 
!        OXFAC = KG2PPM(1)
!        OXLT = DLAYR(1)

!        OXLT  = 0.50 - 0.1*OC(1)
!        OXLT  = MAX(0.001,OXLT)
!        OXLAYR % OXLT = OXLT
!        OXFAC = 1.0/(BD1*OXLT*1.0E-01)

!        OXU   = UREA(1) !* KG2PPM(1) / OXFAC
!        OXN3  = SNO3(1) !* KG2PPM(1) / OXFAC
!        OXH4  = SNH4(1) !* KG2PPM(1) / OXFAC
        OXNI  = 0.05

        TOTAML = 0.0
!        DailyCalc = .TRUE.

        OXPH = PH(1)      !chp 1/27/2004
      ENDIF

      ALGACT = 0.1

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
!      OXFAC = 1.0/(BD1*OXLT*1.0E-01)
!      OXFAC = KG2PPM(1)

      TMPUREA = UREA(1) + DLTUREA(1)
      TMPNH4  = SNH4(1) + DLTSNH4(1)
      TMPNO3  = SNO3(1) + DLTSNO3(1)

!      TMPOXU  = OXU  + DLTOXU  + ADDOXU
!      TMPOXH4 = OXH4 + DLTOXH4 + ADDOXH4

      !FAC1 = 1.0 / (BD1 * 1.E-01 * DLAYR(1))
      !NO3_1  = SNO3(1) * FAC1
      !NH4_1  = SNH4(1) * FAC1
      !UPPM_1 = UREA(1) * FAC1

      !OXH4C   = TMPOXH4   * OXFAC
      !OXN3C   = TMPOXN3   * OXFAC

!!     If fertilizer incorporated, ignore oxidation layer 
!!         (UNINCO set in FPLACE)
!      IF (.NOT. UNINCO) RETURN

!!     LFD10 - 10 days after last fertilizer application (set in FPLACE)
!      IF (YRDOY < LFD10) THEN
!         DailyCalc = .FALSE.
!      ELSE
!         DailyCalc = .TRUE.
!      ENDIF

!      TMPOXU    = AMIN1 (TMPOXU , TMPUREA)
!      TMPOXH4   = AMIN1 (TMPOXH4, TMPNH4)
!      TMPOXN3   = AMIN1 (TMPOXN3, TMPNO3)

!     Compute algal activity - for saturated soil
!      SURAD = SRAD*(1.0-SALB)*EXP(-0.85*XHLAI)
      SURAD = SRAD*(1.0-MSALB)*EXP(-0.85*XHLAI)
      OXALI = 1.0 - EXP(-SURAD/10.0)

      IF (SW(1) .EQ. SAT(1)) THEN
         ALI = 1.0 - EXP(-SURAD/10.0)
      ELSE
         ALI = 1.0
      ENDIF

      IF (ST(1) .LT. 30.0)  THEN
         STI = (ST(1)-15.0)*0.1
      ELSEIF (ST(1) .GE. 30.0) THEN
         STI = 1.0-(ST(1)-30.0)*0.05
      ENDIF

      STI = AMIN1 (STI,1.0)
      SWI = 1.0
      IF (SW(1) .LT. DUL(1)) THEN
         SWI = (SAT(1)-DUL(1))/(SAT(1)-SW(1))
      ENDIF

!     Biological and chemical activity of "OXIDIZED" layer
      ELAG   = AMIN1(OXNI,FPI,ALI,STI,SWI)
      ALGACT = ELAG*(4.0-ALGACT)*ALGACT           ! 4->3.5
      IF (XHLAI .GT. 1.5) THEN
         ALGACT = AMIN1(ALGACT,ALI)
      ENDIF
      ALGACT = AMIN1 (ALGACT,1.00)
      ALGACT = AMAX1 (ALGACT,0.05)

!     Partition soil evaporation
!     AMPES(hr) is 0.35*ES(daily) chamged to 0.45
      AMLOSS  = 0.0
      OXRNTRF = 0.0

!!     2020-04-15 US, CHP - require NBUND > 0 for sub-daily calculations
!      IF ((ALGACT .GE. 0.2 .OR. YRDOY .LT. LFD10) 
!     &    .AND. NBUND .GT. 0) THEN
!        DailyCalc = .FALSE.
!      ENDIF

! IF just dried from flooded conditions, do this:
!      IF (DailyCalc) THEN
         IHDAY = 1
         IST   = 6
         IHR   = 6
!      ELSE
!         IHDAY = 12
!         IST   = 1
!         IHR   = 12
!      ENDIF

!!     Potential Hydrolysis
!!     AK = -1.12+0.31*OC(1)+0.203*PH(1)-0.0355*OC(1)*PH(1)
!      AK = 0.25+0.075*OC(1)
!      AK = AMAX1 (AK,0.25)
!      AK = AMIN1 (AK,1.00)

!     2020-04-13 US & CHP
!     If urease inhibitor is active, reduce hydrolysis rate
      AK = AK * UHreduce

      I = 6
      K = 1

!     DONT NEED THIS IF WE GET SOIL TEMPERATURE FROM ELSEWHERE
!     DON'T NEED TO CALCULATE HTMFAC - CONSTANT
      HTMFAC = 0.931+0.114*K-0.0703*K**2+0.0053*K**3

!     Hourly soil surface temp
!     BRING IN SURFACE TEMPERATURE HERE 
      STEMP  = TMIN + HTMFAC*(TMAX+2.0-TMIN)

      AMPES   = 0.38*ES                           ! 0.35->0.38

!     CONSTANT VALUE
      HES = AMPES*SIN(3.141593*FLOAT(I)/12.0)+0.08    ! 10->9
      HES = AMAX1 (ES/24.0,ABS(HES))

!     BIOACT = AMIN1(1.0,5.0*ALGACT)
      PHSHIFT = 0.75 + 2.0*ALGACT

!         SELECT CASE (FERTYPE)
!           CASE (0:3,8,11:50)
!            IF (OXH4C .LE. OXN3C) THEN
             IF (TMPNH4 .LE. TMPNO3) THEN
                PHSHIFT = 0.6
             ENDIF
!           CASE DEFAULT
!             ! Just go on to UREA hydrolysis
!         END SELECT

!!        UREA hydrolysis function of surface layer OC or
!!        biological activity, whichever is greater.
!         TEMPFU  = 0.04*STEMP - 0.2
!         TEMPFU  = AMIN1 (TEMPFU,1.0)
!         TEMPFU  = AMAX1 (TEMPFU,0.0)
         XL      = DUL(1) - LL(1)*0.5
         XL2     = SW(1)  - LL(1)*0.5
         MF      = XL2/XL
         MF      = AMAX1 (MF,0.0)
!         SWF     = MF + 0.20
!         IF (SW(1) .GE. DUL(1)) THEN
!            SWF = 1.0
!         ENDIF
!         SWF     = AMIN1 (SWF,1.0)
!         SWF     = AMAX1 (SWF,0.0)
!!        UALGCT  = 0.25*ALGACT
!         UALGCT  = 0.0
!         OXUHYDR = AMAX1 (AK,UALGCT)*AMIN1 (SWF,TEMPFU)*TMPOXU

!        CHP added this check, but still get negative urea 
!        need to overhaul this routine - prevent negative values for 
!        all N species.
!         OXUHYDR = AMIN1(TMPUREA, OXUHYDR)  !CHP 5/4/2010

!         TMPOXU     = TMPOXU     - OXUHYDR
!         TMPOXH4    = TMPOXH4    + OXUHYDR

!     Already integrated, so don't need to do here (working with yesterday's value)
!         TMPUREA = TMPUREA - UHYDR1    !OXUHYDR
!         TMPNH4  = TMPNH4  + UHYDR1    !OXUHYDR

         OXPH1 = PH(1)+PHSHIFT*SIN(3.1412*FLOAT(I)/12.0)   ! 8->11

!        Add effects of UREA hydrolysis on surface layer pH here
         IF (MF .GT. 1.5 .AND. OXPH1 .LT. 7.2) THEN
            OXPH1 = 7.2
         ENDIF

!     CHP
!         OXUHYDR = UHYDR1

         IF (UHYDR1 .GT. 0.001) THEN
            UHYDC = UHYDR1 * KG2PPM(1)
            UHYDM = UHYDC*0.001/14.0          ! (Molar conc.)
            PHUHY = AMIN1 (10.0,-LOG10(UHYDM))
            OXPH    = OXPH1 + OXALI*(10.0-PHUHY)/10.0
         ENDIF
         OXPH = AMIN1 (OXPH,9.0)
         OXPH = AMAX1 (OXPH,PH(1))

!        AMMONIA loss routine ... calculate surface layer NH3
         TK     = STEMP + 273.15
!        OXH4C  = TMPOXH4  * OXFAC
         NH4C   = TMPNH4  * KG2PPM(1)
!        OXMINC = OXMIN4* OXFAC
!        OXMINC = OXMIN4 * KG2PPM(1)

!        CALL AMTHERM (OXH4C,OXH4SC,BD(1),CEC,2,BPOXL,OXMINC)
!        OXH4SC = AMIN1(OXH4SC/DUL(1),OXH4C)
!         IF (I .LE. 6 .OR. OXH3C .GE. OXH4C) THEN
!           OXH3C = OXH4C/(1.0+10.0**(0.09018+2729.92/TK-OXPH))
            NH3C = NH4C /(1.0+10.0**(0.09018+2729.92/TK-OXPH))
!         ENDIF

!        Calculate ammonia (Mass) using inverse of (KG2PPM) OXLT in cm)
!        IF (OXH3C .LE. 0.00001 .AND. OXH3C .GT. 0.0) THEN
         IF (NH3C  .LE. 0.00001 .AND. NH3C  .GT. 0.0) THEN
!           OXH3C = 0.0
            NH3C = 0.0
         ENDIF
!        OXH3    = OXH3C / KG2PPM(1)
         SNH3    = NH3C  / KG2PPM(1)
!        IF (OXH3 .GT. (TMPNH4-OXMIN4)) THEN
         IF (SNH3 .GT. (TMPNH4-OXMIN4)) THEN
!           OXH3  = TMPNH4 - OXMIN4
            SNH3  = TMPNH4 - OXMIN4
!           OXH3C = OXH3 * KG2PPM(1)
            NH3C  = SNH3 * KG2PPM(1)
         ENDIF

         WIND    = 7.15*HES           ! 7.15 -> 5.75
         ALOGHK  = 158.559-8621.06/TK-25.6767*ALOG(TK)+0.035388*TK 
         HK      = EXP(ALOGHK)
!        OXH3M   = OXH3C*0.001/14.0
         OXH3M   = NH3C *0.001/14.0
         OXH3P   = 10.0*OXH3M/HK                 
         IF (OXH3P .GT. 0.0) THEN
            AMLOS1  = 0.0012*OXH3P+0.0014*WIND+0.00086*OXH3P**2*WIND
         ENDIF
         IF (NSWITCH .EQ. 8) THEN
            AMLOS1 = 0.0
         ENDIF
         IF (OXH3P .LE. 0.0) THEN
            AMLOS1 = 0.0
         ENDIF
         GLOS1   = AMLOS1
         AMLOS1  = AMIN1 (AMLOS1,TMPNH4-OXMIN4)
         AMLOSS  = AMLOSS + AMLOS1
         TOTAML  = TOTAML + AMLOS1
         TMPNH4    = TMPNH4   - AMLOS1
         NH4C    = TMPNH4   * KG2PPM(1)

      write(888,'(I8,20(1X,E10.4))') yrdoy, amlos1, totaml, tmpnh4, 
     &     snh4(1), tmpno3, sno3(1), OXH3P, WIND, NH3C, NH4C, OXPH

!     DLTSNH4 = TMPNH4 - SNH4(1) is exactly 2X AMLOS1

!     Surface variables
      DLTUREA(1) = MAX(0.0, TMPUREA) - UREA(1)
      DLTSNO3(1) = MAX(0.0, TMPNO3)  - SNO3(1)
      DLTSNH4(1) = MAX(0.0, TMPNH4)  - SNH4(1)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************

      RETURN
      END SUBROUTINE NH3Vol

