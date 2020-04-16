C=======================================================================
C  NH3Vol, Subroutine, U. Singh, C.H.Porter
C  Determines ammonia volatilization in top soil layer
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  04/15/2020 US/CHP adapted from OXLAYER
C=======================================================================

      SUBROUTINE NH3Vol (CONTROL,
     &    ES, FERTDATA, FLOODWAT, NSWITCH, SNH4, SNO3,    !Input
     &    SOILPROP, SRAD, ST, SW, TMAX, TMIN,             !Input
     &    UHYDR1,UREA, XHLAI,                             !Input
     &    DLTSNH4, DLTSNO3, DLTUREA,                      !I/O
     &    TOTAML)                                         !Output

      USE FloodModule
      USE FertType_mod
      IMPLICIT NONE
      SAVE

      CHARACTER*1 RNMODE

      INTEGER  NSWITCH, YRDOY, YRDRY, DYNAMIC
      INTEGER  RUN

      REAL    FPI, SW1  
      REAL    ALGACT,AMLOSS,OXRNTRF
      REAL    ALI
      REAL    OXNI    
      REAL    TOTAML
      REAL NH4C, NH3C, UHYDR1

      LOGICAL UNINCO
      REAL    SNH4MIN  
      REAL    KG2PPM(NL)
      REAL    SURCEC, SURF_THICK

!     Local Variables
      REAL     SURAD,OXALI,STI,SWI,AMPES,     HTMFAC,MF,TK
      REAL     STEMP,HES,PHSHIFT,XL,XL2,OXPH1 !TEMPFU,SWF,UALGCT,OXUHYDR
      REAL     UHYDC, UHYDM, PHUHY, OXPH,             SNH3,WIND
      REAL     ALOGHK,HK,NH3M,NH3P,AMLOS1    !,TFACTOR,OXNC,WF2,GLOS1
      REAL     ELAG       !PHFACT,RNTRFI,HOXRNT,

!     Passed Variables
      REAL     UREA(NL),SNH4(NL),SNO3(NL),SRAD,MSALB, XHLAI
      REAL     SW(NL),SAT(NL),ST(NL),DUL(NL),ES,TMIN,TMAX
      REAL     LL(NL),PH(NL),DLAYR(NL)
      REAL     OC(NL)

      TYPE (ControlType)  CONTROL
      TYPE (SoilType)     SOILPROP
      TYPE (FloodWatType) FloodWat
      TYPE (FertType)     FERTDATA

      REAL DLTUREA(NL),  DLTSNO3(NL),  DLTSNH4(NL)
      REAL TMPUREA, TMPNH4,  TMPNO3

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
      MSALB  = SOILPROP % MSALB
      SAT   = SOILPROP % SAT
      SW1 = SW(1)
      UNINCO  = FERTDATA % UNINCO

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      FPI = 0.50

      !Initialize for non-sequenced runs
      IF (INDEX('FQ',RNMODE) .LE. 0 .OR. RUN .EQ. 1) THEN
        OXNI  = 0.05
        TOTAML = 0.0
        OXPH = PH(1)      !chp 1/27/2004
      ENDIF

      ALGACT = 0.1
      SNH4MIN  = 0.0   

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      TMPUREA = UREA(1) + DLTUREA(1)
      TMPNH4  = SNH4(1) + DLTSNH4(1)
      TMPNO3  = SNO3(1) + DLTSNO3(1)

      IF (UNINCO) THEN
        SURF_THICK = 1.0 !Top 1cm
      ELSE
!       if fertilizer is incorporated, use full layer thickness
        SURF_THICK = DLAYR(1)  
      ENDIF 

!     Compute algal activity - for saturated soil
      SURAD = SRAD*(1.0-MSALB)*EXP(-0.85*XHLAI)
      OXALI = 1.0 - EXP(-SURAD/10.0)

!     chp 2020-04-16
!     It's not likely that SW(1) will be exactly equal to SAT(1). 
!     Maybe use Aloha_mod grandual transition between 90% of SAT and SAT.
      IF (SW(1) .EQ. SAT(1)) THEN
         ALI = 1.0 - EXP(-SURAD/10.0)
      ELSE
         ALI = 1.0
      ENDIF

!     chp 2020-04-16. STI = 1.0 between 25 and 30C. 
      IF (ST(1) .LT. 30.0)  THEN
         STI = (ST(1)-15.0)*0.1       
      ELSEIF (ST(1) .GE. 30.0) THEN
         STI = 1.0-(ST(1)-30.0)*0.05 
      ENDIF
      STI = AMIN1 (STI, 1.0)
!     chp 2020-04-16, prevent negative value for temperatures below 15C
      STI = AMAX1 (STI, 0.0)  
!     
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
      AMLOSS  = 0.0
      OXRNTRF = 0.0

!     I = 6
!     K = 1

!     HTMFAC = 0.931+0.114*K-0.0703*K**2+0.0053*K**3
!     For K=1, HTMFAC = 0.980
      HTMFAC = 0.980

!     Hourly soil surface temp
!     Do we want to bring in actual soil surface temperature here?
      STEMP  = TMIN + HTMFAC * (TMAX + 2.0 - TMIN)

      AMPES   = 0.38*ES                           ! 0.35->0.38

!     CONSTANT VALUE
!     HES = AMPES*SIN(3.141593*FLOAT(I)/12.0)+0.08    ! 10->9
!     HES = AMPES*SIN(3.141593*0.5)+0.08    
      HES = AMPES * 1.08    
!     HES = AMAX1 (ES/24.0,ABS(HES))  !<- es/24 will never be > hes

!     BIOACT = AMIN1(1.0,5.0*ALGACT)
      PHSHIFT = 0.75 + 2.0*ALGACT
      IF (TMPNH4 .LE. TMPNO3) THEN
         PHSHIFT = 0.6
      ENDIF

!     OXPH1 = PH(1)+PHSHIFT*SIN(3.1412*FLOAT(I)/12.0)   ! 8->11
!     OXPH1 = PH(1)+PHSHIFT*SIN(3.1412*0.5)   
      OXPH1 = PH(1) + PHSHIFT       !sin(pi/2) = 1   

!     Add effects of UREA hydrolysis on surface layer pH here
      XL      = DUL(1) - LL(1)*0.5
      XL2     = SW(1)  - LL(1)*0.5
      MF      = XL2/XL
      MF      = AMAX1 (MF,0.0)
      IF (MF .GT. 1.5 .AND. OXPH1 .LT. 7.2) THEN
         OXPH1 = 7.2
      ENDIF

      IF (UHYDR1 .GT. 0.001) THEN
         UHYDC = UHYDR1 * KG2PPM(1) * DLAYR(1) / SURF_THICK
         UHYDM = UHYDC*0.001/14.0          ! (Molar conc.)
         PHUHY = AMIN1 (10.0,-LOG10(UHYDM))
         OXPH    = OXPH1 + 1.0*(10.0-PHUHY)/10.0
      ENDIF
      OXPH = AMIN1 (OXPH,9.0)
      OXPH = AMAX1 (OXPH,PH(1))

!     AMMONIA loss routine ... calculate surface layer NH3
      TK     = STEMP + 273.15
      NH4C   = TMPNH4  * KG2PPM(1) * DLAYR(1) / SURF_THICK
      NH3C = NH4C /(1.0+10.0**(0.09018+2729.92/TK-OXPH))
      
!     Calculate ammonia (Mass) using inverse of (KG2PPM) OXLT in cm)
      IF (NH3C  .LE. 0.00001 .AND. NH3C  .GT. 0.0) THEN
         NH3C = 0.0
      ENDIF
      SNH3    = NH3C  / (KG2PPM(1) * DLAYR(1) / SURF_THICK)
      IF (SNH3 .GT. (TMPNH4 - SNH4MIN)) THEN
         SNH3  = TMPNH4 - SNH4MIN
         NH3C  = SNH3 * KG2PPM(1) * DLAYR(1) / SURF_THICK
      ENDIF
      
      WIND    = 7.15*HES           ! 7.15 -> 5.75
      ALOGHK  = 158.559-8621.06/TK-25.6767*ALOG(TK)+0.035388*TK 
      HK      = EXP(ALOGHK)
!     OXH3M   = OXH3C*0.001/14.0
      NH3M   = NH3C *0.001/14.0  !<-molar
      NH3P   = 10.0 * NH3M / HK  !partial pressure                
      IF (NH3P .GT. 0.0) THEN
         AMLOS1  = 0.0012*NH3P + 0.0014*WIND + 0.00086*NH3P**2 * WIND
      ENDIF
      IF (NSWITCH .EQ. 8) THEN
         AMLOS1 = 0.0
      ENDIF
      IF (NH3P .LE. 0.0) THEN
         AMLOS1 = 0.0
      ENDIF

      AMLOS1  = AMIN1 (AMLOS1,TMPNH4-SNH4MIN)
      AMLOSS  = AMLOSS + AMLOS1
      TOTAML  = TOTAML + AMLOS1
      TMPNH4  = TMPNH4 - AMLOS1

!      write(888,'(I8,20(1X,E12.4))') yrdoy, amlos1, totaml, tmpnh4, 
!     &     snh4(1), tmpno3, sno3(1), NH3P, WIND, NH3C, NH4C, OXPH

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

