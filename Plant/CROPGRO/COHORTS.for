      SUBROUTINE COHORTS(DYNAMIC,YRDOY,F,WLDOTN,NGRLF,SWFAC, !INPUT
     & PAR,DTX,DXR57,NMINEA,NMINEP,NMOBR,VSTAGE,WLIDOT,TMIN, !INPUT
     & FREEZ1,CMOBMX,CMINEA,CMINEP,CADLF,KCAN,               !INPUT
     & WTLF,WTNLF,XLAI,WNRLF,WCRLF)                          !OUTPUT
  
      USE ModuleDefs
!     USE DFPORT

      IMPLICIT NONE
      
      external YR_DOY
      save

      CHARACTER*11 COHORTOUT,COHORTIN
C-GH
      character*12 COHORTOUT1,COHORTOUT2
      CHARACTER*60 HEADER
  
      INTEGER DYNAMIC
      INTEGER YRDOY,YEAR,DOY
      INTEGER I
      INTEGER,PARAMETER::NSWAB = 5
      INTEGER CHRTOUT,CHRTIN
C-GH
      integer CHRTOUT1, CHRTOUT2

      REAL WCRLF  !,CRUSLF
      REAL NMINEA,NMINEP,NMOBR
      REAL CMINEA,CMINEP
!     REAL NRUSLF,SLDOT,SLNDOT
      REAL DTX,DXR57
      REAL PAR  !,CHECK
      REAL WTLF,RATTP,XLAI,WTNLF
      REAL LCMP,WNRLF !,LEAFN
      REAL SWFCAB(NSWAB),SWFAC
      REAL CMOBMX,NMOBMX,NMINER,ALPHL
      REAL TMIN,FREEZ1,WLIDOT,CADLF,NLOFF,NLPEST
      REAL NGRLF,WLDOTN,F
      REAL PROLFF,MAXNMINE,KCAN,ICMP,TCMP,SENDAY
      REAL LFDM(199),LFNSN(199),LFSN(199),LFAREA(199)
      REAL LFNSC(199),LFAGE(199)
      REAL LFCMN(199),LFCAD(199)
      REAL LFNMN(199),LFNAD(199)
      REAL LFNMNSN(199),LFWSSN(199),LFFRZ(199),LFPST(199)
!     REAL SUMLFDM,SUMLFNSN,SUMLFAREA,SUMLFN,PLEAFN
      REAL PLEAFN
      REAL SUMLFWSSN,SUMLFFRZ,SUMLFPST,SUMLFNMNSN

      REAL WSLOSS,NVSMOB
      REAL CUMLFDM,CUMAREA
      REAL SHADEFAC(199)
      REAL PORLFT,VSTAGE
      REAL XSENMX(4),SENMAX(4)
      
C-GH 08/19/2025
      REAL WTLF_C, WNRLF_C, WCRLF_C, XLAI_C, WTNLF_C, PLEAFN_C

      LOGICAL FEXIST

      COHORTOUT = 'COHORTS.OUT'
      CHRTOUT=10
      COHORTIN = 'COHORTS.INP'
      CHRTIN=20
C-GH
      COHORTOUT1 = "COHORTS1.OUT"
      COHORTOUT2 = "COHORTS2.OUT"
      CHRTOUT1=111
      CHRTOUT2=222

!****************************
!Initialization
!****************************
      IF (DYNAMIC .EQ. SEASINIT) THEN

      DO I=1,199
        LFDM(I)=0
        LFAREA(I)=0
        LFNSN(I)=0
        LFSN(I)=0
        LFNSC(I)=0
        LFAGE(I)=0
      END DO
   
      CUMLFDM=0

      DO I=1,NSWAB
        SWFCAB(I)=1
      END DO
      
      
C-GH 08/19/2025
      WTLF_C = 0.0
      WNRLF_C = 0.0
      WCRLF_C = 0.0
      XLAI_C = 0.0
      WTNLF_C = 0.0
      
      CHRTIN=303
      COHORTIN="COHORTS.INP"

      OPEN (UNIT = CHRTIN, FILE = COHORTIN, STATUS = 'OLD')
      READ (CHRTIN,'(A60)') HEADER
      READ (CHRTIN,'(15(F7.0))') PROLFF,NMOBMX,NVSMOB,
     &  ALPHL,ICMP,TCMP,SENDAY,XSENMX(1:4),SENMAX(1:4)
      MAXNMINE = 0.060
          
      CLOSE (CHRTIN)



      INQUIRE (FILE = COHORTOUT, EXIST = FEXIST)
      IF (FEXIST) THEN
        I=SYSTEM("DEL COHORTS.OUT")
        OPEN (UNIT = CHRTOUT, FILE = COHORTOUT, STATUS = 'NEW')
        WRITE(CHRTOUT,'(A19)') "*COHORT OUTPUT FILE"
        WRITE(CHRTOUT,'(A28)')
     &    "@YEAR DOY  LWAD   LAID  LN%D O_LWAD   LAID   LN%D"
      ELSE
        OPEN (UNIT = CHRTOUT, FILE = COHORTOUT, STATUS = 'NEW')
        WRITE(CHRTOUT,'(A19)') "*COHORT OUTPUT FILE"
        WRITE(CHRTOUT,'(A28)')
     &    "@YEAR DOY  LWAD   LAID  LN%D O_LWAD   LAID   LN%D"
      ENDIF



!=================from opgrow====================================
!         Initialize daily growth output file      
          INQUIRE (FILE = OUTG, EXIST = FEXIST)
          IF (FEXIST) THEN
            OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'OLD',
     &        IOSTAT = ERRNUM, POSITION = 'APPEND')
            FIRST = .FALSE.
          ELSE
            OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'NEW',
     &        IOSTAT = ERRNUM)
            WRITE(NOUTDG,'("*GROWTH ASPECTS OUTPUT FILE")')
            FIRST = .TRUE.
          ENDIF

          !Write headers
          CALL HEADER(SEASINIT, NOUTDG, RUN)
        ENDIF    ! VSH

        N_LYR = MIN(10, MAX(4,SOILPROP%NLAYR))

        IF (FMOPT == 'A' .OR. FMOPT == ' ') THEN    ! VSH
          WRITE (NOUTDG, 100) "Root Dens. (cm/cm3) by soil ",
     &      "depth (cm):",(SoilProp%LayerText(L), L=1,N_LYR)
  100     FORMAT("!",251X,A,A,/,"!",246X,10A8) 

          WRITE (NOUTDG,200, ADVANCE='NO')
  200     FORMAT('@YEAR DOY   DAS   DAP',
     &         '   L#SD   GSTD   LAID   LWAD   SWAD   GWAD')
!=================from opgrow====================================


      CLOSE(CHRTOUT)
C-GH     
      INQUIRE (FILE = COHORTOUT1, EXIST = FEXIST)
      IF (FEXIST) THEN
        I=SYSTEM("DEL COHORTS1.OUT")
        OPEN (UNIT = CHRTOUT, FILE = COHORTOUT1, STATUS = 'NEW')
        WRITE(CHRTOUT,'(A19)') "*COHORT OUTPUT FILE"
        WRITE(CHRTOUT,'(A28)')
     &    "@YEAR DOY   LFAGE           "
      ELSE
        OPEN (UNIT = CHRTOUT1, FILE = COHORTOUT1, STATUS = 'NEW')
        WRITE(CHRTOUT,'(A19)') "*COHORT OUTPUT FILE"
        WRITE(CHRTOUT,'(A28)')
     &    "@YEAR DOY   LFAGE           "
      ENDIF

      INQUIRE (FILE = COHORTOUT2, EXIST = FEXIST)
      IF (FEXIST) THEN
        I=SYSTEM("DEL COHORTS2.OUT")
        OPEN (UNIT = CHRTOUT2, FILE = COHORTOUT2, STATUS = 'NEW')
        WRITE(CHRTOUT,'(A19)') "*COHORT OUTPUT FILE"
        WRITE(CHRTOUT,'(A28)')
     &    "@YEAR DOY   LFWT            "
      ELSE
        OPEN (UNIT = CHRTOUT2, FILE = COHORTOUT2, STATUS = 'NEW')
        WRITE(CHRTOUT,'(A19)') "*COHORT OUTPUT FILE"
        WRITE(CHRTOUT,'(A28)')
     &    "@YEAR DOY   LFWT            "
      ENDIF

!***********************
! EMERGENCE CALCULATIONS
!***********************
      ELSEIF (DYNAMIC .EQ. EMERG)THEN

!-------------------------------------
! COHORT VARIABLES FOR NEW LEAF TISSUE
!-------------------------------------
      LFDM(1)=WLDOTN
      LFAREA(1)=LFDM(1)*F
      LFSN(1)=PROLFF*0.16*LFDM(1)
      LFNSN(1)=NGRLF-LFSN(1)
      LFNSC(1)=WLDOTN*ALPHL
      LFAGE(1)=DTX

!------------------------------------
! UPDATING TOTAL LEAF STATE VARIABLES
!------------------------------------
!      WTLF=SUM(LFDM(1:199))
!      WNRLF=SUM(LFNSN(1:199))
!      WCRLF=SUM(LFNSC(1:199))
!      XLAI=SUM(LFAREA(1:199))/10000
!      WTNLF=SUM(LFNSN(1:199))+SUM(LFSN(1:199))
      
      WTLF_C =SUM(LFDM(1:199))
      WNRLF_C =SUM(LFNSN(1:199))
      WCRLF_C =SUM(LFNSC(1:199))
      XLAI_C =SUM(LFAREA(1:199))/10000
      WTNLF_C =SUM(LFNSN(1:199))+SUM(LFSN(1:199))

      CUMLFDM=CUMLFDM+LFDM(1)
      PLEAFN=WTNLF_C/WTLF_C*100
  
      CALL YR_DOY(YRDOY, YEAR, DOY) 

      OPEN(UNIT=CHRTOUT,FILE=COHORTOUT,POSITION='APPEND')

      WRITE (CHRTOUT,310) YEAR, DOY, NINT(WTLF_C*10),XLAI_C,PLEAFN

      CLOSE(CHRTOUT)

!********************
! DAILY INTEGRATION
!********************
      ELSEIF (DYNAMIC .EQ. INTEGR)THEN
!---------------------------
! NON-STRUCTURAL CH2O MINING
!---------------------------
      DO  I=1,199
        IF (LFNSC(I).LE.0.OR.CMOBMX.LE.0)THEN
          LFCMN(I)=0
        ELSE
          LFCMN(I)=LFNSC(I)*CMINEA / CMINEP * CMOBMX * (DTX + DXR57)
          LFCMN(I)=MIN(LFCMN(I),LFNSC(I))
          IF ((LFNSC(I)-LFCMN(I)).LE.0.00001)THEN
            LFCMN(I)=LFNSC(I)
          ENDIF
        ENDIF
      END DO

!-------------------------------------------
! INCREASED N MINING FROM SHADING (SHADEFAC)
!-------------------------------------------
  
      IF (PAR .GT. 0.) THEN
        LCMP = -(1. / KCAN) * ALOG(ICMP / PAR)
      ENDIF

      CUMAREA=0.0
      DO I=1,199
        CUMAREA=CUMAREA+LFAREA(I)/10000
        IF (CUMAREA/LCMP.GE.1)THEN
          SHADEFAC(I)=CUMAREA/LCMP
        ELSE
          SHADEFAC(I)=1
        ENDIF
      ENDDO

!--------------------------------
! NON-STRUCTURAL N MINING (LFNMN)
!--------------------------------
      if (NMINEP .GT. 0.0) then
         NMINER=NMOBR*NMINEA/NMINEP
      else
         NMINER = 0.0
      endif
      
      DO  I=1,199
        IF (LFNSN(I).LE.0.OR.MAXNMINE.LE.0.OR.NMOBMX.LE.0)THEN
          LFNMN(I)=0
        ELSE
          LFNMN(I)=SHADEFAC(I)*(NMINER/NMOBMX)*MAXNMINE*LFNSN(I)
          LFNMN(I)=MIN(LFNMN(I),LFNSN(I))
          IF ((LFNSN(I)-LFNMN(I)).LE.0.00001)THEN
            LFNMN(I)=LFNSN(I)
          ENDIF
        ENDIF
      END DO

!------------------------------
! N MINING SENESCENCE (LFNMNSN)
!------------------------------
  
      DO I=1,199
        IF (LFNMN(I).GE.LFNSN(I))THEN
          LFNMNSN(I)=LFDM(I)-(LFNMN(I)/0.16)
        ELSE
          LFNMNSN(I)=0
        ENDIF
      ENDDO

!---------------------------------
! WATER STRESS SENESCENCE (LFWSSN)
!---------------------------------
      IF (VSTAGE.GE.1)THEN
        DO I = NSWAB,2,-1
          IF (SWFCAB(I-1) .GT. 0) THEN
            SWFCAB(I) = SWFCAB(I-1)
          ELSE
            SWFCAB(I)=0
          ENDIF
        ENDDO
        SWFCAB(1) = SWFAC
        RATTP = SWFCAB(NSWAB)
        WSLOSS = SENDAY * (1. - RATTP) * WTLF
        LFWSSN(1:199)=0
            
        IF (WSLOSS .GT. 0.0) THEN
          DO I=1,4
            IF (VSTAGE.GT.XSENMX(I))THEN
                PORLFT = 1.0 - SENMAX(I)
            ENDIF
          ENDDO
          
          WSLOSS = MIN(WSLOSS, WTLF - CUMLFDM * PORLFT)
          WSLOSS = MAX(WSLOSS, 0.0)
          WSLOSS=WSLOSS-SUM(LFNMNSN(1:199))-SUM(LFNMN(1:199))/0.16
          
          DO I=199,1,-1
            IF (LFDM(I).GT.(LFNMNSN(I)+LFNMN(I)/0.16).AND.
     &                                          WSLOSS.GT.0) THEN
              LFWSSN(I)=MIN((LFDM(I)-LFNMNSN(I)-LFNMN(I)/0.16),WSLOSS)
              LFWSSN(I)=MAX(LFWSSN(I),0.0)
              WSLOSS=WSLOSS-LFWSSN(I)
            ENDIF
          ENDDO
        ENDIF
      ELSE
        LFWSSN(1:199)=0
      ENDIF

!----------------------------
! FREEZING SENESCENCE (LFFRZ)
!----------------------------
      IF(TMIN.LT.FREEZ1)THEN
        DO I=1,199
          LFFRZ(I)=LFDM(I)-LFNMN(I)/0.16-LFNMNSN(I)-LFWSSN(I)
          LFFRZ(I)=MAX(LFFRZ(I),0.0)
        ENDDO
      ENDIF

      DO I=1,199
        IF ((LFDM(I)-LFFRZ(I)).LE.0)THEN
          LFNMNSN(I)=0
          LFWSSN(I)=0
        ELSE
          LFNMNSN(I)=LFNMNSN(I)*(LFDM(I)-LFFRZ(I))/LFDM(I)
          LFWSSN(I)=LFWSSN(I)*(LFDM(I)-LFFRZ(I))/LFDM(I)
        ENDIF
      ENDDO

!--------------------
! PEST DAMAGE (LFPST)
!--------------------
      IF(WLIDOT.GT.0)THEN
! FOR PROPORTIONAL DISTRIBUTION OF PEST DAMAGE:
        DO I=1,199
          IF(LFDM(I).GT.0)THEN
            LFPST(I)=LFDM(I)/WTLF*WLIDOT
          ELSE
            LFPST(I)=0
          ENDIF
        ENDDO

! FOR PEST DAMAGE TO AFFECT OLD TISSUE FIRST:
!        LFPSTDM=WLIDOT
!        DO I=199,1,-1
!          IF(LFDM(I).GT.0)THEN
!            LFPST(I)=MIN(LFDM(I),LFPSTDM)
!            LFPST(I)=MAX(LFPST(I),0.0)
!            LFPSTDM=LFPSTDM-LFPST(I)
!          ENDIF
!        ENDDO

! FOR PEST DAMAGE TO AFFECT NEW TISSUE FIRST:
!        LFPSTDM=WLIDOT
!        DO I=1,199
!          IF(LFDM(I).GT.0)THEN
!            LFPST(I)=MIN(LFDM(I),LFPSTDM)
!            LFPST(I)=MAX(LFPST(I),0.0)
!            LFPSTDM=LFPSTDM-LFPST(I)
!          ENDIF
!        ENDDO
      ELSE
        LFPST(1:199)=0
      ENDIF

      DO I=1,199
        IF ((LFDM(I)-LFPST(I)).LE.0)THEN
          LFNMNSN(I)=0
          LFFRZ(I)=0
          LFWSSN(I)=0
        ELSE
          LFNMNSN(I)=LFNMNSN(I)*(LFDM(I)-LFPST(I))/LFDM(I)
          LFFRZ(I)=LFFRZ(I)*(LFDM(I)-LFPST(I))/LFDM(I)
          LFWSSN(I)=LFWSSN(I)*(LFDM(I)-LFPST(I))/LFDM(I)
        ENDIF
      ENDDO

!----------------------------
! NON-STRUCTURAL CH2O STORING
!----------------------------
      IF(CADLF.GT.0)THEN
        DO I=1,199
          IF (LFDM(I).GT.0)THEN
            LFCAD(I)=((LFDM(I)-LFNSC(I))/(WTLF-WCRLF))*CADLF *
     &  (1.-MIN(1.0,(LFPST(I)+LFFRZ(I)+LFWSSN(I)+LFNMNSN(I))/LFDM(I)))
          ELSE
            LFCAD(I)=0
          ENDIF
        ENDDO
      ELSE
        LFCAD(1:199)=0
      ENDIF

!-------------------------
! NON-STRUCTURAL N STORING
!-------------------------
!      IF(NADLF.GT.0)THEN
!       DO I=1,199
!	  IF (LFDM(I).GT.0)THEN
!	    LFNAD(I)=((LFDM(I)-LFNSC(I))/(WTLF-WCRLF))*NADLF *
!     &    (1.-MIN(1.0,(LFPST(I)+LFFRZ(I)+LFWSSN(I)+LFNMNSN(I))/LFDM(I)))
!	  ELSE
!	    LFNAD(I)=0
!	  ENDIF
!	 ENDDO
!	ELSE
      LFNAD(1:199)=0
 !     ENDIF


!------------------
! TOTAL LEAF N LOSS
!------------------      
      NLOFF=0
      DO I=1,199
        IF (LFDM(I).GT.0)THEN
          NLOFF=NLOFF+(LFWSSN(I)+LFPST(I)+LFFRZ(I))*
     &        ((LFNSN(I)+LFSN(I))/LFDM(I))+LFNMNSN(I)*(LFSN(I)/LFDM(I))
        ENDIF
      ENDDO

!-----------------------------
! LEAF N LOSS FROM PEST DAMAGE
!-----------------------------
      NLPEST=0
      DO I=1,199
        IF (LFDM(I).GT.0)THEN
          NLPEST=NLPEST+LFPST(I)*((LFNSN(I)+LFSN(I))/LFDM(I))
        ENDIF
      ENDDO

!---------------------------------------------
! INTEGRATION OF N AND CH2O MINING AND SENESCENCE
!---------------------------------------------

      DO I=1,199
        IF(LFDM(I).GT.0)THEN
!         LEAF AREA
          LFAREA(I)=LFAREA(I)-
     &      (LFPST(I)+LFFRZ(I)+LFWSSN(I)+LFNMNSN(I))*(LFAREA(I)/LFDM(I))
!         STRUCTURAL N
          LFSN(I)=LFSN(I)-
     &       (LFPST(I)+LFFRZ(I)+LFWSSN(I)+LFNMNSN(I))*(LFSN(I)/LFDM(I))
!         NON-STRUCTURAL N
          LFNSN(I)=LFNSN(I)+LFNAD(I)-LFNMN(I)-
     &       (LFPST(I)+LFFRZ(I)+LFWSSN(I)+LFNMNSN(I))*(LFNSN(I)/LFDM(I))
!         NON-STRUCTURAL CH2O
          LFNSC(I)=LFNSC(I)+LFCAD(I)-LFCMN(I)-
     &       (LFPST(I)+LFFRZ(I)+LFWSSN(I)+LFNMNSN(I))*(LFNSC(I)/LFDM(I))
!         DRY MATTER
          LFDM(I)=LFDM(I)+LFCAD(I)+LFNAD(I)/0.16-LFNMN(I)/0.16-LFCMN(I)-
     &            LFPST(I)-LFFRZ(I)-LFNMNSN(I)-LFWSSN(I)
        ENDIF
      ENDDO

!--------------
! SHIFT COHORTS
!--------------
      DO  I=199,2,-1
        IF (LFDM(I-1).GT.0.AND.
     &      LFAREA(I-1).GT.0.AND.
     &      LFSN(I-1).GT.0.AND.
     &      LFNSN(I-1).GT.0) THEN
          LFDM(I)   = LFDM(I-1)
          LFAREA(I) = LFAREA(I-1)
          LFNSN(I)  = LFNSN(I-1)
          LFSN(I)   = LFSN(I-1)
          LFNSC(I)  = LFNSC(I-1)
          LFAGE(I)  = LFAGE(I-1) + DTX
        ELSE
          LFDM(I)=0.
          LFAREA(I)=0.
          LFSN(I)=0.
          LFNSN(I)=0.
          LFNSC(I)=0.
          LFAGE(I)=0.
        ENDIF
      END DO

!-------------------------------------
! COHORT VARIABLES FOR NEW LEAF TISSUE
!-------------------------------------
      LFDM(1)=WLDOTN
      LFAREA(1)=LFDM(1)*F
      LFSN(1)=PROLFF*0.16*LFDM(1)
      LFNSN(1)=NGRLF-LFSN(1)
      LFNSC(1)=WLDOTN*ALPHL
      LFAGE(1)=DTX

!------------------------------------
! UPDATING TOTAL LEAF STATE VARIABLES
!------------------------------------
      
!       WTLF  =SUM(LFDM(1:199))
!       WNRLF =SUM(LFNSN(1:199))
!       WCRLF =SUM(LFNSC(1:199))
!       XLAI  =SUM(LFAREA(1:199))/10000
!       WTNLF =SUM(LFNSN(1:199))+SUM(LFSN(1:199))
      
      WTLF_C =SUM(LFDM(1:199))
      WNRLF_C =SUM(LFNSN(1:199))
      WCRLF_C =SUM(LFNSC(1:199))
      XLAI_C =SUM(LFAREA(1:199))/10000
      WTNLF_C =SUM(LFNSN(1:199))+SUM(LFSN(1:199))
      CUMLFDM=CUMLFDM+LFDM(1)

      PLEAFN_C=WTNLF_C/WTLF_C*100
  
      SUMLFWSSN=SUM(LFWSSN(1:199))
      SUMLFFRZ=SUM(LFFRZ(1:199))
      SUMLFPST=SUM(LFPST(1:199))
      SUMLFNMNSN=SUM(LFNMNSN(1:199))
  
      CALL YR_DOY(YRDOY, YEAR, DOY) 

      OPEN(UNIT=CHRTOUT,FILE=COHORTOUT,POSITION='APPEND')

      WRITE (CHRTOUT,310) YEAR, DOY, WTLF_C,XLAI_C,PLEAFN_C,
     &       WTLF,XLAI,PLEAFN
310   FORMAT (1X,I4,1X,I3,F10.4,F6.3,F6.3,F10.4, 2F6.3)
      
      
C-GH  
      OPEN(UNIT=CHRTOUT1,FILE=COHORTOUT1,POSITION='APPEND')
      write (CHRTOUT1,320) YEAR,DOY,LFAGE(1:50)
320   format (1X,I4,1X,I3,50F6.1)
      OPEN(UNIT=CHRTOUT2,FILE=COHORTOUT2,POSITION='APPEND')
      write (CHRTOUT2,330) YEAR,DOY,LFDM(1:50)
330   format (1X,I4,1X,I3,50F6.1)

      CLOSE(CHRTOUT)
C-GH
      close(CHRTOUT1)
      close(CHRTOUT2)
      
      END IF
      END



!=======================================================================
!  IPCOHO, Subroutine, C.H. Porter
!-----------------------------------------------------------------------
!  Reads input data for COHORTS subroutine
!-----------------------------------------------------------------------
!  REVISION       HISTORY
!  11/18/2025 CHP Adapted from IPDMND.
!=======================================================================
      SUBROUTINE IPCOHO(
     &  FILECC,                             !Input
     &  CARMIN, FINREF, FNSDT, FRLFF, FRLFMX,             !Output
     &  FRSTMF, LIPOPT, LIPTB, LNGSH, NMOBMX,             !Output
     &  NRCVR, NVSMOB, PLIGSD, PMINSD, POASD,             !Output
     &  PROLFF, PROLFI, PRORTF, PRORTI, PROSTF, PROSTI,   !Output
     &  RCH2O, RLIG, RLIP, RMIN, RNO3C, ROA,              !Output
     &  RPRO, SDLIP, SDPRO, SHLAG, SLAMAX, SLAMIN,        !Output
     &  SLAPAR, SLAREF, SLAVAR, SLOSUM, SIZELF, SIZREF,   !Output
     &  SRMAX, THRESH, TURSLA, TYPSDT, VSSINK, XFRMAX,    !Output
     &  XFRUIT, XLEAF, XSLATM, XTRFAC, XVGROW, XXFTEM,    !Output
     &  YLEAF, YSLATM, YSTEM, YTRFAC, YVREF, YXFTEM,      !Output
     &  XFPHT, XFINT, NSLA)                               !Output

!-----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL GETLUN, ERROR, FIND, IGNORE, WARNING
!-----------------------------------------------------------------------
      CHARACTER*3   TYPSDT
      CHARACTER*6   ERRKEY
      PARAMETER (ERRKEY = 'IPDMND')
      CHARACTER*6   SECTION
      CHARACTER*6   ECOTYP, ECONO
      CHARACTER*30  FILEIO
      CHARACTER*78  MSG(4)
      CHARACTER*80  C80
      CHARACTER*92  FILECC, FILEGC
      CHARACTER*255 C255

      INTEGER LUNCRP, LUNIO, LUNECO, ERR, LINC, LNUM, FOUND, ISECT
      INTEGER I, II

      REAL CARMIN, FINREF, FRLFF, FRLFMX, FRSTMF,
     &  LIPOPT, LIPTB, NMOBMX, NRCVR, NVSMOB,
     &  PLIGSD, PMINSD, POASD, PROLFF,
     &  PROLFI, PRORTF, PRORTI, PROSTF, PROSTI,
     &  RCH2O, RLIG, RLIP, RMIN, RNO3C, ROA,
     &  RPRO, SHLAG, SLAMAX, SLAMIN, SLAPAR,
     &  SLAREF, SLAVAR, SLOSUM, SIZELF, SIZREF,
     &  SRMAX, TURSLA, VSSINK, XFRMAX, XFRUIT
        REAL LNGSH, THRESH, SDPRO, SDLIP, XFPHT, XFINT
        REAL NSLA

        REAL FNSDT(4)
        REAL XVGROW(6), YVREF(6)
        REAL XSLATM(10), YSLATM(10), XTRFAC(10), YTRFAC(10),
     &                  XXFTEM(10), YXFTEM(10)
        REAL XLEAF(25), YLEAF(25), YSTEM(25)

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
!       READ(C80,'(F6.0,6X,2F6.0,6X,F6.0)',IOSTAT=ERR)
!    &          PROLFI, PROLFF, PROSTI, PROSTF
        READ(C80,'(12X,F6.0)',IOSTAT=ERR) PROLFF
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
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
!       READ(C80,'(18X,3F6.0)',IOSTAT=ERR) NMOBMX, NVSMOB, NRCVR
        READ(C80,'(18X,2F6.0)',IOSTAT=ERR) NMOBMX, NVSMOB
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
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
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)  
        READ(CHAR,'(12X,F6.0)',IOSTAT=ERR) SENDAY
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(2F6.0)',IOSTAT=ERR) ICMP, TCMP
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(24X,4F6.0)',IOSTAT=ERR)
     &      (XSENMX(II),II=1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(24X,4F6.0)',IOSTAT=ERR)
     &      (SENMAX(II),II=1,4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)  
        READ(CHAR,'(F6.0)',IOSTAT=ERR) ALPHL
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
!                (g[leaf CH2O]/m2[ground])
! LFNSN(I)     Leaf non-structural (mobile) N for cohort I
!                (g[leaf N]/m2[ground])
! LFSC(I)      Leaf structural (non-mobile) CH2O for cohort I
!                (g[leaf CH2O]/m2[ground])
! LFSN(I)      Leaf structural (non-mobile) N for cohort I
!                (g[leaf N]/m2[ground])
! LFAREA(I)    Leaf area for cohort I (cm2[leaf]/m2[ground]
! LFAGE(I)     Leaf age for cohort I (thermal days)
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
! NSWAB        Number of days lag between a water stress event (SWFAC < 1)
!                and senescence due to water stress
! NVSMOB       Relative rate of N mining during vegetative stage to that in 
!                reproductive stage 
! PAR          Daily photosynthetically active radiation or photon flux 
!                density (moles[quanta]/m2-d)
! PLEAFN       Leaf N concentration averaged over all cohorts today (percent)
! PORLFT       Proportion of leaf weight grown which will have been senesced 
!                if no water stress has occurred prior to this V-stage 
! PROLFF       Final protein concentration of leaf tissue
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
! SWFCAB(I)    SWFAC value from I days ago, used to determine senescence due
!                to water stress
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