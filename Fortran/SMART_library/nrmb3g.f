      SUBROUTINE NRMB3G(IA,I1,J1,IB,I2,J2,ILL)
CS    CALL NRMB3G(IA,I1,J1,IB,I2,J2,ILL)
C
CP** JYUUSIN 4-BUNKATSU SEIKIKA (JYUUSIN WO FRAME NO TYUSIN TO SURU) **
CP**  4-RYOUIKI DE SINSYUKU-HI WO KAETE ICHI OOKISA NO SEIKIKA WO SURU
C
CA I   IA(I1,J1): INPUT FIGURE              I
CA I   I1:IA NO DAIICHI SEIGOU SUNPOU       I
CA I   J1:IA NO DAINI SEIGOU SUNPOU         I
CA O   IB(I2,J2) : OUTPUT FIGURE            I
CA I   I2:IB NO DAIICHI SEIGOU SUNPOU       I
CA I   J2:IB NO DAINI SEIGOU SUNPOU         I
CA O   ILL:ERROR CODE (0:NORMAL END)        I
CD          CODED BY T.WAKABAYASHI         1992.12.22
CK    ICHI-OOKISA NO SEIKIKA
C **********************************************************************
C
      INTEGER IA(I1,J1),IB(I2,J2)
C**  JYUNBI **
C***   (IXC,IYC):WAKU NO TYUSIN       ***
      IXC=(I2+1)/2
      IYC=(J2+1)/2
C***    MOJI NO GAISETSU WAKU WO MOTOMERU         ***
      CALL NSIZEG(IA,I1,J1,IL,IR,JT,JB,ILL)
      IF(ILL.NE.0) RETURN
CT    WRITE(6,*)' IL,IR,JT,JB : ',IL,IR,JT,JB
C***    MOJI NO GYUUSIN WO MOTOMERU               ***
      CALL GCENTB(IA,I1,J1,IGX,IGY,SVX,SVY,ILL)
      IF(ILL.NE.0) RETURN
CT     WRITE(6,*)' IGX,IGY,SVX,SVY : ',IGX,IGY,SVX,SVY
C***
      HI2=FLOAT(I2)/2.
      I22=HI2
      HJ2=FLOAT(J2)/2.
      J22=HJ2
C
C***  1ST SUBIMAGE  (RIGHT-UPPER)  ***
C
      A11=FLOAT(IR-IGX+1)/HI2
      A21=FLOAT(IGY-JT+1)/HJ2
      A12=IGX-A11*FLOAT(IXC)
      A22=IGY-A21*FLOAT(IYC)
CT    WRITE(6,*)'1ST SUBIMAGE ',A11,A21,A12,A22
C***  IMAGE REPRODUCTION  **
      DO 100 I=I22,I2
          RD=A11*FLOAT(I)+A12
          ID=RD
          ALPHA=RD-FLOAT(ID)
          DO 100 J=1,J22
              SD=A21*FLOAT(J)+A22
              JD=SD
              BETA=SD-JD
              IA11=IA(ID,JD)
              IA12=IA(ID+1,JD)
              IA21=IA(ID,JD+1)
              IA22=IA(ID+1,JD+1)
              IF(ID.LT.1) THEN
                  IA11=0
                  IA21=0
              ENDIF
              IF(ID.GT.I1) THEN
                  IA12=0
                  IA22=0
              ENDIF
              IF(JD.LT.1) THEN
                  IA11=0
                  IA12=0
              ENDIF
              IF(JD.GT.J1) THEN
                  IA21=0
                  IA22=0
              ENDIF
              B = FLOAT(IA11)*(1.0-ALPHA)*(1.0-BETA)
     *           +FLOAT(IA12)*ALPHA*(1.0-BETA)
     *           +FLOAT(IA21)*(1.0-ALPHA)*BETA
     *           +FLOAT(IA22)*ALPHA*BETA
              IB(I,J) = B * 255.0
C              IB(I,J) = B
  100 CONTINUE
C
C*** 2ND SUBIMAGE (LEFT-UPPER)  ***
C
      A11=FLOAT(IGX-IL+1)/HI2
      A21=FLOAT(IGY-JT+1)/HJ2
      A12=IGX-A11*FLOAT(IXC)
      A22=IGY-A21*FLOAT(IYC)
CT    WRITE(6,*) '2ND SUBIMAGE ',A11,A21,A12,A22
      DO 200 I=1,I22
          RD=A11*FLOAT(I)+A12
          ID=RD
          ALPHA=RD-FLOAT(ID)
          DO 200 J=1,J22
              SD=A21*FLOAT(J)+A22
              JD=SD
              BETA=SD-JD
              IA11=IA(ID,JD)
              IA12=IA(ID+1,JD)
              IA21=IA(ID,JD+1)
              IA22=IA(ID+1,JD+1)
              IF(ID.LT.1) THEN
                  IA11=0
                  IA21=0
              ENDIF
              IF(ID.GT.I1) THEN
                  IA12=0
                  IA22=0
              ENDIF
              IF(JD.LT.1) THEN
                  IA11=0
                  IA12=0
              ENDIF
              IF(JD.GT.J1) THEN
                  IA21=0
                  IA22=0
              ENDIF
              B = FLOAT(IA11)*(1.0-ALPHA)*(1.0-BETA)
     C           +FLOAT(IA12)*ALPHA*(1.0-BETA)
     C           +FLOAT(IA21)*(1.0-ALPHA)*BETA
     C           +FLOAT(IA22)*ALPHA*BETA
              IB(I,J) = B * 255.0
C              IB(I,J) = B
  200 CONTINUE
C
C***  3RD SUBIMAGE  (LEFT-LOWER)  ***
C
      A11=FLOAT(IGX-IL+1)/HI2
CT    WRITE(6,*)'JB,IGY,HJ2',JB,IGY,HJ2
      A21=FLOAT(JB-IGY+1)/HJ2
      A12=IGX-A11*FLOAT(IXC)
      A22=IGY-A21*FLOAT(IYC)
CT    WRITE(6,*)'3RD SUBIMAGE ',A11,A21,A12,A22
      DO 300 I=1,I22
          RD=A11*FLOAT(I)+A12
          ID=RD
          ALPHA=RD-FLOAT(ID)
          DO 300 J=J22,J2
              SD=A21*FLOAT(J)+A22
              JD=SD
              BETA=SD-JD
              IA11=IA(ID,JD)
              IA12=IA(ID+1,JD)
              IA21=IA(ID,JD+1)
              IA22=IA(ID+1,JD+1)
              IF(ID.LT.1) THEN
                  IA11=0
                  IA21=0
              ENDIF
              IF(ID.GT.I1) THEN
                  IA12=0
                  IA22=0
              ENDIF
              IF(JD.LT.1) THEN
                  IA11=0
                  IA12=0
              ENDIF
              IF(JD.GT.J1) THEN
                  IA21=0
                  IA22=0
              ENDIF
              B = FLOAT(IA11)*(1.0-ALPHA)*(1.0-BETA)
     C           +FLOAT(IA12)*ALPHA*(1.0-BETA)
     C           +FLOAT(IA21)*(1.0-ALPHA)*BETA
     C           +FLOAT(IA22)*ALPHA*BETA
              IB(I,J) = B * 255.0
C              IB(I,J) = B
  300 CONTINUE
C
C***  4TH SUBIMAGE (RIGHT-LOWER)   ***
C
      A11=FLOAT(IR-IGX+1)/HI2
      A21=FLOAT(JB-IGY+1)/HJ2
      A12=IGX-A11*FLOAT(IXC)
      A22=IGY-A21*FLOAT(IYC)
CT    WRITE(6,*) '4TH SUBIMAGE ',A11,A21,A12,A22
      DO 400 I=I22,I2
          RD=A11*FLOAT(I)+A12
          ID=RD
          ALPHA=RD-FLOAT(ID)
          DO 400 J=J22,J2
              SD=A21*FLOAT(J)+A22
              JD=SD
              BETA=SD-JD
              IA11=IA(ID,JD)
              IA12=IA(ID+1,JD)
              IA21=IA(ID,JD+1)
              IA22=IA(ID+1,JD+1)
              IF(ID.LT.1) THEN
                  IA11=0
                  IA21=0
              ENDIF
              IF(ID.GT.I1) THEN
                  IA12=0
                  IA22=0
              ENDIF
              IF(JD.LT.1) THEN
                  IA11=0
                  IA12=0
              ENDIF
              IF(JD.GT.J1) THEN
                  IA21=0
                  IA22=0
              ENDIF
              B = FLOAT(IA11)*(1.0-ALPHA)*(1.0-BETA)
     C           +FLOAT(IA12)*ALPHA*(1.0-BETA)
     C           +FLOAT(IA21)*(1.0-ALPHA)*BETA
     C           +FLOAT(IA22)*ALPHA*BETA
              IB(I,J) = B * 255.0
C              IB(I,J) = B
  400 CONTINUE
C******************************************
      RETURN
      END
