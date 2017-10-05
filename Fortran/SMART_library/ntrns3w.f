      SUBROUTINE NTRNS3W(IA,I1,J1,IB,I2,J2,IL,IR,JU,JD)
CS    CALL NTRNS3W(IA,I1,J1,IB,I2,J2,IL,IR,JU,JD)
CP****TRANSFORMATION OF PATTERN
CA I   IA-------INPUT BINARY PATTERN       (I1,J1).....I
CA I   I1-------X-HOKO DIMENSION.......................I
CA I   J1-------Y-HOKO DIMENSION.......................I
CA O   IB-------TRANSFORMED BINARY PATTERN (I2,J2).....I
CA I   I2-------X-HOKO DIMENSION.......................I
CA I   J2-------Y-HOKO DIMENSION.......................I
CA I   IL-------SATAN    ..............................I
CA I   IR-------UTAN     ..............................I
CA I   JU-------JYOUTAN  ..............................I
CA I   JD-------KATAN    ..............................I
CD     CODED BY T.WAKABAYASHI
CM     2006.5.30
C*****
      DIMENSION IA(I1,J1),IB(I2,J2)
C*****
C      IW=IR-IL+2
C      JH=JD-JU+2
      IW=IR-IL
      JH=JD-JU
C      MX1=MAX(IW,JH)
      IF(IW.GE.JH) THEN
        MX1=IW
        MX2=I2-1
      ELSE
        MX1=JH
        MX2=J2-1
      ENDIF
      C11=FLOAT(MX1)/FLOAT(MX2)
      C21=C11
      XAC=FLOAT(IW)/2.0+FLOAT(IL)
      YAC=FLOAT(JH)/2.0+FLOAT(JU)
      XBC=FLOAT(I2-1)/2.0+1.0
      YBC=FLOAT(J2-1)/2.0+1.0
      DO 100 I=1,I2
        ID=C11*(FLOAT(I)-XBC)+XAC+0.5
        IF(ID.LT.1) ID=1
        IF(ID.GT.I1)ID=I1
        DO 100 J=1,J2
          JD=C21*(FLOAT(J)-YBC)+YAC+0.5
          IF(JD.LT.1) JD=1
          IF(JD.GT.J1)JD=J1
          IB(I,J)=IA(ID,JD)
  100 CONTINUE
      RETURN
      END
