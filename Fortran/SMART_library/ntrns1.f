      SUBROUTINE NTRNS1(IA,I1,J1,IB,I2,J2,C11,IL,C21,JU)
CS    CALL NTRNS1(IA,I1,J1,IB,I2,J2,C11,IL,C21,JU)
CP****TRANSFORMATION OF PATTERN
CA I   IA-------INPUT BINARY PATTERN       (I1,J1).....I
CA I   I1-------X-HOKO DIMENSION.......................I
CA I   J1-------Y-HOKO DIMENSION.......................I
CA O   IB-------TRANSFORMED BINARY PATTERN (I2,J2).....I
CA I   I2-------X-HOKO DIMENSION.......................I
CA I   J2-------Y-HOKO DIMENSION.......................I
CA I   C11------CO-EFFICIENT OF TRANSFORMATION.........R
CA I   IL-------SATAN    ..............................I
CA I   C12------CO-EFFICIENT OF TRANSFORMATION.........R
CA I   JU-------JYOUTAN  ..............................I
CD     CODED BY H.MURASE
CM     1985.6.24  MODIFIED BY M.YOSIMURA
C*****
      DIMENSION IA(I1,J1),IB(I2,J2)
C*****
      DO 100 I=1,I2
      ID=C11*FLOAT(I-1)+IL+0.5
      IF(ID.LT.1) ID=1
      IF(ID.GT.I1)ID=I1
      DO 100 J=1,J2
      JD=C21*FLOAT(J-1)+JU+0.5
      IF(JD.LT.1) JD=1
      IF(JD.GT.J1)JD=J1
      IB(I,J)=IA(ID,JD)
  100 CONTINUE
      RETURN
      END
