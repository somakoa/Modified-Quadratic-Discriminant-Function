      SUBROUTINE GCENTM(IA,I1,J1,IGX,IGY,SVX,SVY,IT,ILL)
CS    CALL  GCENTM(IA,I1,J1,IGX,IGY,SVX,SVY,IT,ILL)
CP****MOJI-PATTERN NO JYUUSIN NADO (1,2-JI MOMENT) WO MOTOMERU
CA I   IA-------INPUT GRAY PATTERN (I1,J1).......................I
CA I   I1-------SCALE OF  X-DIRECTION............................I
CA I   J1-------SCALE OF  Y-DIRECTION............................I
CA O   IGX------YOKO HOUKOU NO 1-JI-MOMENT( JYUUSIN )............I
CA O   IGY------TATE HOUKOU NO 1-JI-MOMENT( JYUUSIN )............I
CA O   SVX------YOKO HOUKOU NO 2-JI-MOMENT NO HEIHOUKON .........R
CA O   SVY------TATE HOUKOU NO 2-JI-MOMENT NO HEIHOUKON .........R
CA I   IT-------SIKIICHI  .......................................I
CE O   ILL------ERROR CODE  0,900  GCENTM NI MOTOZUKU............I
CE              =900 .... ALL 0 PATTERN
CD     CODED BY H.MURASE
CM     1985.7.29  MODIFIED BY M.YOSIMURA
C*****
      INTEGER IA(I1,J1)
      ILL=0
      GX=0.
      GY=0.
      K=0
      DO 110 J=1,J1
      DO 110 I=1,I1
      IF(IA(I,J).LT.IT) GO TO 110
      MM=IA(I,J)
      GX=GX+I*MM
      GY=GY+J*MM
      K=K+MM
  110 CONTINUE
      IF(K.EQ.0) GO TO 900
      GX=GX/K
      GY=GY/K
      IGX=GX+0.5
      IGY=GY+0.5
      VX=0.
      VY=0.
      DO 120 J=1,J1
      DO 120 I=1,I1
      IF(IA(I,J).LT.IT) GO TO 120
      MM=IA(I,J)
      VX=MM*(I-GX)*(I-GX)+VX
      VY=MM*(I-GY)*(I-GY)+VY
  120 CONTINUE
      VX=VX/K
      VY=VY/K
      SVX=SQRT(VX)
      SVY=SQRT(VY)
      RETURN
  900 CONTINUE
C*    WRITE(6,60) K
   60 FORMAT(' ***** ERROR ???? ... MOJI GA NAI !!! K=',I5,5X,
     1 'AT GCENTM')
      ILL=900
      RETURN
      END
