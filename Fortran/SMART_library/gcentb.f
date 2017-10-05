      SUBROUTINE GCENTB(IA,I1,J1,IGX,IGY,SVX,SVY,ILL)
CS    CALL  GCENTB(IA,I1,J1,IGX,IGY,SVX,SVY,ILL)
CP****MOJI-PATTERN NO JYUUSIN NADO (1,2-JI MOMENT) WO MOTOMERU
CA I   IA-------INPUT BINARY PATTERN (I1,J1)................I
CA I   I1-------SCALE OF  X-DIRECTION......I
CA I   J1-------SCALE OF  Y-DIRECTION......I
CA O   IGX------YOKO HOUKOU NO 1-JI-MOMENT( JYUUSIN )............I
CA O   IGY------TATE HOUKOU NO 1-JI-MOMENT( JYUUSIN )............I
CA O   SVX------YOKO HOUKOU NO 2-JI-MOMENT NO HEIHOUKON .........I
CA O   SVY------TATE HOUKOU NO 2-JI-MOMENT NO HEIHOUKON .........I
CE O   ILL------ERROR CODE  0,900  GCENTB NI MOTOZUKU
CE              =900 ... ALL 0 PATTERN
CD     CODED BY H.MURASE
CM     1985.7.29  MODIFIED BY M.YOSIMURA
C*****
      INTEGER IA(I1,J1)
      GX=0.
      GY=0.
      ILL=0
      K=0
      DO 115 J=1,J1
          DO 110 I=1,I1
              IF(IA(I,J).NE.0) THEN
                  GX=GX+I
                  GY=GY+J
                  K=K+1
              ENDIF
  110     CONTINUE
  115 CONTINUE
      IF(K.EQ.0) GO TO 900
      GX=GX/K
      GY=GY/K
      IGX=GX+0.5
      IGY=GY+0.5
      VX=0.
      VY=0.
      DO 215 J=1,J1
          DO 210 I=1,I1
              IF(IA(I,J).NE.0) THEN
                  VX=(I-GX)*(I-GX)+VX
                  VY=(I-GY)*(I-GY)+VY
              ENDIF
  210     CONTINUE
  215 CONTINUE
      VX=VX/K
      VY=VY/K
      SVX=SQRT(VX)
      SVY=SQRT(VY)
      RETURN
  900 CONTINUE
C*    WRITE(6,60) K
C  60 FORMAT(' ***** ERROR ???? ... MOJI GA NAI !!! K=',I5,5X,
C     1 'AT GCENTB')
      ILL=900
      RETURN
      END
