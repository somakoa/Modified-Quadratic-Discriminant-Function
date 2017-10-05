      SUBROUTINE NSIZEM(IA,I1,J1,IL,IR,JT,JB,IT,ILL)
CS    CALL NSIZEM(IA,I1,J1,IL,IR,JT,JB,IT,ILL)
CP****PATTERN NO SIZE O CHECK
CA I   IA-------INPUT GRAY PATTERN (I1,J1)..............I
CA I   I1-------X-HOKO DIMENSION...................I
CA I   J1-------Y-HOKO DIMENSION...................I
CA O   IL-------SATAN..............................I
CA O   IR-------UTAN...............................I
CA O   JT-------JYOTAN.(TOP).......................I
CA O   JB-------KATAN.(BOTTOM).....................I
CA I   IT-------SIKIICHI...........................I
CE O   ILL------ERROR CODE  0,900,901,902,903
CE              =900,901,902,903 ... ALL 0 PATTERN
CD     CODED BY H.MURASE
CM     1985.6.24  MODIFIED BY M.YOSIMURA
C*****
      INTEGER IA(I1,J1)
C*****
      ILL=0
      DO 10 J=1,J1
      DO 20 I=1,I1
      IF(IA(I,J).GE.IT)GO TO 1000
   20 CONTINUE
   10 CONTINUE
C*****
C*    WRITE(6,600)
  600 FORMAT(1H1,10X,'*** WARNING: ALL 0  ***')
      IL=0
      ILL=900
      RETURN
C*****
 1000 CONTINUE
      JT=J
      DO 30 JJ=1,J1
      J=J1-JJ+1
      DO 40 I=1,I1
      IF(IA(I,J).GE.IT)GO TO 1100
   40 CONTINUE
   30 CONTINUE
      ILL=901
      RETURN
C*****
 1100 CONTINUE
      JB=J
      DO 50 I=1,I1
      DO 60 J=JT,JB
      IF(IA(I,J).GE.IT)GO TO 1200
   60 CONTINUE
   50 CONTINUE
      ILL=902
      RETURN
 1200 CONTINUE
      IL=I
      DO 70 II=1,I1
      I=I1-II+1
      DO 80 J=JT,JB
      IF(IA(I,J).GE.IT)GO TO 1300
   80 CONTINUE
   70 CONTINUE
      ILL=903
      RETURN
 1300 CONTINUE
      IR=I
      RETURN
      END
