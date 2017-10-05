      SUBROUTINE SGCENTB2(IA,I1,J1,iss,jss,ic,jc)
CS    CALL  GCENTB(IA,I1,J1,iss,jss,ic,jc)
CP****MOJI-PATTERN NO JYUUSIN NADO (1,2-JI MOMENT) WO MOTOMERU
CA I   IA-------INPUT BINARY PATTERN (I1,J1)................I
CA I   I1-------SCALE OF  X-DIRECTION......I
CA I   J1-------SCALE OF  Y-DIRECTION......I
c      iss,jss:	declared size of ia        i	; by Kimura
CA O   ic------YOKO HOUKOU NO 1-JI-MOMENT( JYUUSIN )............I
CA O   jc------TATE HOUKOU NO 1-JI-MOMENT( JYUUSIN )............I
CD     CODED BY H.MURASE
CM     1985.7.29  MODIFIED BY M.YOSIMURA
cm     1994.5.2   modified by F.Kimura
C*****
c      INTEGER IA(I1,J1)
      INTEGER IA(iss,jss)
      ic=0.
      jc=0.
      K=0
      DO 115 J=1,J1
         DO 110 I=1,I1
            IF(IA(I,J).eq.0) go to 110 
            ic=ic+i
            jc=jc+j
            K=K+1
  110      CONTINUE
  115    CONTINUE
      IF(K.EQ.0) return 
      ic=ic/float(k)+0.5
      jc=jc/float(k)+0.5
      RETURN
      END
