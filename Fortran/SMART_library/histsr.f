       SUBROUTINE HISTSR(JBLX,JBLY,JBL,JBL1,HIST,IH,JH,I1,J1,ISTEP)
CS     CALL HISTSS(JBLX,JBLY,INDEX4,JBL,JBL1,JHIST,IH,JH,I1,J1)
CP***   XY-RINKAKUTEN RETSU YORI SUBREGION NI OKERU 4-HOUKOU SISUU
CP***   HISTGRAM O MOTOMERU                     HOUKOU SISUU
CD***   CODED BY S.TSURUOKA                      --------> J
CN***   CALL BDFL2 NO KEKKA(RINKAKU              | 4 3 2
CN***   TENRETSU) O INPUT(JBLX,JBLY)          J  | 1 X 1
C****                                            | 2 3 4
CA I    JBLX(JBL):RINKAKUTENRETSU NO X-ZAHYO                          I
CA I    JBLY(JBL):RINKAKUTENRETSU NO Y-ZAHYO                          I
CA O    INDEX4(JBL):RINKAKUTENRETSU NO 4-HOUKOUSISUU                  I
CA I    JBL:HAIRETSU JBLX,JBLY,INDEX4 NO SEIGOUSUNPOU                 I
CA I    JBL1:JBLX,JBLY,INDEX4 NO YUUKOU HANI                          I
CA O    JHIST(IH,JH,4):BUNKATSU RYOUIKI DENO 4-HOUKOUSISUU HISTGRAM   I
CA I    IH:YOKO HOUKOU NO RYOUIKI SUU                                 I
CA I    JH:TATE HOUKOU NO RYOUIKI SUU                                 I
CA I    (I1,J1):JBLX,JBLY WO MOTOMETA TOKI NO GAZOO NO OOKISA         I
CK      HOUKOUSISUU,HISTGRAM,RINKAKUTENRETSU,BUNKATSU-RYOIKI
C****
      INTEGER JBLX(JBL),JBLY(JBL),ISTEP
      REAL HIST(IH,JH,4),DIRLNG
C***
      COS45=0.7071
      ISR=I1/IH
      JSR=J1/JH
      DO 30 KH=1,4
          DO 20 JJ=1,JH
              DO 10 II=1,IH
                  HIST(II,JJ,KH)=0
   10         CONTINUE
   20     CONTINUE
   30 CONTINUE
      ILAST=1
      I=1+ISTEP
   40     IF (I.GT.JBL1) I=JBL1
          J=ILAST
   50         IF (JBLX(J).EQ.0) THEN
                  I=J-1
              ELSE
                  J=J+1
                  IF (J.LE.I) GOTO 50
              ENDIF
          IF (I.GT.ILAST) THEN
              KX=JBLX(I)-JBLX(ILAST)
              KY=JBLY(I)-JBLY(ILAST)
              IF (KY.LT.0) THEN
                  KX=-KX
                  KY=-KY
              ENDIF
              NSRX=(JBLX(I)-1)/ISR+1
              NSRY=(JBLY(I)-1)/JSR+1
              IF (NSRX.GT.IH) NSRX=IH
              IF (NSRY.GT.JH) NSRY=JH
              HIST(NSRX,NSRY,1)=HIST(NSRX,NSRY,1)+
     -             DIRLNG(KX,KY,1.0,0.0)+DIRLNG(KX,KY,-1.0,0.0)
              HIST(NSRX,NSRY,2)=HIST(NSRX,NSRY,2)+
     -             DIRLNG(KX,KY,-COS45,COS45)*COS45
              HIST(NSRX,NSRY,3)=HIST(NSRX,NSRY,3)+
     -             DIRLNG(KX,KY,0.0,1.0)
              HIST(NSRX,NSRY,4)=HIST(NSRX,NSRY,4)+
     -             DIRLNG(KX,KY,COS45,COS45)*COS45
          ENDIF
          IF (I.LT.JBL1) THEN
              ILAST=I
              IF (JBLX(I+1).EQ.0) ILAST=I+2
              I=ILAST+ISTEP
              GOTO 40
          ENDIF
      RETURN
      END
C
C
C
      REAL FUNCTION DIRLNG(KX,KY,X,Y)
      INTEGER KX,KY
      REAL X,Y,RX,RY,DIST
C
      THR=0.85
      RX=KX
      RY=KY
      DIST=SQRT(RX**2+RY**2)
      DIRLNG=(RX*X+RY*Y-DIST*THR)/(1-THR)
      IF (DIRLNG.LT.0) DIRLNG=0.0
      RETURN
      END
