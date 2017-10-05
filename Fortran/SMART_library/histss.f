       SUBROUTINE HISTSS(JBLX,JBLY,INDEX4,JBL,JBL1,JHIST,IH,JH,I1,J1)
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
      INTEGER JBLX(JBL),JBLY(JBL),INDEX4(JBL)
      INTEGER JHIST(IH,JH,4)
C***
      INDEX4(1)=0
      LS=2
***  XY-RINKAKUTEN RETSU YORI 4-HOUKOU SISUU WO INDEX4 NI MOTOMERU  ***
   10 CONTINUE
      DO 100 I=LS,JBL1
          IF(JBLX(I).EQ.0) GOTO 101
          KX=JBLX(I)-JBLX(I-1)
          KY=JBLY(I)-JBLY(I-1)
          IF(KX.EQ.1.AND.KY.EQ.0) THEN
              INDEX4(I)=1
          ELSE IF(KX.EQ.1.AND.KY.EQ.-1) THEN
              INDEX4(I)=2
          ELSE IF(KX.EQ.0.AND.KY.EQ.-1) THEN
              INDEX4(I)=3
          ELSE IF(KX.EQ.-1.AND.KY.EQ.-1) THEN
              INDEX4(I)=4
          ELSE IF(KX.EQ.-1.AND.KY.EQ.0) THEN
              INDEX4(I)=1
          ELSE IF(KX.EQ.-1.AND.KY.EQ.1) THEN
              INDEX4(I)=2
          ELSE IF(KX.EQ.0.AND.KY.EQ.1) THEN
              INDEX4(I)=3
          ELSE IF(KX.EQ.1.AND.KY.EQ.1) THEN
              INDEX4(I)=4
          ELSE
              INDEX4(I)=0
          ENDIF
  100 CONTINUE
      GOTO 200
*****  RINKAKU TSUISEKI KAISITEN NO SYORI  *****
  101 LS=I+2
       INDEX4(I)=0
       INDEX4(I+1)=0
       GOTO 10
***** 4-HOUKOU NO HISTGRAM O MOTOMERU   *****
  200 CONTINUE
*****  SIZE OF SUBREGION  *****
      ISR=I1/IH
      JSR=J1/JH
*****  JHIST NO SYOKIKA  *****
      DO 300 KH=1,4
          DO 300 JJ=1,JH
              DO 300 II=1,IH
                  JHIST(II,JJ,KH)=0
  300 CONTINUE
***** BUNKATSU RYOUIKI DENO HISTGRAM NO SAKUSEISURU *****
      DO 400 I=1,JBL1
          KH=INDEX4(I)
          IF(KH.NE.0) THEN
              IX=JBLX(I)
              IY=JBLY(I)
              NSRX=(IX-1)/ISR+1
              NSRY=(IY-1)/JSR+1
              IF(NSRX.GT.IH)  NSRX=IH
              IF(NSRY.GT.JH)  NSRY=JH
              JHIST(NSRX,NSRY,KH)=JHIST(NSRX,NSRY,KH)+1
          ENDIF
  400 CONTINUE
      RETURN
      END
