      SUBROUTINE WEIGT1(IA,I1,J1,B,I2,J2,W,K1,K2,IP,JP,IS,JS,ILL)
***
CS    CALL WEIGT1(IA,I1,J1,B,I2,J2,W,K1,K2,IP,JP,IS,JS,ILL)
***
CP    DAI 2 HYOUHONKA O OKONAU
***
CA    IA  ---  NYUURYOKU GAZOU (2 JIGEN HAIRETSU)
CA    I1  ---  IA NO SEIGOU SUNPOU (DAI 1 SOEJI NO SUNPOU)
CA    J1  ---  IA NO SEIGOU SUNPOU (DAI 2 SOEJI NO SUNPOU)
CA    B   ---  SYUTSURYOKU GAZOU (2 JIGEN HAIRETSU)
CA    I2  ---  B NO SEIGOU SUNPOU (DAI 1 SOEJI NO SUNPOU)
CA    J2  ---  B NO SEIGOU SUNPOU (DAI 2 SOEJI NO SUNPOU)
CA    W   ---  WEIGHTING FILTER (2 JIGEN HAIRETSU)
CA    K1  ---  W NO SEIGOU SUNPOU (DAI 1 SOEJI NO SUNPOU)
CA    K2  ---  W NO SEIGOU SUNPOU (DAI 2 SOEJI NO SUNPOU)
CA    IP  ---  YOKO HOUKOU STARTING POINT
CA    JP  ---  TATE HOUKOU STARTING POINT
CA    IS  ---  YOKO HOUKOU SAMPLING INTERVAL
CA    JS  ---  TATE HOUKOU SAMPLING INTERVAL
CA    ILL ---  ILLEGAL PARAMETER
***
CE    ILL=    0  ---  SEIJYOU SYUURYOU
CE    ILL=30000  ---  HIKISUU I2 J2 NO ATAI GA FUTEKITOU
***
CN      FILTER W(K1,K2) GA NYUURYOKU GAZOU HAIRETSU IA(I1,J1)
CN    O HAMIDASHITA RYOUIKI NI TSUITEHA KEISAN O SHINAI
***
CD   CODED    BY  Y.YAMAKAWA  1984.10.01
CD   REVISED  BY  H.KAKENO    1985.06.17
***
      INTEGER IA(I1,J1),IWIS,IWIE,IWJS,IWJE,
     @        ICHCKI,ICHCKJ,IAJS,IAIS,ILL
      REAL B(I2,J2),W(K1,K2)
*C
      ILL=0
***
CF    HIKISUU I2 J2 NO CHECK
***
      I3=(I1-IP)/IS+1
      J3=(J1-JP)/JS+1
      IF((I2.NE.I3).OR.(J2.NE.J3)) THEN
          ILL=30000
          RETURN
*C
      ELSE
***
CF    FILTER O KAKERU
***
          KK1=(K1-1)/2
          KK2=(K2-1)/2
*C
          DO 10 J=1,J2
              DO 10 I=1,I2
***
CF     FILTER NO HAMIDASHI CHECK
***
                  II=IP+(I-1)*IS
                  JJ=JP+(J-1)*JS
                  ILE=II-KK1
                  IRI=II+KK1
                  JUP=JJ-KK2
                  JUN=JJ+KK2
***
CF     HIDARI HASHI NO CHECK
***
                  IF(ILE.GE.1)THEN
                      IWIS=1
                      IAIS=ILE
                  ELSE
                      IWIS=-ILE+2
                      IAIS=1
                  END IF
***
CF     MIGI HASHI NO CHECK
***
                  ICHCKI=I1-IRI
                  IF(ICHCKI.GE.0)THEN
                      IWIE=K1
                  ELSE
                      IWIE=K1+ICHCKI
                  END IF
***
CF     JYOUTAN NO CHECK
***
                  IF(JUP.GE.1)THEN
                      IWJS=1
                      IAJS=JUP
                  ELSE
                      IWJS=-JUP+2
                      IAJS=1
                  END IF
***
CF     KATAN NO CHECK
***
                  ICHCKJ=J1-JUN
                  IF(ICHCKJ.GE.0)THEN
                      IWJE=K2
                  ELSE
                      IWJE=K2+ICHCKJ
                  END IF
***
CF     FILTER O KAKERU
***
                  WSUM=0.0
C$DIR SCALAR
                  DO 20 K=IWIS,IWIE
C$DIR SCALAR
                      DO 20 L=IWJS,IWJE
                          KW=K-IWIS
                          LW=L-IWJS
                          WSUM=WSUM+W(K,L)*IA(IAIS+KW,IAJS+LW)
   20             CONTINUE
                  B(I,J)=WSUM
   10     CONTINUE
*C
      END IF
      RETURN
      END
