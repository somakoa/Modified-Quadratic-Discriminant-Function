      SUBROUTINE SSMOOTH(KP,KPX,KPY,KPXS,KPYS,I1,I2,J1,J2,KPP,IT)
CS    CALL SSMOOTH(KP,KPX,KPY,I1,I2,J1,J2,KPP,IT)
CP*** 4-KINBOU DE 1-PIXEL GA 3 IJYO NARA HEIJIN NOODOCHI *****
CP*** 8-KINBOU SUBETE GA 0 NARA 0 NI SURU       *******
CA I  KP(KPX,KPY):NYURYOKU GAZOU          I
CA I  KPX:KP NO YOKO HOUKOU NO OOKISA     I
CA I  KPY:KP NO TATE HOUKOU NO OOKISA     I
CA I  I1:SYORI HANI (SAGEN)               I
CA I  I2:SYORI HANI (UGEN)                I
CA I  J1:SYORI HANI (JYOUGEN)             I
CA I  J2:SYORI HANI (KAGEN)               I
CA I  IT:SIKIICHI(=1:BINARY)              I
CA O  KPP(KPX,KPY):SITSURYOKU GAZOU       I
CD     1985.06.22   CODED BY S.TSURUOKA
CK    SMOOTH,FILTER,KINBOU
C*****
      INTEGER KP(KPXS,KPYS),KPP(KPXS,KPYS)
C*****
      DO 10 J=1,KPY
        DO 11 I=1,KPX
          KPP(I,J)=0
   11 CONTINUE
   10 CONTINUE
C*****
      DO 50 J=J1,J2
          DO 50 I=I1,I2
              KPP(I,J)=KP(I,J)
              IF(KP(I,J).LT.IT) THEN
C**** 4-RENKETSU NO ANA WO UMERU  ****
                  L=0
                  NC=0
                  IF(KP(I+1,J).GE.IT) THEN
                      NC=NC+1
                      L=L+KP(I+1,J)
                  ENDIF
                  IF(KP(I,J-1).GE.IT) THEN
                      NC=NC+1
                      L=L+KP(I,J-1)
                  ENDIF
                  IF(KP(I-1,J).GE.IT) THEN
                      NC=NC+1
                      L=L+KP(I-1,J)
                  ENDIF
                  IF(KP(I,J+1).GE.IT) THEN
                      NC=NC+1
                      L=L+KP(I,J+1)
                  ENDIF
                  IF(NC.GE.3) THEN
                      KPP(I,J)=L/NC
                  ENDIF
              ELSE
C*** 8-RENKETSU KORITSUTEN JYOKYO  ***
                  IF((KP(I+1,J).LT.IT).AND.(KP(I+1,J-1).LT.IT).AND.
     -                 (KP(I,J-1).LT.IT).AND.(KP(I-1,J-1).LT.IT).AND.
     -                 (KP(I-1,J).LT.IT).AND.(KP(I-1,J+1).LT.IT).AND.
     -                 (KP(I,J+1).LT.IT).AND.(KP(I+1,J+1).LT.IT)) THEN
                      KPP(I,J)=0
                  ENDIF
              ENDIF
   50 CONTINUE
      RETURN
      END
