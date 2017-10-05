      SUBROUTINE FLTM8C(IA,I1,J1,IS,JS,IB)
***
CS    CALL FLTM8C(IA,I1,J1,IS,JS,IB)
***
CP    GAZOU DATA NI 8 KINBOU NI YORU HEIKINCHI FILTER O KAKERU
***
CA    IA --- NYUURYOKU GAZOU (2 JIGEN HAIRETSU)
CA    I1 --- IA NO SEIGOU SUNPOU (DAI 1 SOEJI NO SUNPOU)
CA    J1 --- IA NO SEIGOU SUNPOU (DAI 2 SOEJI NO SUNPOU)
CA    IB --- SYUTSURYOKU GAZOU (2 JIGEN HAIRETSU)
***
CN      FILTER NO CHUUSHIN IDOU HANI
CN          YOKO HOUKOU ----- I = 2 , I1-1
CN          TATE HOUKOU ----- J = 2 , J1-1
CN
CN      SYUTSURYOKU GAZOU HAIRETSU (IB) NO SAIGAIKAKU YOUSO NIHA
CN    HITOTSU UCHIGAWA NO HAIRETSU YOUSO NO NAIYOU GA IRERARERU
***
CD    CODED    BY  S.MIURA   1984.10.01
CD    REVISED  BY  H.KAKENO  1985.06.17
CD    REVISED  BY  T.WAKA    1994.07.27
***
      INTEGER IA(IS,JS),IB(IS,JS)
***
C*** SHOKIKA by waka
C      DO 5 J=1,J1
C        DO 5 I=1,I1
C          IB(I,J)=0
C    5 CONTINUE
CF    FILTER O KAKERU
***
      DO 10 J=2,J1-1
          DO 10 I=2,I1-1
              IB(I,J)=NINT((IA(I-1,J-1)+IA(I,J-1)+IA(I+1,J-1)
     @             +IA(I-1,J)+2*IA(I,J)+IA(I+1,J)
     @             +IA(I-1,J+1)+IA(I,J+1)+IA(I+1,J+1))*0.1)
   10     CONTINUE
***
*C*                       *
*C*    0.111 = 1.0/9.0    *
*C*                       *
***
CF      SYUTSURYOKU GAZOU HAIRETSU (IB) NO SAIGAIKAKU YOUSO NI
CF    HITOTSU UCHIGAWA NO HAIRETSU YOUSO NO NAIYOU O IRERU
***
C$DIR SCALAR
      DO 20 I=1,I1
          IB(I,1)=IB(I,2)
          IB(I,J1)=IB(I,J1-1)
   20 CONTINUE
C$DIR SCALAR
      DO 30 J=1,J1
          IB(1,J)=IB(2,J)
          IB(I1,J)=IB(I1-1,J)
   30 CONTINUE
*C
      RETURN
      END
