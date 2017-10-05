      SUBROUTINE FLTM8F(A,I1,J1,IS,JS,B)
***
CS    CALL FLTM8F(A,I1,J1,IS,JS,B)
***
CP    GAZOU DATA NI 8 KINBOU NI YORU HEIKINCHI FILTER O KAKERU
***
CA    A --- NYUURYOKU GAZOU (REAL 2 JIGEN HAIRETSU)
CA    I1 --- A NO SEIGOU SUNPOU (DAI 1 SOEJI NO SUNPOU)
CA    J1 --- A NO SEIGOU SUNPOU (DAI 2 SOEJI NO SUNPOU)
CA    B --- SYUTSURYOKU GAZOU (REAL 2 JIGEN HAIRETSU)
***
CN      FILTER NO CHUUSHIN IDOU HANI
CN          YOKO HOUKOU ----- I = 2 , I1-1
CN          TATE HOUKOU ----- J = 2 , J1-1
CN
CN      SYUTSURYOKU GAZOU HAIRETSU (B) NO SAIGAIKAKU YOUSO NIHA
CN    HITOTSU UCHIGAWA NO HAIRETSU YOUSO NO NAIYOU GA IRERARERU
***
CD    CODED    BY  S.MIURA   1984.10.01
CD    REVISED  BY  H.KAKENO  1985.06.17
CD    REVISED  BY  T.WAKA    1994.07.27
CD    REVISED  BY  T.WAKA    2007.06.18  (REAL IMAGE)
***
      REAL A(IS,JS),B(IS,JS)
***
C*** SHOKIKA by waka
C      DO 5 J=1,J1
C        DO 5 I=1,I1
C          B(I,J)=0
C    5 CONTINUE
CF    FILTER O KAKERU
***
      DO 10 J=2,J1-1
          DO 10 I=2,I1-1
              B(I,J)=(A(I-1,J-1)+A(I,J-1)+A(I+1,J-1)
     @             +A(I-1,J)+2*A(I,J)+A(I+1,J)
     @             +A(I-1,J+1)+A(I,J+1)+A(I+1,J+1))*0.111
   10     CONTINUE
***
*C*                       *
*C*    0.111 = 1.0/9.0    *
*C*                       *
***
CF      SYUTSURYOKU GAZOU HAIRETSU (B) NO SAIGAIKAKU YOUSO NI
CF    HITOTSU UCHIGAWA NO HAIRETSU YOUSO NO NAIYOU O IRERU
***
C$DIR SCALAR
      DO 20 I=1,I1
          B(I,1)=B(I,2)
          B(I,J1)=B(I,J1-1)
   20 CONTINUE
C$DIR SCALAR
      DO 30 J=1,J1
          B(1,J)=B(2,J)
          B(I1,J)=B(I1-1,J)
   30 CONTINUE
*C
      RETURN
      END
