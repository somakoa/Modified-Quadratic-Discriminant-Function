      SUBROUTINE MSWAKU(IA,I1,J1,IB,M)
CS    CALL MSWAKU(IA,I1,J1,IB,M)
CP****MOJI DATA NO MAWARI NI '0' WO TUKERU
CA I  IA --- NYUURYOKU GAZOU (2 JIGEN HAIRETU)            I
CA I  I1 --- IA NO SEIGOU SUNPOU (DAI 1 SOEJI NO SUNPOU)  I
CA I  J1 --- IA NO SEIGOU SUNPOU (DAI 2 SOEJI NO SUNPOU)  I
CA O  IB --- SYUTURYOKU GAZOU (2 JIGEN HAIRETU)           I
CA I  M ---- '0' WO TUKERU HABA                           I
CN    IA(I1,J1) NO MAWARI NI JYOUGE SAYUU HABA M NO '0' WO TUKERU
CN    SYUTURYOKU GAZOU NO SUNPOU HA IB(I1+2*M,J1+2*M) TO NARU
CD    1984.6.10    CODED BY TAKESHITA.TETSUO
***
      INTEGER   IA(I1,J1),IB(I1+2*M,J1+2*M)
*     ALL 0
*     CALL MVCL(IB,0,4*(IX+2*M)*(IY+2*M),0,0)
***
CF    IB NI ALL '0' WO DAINYUU SURU
***
      DO 10 J=1,J1+2*M
          DO 10 I=1,I1+2*M
              IB(I,J)=0
   10 CONTINUE
***
CF    IB(I+M,J+M) NI IA(I,J) WO DAINYUU SURU
***
      DO 20 J=1,J1
          DO 20 I=1,I1
              IB(I+M,J+M)=IA(I,J)
   20 CONTINUE
      RETURN
      END