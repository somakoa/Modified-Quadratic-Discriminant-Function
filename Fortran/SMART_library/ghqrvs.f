      SUBROUTINE GHQRVS(A,B,KK,N,E,F,EPS,IND)                           00000010
      REAL*8 D                                                          00000020
      DIMENSION A(KK,N),B(KK,N),E(N),F(N)                               00000030
      DATA AMACH/1.E-6/                                                 00000040
      LL=1                                                              00000050
      GO TO 10                                                          00000060
      ENTRY      GHQRUS(A,B,KK,N,E,F,EPS,IND)                           00000070
      LL=2                                                              00000080
   10 IF(N.GT.KK.OR.N.LT.2.OR.EPS.EQ.0.0) GO TO 160                     00000090
      INDEX=IND                                                         00000100
      DEL=AMAX1(ABS(EPS),AMACH)                                         00000110
      IF(EPS.LT.0.0) GO TO 20                                           00000120
      IND=0                                                             00000130
      DET=0.0                                                           00000140
      CALL CHOLFS(B,KK,N,F,N,0,DET,DEL,IND)                             00000150
      IF(IND.NE.0) GO TO 170                                            00000160
   20 DO 50 J=1,N                                                       00000170
      DO 40 I=1,J                                                       00000180
      D=0.D0                                                            00000190
      DO 30 K=1,I-1                                                     00000200
   30 D=A(K,J)*B(K,I)+D                                                 00000210
   40 A(I,J)=(A(I,J)-D)/B(I,I)                                          00000220
   50 CONTINUE                                                          00000230
      DO 90 I=1,N                                                       00000240
      DO 60 J=1,I-1                                                     00000250
   60 F(J)=A(J,I)                                                       00000260
      DO 90 J=I,N                                                       00000270
      D=0.D0                                                            00000280
      DO 70 K=1,J-1                                                     00000290
   70 D=B(K,J)*F(K)+D                                                   00000300
   80 A(I,J)=(A(I,J)-D)/B(J,J)                                          00000310
   90 F(J)=A(I,J)                                                       00000320
      IND=INDEX                                                         00000330
      GO TO (100,110),LL                                                00000340
  100 CALL HOQRVS(A,KK,N,E,F,DEL,IND)                                   00000350
      GO TO 120                                                         00000360
  110 CALL HOQRUS(A,KK,N,E,F,DEL,IND)                                   00000370
  120 IF(INDEX.EQ.0) RETURN                                             00000380
      DO 150 J=1,N                                                      00000390
      A(N,J)=A(N,J)/B(N,N)                                              00000400
      DO 140 I=N-1,1,-1                                                 00000410
      D=0.D0                                                            00000420
      DO 130 K=I+1,N                                                    00000430
  130 D=B(I,K)*A(K,J)+D                                                 00000440
  140 A(I,J)=(A(I,J)-D)/B(I,I)                                          00000450
  150 CONTINUE                                                          00000460
      RETURN                                                            00000470
  160 IND=30000                                                         00000480
      RETURN                                                            00000490
  170 IND=1                                                             00000500
      RETURN                                                            00000510
      END                                                               00000520

      SUBROUTINE CHOLFS(A,KA,N,X,KX,M,D,EPS,IND)                        00000530
      REAL*8 DS                                                         00000540
      DIMENSION A(KA,N),X(KX,M)                                         00000550
      IF(N.GT.KA.OR.N.GT.KX.OR.N.LT.1.OR.M.LT.0.OR.EPS.LE.0.) GO TO 130 00000560
      IF(D.NE.0.0) D=1.0                                                00000570
      IF(IND.NE.0) GO TO 50                                             00000580
      DO 40 K=1,N                                                       00000590
      DS=A(K,K)                                                         00000600
      DO 10 I=1,K-1                                                     00000610
   10 DS=DS-A(I,K)**2                                                   00000620
      IF(DS.LT.EPS) GO TO 120                                           00000630
      D=DS*D                                                            00000640
      A(K,K)=DSQRT(DS)                                                  00000650
      DO 30 J=K+1,N                                                     00000660
      DS=A(K,J)                                                         00000670
      DO 20 I=1,K-1                                                     00000680
   20 DS=DS-A(I,K)*A(I,J)                                               00000690
   30 A(K,J)=DS/A(K,K)                                                  00000700
   40 CONTINUE                                                          00000710
   50 IF(M.EQ.0) GO TO 110                                              00000720
      DO 100 J=1,M                                                      00000730
      DO 70 I=1,N                                                       00000740
      DS=X(I,J)                                                         00000750
      DO 60 K=1,I-1                                                     00000760
   60 DS=DS-A(K,I)*X(K,J)                                               00000770
   70 X(I,J)=DS/A(I,I)                                                  00000780
      DO 90 I=N,1,-1                                                    00000790
      DS=X(I,J)                                                         00000800
      DO 80 K=I+1,N                                                     00000810
   80 DS=DS-A(I,K)*X(K,J)                                               00000820
   90 X(I,J)=DS/A(I,I)                                                  00000830
  100 CONTINUE                                                          00000840
  110 IND=0                                                             00000850
      RETURN                                                            00000860
  120 IND=K                                                             00000870
      A(K,K)=DS                                                         00000880
      RETURN                                                            00000890
  130 IND=30000                                                         00000900
      RETURN                                                            00000910
      END                                                               00000920
