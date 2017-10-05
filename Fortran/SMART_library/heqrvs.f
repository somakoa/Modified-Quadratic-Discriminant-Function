      SUBROUTINE HEQRVS(A,KA,N,E,F,G,H,NV,EPS0,IW,W,ILL)
      REAL*8 D,D1,D2
      COMPLEX*8 Z
      DIMENSION A(KA,N),E(N),F(N),G(KA,1),H(KA,N),IW(2,N),W(2,N),WW(2)
      INTEGER U,UM1,UM2
      DATA AMACH/1.0E-6/
      IF(N.LE.2.OR.N.GT.KA.OR.N.LT.NV.OR.NV.LT.0.OR.EPS0.LE.0.0)
     *GO TO 850
      EPS=AMAX1(EPS0,AMACH)
      RN=0.0
      RA=EPS*0.6180339
      T=0.0
      DO 20 J=1,N
      S=0.0
      DO 10 I=1,N
   10 S=ABS(A(I,J))+S
   20 T=AMAX1(T,S)
      IF(T.EQ.0.0) GO TO 860
      DEL=EPS*T/FLOAT(N)
      EPS1=EPS*SQRT(FLOAT(N))
C     TRANSFORMATION TO HESSENBERG FORM
      NM2=N-2
      DO 120 K=1,NM2
      KP1=K+1
      KP2=K+2
      M=KP1
      DO 30 I=KP2,N
      IF(ABS(A(I,K)).GT.ABS(A(M,K))) M=I
   30 CONTINUE
      IW(1,K)=0
      IF(ABS(A(M,K)).LT.DEL) GO TO 120
      IW(1,K)=M
      T=-A(M,K)
      IF(M.EQ.KP1) GO TO 40
      A(M,K)=A(KP1,K)
      A(KP1,K)=-T
   40 DO 50 I=KP2,N
   50 A(I,K)=A(I,K)/T
      DO 70 J=KP1,N
      T=A(M,J)
      IF(M.EQ.KP1) GO TO 60
      A(M,J)=A(KP1,J)
      A(KP1,J)=T
   60 DO 70 I=KP2,N
   70 A(I,J)=A(I,K)*T+A(I,J)
      IF(M.EQ.KP1) GO TO 90
      DO 80 I=1,N
      T=A(I,M)
      A(I,M)=A(I,KP1)
   80 A(I,KP1)=T
   90 DO 110 I=1,N
      D=0.0
      DO 100 J=KP2,N
  100 D=A(J,K)*A(I,J)+D
  110 A(I,KP1)=A(I,KP1)-D
  120 CONTINUE
C     DOUBLE QR MEHTOD
      IF(NV.EQ.0) GO TO 140
      DO 130 J=1,N
      DO 130 I=1,N
  130 H(I,J)=A(I,J)
  140 DO 150 J=1,NM2
      IF(J+3.LE.N) A(J+3,J)=0.0
  150 A(J+2,J)=0.0
      ITMAX=N*100
      IT=0
      L=1
      U=N
      GO TO 250
  160 IT=IT+1
      IF(IT.GT.ITMAX) GO TO 870
      IF(S.EQ.0.0) S=DEL
      DO 220 K=L,UM2
      KM1=K-1
      KP1=K+1
      KP2=K+2
      IF(K.GT.L) GO TO 170
      T=A(K,K)-S
      G1=A(K,K)*T+A(KP1,K)*A(K,KP1)+P
      G2=(A(KP1,KP1)+T)*A(KP1,K)
      G3=A(KP2,KP1)*A(KP1,K)
      IF(ABS(G1)+ABS(G2).NE.0.0) GO TO 180
      G1=G3
      G2=G3
      GO TO 180
  170 G1=A(K,KM1)
      G2=A(KP1,KM1)
      G3=A(KP2,KM1)
  180 R=SIGN(SQRT(G1*G1+G2*G2+G3*G3),G1)
      T=R+G1
      IF(T.EQ.0.0) GO TO 220
      P1=G2/T
      P2=G3/T
      IF(K.EQ.L) GO TO 190
      A(K,KM1)=-R
      A(KP1,KM1)=0.0
      A(KP2,KM1)=0.0
  190 B=-2./(P1*P1+P2*P2+1.0)
      DO 200 J=K,U
      T=(A(KP1,J)*P1+A(KP2,J)*P2+A(K,J))*B
      A(K,J)=T+A(K,J)
      A(KP1,J)=T*P1+A(KP1,J)
  200 A(KP2,J)=T*P2+A(KP2,J)
      IM=MIN0(K+3,U)
      DO 210 I=L,IM
      T=(A(I,KP1)*P1+A(I,KP2)*P2+A(I,K))*B
      A(I,K)=T+A(I,K)
      A(I,KP1)=T*P1+A(I,KP1)
  210 A(I,KP2)=T*P2+A(I,KP2)
  220 CONTINUE
      G1=A(UM1,UM2)
      G2=A(U,UM2)
      R=SIGN(SQRT(G1*G1+G2*G2),G1)
      IF(R+G1.EQ.0.0) GO TO 250
      P1=G2/(R+G1)
      A(UM1,UM2)=-R
      A(U,UM2)=0.0
      B=-2.0/(P1*P1+1.0)
      DO 230 J=UM1,U
      T=(A(U,J)*P1+A(UM1,J))*B
      A(UM1,J)=T+A(UM1,J)
  230 A(U,J)=T*P1+A(U,J)
      DO 240 I=L,U
      T=(A(I,U)*P1+A(I,UM1))*B
      A(I,UM1)=T+A(I,UM1)
  240 A(I,U)=T*P1+A(I,U)
  250 LP1=L+1
      DO 260 I=LP1,U
      IF(ABS(A(I,I-1)).GT.DEL) GO TO 260
      IW(2,I)=L
      L=I
  260 CONTINUE
  270 IF(U.GT.L) GO TO 280
      W(1,L)=A(L,L)
      W(2,L)=0.0
      GO TO 300
  280 UM1=U-1
      UM2=U-2
      S=A(UM1,UM1)+A(U,U)
      P=A(UM1,UM1)*A(U,U)-A(UM1,U)*A(U,UM1)
      IF(UM1.GT.L) GO TO 160
      DISC=S*S-P*4.0
      IF(DISC.LE.0.0) GO TO 290
      W(1,L)=(SIGN(SQRT(DISC),S)+S)*0.5
      W(1,U)=P/W(1,L)
      W(2,L)=0.0
      W(2,U)=0.0
      GO TO 300
  290 W(1,L)=S*0.5
      W(1,U)=W(1,L)
      W(2,L)=SQRT(-DISC)*0.5
      W(2,U)=-W(2,L)
  300 U=L-1
      L=IW(2,L)
      IF(U.GT.0) GO TO 270
C     SORTING OF EIGENVALUES IN DECREASING OR INCREASING ORDER OF MODULI
      IF(ILL.EQ.0) GO TO 320
      DO 310 I=1,N
  310 E(I)=W(1,I)*W(1,I)+W(2,I)*W(2,I)
      IND=0
      IF(ILL.GT.0) IND=1
      CALL SRTVSS(N,E,W,2,2,F,WW,IND)
  320 DO 330 I=1,N
      E(I)=W(1,I)
  330 F(I)=W(2,I)
      IF(NV.EQ.0) GO TO 840
C     INVERSE ITERATIONS FOR EIGENVECTORS
      DO 340 J=1,N
      DO 340 I=1,N
  340 A(I,J)=H(I,J)
      L=1
  350 IF(F(L).NE.0.0) GO TO 540
C     REAL EIGENVECTORS
      DO 400 J=1,N
      IW(2,J)=0
      RN=RN+RA
      IF(RN.GE.EPS) RN=RN-EPS
      G(J,L)=RN
      JP1=J+1
      IM=MIN0(JP1,N)
      DO 360 I=1,IM
  360 H(I,J)=A(I,J)
      H(J,J)=H(J,J)-E(L)
C     LU DECOMPOSITION
      IF(J.EQ.1) GO TO 380
      DO 370 I=2,J
      IM1=I-1
      IF(IW(2,IM1).EQ.0) GO TO 370
      T=H(I,J)
      H(I,J)=H(IM1,J)
      H(IM1,J)=T
  370 H(I,J)=H(I,IM1)*H(IM1,J)+H(I,J)
      IF(J.EQ.N) GO TO 410
  380 IF(ABS(H(J,J)).GE.ABS(H(JP1,J))) GO TO 390
      IW(2,J)=1
      T=H(JP1,J)
      H(JP1,J)=H(J,J)
      H(J,J)=T
  390 IF(ABS(H(J,J)).LE.DEL) H(J,J)=DEL
  400 H(JP1,J)=-H(JP1,J)/H(J,J)
  410 IF(ABS(H(N,N)).LE.DEL) H(N,N)=DEL
C     INVERSE ITERATION
      DO 470 IT=1,5
      IF(IT.EQ.1) GO TO 430
C     FORWARD SUBSTITUTION
      DO 420 I=2,N
      IM1=I-1
      IF(IW(2,IM1).EQ.0) GO TO 420
      T=G(I,L)
      G(I,L)=G(IM1,L)
      G(IM1,L)=T
  420 G(I,L)=H(I,IM1)*G(IM1,L)+G(I,L)
C     BACKWARD SUBSTITUTION
  430 G(N,L)=G(N,L)/H(N,N)
      VN=ABS(G(N,L))
      I=N-1
  440 IP1=I+1
      D=0.D0
      DO 450 J=IP1,N
  450 D=H(I,J)*G(J,L)+D
      G(I,L)=(G(I,L)-D)/H(I,I)
      VN=AMAX1(ABS(G(I,L)),VN)
      I=I-1
      IF(I.GE.1) GO TO 440
      IF(IT.GT.1.AND.VN.GT.1.0) GO TO 480
      S=EPS1/VN
      DO 460 J=1,N
  460 G(J,L)=G(J,L)*S
  470 CONTINUE
C     TRANSFORMATION OF EIGENVECTOR
  480 I=N
  490 IM1=I-1
      M=IW(1,I-2)
      IF(M.EQ.0) GO TO 510
      T=-G(IM1,L)
      DO 500 J=I,N
  500 G(J,L)=A(J,I-2)*T+G(J,L)
      IF(M.EQ.IM1) GO TO 510
      G(IM1,L)=G(M,L)
      G(M,L)=-T
  510 I=IM1
      IF(I.GE.3) GO TO 490
C     NORMALIZATION OF EIGENVECTOR
      D=0.D0
      DO 520 J=1,N
  520 D=G(J,L)*G(J,L)+D
      S=D
      S=1.0/SQRT(S)
      DO 530 J=1,N
  530 G(J,L)=G(J,L)*S
      L=L+1
      GO TO 780
C     COMPLEX EIGENVECTORS
C     LU-DECOMPOSITION
  540 DO 570 J=1,N
      RN=RN+RA
      IF(RN.GE.EPS) RN=RN-EPS
      G(J,L)=RN
      G(J,L+1)=0.0
      DO 550 I=1,J
  550 H(I,J+1)=A(I,J)
      H(J,J+1)=H(J,J+1)-E(L)
      H(J,J)=-F(L)
      IF(J.EQ.N) GO TO 570
      W(1,J)=A(J+1,J)
      W(2,J)=0.0
      JP1=J+1
      DO 560 I=JP1,N
  560 H(I,J)=0.0
  570 CONTINUE
      DO 620 J=1,N
      IF(J.EQ.1) GO TO 600
      DO 590 I=2,J
      IM1=I-1
      IF(IW(2,IM1).EQ.0) GO TO 580
      T=H(I,J+1)
      H(I,J+1)=H(IM1,J+1)
      H(IM1,J+1)=T
      T=H(J,I)
      H(J,I)=H(J,IM1)
      H(J,IM1)=T
  580 H(I,J+1)=H(IM1,J+1)*W(1,IM1)-H(J,IM1)*W(2,IM1)+H(I,J+1)
  590 H(J,I)=H(IM1,J+1)*W(2,IM1)+H(J,IM1)*W(1,IM1)+H(J,I)
  600 P=H(J,J+1)
      Q=H(J,J)
      Z=CMPLX(P,Q)
      R=W(1,J)
      X=CABS(Z)
      Y=ABS(R)
      IF(J.EQ.N) GO TO 610
      IF(X.GE.Y) GO TO 610
      IF(Y.LT.DEL) R=SIGN(DEL,R)
      IW(2,J)=1
      H(J,J+1)=1.0/R
      H(J,J)=0.0
      W(1,J)=-P/R
      W(2,J)=-Q/R
      GO TO 620
  610 IF(X.LT.DEL) Z=DEL
      Z=1.0/Z
      IW(2,J)=0
      H(J,J+1)=REAL(Z)
      H(J,J)=AIMAG(Z)
      W(1,J)=-H(J,J+1)*R
      W(2,J)=-H(J,J)*R
  620 CONTINUE
C     INVERSE ITERATION
      DO 710 IT=1,5
      IF(IT.EQ.1) GO TO 650
C     FORWARD SUBSTITUTION
      DO 640 I=2,N
      IM1=I-1
      IF(IW(2,IM1).EQ.0) GO TO 630
      T=G(I,L)
      G(I,L)=G(IM1,L)
      G(IM1,L)=T
      T=G(I,L+1)
      G(I,L+1)=G(IM1,L+1)
      G(IM1,L+1)=T
  630 G(I,L)=W(1,IM1)*G(IM1,L)-W(2,IM1)*G(IM1,L+1)+G(I,L)
  640 G(I,L+1)=W(1,IM1)*G(IM1,L+1)+W(2,IM1)*G(IM1,L)+G(I,L+1)
C     BACKWARD SUBSTITUTION
  650 I=N
      VN=0.0
  660 D1=0.D0
      D2=0.D0
      IF(I.EQ.N) GO TO 680
      IP1=I+1
      DO 670 J=IP1,N
      D1=H(I,J+1)*G(J,L)-H(J,I)*G(J,L+1)+D1
  670 D2=H(I,J+1)*G(J,L+1)+H(J,I)*G(J,L)+D2
  680 G1=G(I,L)-D1
      G2=G(I,L+1)-D2
      G(I,L)=H(I,I+1)*G1-H(I,I)*G2
      G(I,L+1)=H(I,I+1)*G2+H(I,I)*G1
  690 VN=AMAX1(ABS(G(I,L))+ABS(G(I,L+1)),VN)
      I=I-1
      IF(I.GE.1) GO TO 660
      IF(IT.GT.1.AND.VN.GT.1.0) GO TO 720
      S=EPS1/VN
      DO 700 I=1,N
      G(I,L)=G(I,L)*S
  700 G(I,L+1)=G(I,L+1)*S
  710 CONTINUE
C     TRANSFORMATION OF EIGENVECTOR
  720 I=N
  730 IM1=I-1
      M=IW(1,I-2)
      IF(M.EQ.0) GO TO 750
      G1=-G(IM1,L)
      G2=-G(IM1,L+1)
      DO 740 J=I,N
      G(J,L)=A(J,I-2)*G1+G(J,L)
  740 G(J,L+1)=A(J,I-2)*G2+G(J,L+1)
      IF(M.EQ.IM1) GO TO 750
      G(IM1,L)=G(M,L)
      G(M,L)=-G1
      G(IM1,L+1)=G(M,L+1)
      G(M,L+1)=-G2
  750 I=IM1
      IF(I.GE.3) GO TO 730
C     NORMALIZATION OF EIGENVECTOR
      D=0.D0
      DO 760 J=1,N
  760 D=G(J,L)*G(J,L)+G(J,L+1)*G(J,L+1)+D
      S=D
      S=1.0/SQRT(S)
      DO 770 J=1,N
      G(J,L)=G(J,L)*S
  770 G(J,L+1)=G(J,L+1)*S
      L=L+2
  780 IF(L.LE.NV) GO TO 350
      DO 830 J=1,NV
      IF(F(J)) 830,790,810
  790 DO 800 I=1,N
  800 H(I,J)=0.0
      GO TO 830
  810 DO 820 I=1,N
      H(I,J)=G(I,J+1)
      H(I,J+1)=-H(I,J)
  820 G(I,J+1)=G(I,J)
  830 CONTINUE
  840 ILL=0
      RETURN
  850 ILL=30000
      RETURN
  860 ILL=1
      RETURN
  870 ILL=2
      RETURN
      END
