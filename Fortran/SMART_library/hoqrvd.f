      SUBROUTINE HOQRVD(A,KA,N,E,F,EPS,ILL)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(KA,N),E(N),F(N)
      DATA DMACH/2.D-16/
      LLL=0
      GO TO 10
      ENTRY HOQRUD(A,KA,N,E,F,EPS,ILL)
      LLL=1
   10 IF(N.LT.2.OR.N.GT.KA.OR.EPS.LE.0.D0) GO TO 280
      LL=ILL
      DO 80 K=1,N-2
      E(K)=A(K,K)
      S=0.D0
      DO 20 J=K+1,N
      E(J)=A(K,J)
   20 S=E(J)*E(J)+S
      S=DSIGN(DSQRT(S),E(K+1))
      F(K)=-S
      E(K+1)=E(K+1)+S
      A(K,K+1)=E(K+1)
      H=E(K+1)*S
      IF(H.LE.0.D0) GO TO 80
      R=0.D0
      DO 60 I=K+1,N
      S=0.D0
      DO 30 J=K+1,I
   30 S=A(J,I)*E(J)+S
      DO 40 J=I+1,N
   40 S=A(I,J)*E(J)+S
   50 F(I)=S/H
   60 R=E(I)*F(I)+R
      W=R*0.5D0/H
      DO 70 J=K+1,N
      F(J)=E(J)*W-F(J)
      DO 70 I=K+1,J
   70 A(I,J)=E(J)*F(I)+E(I)*F(J)+A(I,J)
   80 A(K,K)=H
      E(N)=A(N,N)
      A(N,N)=1.D0
      E(N-1)=A(N-1,N-1)
      F(N-1)=A(N-1,N)
      F(N)=0.D0
      A(N-1,N-1)=1.D0
      A(N-1,N)=0.D0
      A(N,N-1)=0.D0
      IF(LL.EQ.0) GO TO 130
      DO 120 K=N-2,1,-1
      H=-A(K,K)
      A(K,K)=1.D0
      IF(H.GE.0.D0) GO TO 110
      DO 100 J=K+1,N
      S=0.D0
      DO 90 I=K+1,N
   90 S=A(I,J)*A(K,I)+S
      S=S/H
      DO 100 I=K+1,N
  100 A(I,J)=A(K,I)*S+A(I,J)
  110 DO 120 I=K+1,N
      A(K,I)=0.D0
  120 A(I,K)=0.D0
  130 GN=DABS(E(1))
      DO 140 J=N,2,-1
      F(J)=F(J-1)
  140 GN=DMAX1(DABS(F(J))+DABS(E(J)),GN)
      IF(GN.EQ.0.D0) GO TO 270
      DEL=GN*DMAX1(EPS,DMACH)
      DO 200 K=N,2,-1
  150 DO 160 L=K,2,-1
      IF(DABS(F(L)).LT.DEL) GO TO 170
  160 CONTINUE
      L=1
  170 IF(L.EQ.K) GO TO 200
      W=(E(K-1)+E(K))*0.5D0
      R=E(K)-W
      Z=W-DSIGN(DSQRT(F(K)*F(K)+R*R),W)
      E(L)=E(L)-Z
      C=1.D0
      S=0.D0
      DO 190 J=L,K-1
      R=DSQRT(E(J)*E(J)+F(J+1)*F(J+1))
      F(J)=S*R
      EE=E(J)*C
      FF=F(J+1)*C
      C=E(J)/R
      S=F(J+1)/R
      W=E(J+1)-Z
      E(J)=(FF*C+W*S)*S+EE+Z
      E(J+1)=C*W-S*FF
      IF(LL.EQ.0) GO TO 190
      DO 180 I=1,N
      R=A(I,J+1)
      A(I,J+1)=R*C-A(I,J)*S
  180 A(I,J)=A(I,J)*C+R*S
  190 CONTINUE
      F(K)=E(K)*S
      E(K)=E(K)*C+Z
      GO TO 150
  200 CONTINUE
      DO 210 I=1,N
      F(I)=E(I)
  210 IF(LLL.EQ.1) F(I)=DABS(F(I))
      J=N
  220 L=1
      II=1
      JJ=1
      DO 240 I=2,J
      IF(F(I).GT.F(L)) GO TO 230
      L=I
      GO TO 240
  230 JJ=L
      II=I
  240 CONTINUE
      IF(II.EQ.JJ) GO TO 260
      W=E(JJ)
      E(JJ)=E(II)
      E(II)=W
      F(JJ)=F(II)
      IF(LL.EQ.0) GO TO 260
      DO 250 I=1,N
      W=A(I,JJ)
      A(I,JJ)=A(I,II)
  250 A(I,II)=W
  260 J=II-1
      IF(J.GE.2) GO TO 220
  270 ILL=0
      RETURN
  280 ILL=30000
      RETURN
      END
