      SUBROUTINE MINVS(A,KA,N,EPS,ILL)
      DIMENSION A(KA,N)
      INTEGER*2 MX(1000)
      REAL*8 S
      IF(N.LT.1.OR.N.GT.1000.OR.N.GT.KA.OR.EPS.LE.0.) GO TO 250
C-----LU DECOMPOSITION--------------------------------------------------
      NM1=N-1
      DO 90 J=1,N
      IF(J.EQ.1) GO TO 30
      JM1=J-1
      DO 20 I=1,JM1
      M=MX(I)
      S=A(M,J)
      A(M,J)=A(I,J)
      IF(I.EQ.1) GO TO 20
      IM1=I-1
      DO 10 K=1,IM1
   10 S=A(I,K)*A(K,J)+S
   20 A(I,J)=S
   30 AM=0.
      DO 60 I=J,N
      S=A(I,J)
      IF(J.EQ.1) GO TO 50
      DO 40 K=1,JM1
   40 S=A(I,K)*A(K,J)+S
      A(I,J)=S
   50 AA=ABS(A(I,J))
      IF(AA.LE.AM) GO TO 60
      AM=AA
      M=I
   60 CONTINUE
      IF(AM.LT.EPS) GO TO 240
      MX(J)=M
      IF(M.EQ.J) GO TO 80
      DO 70 K=1,J
      W=A(M,K)
      A(M,K)=A(J,K)
   70 A(J,K)=W
   80 IF(J.EQ.N) GO TO 100
      JP1=J+1
      W=-A(J,J)
      DO 90 I=JP1,N
   90 A(I,J)=A(I,J)/W
  100 IF(N.LE.2) GO TO 130
C-----INPLACE INVERSION OF L-COMPONENT----------------------------------
      DO 120 I=3,N
      IM1=I-1
      IM2=I-2
      DO 120 J=1,IM2
      S=A(I,J)
      JP1=J+1
      DO 110 K=JP1,IM1
  110 S=A(I,K)*A(K,J)+S
  120 A(I,J)=S
C-----INPLACE INVERSION OF U-COMPONENT----------------------------------
  130 A(1,1)=1./A(1,1)
      IF(N.EQ.1) GO TO 230
      DO 150 J=2,N
      A(J,J)=1./A(J,J)
      P=-A(J,J)
      JM1=J-1
      DO 150 I=1,JM1
      S=0.
      DO 140 K=I,JM1
  140 S=A(I,K)*A(K,J)+S
  150 A(I,J)=S*P
C-----INPLACE MULTIPLICATION OF L AND U COMPONENT-----------------------
      DO 190 J=1,NM1
      JP1=J+1
      DO 170 I=1,J
      S=A(I,J)
      DO 160 K=JP1,N
  160 S=A(I,K)*A(K,J)+S
  170 A(I,J)=S
      DO 190 I=JP1,N
      S=0.
      DO 180 K=I,N
  180 S=A(I,K)*A(K,J)+S
  190 A(I,J)=S
C------INTERCHANGE OF COLUMNS-------------------------------------------
      J=NM1
  200 M=MX(J)
      IF(M.EQ.J) GO TO 220
      DO 210 I=1,N
      W=A(I,M)
      A(I,M)=A(I,J)
  210 A(I,J)=W
  220 J=J-1
      IF(J.GE.1) GO TO 200
  230 ILL=0
      RETURN
  240 ILL=J
      RETURN
  250 ILL=30000
      RETURN
      END
