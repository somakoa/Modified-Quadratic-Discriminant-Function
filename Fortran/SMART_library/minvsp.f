      SUBROUTINE MINVSP(A,KA,N,EPS,ILL)
      DIMENSION A(KA,N)
      REAL*8 S
      IF(N.LT.2.OR.N.GT.KA.OR.EPS.LE.0.) GO TO 30
      P=A(1,1)
      DO 1 K=1,N
      IF(K.EQ.1) GO TO 4
      A(1,K)=A(1,K)/A(1,1)
      DO 2 J=2,K
      S=0.
      JM1=J-1
      DO 3 I=1,JM1
    3 S=A(I,J)*A(I,K)+S
      P=A(J,K)-S
      IF(J.EQ.K) GO TO 4
    2 A(J,K)=P/A(J,J)
    4 IF(P.LT.EPS) GO TO 20
    1 A(K,K)=SQRT(P)
      A(1,1)=1./A(1,1)
      DO 6 J=2,N
      A(J,J)=1./A(J,J)
      P=-A(J,J)
      JM1=J-1
      DO 6 I=1,JM1
      S=0.
      DO 5 K=I,JM1
    5 S=A(I,K)*A(K,J)+S
    6 A(I,J)=S*P
      DO 8 I=1,N
      DO 8 J=I,N
      S=0.
      DO 9 K=J,N
    9 S=A(I,K)*A(J,K)+S
    8 A(I,J)=S
      ILL=0
      RETURN
   20 ILL=K
      RETURN
   30 ILL=30000
      RETURN
      END
