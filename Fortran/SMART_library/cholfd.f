      SUBROUTINE CHOLFD(A,KA,N,X,KX,M,D,EPS,IND)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(KA,N),X(KX,M)
      IF(N.GT.KA.OR.N.GT.KX.OR.N.LT.1.OR.M.LT.0.OR.EPS.LE.0.) GO TO 130
      IF(D.NE.0.D0) D=1.D0
      IF(IND.NE.0) GO TO 50
      DO 40 K=1,N
      DO 10 I=1,K-1
   10 A(K,K)=A(K,K)-A(I,K)**2
      IF(A(K,K).LT.EPS) GO TO 120
      D=A(K,K)*D
      A(K,K)=DSQRT(A(K,K))
      DO 30 J=K+1,N
      DO 20 I=1,K-1
   20 A(K,J)=A(K,J)-A(I,K)*A(I,J)
   30 A(K,J)=A(K,J)/A(K,K)
   40 CONTINUE
   50 IF(M.EQ.0) GO TO 110
      DO 100 J=1,M
      DO 70 I=1,N
      DO 60 K=1,I-1
   60 X(I,J)=X(I,J)-A(K,I)*X(K,J)
   70 X(I,J)=X(I,J)/A(I,I)
      DO 90 I=N,1,-1
      DO 80 K=I+1,N
   80 X(I,J)=X(I,J)-A(I,K)*X(K,J)
   90 X(I,J)=X(I,J)/A(I,I)
  100 CONTINUE
  110 IND=0
      RETURN
  120 IND=K
      RETURN
  130 IND=30000
      RETURN
      END
