C
      SUBROUTINE submprd(X,MV,EVAL,EVEC,N,K,xd,cval,mprd)
C
CD        CODED BY F.KIMURA
CP        Subspace method (Squared error of KL-expansion) 
CP	   = Projection distance
C
CA        X : FEATURE VECTOR
CA        MV : MEAN VECTOR
CA        EVAL : EIGENVALUES OF COVARIANCE MATRIX
CA        EVEC : EIGENVECTORS OF COVARIANCE MATRIX
CA        N : FEATURE SIZE
CA        K : KEISAN NI TSUKAU EIGENVECTOR NO KOSU
c         xd : Working area
c	  cval(i) : (1-alpha)*eval(i)/((1-alpha)*eval(i)+alpha*ss)
C
      REAL MV(N),EVAL(N),EVEC(N,K),X(N),xd(n),cval(N),mprd
c
      SD=0.0
      DO 50 I=1,N
         xdd=x(i)-mv(i)
         xd(i)=xdd
         SD=SD+xdd*xdd
   50    CONTINUE
c
      mprd=SD
      DO 100 I=1,K
         P=0.0
         DO 200 J=1,N
             P=P+EVEC(J,I)*xd(j)
  200        CONTINUE
         mprd=mprd-(cval(I)*p*p)
  100    CONTINUE
C     WRITE(6,*) mprd
      RETURN
      END
