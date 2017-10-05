C
      REAL FUNCTION ssp2(X,MV,EVEC,N,K,xd)
C
CD        CODED BY F.KIMURA
CP        Subspace method (Squared error of KL-expansion) 
CP	   = Projection distance
C
CA        X : FEATURE VECTOR
CA        MV : MEAN VECTOR
CA        EVEC : EIGENVECTORS OF COVARIANCE MATRIX
CA        N : FEATURE SIZE
CA        K : KEISAN NI TSUKAU EIGENVECTOR NO KOSU
c         xd : Working area
C
      REAL MV(N),EVEC(N,K),X(N),xd(n)
c
      SD=0.0
      DO 50 I=1,N
         xdd=x(i)-mv(i)
         xd(i)=xdd
         SD=SD+xdd*xdd
   50    CONTINUE
c
      ssp2=SD
      DO 100 I=1,K
         P=0.0
         DO 200 J=1,N
             P=P+EVEC(J,I)*xd(j)
  200        CONTINUE
         ssp2=ssp2-p*p
  100    CONTINUE
C     WRITE(6,*) ssp2
      RETURN
      END
