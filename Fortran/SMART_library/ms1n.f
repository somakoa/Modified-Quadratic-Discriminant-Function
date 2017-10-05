C
      REAL FUNCTION MS1N(X,EVEC,N,K)
C
CD        CODED BY F.KIMURA
CD        MODIFIED BY T.WAKA
CP        FUKUGOU RUIJIDO (MULTIPLE SIMILARITY)
C
CA        X : FEATURE VECTOR
CA        EVEC : EIGENVECTOR OF AUTOCORELATION MATRIX
CA        N : FEATURE SIZE
CA        K : SHIKIBETSU NI TSUKAU EIGENVECTOR NO KOSU
C
CS        MS1N(X,EVEC,N,K)
C
      DIMENSION X(N),EVEC(N,K)
      REAL SUM
C
C---- Feature Normalization
      SUM=0.0
      DO 10 I=1,N
         SUM=SUM+X(I)*X(I)
   10 CONTINUE 
      SUM=1.0/SQRT(SUM)
      DO 20 I=1,N
         X(I)=X(I)*SUM
   20 CONTINUE
C
      MS1N=0.0
      DO 100 I=1,K
         S=0.0
         DO 200 J=1,N
            S=S+(EVEC(J,I)*X(J))
  200       CONTINUE
C         MS1N=MS1N+S**2
         MS1N=MS1N+S*S
  100    CONTINUE
      RETURN
      END
