C
      REAL FUNCTION MD1(X,MV,EVAL,EVEC,N)
C
CD        CODED BY F.KIMURA
CP        MAHALANOBIS DISTANCE
C
CA        X : FEATURE VECTOR
CA        MV : MEAN VECTOR
CA        N : FEATURE SIZE
C
CS        MD1(X,MV,EVAL,EVEC,N)
C
      DIMENSION MV(N),EVAL(N),EVEC(N,N),X(N)
      REAL MV
      MD1=0.0
      DO 100 I=1,N
         P=0.0
         DO 200 J=1,N
            P=P+EVEC(J,I)*(X(J)-MV(J))
  200       CONTINUE
         MD1=MD1+P*P/EVAL(I)
  100    CONTINUE
      RETURN
      END
