      REAL FUNCTION ED1(X,MV,N)
C
CD        CODED BY F.KIMURA
CP        EUCLID  DISTANCE
C
CA        X : FEATURE VECTOR
CA        MV : MEAN VECTOR
CA        N : FEATURE SIZE
C
CS        ED1(X,MV,N)
C
      DIMENSION X(N),MV(N)
      REAL MV
      REAL TMP
      ED1=0.0
      DO 100 I=1,N
         TMP=X(I)-MV(I)
         ED1=ED1+TMP*TMP
  100    CONTINUE
      RETURN
      END
