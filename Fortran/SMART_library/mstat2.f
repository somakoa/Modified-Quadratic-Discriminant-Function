C
      SUBROUTINE MSTAT2(TSP,AMEAN,COVAR,N,M)
C
CD        CODED BY F.KIMURA
CP        MEAN VECTOR, COVARIANCE MATRIX NO KEISAN
C
CA        TSP : TRAINING SAMPLE VECTORS (I)
CA        AMEAN : MEAN VECTOR (O)
CA        COVAR : COVARIANCE MATRIX (O)
CA        N : FEATURE SIZE (I)
CA        M : TRAINING SAMPLE SIZE (I)
C
CS        CALL MSTAT2(TSP,AMEAN,COVAR,N,M)
C
      DIMENSION TSP(N,M),AMEAN(N),COVAR(N,N)
C
C     INITIALIZE
C
      DO 100 I=1,N
         DO 200 J=1,N
            COVAR(J,I)=0.0
  200   CONTINUE
        AMEAN(I)=0.0
  100 CONTINUE
C
C     MEAN, COVARIANCE
C
      DO 300 I=1,M
          DO 400 J=1,N
             DO 500 K=J,N
                 COVAR(K,J)=COVAR(K,J)+TSP(K,I)*TSP(J,I)
  500        CONTINUE
             AMEAN(J)=AMEAN(J)+TSP(J,I)
  400    CONTINUE
  300 CONTINUE
      DO 600 I=1,N
          AMEAN(I)=AMEAN(I)/M
  600 CONTINUE
      DO 700 I=1,N
          DO 800 J=I,N
              CJI=COVAR(J,I)/M-AMEAN(I)*AMEAN(J)
              COVAR(J,I)=CJI
              COVAR(I,J)=CJI
  800     CONTINUE
  700 CONTINUE
      RETURN
      END
