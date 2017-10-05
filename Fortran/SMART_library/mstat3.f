C
      SUBROUTINE MSTAT3(LSP,MV,ACOR,N,M)
C
CD        CODED BY F.KIMURA
CP        MEAN VECTOR, AUTOCORELATION MATRIX NO KEISAN
C
CA        LSP : LEARNING SAMPLE VECTORS (I)
CA        MV : MEAN VECTOR (O)
CA        ACOR : AUTOCORELATION MATRIX (O)
CA        N : FEATURE SIZE (I)
CA        M : LEARNING SAMPLE SIZE (I)
C
CS        CALL MSTAT3(LSP,MV,ACOR,N,M)
C
      REAL LSP(N,M),MV(N),ACOR(N,N)
C
C     INITIALIZE
C
      DO 100 I=1,N
         DO 200 J=1,N
            ACOR(J,I)=0.
  200       CONTINUE
         MV(I)=0.
  100    CONTINUE
C
C     MEAN, COVARIANCE
C
      DO 300 I=1,M
         DO 400 J=1,N
            DO 500 K=J,N
               ACOR(K,J)=ACOR(K,J)+LSP(K,I)*LSP(J,I)
  500          CONTINUE
            MV(J)=MV(J)+LSP(J,I)
  400       CONTINUE
  300    CONTINUE
      DO 600 I=1,N
         MV(I)=MV(I)/M
  600    CONTINUE
      DO 700 I=1,N
         DO 800 J=I,N
            CJI=ACOR(J,I)/M
            ACOR(J,I)=CJI
            ACOR(I,J)=CJI
  800       CONTINUE
  700    CONTINUE
      RETURN
      END
