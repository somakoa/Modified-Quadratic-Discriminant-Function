      SUBROUTINE MSTAT6(MV,COV,W,W0,WORK,N,L,EPS,ILL)
      REAL MV(N,L),COV(N,N,L),W(N,L),W0(L),WORK(N,N)
C
CD        CODED BY F.KIMURA
CP        LD1 NO WEIGHT VECTOR WO MOTOMERU
C
CA        MV : MEAN VECTOR (I)
CA        COV : COVARIANCE MATRIX (I)
CA        W : WEIGHT VECTORS (O)
CA        W0 : CONSTANTS (O)
CA        WORK : SAGYOU HAIRETSU (WORK)
CA        N: FEATURE SIZE (I)
CA        L : CATEGORY NO KAZU
CA        EPS : SHUUSOKU JYOKEN (I)
CA        ILL : ILLEAGAL PARAMETER (O)
C
C     POOLED WITHIN COVARIANCE NO KEISAN
C
      DO 50 J=1,N
         DO 60 I=1,N
            WORK(I,J)=0.0
   60       CONTINUE
   50    CONTINUE
      DO 100 K=1,L
         DO 200 J=1,N
            DO 300 I=1,N
               WORK(I,J)=WORK(I,J)+COV(I,J,K)/L
  300          CONTINUE
  200       CONTINUE
  100    CONTINUE
C      DO 101 J=1,10
C        DO 101 I=1,10
C          WRITE(6,*) 'J=',J,' I=',I,' SW=',WORK(I,J)
C  101 CONTINUE
      CALL MINVS(WORK,N,N,EPS,ILL)

C
C     WEIGHT VECTOR NO KEISAN
C
      DO 400 K=1,L
         DO 500 I=1,N
            W(I,K)=0.0
            DO 600 J=1,N
               W(I,K)=W(I,K)+WORK(I,J)*MV(J,K)
  600          CONTINUE
  500       CONTINUE
         W0(K)=0.0
         DO 700 J=1,N
            W0(K)=W0(K)+MV(J,K)*W(J,K)
  700       CONTINUE
         W0(K)=-0.5*W0(K)
  400    CONTINUE
      RETURN
      END
