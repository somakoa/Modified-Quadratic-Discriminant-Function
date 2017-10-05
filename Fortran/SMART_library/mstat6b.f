      SUBROUTINE MSTAT6B(MV,SW,W,W0,N,L,EPS,ILL)
      REAL MV(N,L),SW(N,N),W(N,L),W0(L)
C
CD        CODED BY F.KIMURA
CD        MODIFIED BY T.WAKA
CP        LD1 NO WEIGHT VECTOR WO MOTOMERU
C
CA        MV : MEAN VECTOR (I)
CA        SW : WITHIN CLASS COVARIANCE MATRIX (I)
CA        W : WEIGHT VECTORS (O)
CA        W0 : CONSTANTS (O)
CA        N: FEATURE SIZE (I)
CA        L : CATEGORY NO KAZU
CA        EPS : SHUUSOKU JYOKEN (I)
CA        ILL : ILLEAGAL PARAMETER (O)
C
C
      CALL MINVS(SW,N,N,EPS,ILL)

C
C     WEIGHT VECTOR NO KEISAN
C
      DO 400 K=1,L
         DO 500 I=1,N
            W(I,K)=0.0
            DO 600 J=1,N
               W(I,K)=W(I,K)+SW(I,J)*MV(J,K)
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
