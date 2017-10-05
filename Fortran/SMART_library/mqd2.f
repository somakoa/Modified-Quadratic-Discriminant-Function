      REAL FUNCTION MQD2(X,MV,EVAL,EVEC,N,K,H)
C
CD        CODED BY F.KIMURA
CP        MODIFIED QUADRATIC DISCRIMINANT FUNCTION (2)
C
CA        X : FEATURE VECTOR
CA        MV : MEAN VECTOR
CA        EVAL : EIGENVALUES OF COVARIANCE MATRIX
CA        EVEC : EIGENVECTORS OF COVARIANCE MATRIX
CA        N : FEATURE SIZE
CA        K : KEISAN NI TSUKAU EIGENVECTOR NO KOSU
CA        H : CONSTANT
C
CS        MQD2(X,MV,EVAL,EVEC,N,K,H)
C
      REAL MV(N),EVAL(N),EVEC(N,K),X(N)
      real xmv
      SH=H
      IF (H.LE.0.0) THEN
          SH=0.0
          DO 10 I=K+1,N
              SH=SH+EVAL(I)
   10     CONTINUE
          SH=SH/(N-K)
      ENDIF
      MQD2=0.0
      DET=0.0
      SD=0.0
      DO 50 I=1,N
          xmv = X(I)-MV(I)
          SD = SD + xmv * xmv
C*****          SD=SD+(X(I)-MV(I))**2
   50 CONTINUE
      DO 100 I=1,K
          P=0.0
          DET=DET+LOG(EVAL(I))
          DO 200 J=1,N
              P=P+EVEC(J,I)*(X(J)-MV(J))
  200     CONTINUE
          MQD2=MQD2+(1.0-SH/EVAL(I))*P*P
  100 CONTINUE
      MQD2=(SD-MQD2)/SH+DET+LOG(SH)*(N-K)
C     WRITE(6,*) MQD2
      RETURN
      END
