      REAL FUNCTION CMQD5B(X,MV1,MV2,EVAL,EVEC,N,K,ns,n0,ss)
C
CD        CODED BY F.KIMURA
CP        Bayes DISCRIMINANT FUNCTION (2)
C
CA        X : FEATURE VECTOR
CA        MV : MEAN VECTOR
CA        EVAL : EIGENVALUES OF COVARIANCE MATRIX
CA        EVEC : EIGENVECTORS OF COVARIANCE MATRIX
CA        N : FEATURE SIZE
CA        K : KEISAN NI TSUKAU EIGENVECTOR NO KOSU
c         ns : number of design sample
c         ss : initial variance
c         n0 : confidence constant of ss 
C
      REAL MV1(N),MV2(N),EVAL(N),EVEC(N,K),X(N)
      REAL XD(1000),MD(1000)
      REAL ALPHA

      ALPHA=FLOAT(n0)/FLOAT(ns+n0)
      sh=ss*n0/ns
      DET=0.0
      DO 100 I=1,K
          DET=DET+LOG(eval(i)+sh)
  100 CONTINUE
      CMQD5B=CPM(X,MV1,MV2,EVAL,EVEC,N,K,ss,ALPHA,xd,md)
      CMQD5B=(ns+n0+n-1)*log(1.0 + CMQD5B/(ns+n0))+DET+(n-k)*log(sh)
C     WRITE(6,*) CMQD5B
      RETURN
      END
