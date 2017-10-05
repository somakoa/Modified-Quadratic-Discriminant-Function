      REAL FUNCTION CMQD5M(X,MV1,MV2,EVAL,EVEC,N,K,ns,n0,ss,mu)
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
      REAL XD(1000),MD(1000),XDD,MU
      REAL ALPHA,TMP
      real ainp20

      ALPHA=FLOAT(n0)/FLOAT(ns+n0)
      SD=0.0
      DO 50 I=1,N
         xdd=x(i)-MV1(i)
         xd(i)=xdd
         SD=SD+xdd*xdd
   50    CONTINUE
      sh=ss*n0/ns
      CMQD5M=0.0
      DET=0.0
      MM=20*(N/20)
      DO 100 I=1,K
          DET=DET+LOG(eval(i)+sh)
          P=0.0
          DO 200 J=1,MM,20
	      P=P+AINP20(evec(j,i),xd(j))
  200         CONTINUE
	  DO 250 J=MM+1,N
	      P=P+evec(j,i)*xd(j)
  250	      CONTINUE
          CMQD5M=CMQD5M+(eval(i)/(eval(i)+sh))*P*P
  100     CONTINUE
      TMP=MU*CPM(X,MV1,MV2,EVAL,EVEC,N,K,ss,ALPHA,xd,md)/(ns+n0)
      CMQD5M=(ns+n0+n-1)*log(1.0+(SD-CMQD5M)/(n0*ss)+TMP)
     *         +DET+(n-k)*log(sh)
C     WRITE(6,*) CMQD5M
      RETURN
      END
