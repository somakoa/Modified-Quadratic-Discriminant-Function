      REAL FUNCTION MQD7(X,EVAL,EVEC,N,K,ns,n0,ss)
C
CD        CODED BY F.KIMURA
CP        Bayes DISCRIMINANT FUNCTION (2)
C
CA        X : FEATURE VECTOR
CA        EVAL : EIGENVALUES OF COVARIANCE MATRIX
CA        EVEC : EIGENVECTORS OF COVARIANCE MATRIX
CA        N : FEATURE SIZE
CA        K : KEISAN NI TSUKAU EIGENVECTOR NO KOSU
c         ns : number of design sample
c         ss : initial variance
c         n0 : confidence constant of ss 
C
      REAL EVAL(N),EVEC(N,K),X(N),xdd
      real ainp20

      SD=0.0
      DO 50 I=1,N
         xdd=x(i)
         SD=SD+xdd*xdd
   50    CONTINUE
      sh=ss*n0/ns
      MQD7=0.0
      DET=0.0
      MM=20*(N/20)
      DO 100 I=1,K
          DET=DET+LOG(eval(i)+sh)
          P=0.0
          DO 200 J=1,MM,20
	      P=P+AINP20(evec(j,i),x(j))
  200         CONTINUE
	  DO 250 J=MM+1,N
	      P=P+evec(j,i)*x(j)
  250	      CONTINUE
          MQD7=MQD7+(eval(i)/(eval(i)+sh))*P*P
  100     CONTINUE
      MQD7=(ns+n0+n-1)*log(1.0+(SD-MQD7)/(n0*ss))+DET+(n-k)*log(sh)
C     WRITE(6,*) MQD7
      RETURN
      END
