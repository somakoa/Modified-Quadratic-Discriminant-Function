      REAL FUNCTION BAYES1(X,MV,EVAL,EVEC,N,K,ns,n0,ss,DET,LG)
C
CD        CODED BY F.KIMURA & T.WAKABAYASHI
CP        Bayes DISCRIMINANT FUNCTION
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
c         LG : log gamma 
C
      REAL MV(N),EVAL(N),EVEC(N,K),X(N),xd(N),xdd,DET,LG
      real ainp20

      SD=0.0
      DO 50 I=1,N
         xdd=x(i)-mv(i)
         xd(i)=xdd
         SD=SD+xdd*xdd
   50    CONTINUE
      sh=ss*n0/ns
      BAYES1=0.0
      MM=20*(N/20)
      DO 100 I=1,K
          P=0.0
          DO 200 J=1,MM,20
	      P=P+AINP20(evec(j,i),xd(j))
  200         CONTINUE
	  DO 250 J=MM+1,N
	      P=P+evec(j,i)*xd(j)
  250	      CONTINUE
          BAYES1=BAYES1+(eval(i)/(eval(i)+sh))*P*P
  100     CONTINUE
C      BAYES1=(ns+n0)*log(1.0+(SD-BAYES1)/(n0*ss))+DET+LG
      BAYES1=(ns+n0+1)*log(1.0+(SD-BAYES1)/(n0*ss))+DET+LG
C     WRITE(6,*) BAYES1
      RETURN
      END
