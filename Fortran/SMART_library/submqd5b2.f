      SUBROUTINE SUBMQD5B2(X,MV,EVAL,EVEC,N,K,DET,ns,n0,ss,MQD5B)
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
      REAL MV(N),EVAL(N),EVEC(N,K),X(N),xd(600),xdd
      REAL MQD5B
      real ainp20

      SD=0.0
      DO 50 I=1,N
         xdd=x(i)-mv(i)
         xd(i)=xdd
         SD=SD+xdd*xdd
   50    CONTINUE
      sh=ss*n0/ns
      MQD5B=0.0
c      DET=0.0
      MM=20*(N/20)
      DO 100 I=1,K
c          DET=DET+LOG(eval(i)+sh)
          P=0.0
          DO 200 J=1,MM,20
	      P=P+AINP20(evec(j,i),xd(j))
  200         CONTINUE
	  DO 250 J=MM+1,N
	      P=P+evec(j,i)*xd(j)
  250	      CONTINUE
          MQD5B=MQD5B+(eval(i)/(eval(i)+sh))*P*P
  100     CONTINUE
      MQD5B=(ns+n0+n-1)*log(1.0+(SD-MQD5B)/(n0*ss))+DET+(n-k)*log(sh)
C     WRITE(6,*) MQD5B
      RETURN
      END
