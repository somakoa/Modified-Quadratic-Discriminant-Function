      SUBROUTINE SUBMQD5B(X,MV,EVAL,EVEC,N,K,ns,n0,ss,MQD5B)
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
          MQD5B=MQD5B+(eval(i)/(eval(i)+sh))*P*P
  100     CONTINUE
      MQD5B=(ns+n0+n-1)*log(1.0+(SD-MQD5B)/(n0*ss))+DET+(n-k)*log(sh)
C     WRITE(6,*) MQD5B
      RETURN
      END
C
C       inner product
C
      real function ainp20(a,x)
      real a(20), x(20)
      ainp20=a(1)*x(1)+a(2)*x(2)+a(3)*x(3)+a(4)*x(4)+a(5)*x(5)
     *      +a(6)*x(6)+a(7)*x(7)+a(8)*x(8)+a(9)*x(9)+a(10)*x(10)
     *      +a(11)*x(11)+a(12)*x(12)+a(13)*x(13)+a(14)*x(14)+a(15)*x(15)
     *      +a(16)*x(16)+a(17)*x(17)+a(18)*x(18)+a(19)*x(19)+a(20)*x(20)
      return
      end
