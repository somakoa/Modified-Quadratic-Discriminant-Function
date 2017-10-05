      REAL*8 FUNCTION MQD5D(X,MV,EVAL,EVEC,N,K,ns,n0,ss)
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
      REAL*8 X(N)
      REAL*8 MV(N),EVAL(N),EVEC(N,K),xd(N),xdd,ss
      REAL*8 DET, SD, SH, P
      real*8 aip20d

      sh=ss*n0/ns
      MQD5D=0.0
      DET=0.0
      SD=0.0
      DO 50 I=1,N
         xdd=x(i)-mv(i)
         xd(i)=xdd
         SD=SD+xdd*xdd
         DET=DET+LOG(EVAL(I)+sh)
   50    CONTINUE
      MM=20*(N/20)
      DO 100 I=1,K
          P=0.0
          DO 200 J=1,MM,20
              P=P+aip20d(evec(j,i),xd(j))
  200         CONTINUE
          DO 250 J=MM+1,N
              P=P+evec(j,i)*xd(j)
  250         CONTINUE
          MQD5D=MQD5D+(eval(i)/(eval(i)+sh))*P*P
  100     CONTINUE
      MQD5D=(ns+n0+n-1)*log(1.0+(SD-MQD5D)/(n0*ss))+DET
C     WRITE(6,*) MQD5D
      RETURN
      END
C
C       inner product
C
      real*8 function aip20d(a,x)
      real*8 a(20)
      real*8 x(20)
      aip20d=a(1)*x(1)+a(2)*x(2)+a(3)*x(3)+a(4)*x(4)+a(5)*x(5)
     *      +a(6)*x(6)+a(7)*x(7)+a(8)*x(8)+a(9)*x(9)+a(10)*x(10)
     *      +a(11)*x(11)+a(12)*x(12)+a(13)*x(13)+a(14)*x(14)+a(15)*x(15)
     *      +a(16)*x(16)+a(17)*x(17)+a(18)*x(18)+a(19)*x(19)+a(20)*x(20)
      return
      end
