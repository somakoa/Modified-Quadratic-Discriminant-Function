      REAL FUNCTION MQD6(X,MV,evalp,evaln,EVEC,N,K,ns,n0,ss)
C
CD        CODED BY F.KIMURA -> eval+ eval- by waka
CP        Bayes DISCRIMINANT FUNCTION (2)
C
CA        X : FEATURE VECTOR
CA        MV : MEAN VECTOR
CA        evalp : EIGENVALUES OF COVARIANCE MATRIX for positive area
CA        evaln : EIGENVALUES OF COVARIANCE MATRIX for negative area
CA        EVEC : EIGENVECTORS OF COVARIANCE MATRIX
CA        N : FEATURE SIZE
CA        K : KEISAN NI TSUKAU EIGENVECTOR NO KOSU
c         ns : number of design sample
c         ss : initial variance
c         n0 : confidence constant of ss 
C
      REAL MV(N),evalp(N),evaln(N),EVEC(N,K),X(N)
      REAL SPRO(196)
      sh=ss*n0/ns
      MQD6=0.0
      DET=0.0
      SD=0.0
C-------------------------------------
      DO 51 I=1,K
         SPRO(I) = 0.0
         DO 51 J=1,N
           SPRO(I)=SPRO(I)+X(J)*EVEC(J,I)
   51 CONTINUE
      DO 52 I=K+1,N
         SPRO(I)=0.0
   52 CONTINUE
C
      DO 50 I=1,N
         SD=SD+(X(I)-MV(I))**2
         if (SPRO(I).GE.0) then
            DET=DET+LOG(evalp(I)+sh)
         else
            DET=DET+LOG(evaln(I)+sh)
         endif
   50 CONTINUE
C-------------------------------------
      DO 100 I=1,K
          P=0.0
          DO 200 J=1,N
              P=P+EVEC(J,I)*(X(J)-MV(J))
  200     CONTINUE
          if (SPRO(I).GE.0) then
             MQD6=MQD6+(evalp(i)/(evalp(i)+sh))*P*P
          else
             MQD6=MQD6+(evaln(i)/(evaln(i)+sh))*P*P
          endif
  100     CONTINUE
      MQD6=(ns+n0+n-1)*log(1.0+(SD-MQD6)/(n0*ss))+DET
C     WRITE(6,*) MQD6
      RETURN
      END
C
