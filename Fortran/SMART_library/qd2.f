      REAL FUNCTION QD2(X,MV,EVAL,EVEC,N,xd)
C
CD        CODED BY F.KIMURA
CP        Quadratic DISCRIMINANT FUNCTION (2)
C
CA        X : FEATURE VECTOR
CA        MV : MEAN VECTOR
CA        EVAL : EIGENVALUES OF COVARIANCE MATRIX
CA        EVEC : EIGENVECTORS OF COVARIANCE MATRIX
CA        N : FEATURE SIZE
c         xd : Working area
C
      REAL MV(N),EVAL(N),EVEC(N,N),X(N),xd(n)
      DO 50 I=1,N
         xd(i)=x(i)-mv(i)
   50    CONTINUE
c
      QD2=0.0
      det=0
      DO 100 I=1,N
         det=det+log(eval(i))
         P=0.0
         DO 200 J=1,N
             P=P+EVEC(J,I)*xd(j)
  200        CONTINUE
         QD2=QD2+P*P/eval(i)
  100    CONTINUE
      QD2=QD2+DET
C     WRITE(6,*) QD2
      RETURN
      END
