C
      SUBROUTINE SUBCPM(X,MV1,MV2,EVAL,EVEC,N,K,SIG2,ALPHA,xd,md,CPM)
C
CD        CODED BY T.Waka
CP        Compound Pseudo Mahalanobis
C
CA        X : FEATURE VECTOR
CA        MV1 : MEAN VECTOR OF OWN CLASS
CA        MV2 : MEAN VECTOR OF OPPOSITE CLASS
CA        EVAL : EIGENVALUES OF COVARIANCE MATRIX (OWN CLASS)
CA        EVEC : EIGENVECTORS OF COVARIANCE MATRIX (OWN CLASS)
CA        N : FEATURE SIZE
CA        K : KEISAN NI TSUKAU EIGENVECTOR NO KOSU
CA        SIG2 : sigma^2
CA        ALPHA :
c         xd : Working area
c         md : Working area
C
      REAL MV1(N),MV2(N),EVAL(N),EVEC(N,K),X(N),xd(n),md(n)
      REAL SIG2,ALPHA
      REAL SPRO2,EV,P,AL
      REAL CPM
c
      AL=ALPHA*SIG2
      DO 50 I=1,N
         XD(i)=X(i)-MV1(i)
         MD(i)=MV2(i)-MV1(i)
   50 CONTINUE
c
      BUNSI=SPRO2(MD,XD,N)
      BUNBO=SPRO2(MD,MD,N)
      DO 100 I=1,K
         EV=EVAL(I)/(EVAL(I)+AL/(1.0-ALPHA))
         P=SPRO2(MD,EVEC(1,I),N)
         BUNSI= BUNSI - EV * P * SPRO2(XD,EVEC(1,I),N)
         BUNBO= BUNBO - EV * P * P
  100 CONTINUE
      CPM = BUNSI*BUNSI / BUNBO / AL
C     WRITE(6,*) CPM
      RETURN
      END

      REAL FUNCTION SPRO2(X1,X2,N)
      REAL X1(N),X2(N)
      SPRO2=0
      DO 100 I=1,N
         SPRO2 = SPRO2 +  X1(I)*X2(I)
  100 CONTINUE
      RETURN
      END
      
