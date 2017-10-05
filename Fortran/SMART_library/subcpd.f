C
      SUBROUTINE SUBCPD(X,MV1,MV2,EVEC,N,K,xd,md,CPD)
C
CD        CODED BY T.Waka
CP        Compound Projection Distance
C
CA        X : FEATURE VECTOR
CA        MV1 : MEAN VECTOR OF OWN CLASS
CA        MV2 : MEAN VECTOR OF OPPOSITE CLASS
CA        EVEC : EIGENVECTORS OF COVARIANCE MATRIX (OWN CLASS)
CA        N : FEATURE SIZE
CA        K : KEISAN NI TSUKAU EIGENVECTOR NO KOSU
c         xd : Working area
c         md : Working area
C
      REAL MV1(N),MV2(N),EVEC(N,K),X(N),xd(n),md(n)
      REAL SPRO
      REAL CPD
c
      DO 50 I=1,N
         XD(i)=X(i)-MV1(i)
         MD(i)=MV2(i)-MV1(i)
   50 CONTINUE
c
      BUNSI=SPRO(MD,XD,N)
      BUNBO=SPRO(MD,MD,N)
      DO 100 I=1,K
         P=SPRO(MD,EVEC(1,I),N)
         BUNSI= BUNSI - P * SPRO(XD,EVEC(1,I),N)
         BUNBO= BUNBO - P*P
  100 CONTINUE
      CPD = BUNSI*BUNSI / BUNBO
C     WRITE(6,*) CPD
      RETURN
      END

      REAL FUNCTION SPRO(X1,X2,N)
      REAL X1(N),X2(N)
      SPRO=0
      DO 100 I=1,N
         SPRO = SPRO +  X1(I)*X2(I)
  100 CONTINUE
      RETURN
      END
      
