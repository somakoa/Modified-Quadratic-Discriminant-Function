      SUBROUTINE GHQRVD(A,B,KK,N,E,F,EPS,IND)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(KK,N),B(KK,N),E(N),F(N)
      DATA DMACH/2.D-16/
      LL=1
      GO TO 10
      ENTRY      GHQRUD(A,B,KK,N,E,F,EPS,IND)
      LL=2
   10 IF(N.GT.KK.OR.N.LT.2.OR.EPS.EQ.0.D0) GO TO 160
      INDEX=IND
      DEL=DMAX1(DABS(EPS),DMACH)
      IF(EPS.LT.0.D0) GO TO 20
      IND=0
      DET=0.D0
      CALL CHOLFD(B,KK,N,F,N,0,DET,DEL,IND)
      IF(IND.NE.0) GO TO 170
   20 DO 50 J=1,N
      DO 40 I=1,J
      S=0.D0
      DO 30 K=1,I-1
   30 S=A(K,J)*B(K,I)+S
   40 A(I,J)=(A(I,J)-S)/B(I,I)
   50 CONTINUE
      DO 90 I=1,N
      DO 60 J=1,I-1
   60 F(J)=A(J,I)
      DO 90 J=I,N
      S=0.D0
      DO 70 K=1,J-1
   70 S=B(K,J)*F(K)+S
   80 A(I,J)=(A(I,J)-S)/B(J,J)
   90 F(J)=A(I,J)
      IND=INDEX
      GO TO (100,110),LL
  100 CALL HOQRVD(A,KK,N,E,F,DEL,IND)
      GO TO 120
  110 CALL HOQRUD(A,KK,N,E,F,DEL,IND)
  120 IF(INDEX.EQ.0) RETURN
      DO 150 J=1,N
      A(N,J)=A(N,J)/B(N,N)
      DO 140 I=N-1,1,-1
      S=0.D0
      DO 130 K=I+1,N
  130 S=B(I,K)*A(K,J)+S
  140 A(I,J)=(A(I,J)-S)/B(I,I)
  150 CONTINUE
      RETURN
  160 IND=30000
      RETURN
  170 IND=1
      RETURN
      END
