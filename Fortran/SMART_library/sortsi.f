C ND50345.SORTSI.FORT
C A40287A.YADA.FORT
C     ***************************************************************
C     *                                                             *
C     *  SUBROUTINE SORTSS(N,AK,A,IND)     CODED BY I.NINOMIYA     *
C     *  SUBROUTINE SORTSI(N,AK,A,IND)     CODED BY I.NINOMIYA     *
C     *                                                             *
C     ***************************************************************
C     ---------------------------------------------------------------
      SUBROUTINE SORTSS(N,AK,A,IND)
C     ---------------------------------------------------------------
C     ---------------------------------------------------------------
C     DECLARATIONS
C     ---------------------------------------------------------------
      INTEGER R,RR
      DIMENSION A(N),AK(N),RR(30),LL(30)
C     ---------------------------------------------------------------
C     ENTRY POINT FOR SORTSI
C     ---------------------------------------------------------------
      ENTRY      SORTSI(N,AK,A,IND)
C     ---------------------------------------------------------------
C     PARAMETER ERROR CHECK
C     ---------------------------------------------------------------
      IF(N.GE.1) THEN
          IF(N.NE.1) THEN
C     ---------------------------------------------------------------
C     INITIALIZATION
C     ---------------------------------------------------------------
              IF(IND.NE.0) THEN
                  DO 10 I=1,N
                      AK(I)=-AK(I)
   10             CONTINUE
              ENDIF
              ISP=0
              L=1
              R=N
C     ---------------------------------------------------------------
C     ---------------------------------------------------------------
C     IF THE LENGTH OF THE CURRENT SUB-ARRAY IS LESS THAN 16,
C     SORT IT BY THE STRAIGHT INSERTION SORT. OTHERWISE PICK UP
C     THE LEFTMOST, MIDDLE AND RIGHTMOST ELEMENTS AND PUT THE
C     LARGEST IN THE RIGHT,SMALLEST IN THE MIDDLE AND THE REST
C     IN THE LEFT.
C     ---------------------------------------------------------------
   30         IF(R-L.LT.16) GO TO 120
              M=(R+L)/2
              MAX=R
              IF(AK(M).GT.AK(R)) MAX=M
              IF(AK(L).GT.AK(MAX)) MAX=L
              IF(MAX.NE.R) THEN
                  B=A(MAX)
                  BK=AK(MAX)
                  A(MAX)=A(R)
                  AK(MAX)=AK(R)
                  A(R)=B
                  AK(R)=BK
              ENDIF
              IF(AK(L).LT.AK(M)) THEN
                  B=A(L)
                  BK=AK(L)
                  A(L)=A(M)
                  AK(L)=AK(M)
                  A(M)=B
                  AK(M)=BK
              ENDIF
C     ---------------------------------------------------------------
C     PUT THE LEFTMOST ELEMENT TO THE CORRECT POSITION,TRANSFERRING
C     SMALLER ELEMENTS TO THE LEFT AND LARGER ELEMENTS TO THE RIGHT.
C     ---------------------------------------------------------------
              B=A(L)
              BK=AK(L)
              I=L
              J=R
              GO TO 80
   60         A(J)=A(I)
              AK(J)=AK(I)
   70         J=J-1
   80         IF(BK.LT.AK(J)) GO TO 70
              IF(J.LE.I) GO TO 100
              A(I)=A(J)
              AK(I)=AK(J)
   90         I=I+1
              IF(AK(I).LT.BK) GO TO 90
              IF(J.GT.I) GO TO 60
C     ---------------------------------------------------------------
C     STORE THE END POSITIONS OF LARGER HALF SUB-ARRAY INTO STACK,
C     AND ATTACK THE SMALLER HALF.
C     ---------------------------------------------------------------
              I=J
  100         A(I)=B
              AK(I)=BK
              ISP=ISP+1
              IF(R-I.LT.I-L) THEN
                  LL(ISP)=L
                  RR(ISP)=I-1
                  L=I+1
                  GO TO 30
              ENDIF
              LL(ISP)=I+1
              RR(ISP)=R
              R=I-1
              GO TO 30
C     ---------------------------------------------------------------
C     STRAIGHT INSERTION SORT.
C     ---------------------------------------------------------------
  120         IF(R-L.GE.1) THEN
                  J=R
  130             B=A(J-1)
                  BK=AK(J-1)
                  I=J
  140             IF(AK(I).LT.BK) THEN
                      A(I-1)=A(I)
                      AK(I-1)=AK(I)
                      I=I+1
                      IF(I.LE.R) GO TO 140
                  ENDIF
                  A(I-1)=B
                  AK(I-1)=BK
                  J=J-1
                  IF(J.GT.L) GO TO 130
              ENDIF
C     ---------------------------------------------------------------
C     POP UP THE LATEST INFORMATIOINS FROM THE STACK.
C     ---------------------------------------------------------------
              IF(ISP.EQ.0) GO TO 170
              L=LL(ISP)
              R=RR(ISP)
              ISP=ISP-1
              GO TO 30
C     ---------------------------------------------------------------
C     NORMAL EXIT.
C     ---------------------------------------------------------------
  170         IF(IND.EQ.0) RETURN
              DO 180 I=1,N
                  AK(I)=-AK(I)
  180         CONTINUE
              IND=0
              RETURN
C     ---------------------------------------------------------------
C     ERROR EXITS.
C     ---------------------------------------------------------------
          ENDIF
          IND=0
          RETURN
      ENDIF
      IND=30000
      RETURN
      END
