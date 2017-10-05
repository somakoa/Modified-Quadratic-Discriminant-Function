      SUBROUTINE HOQRVS(A,KA,N,E,F,EPS,ILL)
      REAL*8 D,DD
      LOGICAL LL,LLL
      DIMENSION A(KA,N),E(N),F(N)
      DATA AMACH/1.E-6/

      LLL=.FALSE.
      GO TO 10
      ENTRY HOQRUS(A,KA,N,E,F,EPS,ILL)
      LLL=.TRUE.
   10 IF(N.GE.2.AND.N.LE.KA.AND.EPS.GT.0.0) THEN
          LL=ILL.EQ.0
          DO 80 K=1,N-2
              E(K)=A(K,K)
              D=0.D0
              DO 20 J=K+1,N
                  E(J)=A(K,J)
                  D=E(J)*E(J)+D
   20         CONTINUE
              S=D
              S=SIGN(SQRT(S),E(K+1))
              F(K)=-S
              E(K+1)=E(K+1)+S
              A(K,K+1)=E(K+1)
              H=E(K+1)*S
              IF(H.GT.0.0) THEN
              DD=0.D0
              DO 60 I=K+1,N
                  D=0.D0
                  DO 30 J=K+1,I
                      D=A(J,I)*E(J)+D
   30             CONTINUE
                  DO 40 J=I+1,N
                      D=A(I,J)*E(J)+D
   40             CONTINUE
                  F(I)=D/H
                  DD=E(I)*F(I)+DD
   60         CONTINUE
              W=DD*0.5D0/H
              DO 75 J=K+1,N
                  F(J)=E(J)*W-F(J)
                  DO 70 I=K+1,J
                      A(I,J)=E(J)*F(I)+E(I)*F(J)+A(I,J)
   70             CONTINUE
   75         CONTINUE
              A(K,K)=H
              ENDIF
   80     CONTINUE
          E(N)=A(N,N)
          A(N,N)=1.0
          E(N-1)=A(N-1,N-1)
          F(N-1)=A(N-1,N)
          F(N)=0.0
          A(N-1,N-1)=1.0
          A(N-1,N)=0.0
          A(N,N-1)=0.0
          IF(.NOT.LL) THEN
          DO 125 K=N-2,1,-1
              H=-A(K,K)
              A(K,K)=1.0
              IF(H.LT.0.0) THEN
              DO 105 J=K+1,N
                  S=0.0
                  DO 90 I=K+1,N
                      S=A(I,J)*A(K,I)+S
   90             CONTINUE
                  S=S/H
                  DO 100 I=K+1,N
                      A(I,J)=A(K,I)*S+A(I,J)
  100             CONTINUE
  105         CONTINUE
              ENDIF
              DO 120 I=K+1,N
                  A(K,I)=0.0
                  A(I,K)=0.0
  120         CONTINUE
  125     CONTINUE
          ENDIF
          GN=ABS(E(1))
          DO 140 J=N,2,-1
              F(J)=F(J-1)
              GN=AMAX1(ABS(F(J))+ABS(E(J)),GN)
  140     CONTINUE
          IF(GN.NE.0.0) THEN
              DEL=GN*AMAX1(EPS,AMACH)
              DO 200 K=N,2,-1
  150             DO 160 L=K,2,-1
                      IF(ABS(F(L)).LT.DEL) GO TO 170
  160             CONTINUE
                  L=1
  170             IF(L.NE.K) THEN
                      W=(E(K-1)+E(K))*0.5D0
                      R=E(K)-W
                      Z=W-SIGN(SQRT(F(K)*F(K)+R*R),W)
                      E(L)=E(L)-Z
                      C=1.0
                      S=0.0
                      DO 190 J=L,K-1
                          R=SQRT(E(J)*E(J)+F(J+1)*F(J+1))
                          F(J)=S*R
                          EE=E(J)*C
                          FF=F(J+1)*C
                          C=E(J)/R
                          S=F(J+1)/R
                          W=E(J+1)-Z
                          E(J)=(FF*C+W*S)*S+EE+Z
                          E(J+1)=C*W-S*FF
                          IF(.NOT.LL) THEN
                              DO 180 I=1,N
                                  R=A(I,J+1)
                                  A(I,J+1)=R*C-A(I,J)*S
                                  A(I,J)=A(I,J)*C+R*S
  180                         CONTINUE
                          ENDIF
  190                 CONTINUE
                      F(K)=E(K)*S
                      E(K)=E(K)*C+Z
                      GO TO 150
                  ENDIF
  200         CONTINUE
              DO 210 I=1,N
                  F(I)=E(I)
                  IF(LLL) F(I)=ABS(F(I))
  210         CONTINUE
              J=N
  220         L=1
              II=1
              JJ=1
              DO 240 I=2,J
                  IF(F(I).LE.F(L)) THEN
                      L=I
                  ELSE
                      JJ=L
                      II=I
                  ENDIF
  240         CONTINUE
              IF(II.NE.JJ) THEN
                  W=E(JJ)
                  E(JJ)=E(II)
                  E(II)=W
                  F(JJ)=F(II)
                  IF(.NOT.LL) THEN
                      DO 250 I=1,N
                          W=A(I,JJ)
                          A(I,JJ)=A(I,II)
                          A(I,II)=W
  250                 CONTINUE
                  ENDIF
              ENDIF
              J=II-1
              IF(J.GE.2) GO TO 220
          ENDIF
          ILL=0
      ELSE
          ILL=30000
      ENDIF
      RETURN
      END
