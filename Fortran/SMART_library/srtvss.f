      SUBROUTINE SRTVSS(N,AK,V,KV,L,IW,W,IND)
      DIMENSION V(KV,N),AK(N),IW(N),W(L)
      IF(L.LT.1.OR.N.LT.1.OR.L.GT.KV) GO TO 90
      DO 10 I=1,N
   10 IW(I)=I
      CALL SORTSI(N,AK,IW,IND)
      DO 70 K=1,N
      IF(IW(K).EQ.K) GO TO 70
      DO 20 I=1,L
   20 W(I)=V(I,K)
      J=K
   30 JJ=IW(J)
      IW(J)=J
      IF(JJ.EQ.K) GO TO 50
      DO 40 I=1,L
   40 V(I,J)=V(I,JJ)
      J=JJ
      GO TO 30
   50 DO 60 I=1,L
   60 V(I,J)=W(I)
   70 CONTINUE
   80 IND=0
      RETURN
   90 IND=30000
      RETURN
      END
