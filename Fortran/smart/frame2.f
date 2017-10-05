      SUBROUTINE FRAME2(IM,M,N,IS,JS)
      INTEGER IM(IS,JS)
      DO 100 I=1,M
         IM(I,1)=0
         IM(I,N)=0
  100    CONTINUE
      DO 200 J=1,N
         IM(1,J)=0
         IM(M,J)=0
  200    CONTINUE
      RETURN
      END
