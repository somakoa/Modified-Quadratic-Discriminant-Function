      SUBROUTINE BDFL8A(IM,M,N,iss,jss,LX,LY,L,LL,IS,JS,IDS)
      DIMENSION LX(LL),LY(LL),ID(0:7),JD(0:7)
      INTEGER IM(iss,jss)
      DATA ID/1,1,0,-1,-1,-1,0,1/,JD/0,-1,-1,-1,0,1,1,1/
      data nil/-1/
      IM(IS,JS)=-3
      IMP=3
  50  L=L+1
      LX(L)=IS
      LY(L)=JS
C      WRITE(6,*) 'IS,JS,IDS',IS,JS,IDS
      IF(IMP.EQ.-3) GO TO 300
      DO 100 K=IDS+7,IDS,-1
         KK=MOD(K,8)
         IDKK=ID(KK)
         JDKK=JD(KK)
         IMP=IM(IS+IDKK,JS+JDKK)
         IF(IMP.NE.0) THEN
            IS=IS+IDKK
            JS=JS+JDKK
            IM(IS,JS)=3
            IDS=KK+4
C            WRITE(6,*) 'NEXT IS,JS,IDS',IS,JS,IDS
            GOTO 50
            ENDIF
  100    CONTINUE
      IM(IS,JS)=2
C      WRITE(6,*) 'ISOLATED POINT'
  300 L=L+1
      LX(L)=nil
      LY(L)=nil
      RETURN
      END
