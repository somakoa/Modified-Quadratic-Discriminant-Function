      SUBROUTINE BDFL8B(IM,M,N,is,js,LX,LY,L,LL)
c
cd    Coded by F.Kimura
cp    Border Following
c
ca    IM(M,N) : Input Binary Image (i)
ca    M,N     : Declared Size of IM (i)
ca    LX(LL)  : List of X-coordinates of Border pixels (o) 
ca    LY(LL)  : List of Y-coordinates of Border pixels (o) 
ca    L       : Length of LX and LY (o)
ca    LL      : Declared Size of LX and LY (i) 
c
      DIMENSION LX(LL),LY(LL)
      INTEGER IM(is,js)
      L=0
      DO 100 J=1,N
         DO 200 I=1,M
            IF(IM(I,J).NE.1) GO TO 200
            if(im(i-1,j).ne.0) go to 200
            ids=4
            CALL BDFL8A(IM,M,N,is,js,LX,LY,L,LL,I,J,IDS)
  200       CONTINUE
  100    CONTINUE
c
c     Recovery of side efect on input image
c
      do 300 k=1,l
         i=lx(k)
         if(i.eq.-1) go to 300
         im(i,ly(k))=1
  300    continue
      RETURN
      END
