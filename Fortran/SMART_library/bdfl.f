      SUBROUTINE BDFL8(IM,M,N,is,js,LX,LY,L,LL)
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
            IF(IM(I+1,J).EQ.0) THEN
               IDS=0
               ELSEIF(IM(I,J-1).EQ.0) THEN
               IDS=2
               ELSEIF(IM(I-1,J).EQ.0) THEN
               IDS=4
               ELSEIF(IM(I,J+1).EQ.0) THEN
               IDS=6
               ELSE
               GO TO 200
               ENDIF
            CALL BDFL8A(IM,M,N,is,js,LX,LY,L,LL,I,J,IDS )
  200       CONTINUE
  100    CONTINUE
      RETURN
      END

