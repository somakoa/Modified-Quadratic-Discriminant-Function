      SUBROUTINE BDFL1(IP,JP,ISX,ISY,LISTX,LISTY,LENG,LENG1,IX0,IY0,
     &             IS,NC,KERR)
C  ***  BORDER FOLLOWING ALGORITHM FROM A SPECIFIED STARTING POINT
C  ***  FOLLOWING OF ALL BORDERS
C  ***
C  ***  IP(ISX,ISY)  : INPUT BINARY IMAGE
C  ***  JP(ISX,ISY)  : OUTPUT THREE-VALUED IMAGE
C  ***  LISTX(LENG) : ONE-DIMENSIONAL ARRAY (TEN NO X-ZAHYOU)
C  ***  LISTY(LENG) : ONE-DIMENSIONAL ARRAY (TEN NO Y-ZAHYOU)
C  ***  IX0,IY0  : STARTING POINT
C  ***  IS       :  DIRECTION IN TRACE STARTING POINT
C  ***  NC       :  NEIGHBOR CONNECTION  ( 4 OR 8 )
C  ***  LENG1   :  LENGTH OF LISTX,LISTY
C  ***  KERR    :  ERROR CODE
C   ***
C  ***   SAKUSEI  S. YOKOI (BODR1I)
C  ***   S.  75.  11.  20
C  ****   MODIFIED BY S.TSURUOKA (1987.12.14)
      DIMENSION IP(ISX,ISY),JP(ISX,ISY),LISTX(LENG),LISTY(LENG)
      INTEGER IXP(8),IYP(8)
      DATA IXP/0,-1,-1,-1,0,1,1,1/
      DATA IYP/1,1,0,-1,-1,-1,0,1/
      IF (KERR.LT.0) RETURN
C**
      DO 1 IY=1,ISY
          DO 2 IX=1,ISX
              JP(IX,IY)=IP(IX,IY)
    2     CONTINUE
    1 CONTINUE
      KERR=0
      IX=IX0
      IY=IY0
      IXN=0
      IYN=0
      ISW=0
      NNN=1
      IF(NC.EQ.4) NNN=2
      JP(IX,IY)=2
C      1-LINE TRACKING
C      SEARCH OF NEXT POINT
   11 CONTINUE
      LISTX(LENG1)=IX
      LISTY(LENG1)=IY
      IS=IS+NNN
      IS8=IS+8
      DO 10 IN1=IS,IS8,NNN
          IN=IN1
          IF(IN.GT.8) IN=IN-8
          IXW=IX+IXP(IN)
          IYW=IY+IYP(IN)
          IF(JP(IXW,IYW).NE.0) THEN
              JP(IXW,IYW)=2
              GOTO 20
          ENDIF
   10 CONTINUE
C    ISOLATED POINT
      RETURN
C********
   20 CONTINUE
      IS=IN+4
      IF(ISW.NE.1) THEN
          ISW=1
          IXN=IXW
          IYN=IYW
      ENDIF
      IF(IS.GT.8) IS=IS-8
      LENG1=LENG1+1
C***  BEYOND THE LENGTH OF THE LIST
      IF(LENG1.LE.LENG) THEN
          IX=IXW
          IY=IYW
          IF(IX.NE.IX0.OR.IY.NE.IY0) GO TO 11
C----------------
          IS=IS+NNN
          IS8=IS+8
          DO 40 IN1=IS,IS8,NNN
              IN=IN1
              IF(IN.GT.8) IN=IN-8
              IXW=IX+IXP(IN)
              IYW=IY+IYP(IN)
              IF(JP(IXW,IYW).NE.0) GO TO 50
   40     CONTINUE
   50     CONTINUE
          IS=IS-NNN
          IF(IXW.NE.IXN.OR.IYW.NE.IYN) GO TO 11
          LISTX(LENG1)=IX
          LISTY(LENG1)=IY
          RETURN
C***   ERROR CODE  GENERATION
C      LISTX AND LISTY HAVE OVERFLOWED
      ENDIF
      KERR=-1
      RETURN
      END
