      SUBROUTINE SCAPRO( HIST, EVEC, IU, HIST2, NIU )
      REAL HIST(IU), EVEC(IU,NIU), HIST2(NIU)
      INTEGER IU, NIU
      REAL TMP
C
      DO 100 J=1,NIU
          TMP = 0.0
          DO 200 I=1,IU
              TMP = TMP + EVEC(I,J) * HIST(I)
  200     CONTINUE
          HIST2(J) = TMP
  100 CONTINUE
      RETURN
      END
