C--------------------------------------------------
C     Orthonormal Discriminant Analysis
C                                  May.2.1995
C                                  by Waka
C--------------------------------------------------
      SUBROUTINE ODV(SB,SW,P,F,SB2,SW2,WORK,N,NS)
C
C     SB	I	Between Class covariance
C     SW	I	Within Class covariance
C     P		O	Eigen vector
C     F		O	Eigen value
C     SB2	/	Working area
C     SW2	/	Working area
C     WORK	/	Working area
C
      REAL SB(NS,NS)
      REAL SW(NS,NS)
      REAL P(NS,NS)
      REAL F(NS)
C
      REAL SB2(NS,NS)
      REAL SW2(NS,NS)
      REAL WORK(NS,NS)
C
      REAL EPS
      INTEGER IND
C
      EPS=1.0E-5
C--------Initialize P---------------
      DO 10 J=1,N
         DO 10 I=1,N
            P(I,J)=0.0
   10 CONTINUE
      DO 20 I=1,N
         P(I,I)=1.0
   20 CONTINUE

C-----------backup SB,SW-----------------
      CALL MCOPY(SB,SB2,N,N,NS,NS)
      CALL MCOPY(SW,SW2,N,N,NS,NS)
C----------- Main Loop -------------
      DO 30 L=1,N-1
         NN=N-L+1
C        ------ Canonical Discriminant Analysys--------
         IND=1
         CALL GHQRVS(SB2,SW2,NS,NN,F(L),WORK,EPS,IND)
C        ------ P(r-1)*d(i) --------------------------
         CALL MMUL(SB2,P(1,L),WORK,N,NN,NN,NS,NS)
C        ------ Orthogonalize -----------------------
         CALL ORTHO(WORK,P(1,L),N,NS,NN)
C        ------ Translation of Sb Sw ----------------
         CALL MMUL(P(1,L+1),SB,WORK,N,NN-1,N,NS,NS)
         CALL MMUL_BT(WORK,P(1,L+1),SB2,NN-1,NN-1,N,NS,NS)
         CALL MMUL(P(1,L+1),SW,WORK,N,NN-1,N,NS,NS)
         CALL MMUL_BT(WORK,P(1,L+1),SW2,NN-1,NN-1,N,NS,NS)
   30 CONTINUE
      f(n)=sb2(1,1)/sw2(1,1)
      RETURN
      END

C
C     ORTHOGONALIZATION
C     Gram & Shmidt
C                        Apr.28.1995
C                        by Waka
C
      SUBROUTINE ORTHO(V1,V2,N,NS,M)
C
C V1:Input vectors
C V2:Output vectors
C N :Dimensionality
C NS:Array Size
C M :Number of vectors
C
      REAL V1(NS,NS)
      REAL V2(NS,NS)
      REAL SUM,S,SINV
C
C     First Vector Normalization
C
      SUM=0.0
      DO 10 I=1,N
         SUM=SUM+V1(I,1)*V1(I,1)
   10 CONTINUE
      SINV=1.0/SQRT(SUM)
      DO 20 I=1,N
         V2(I,1)=V1(I,1)*SINV
   20 CONTINUE
C
      DO 80 K=2,M
         DO 55 J=1,K-1
            SUM=0.0
            DO 40 I=1,N
               SUM=SUM+V2(I,J)*V1(I,K)
   40       CONTINUE
            S=-SUM
            DO 50 I=1,N
               V1(I,K)=V1(I,K)+S*V2(I,J)
   50       CONTINUE
   55    CONTINUE
C
C        Normalization
C
         SUM=0.0
         DO 60 I=1,N
            SUM=SUM+V1(I,K)*V1(I,K)
   60    CONTINUE
         SINV=1.0/SQRT(SUM)
         DO 70 I=1,N
            V2(I,K)=V1(I,K)*SINV
   70    CONTINUE
   80 CONTINUE
      RETURN
      END
      
c
      subroutine madd(a,b,c,i1,j1,is,js)
      real a(is,js),b(is,js),c(is,js)
      do 100 j=1,j1
         do 200 i=1,i1
            c(i,j)=a(i,j)+b(i,j)
  200       continue
  100    continue
      return
      end
c
      subroutine msub(a,b,c,i1,j1,is,js)
      real a(is,js),b(is,js),c(is,js)
      do 100 j=1,j1
         do 200 i=1,i1
            c(i,j)=a(i,j)-b(i,j)
  200       continue
  100    continue
      return
      end
c
      subroutine nrmz(a,i1)
      real a(i1)
      s=0.0
      do 100 i=1,i1
         s=s+a(i)**2
  100    continue
      s=sqrt(s)
      do 200 i=1,i1
         a(i)=a(i)/s
  200    continue
      return
      end
c
      subroutine mcopy(a,b,i1,j1,is,js)
      real a(is,js),b(is,js)
      do 100 j=1,j1
         do 200 i=1,i1
            b(i,j)=a(i,j)
  200       continue
  100    continue
      return
      end 
c
C      subroutine mtrans(a,at,i1,is)
C      real a(is,is),at(is,is)
C      do 100 j=1,i1
C         do 200 i=1,i1
C            at(i,j)=a(j,i)
C  200       continue
C  100    continue
C      return
C      end
c
      subroutine mtrans(a,at,i1,j1,is,js)
      real a(is,js),at(js,is)
      do 100 j=1,j1
         do 200 i=1,i1
            at(j,i)=a(i,j)
  200       continue
  100    continue
      return
      end
c
      subroutine mmul(a,b,c,i1,j1,k1,is,js)
      real a(is,js),b(is,js),c(is,js)
      real sum
C     C(i1,j1)=a(k1,j1)*b(i1,k1)
      do 100 j=1,j1
         do 200 i=1,i1
            sum=0.0
            do 300 k=1,k1
               sum=sum+a(k,j)*b(i,k)
  300          continue
            c(i,j)=sum
  200    continue
  100 continue
      return
      end
c
      subroutine mmul_at(a,b,c,i1,j1,k1,is,js)
      real a(is,js),b(is,js),c(is,js)
      real sum
C     C(i1,j1)=at(j1,k1)*b(i1,k1)
      do 100 j=1,j1
         do 200 i=1,i1
            sum=0.0
            do 300 k=1,k1
               sum=sum+a(j,k)*b(i,k)
  300       continue
            c(i,j)=sum
  200    continue
  100 continue
      return
      end
c
      subroutine mmul_atct(a,b,c,i1,j1,k1,is,js)
      real a(is,js),b(is,js),c(is,js)
      real sum
C     C(j1,i1)=at(j1,k1)*b(i1,k1)
      do 100 j=1,j1
         do 200 i=1,i1
            sum=0.0
            do 300 k=1,k1
               sum=sum+a(j,k)*b(i,k)
  300       continue
            c(j,i)=sum
  200    continue
  100 continue
      return
      end
c
      subroutine mmul_bt(a,b,c,i1,j1,k1,is,js)
      real a(is,js),b(is,js),c(is,js)
      real sum
C     C(i1,j1)=a(k1,j1)*bt(k1,i1)
      do 100 j=1,j1
         do 200 i=1,i1
            sum=0.0
            do 300 k=1,k1
               sum=sum+a(k,j)*b(k,i)
  300       continue
            c(i,j)=sum
  200    continue
  100 continue
      return
      end
c
      subroutine mprint(a,i1,j1,is,js,str,n)
      real a(is,js)
      character str(n)
      write(6,*)
      write(6,*) str
      write(6,*)
      do 100 j=1,j1
         write(6,101) (a(i,j),i=1,i1)
  101    format(10f7.3)
  100    continue
      return
      end
c
      subroutine mclear(a,i1,j1,is,js)
      real a(is,js)
      do 100 j=1,j1
        do 100 i=1,i1
          a(i,j)=0.0
  100 continue
      return
      end

