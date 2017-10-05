c
c     binarization by otsu algorithm
c
      subroutine bnrz(im,i1,j1,iss,jss)
      integer im(iss,jss)
      integer ihist(0:255)
c
c     histgram calculation
c
      do 250 k=0,255
         ihist(k)=0
  250    continue
c
      do 300 j=1,j1
         do 400 i=1,i1
            imij=im(i,j)
c      write(6,*) i,j,imij
            ihist(imij)=ihist(imij)+1
  400       continue
  300    continue
c
c      write(6,301) (k,ihist(k),k=0,255)
c  301 format(i5,i10)
c
c     power transformation
c
      do 350 k=0,255
         ihist(k)=sqrt(float(ihist(k)))
  350    continue
c
c     threshold determination
c
      call otsu1(ihist,0,255,kt)
c      write(6,352) kt
c  352 format(1h ,'  th ',i4)
c
c     binarization
c
      do 500 j=1,j1
         do 600 i=1,i1
            if(im(i,j).le.kt) then
c               im(i,j)=1
               im(i,j)=0
               else
c               im(i,j)=0
c               im(i,j)=255
               im(i,j)=1
               endif
  600       continue
  500    continue
c      do 700 j=1,j1
c         write(6,101) j,(im(i,j),i=1,min(i1,70))
c  101    format(70i1)
c  700    continue
      return
      end
c
c threshold selection program
c           (by otsu algorithm)
c
      subroutine otsu1(ihist,k1,k2,kt)
      real m1,m2
      dimension ihist(k1:k2)
c     1 to kt -> first class, kt+1 to n -> second class
      sum=0.0
      n=0
      do 100 i=k1,k2
         sum=sum+ihist(i)*i
         n=n+ihist(i)
  100    continue
      csum=0.0
      fmax=-1.0
      n1=0
c      write(6,51)
c   51 format(1h ,1x,'  i    n1   n2   m1   m2     sb')
      do 200 i=k1,k2-1
         n1=n1+ihist(i)
         if(n1.eq.0) go to 200
         n2=n-n1
         if(n2.eq.0) return
         csum=csum+ihist(i)*i
         m1=csum/n1
         m2=(sum-csum)/n2
         sb=n1*n2*(m1-m2)**2
         if(sb.gt.fmax) then
            fmax=sb
            kt=i
            endif
c         write(6,*) i,n1,n2,m1,m2,sb,kt
c         write(6,*) i,ihist(i),kt
  200    continue
      return
      end
c
