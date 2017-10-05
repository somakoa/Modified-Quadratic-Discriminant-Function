c
c     Matrix inversion
c
      subroutine invs(a,b,w,n,ns,eps,ill)
      real a(ns,ns),b(ns,ns),w(n)
c      write(6,*) 'Input matrix'
c      do 400 j=1,10
c         write(6,401) (a(i,j),i=1,10)
c  401    format(1h ,10f8.3)
c  400    continue
      ill=1
      call hoqrus(a,n,ns,w,b,eps,ill)
      do 10 j=1,n
         do 20 i=1,n
            b(i,j)=0
   20       continue
   10    continue
      do 300 k=1,n
         wk=1/w(k)
c         wk=w(k)
         do 100 i=1,n
            do 200 j=1,n
               b(i,j)=b(i,j)+wk*a(i,k)*a(j,k)
  200          continue
  100       continue
  300    continue
c      write(6,*) 'Output matrix'
c      do 500 j=1,10
c         write(6,401) (b(i,j),i=1,10)
c  500    continue
      return
      end
