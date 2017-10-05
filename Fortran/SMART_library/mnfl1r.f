c
c     Mean filter
c
      subroutine mnfl1r(ia,ib,i1,j1,iss,jss)
      integer ia(iss,jss),ib(iss,jss)
c
      do 300 i=1,i1
         ib(i,1)=ia(i,1)
         ib(i,j1)=ia(i,j1)
  300    continue
      do 400 j=1,j1
         ib(1,j)=ia(1,j)
         ib(i1,j)=ia(i1,j)
  400    continue
c
      do 100 j=2,j1-1
         do 200 i=2,i1-1
            ibb=ib(i,j-1)+ib(i-1,j)+ia(i,j)+ia(i+1,j)
     2          +ia(i,j+1)
     3          +ib(i-1,j-1)+ib(i+1,j-1)
     4          +ia(i-1,j+1)+ia(i+1,j+1)
c            ib(i,j)=nint(ibb/5.0)
c            if(ibb.ge.3) then
            if(ibb.ge.5) then
               ib(i,j)=1
               else
               ib(i,j)=0
               endif
  200       continue
  100    continue
      return
      end
