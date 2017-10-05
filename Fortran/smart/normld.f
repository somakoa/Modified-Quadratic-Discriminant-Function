      subroutine normldg(oldb,oldg,x1,y1,xs1,ys1,new,x2,y2,xs2,ys2
     *                 ,alpha)
      integer x1,y1,xs1,ys1
      integer x2,y2,xs2,ys2
      real(8) alpha
      integer oldb(xs1,ys1),oldg(xs1,ys1),new(xs2,ys2)
      parameter(loxs=1000,loys=1000)
      real lo(loxs,loys)
      real hx(loxs),hy(loys)
      real ahx(loxs),ahy(loys)
      real nxsum,nysum
      real t1,t2,t3,t1x,t1y,t2x,t2y,u_t2x,v_t2y
      real newtmp
      integer ii(loxs),jj(loys)

      call lden(oldb,x1,y1,xs1,ys1,lo,loxs,loys,ill)

      nxsum=0.0
      do 100 i=1,x1
         hx(i)=0.0
         do 110 j=1,y1
            hx(i)=hx(i)+lo(i,j)
  110    continue
         hx(i)=hx(i)+alpha
         nxsum=nxsum+hx(i)
         ahx(i)=nxsum
  100 continue

      nysum=0.0
      do 200 j=1,y1
         hy(j)=0.0
         do 210 i=1,x1
            hy(j)=hy(j)+lo(i,j)
  210    continue
         hy(j)=hy(j)+alpha
         nysum=nysum+hy(j)
         ahy(j)=nysum
  200 continue

C      t1=nxsum/x2
      t1=nxsum/(x2-1)
      do 300 i=1,x2
C         t2=(float(i)-0.5)*t1
         t2=float(i-1)*t1
         t3=0.0
         do 310 k=1,x1
            t3=t3+hx(k)
            if(t3.ge.t2) goto 320
  310    continue
  320    ii(i)=k
  300 continue

C      t1=nysum/y2
      t1=nysum/(y2-1)
      do 350 j=1,y2
C         t2=(float(j)-0.5)*t1
         t2=float(j-1)*t1
         t3=0.0
         do 360 k=1,y1
            t3=t3+hy(k)
            if(t3.ge.t2) goto 370
  360    continue
  370    jj(j)=k
  350 continue

      t1x=nxsum/(x2-1)
      t1y=nysum/(y2-1)
      do 400 j=1,y2
         t2y = float(j-1)*t1y
         if(jj(j).eq.1) then
            v_t2y=t2y/ahy(jj(j))
         else
            v_t2y=(t2y-ahy(jj(j)-1))/(ahy(jj(j))-ahy(jj(j)-1))
     *           +float(jj(j)-1)
         endif
         do 410 i=1,x2
            t2x = float(i-1)*t1x
            if(ii(i).eq.1) then
               u_t2x=t2x/ahx(ii(i))
            else
               u_t2x=(t2x-ahx(ii(i)-1))/(ahx(ii(i))-ahx(ii(i)-1))
     *              +float(ii(i)-1)
            endif
            newtmp=0.0
            do 420 l=jj(j)-2,jj(j)+1
               do 430 k=ii(i)-2,ii(i)+1
                  if(((l.ge.1).and.(l.le.y1)).and.
     *               ((k.ge.1).and.(k.le.x1))) then
                         newtmp=newtmp+float(oldg(k,l))*
     *                      c_sinc(float(k)-u_t2x)*
     *                      c_sinc(float(l)-v_t2y)
                  endif
  430          continue
  420       continue
            new(i,j)=newtmp*100.0
C            new(i,j)=newtmp
C             new(i,j)=oldg(ii(i),jj(j))
  410    continue
  400 continue
         
      return
      end

      real function c_sinc(x)
      real x,x2
      x2=abs(x)
      if ((x2.ge.0.0).and.(x2.lt.1.0)) then
         c_sinc=1.0 - 2.0*x2*x2 + x2*x2*x2
      else if ((x2.ge.1.0).and.(x2.lt.2.0)) then
         c_sinc=4.0 - 8.0*x2 + 5.0*x2*x2 - x2*x2*x2
      else if (x2.ge.2.0) then
         c_sinc=0.0
      endif 
C      WRITE(*,*) x2,c_sinc
      return
      end
    

      subroutine lden( old,x1,y1,xs,ys,lo,loxs,loys,ill )
      integer x1,y1,xs,ys
      integer old(xs,ys)
      integer loxs,loys
      real lo(loxs,loys)
      real lx(1000,1000), ly(1000,1000)
      real w,w2,w4,w6
      integer lin(1000)
      integer flag
      real tmp,tmpmul
      parameter(tmpmul=1.0/0.75)
C      parameter(tmpmul=0.5)
      data nil/-1/

C ---- calcurate line gap lx(i,j) ----

      w=float(x1)
      w2=w*2 
      w4=w*4
      w6=w*6

      do 100 j=1,y1
C ------ List out cross points ----
         k=0
         flag=0
         do 200 i=1,x1
            if(old(i,j).ne.flag) then
               k=k+1
               lin(k)=i-flag
               flag=1-flag
            endif
  200    continue
         if(old(x1,j).eq.1) then
            k=k+1
            lin(k)=x1
         endif
         lin(k+1)=nil
C         write(6,*) '-----k=',k,'-------'
C         do 210 kk=1,k
C            write(6,*) lin(kk)
C  210    continue
C ------ fill lx(i,j) i=1,x1 ----
         if(mod(k,2).eq.1) then
            ill=1
            write(6,*) 'write error'
            goto 999
         elseif(k.eq.0) then
            do 300 i=1,x1
               lx(i,j)=w4
  300       continue
         elseif(k.eq.2) then
            do 400 i=1,x1
               lx(i,j)=w2
  400       continue
         else
C --------- background ----
            do 500 i=1,lin(1)-1
               lx(i,j)=w2
  500       continue
            do 510 i=lin(k)+1,x1
               lx(i,j)=w2
  510       continue
            do 520 l=3,k-1,2
               tmp=float(lin(l+1)-lin(l-1)+lin(l)-lin(l-2))*tmpmul
               do 530 i=lin(l-1)+1,lin(l)-1
                  lx(i,j)=tmp
  530          continue
  520       continue
C --------- in character ----
            tmp = float(lin(3)-lin(1))
            do 600 i=lin(1),lin(2)
               lx(i,j)=tmp
  600       continue 
            tmp = float(lin(k)-lin(k-2))
            do 610 i=lin(k-1),lin(k)
               lx(i,j)=tmp
  610       continue
            do 620 l=4,k-2,2
               tmp=float(lin(l)-lin(l-2)+lin(l+1)-lin(l-1))*tmpmul
               do 630 i=lin(l-1),lin(l)
                  lx(i,j)=tmp
  630          continue
  620       continue
         endif
  100 continue        
C      call prlx( lx, x1, y1)
      

C ---- calcurate line gap ly(i,j) ----

      w=float(y1)
      w2=w*2 
      w4=w*4
      w6=w*6

      do 101 i=1,x1
C ------ List out cross points ----
         k=0
         flag=0
         do 201 j=1,y1
            if(old(i,j).ne.flag) then
               k=k+1
               lin(k)=j-flag
               flag=1-flag
            endif
  201    continue
         if(old(i,y1).eq.1) then
            k=k+1
            lin(k)=y1
         endif
         lin(k+1)=nil
C ------ fill ly(i,j) j=1,y1 ----
         if(mod(k,2).eq.1) then
            ill=1
            write(6,*) 'write error'
            goto 999
         elseif(k.eq.0) then
            do 301 j=1,y1
               ly(i,j)=w4
  301       continue
         elseif(k.eq.2) then
            do 401 j=1,y1
               ly(i,j)=w2
  401       continue
         else
C --------- background ----
            do 501 j=1,lin(1)-1
               ly(i,j)=w2
  501       continue
            do 511 j=lin(k)+1,y1
               ly(i,j)=w2
  511       continue
            do 521 l=3,k-1,2
               tmp=float(lin(l+1)-lin(l-1)+lin(l)-lin(l-2))*tmpmul
               do 531 j=lin(l-1)+1,lin(l)-1
                  ly(i,j)=tmp
  531          continue
  521       continue
C --------- in character ----
            tmp = float(lin(3)-lin(1))
            do 601 j=lin(1),lin(2)
               ly(i,j)=tmp
  601       continue 
            tmp = float(lin(k)-lin(k-2))
            do 611 j=lin(k-1),lin(k)
               ly(i,j)=tmp
  611       continue
            do 621 l=4,k-2,2
               tmp=float(lin(l)-lin(l-2)+lin(l+1)-lin(l-1))*tmpmul
               do 631 j=lin(l-1),lin(l)
                  ly(i,j)=tmp
  631          continue
  621       continue
         endif
  101 continue        
C      call prlx( ly, x1, y1)

C ---- Calculate lo ----

      w=float(x1)
      w6=w*6

      do 700 j=1,y1
         do 710 i=1,x1
            if((lx(i,j)+ly(i,j)).lt.w6) then
               lo(i,j)=max(w/lx(i,j),w/ly(i,j))
            else
               lo(i,j)=0.0
            endif
  710    continue
  700 continue
      ill=0      
  999 return
      end
            
