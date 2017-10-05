      subroutine gsmpl1(hist,ghist,ns)
      real hist(ns,ns),ghist(ns,ns)
      real w(-2:2,-2:2)
      data w/.0000,.0092,.0169,.0092,.0000,
     1       .0092,.0573,.1054,.0573,.0092,
     1       .0169,.1054,.1939,.1054,.0169,
     1       .0092,.0573,.1054,.0573,.0092,
     1       .0000,.0092,.0169,.0092,.0000/
c
      do 200 j=1,ns
         do 300 i=1,ns
            sum=0
            do 400 jd=-2,2
               jj=j+jd
               if(jj.lt.1.or.jj.gt.ns) go to 400
               do 500 id=-2,2
                  ii=i+id
                  if(ii.lt.1.or.ii.gt.ns) go to 500
                  sum=sum+hist(ii,jj)*w(id,jd)
  500             continue
  400          continue
            ghist(i,j)=sum
  300       continue
  200    continue
      return
      end
C
C
      subroutine gsmpl2(hist,ghist,ns)
      real hist(ns,ns),ghist(ns,ns)
      real w(-2:2,-2:2)
      data w/.0000,.0092,.0169,.0092,.0000,
     1       .0092,.0573,.1054,.0573,.0092,
     1       .0169,.1054,.1939,.1054,.0169,
     1       .0092,.0573,.1054,.0573,.0092,
     1       .0000,.0092,.0169,.0092,.0000/
c
      do 200 j=1,ns
         do 300 i=1,ns
            sum=0
            do 400 jd=-2,2
               jj=j+jd*2
               if(jj.lt.1.or.jj.gt.ns) go to 400
               do 500 id=-2,2
                  ii=i+id*2
                  if(ii.lt.1.or.ii.gt.ns) go to 500
                  sum=sum+hist(ii,jj)*w(id,jd)
  500             continue
  400          continue
            ghist(i,j)=sum
  300       continue
  200    continue
      return
      end
C
C
      subroutine gsmpl3(hist,ghist,ns)
      real hist(ns,ns),ghist(ns,ns)
      real w(-2:2,-2:2)
      data w/.0000,.0092,.0169,.0092,.0000,
     1       .0092,.0573,.1054,.0573,.0092,
     1       .0169,.1054,.1939,.1054,.0169,
     1       .0092,.0573,.1054,.0573,.0092,
     1       .0000,.0092,.0169,.0092,.0000/
c
      do 200 j=1,ns
         do 300 i=1,ns
            sum=0
            do 400 jd=-2,2
               jj=j+jd*4
               if(jj.lt.1.or.jj.gt.ns) go to 400
               do 500 id=-2,2
                  ii=i+id*4
                  if(ii.lt.1.or.ii.gt.ns) go to 500
                  sum=sum+hist(ii,jj)*w(id,jd)
  500             continue
  400          continue
            ghist(i,j)=sum
  300       continue
  200    continue
      return
      end
C
      subroutine gsmpln(hist,nss,ghist,ns,ndir,w,ngs)
      real hist(nss,nss,ndir),ghist(ns,ns,ndir)
      real w(ngs,ngs)
c
      ncent=ngs/2+1
      ndist=(nss-1)/(ns-1)
      do 100 k=1,ndir
         do 200 j=1,ns
            j0=ndist*(j-1)+1
            do 300 i=1,ns
               i0=ndist*(i-1)+1
               sum=0
               do 400 jd=1,ngs
                  jj=j0+(jd-ncent)
                  if(jj.lt.1.or.jj.gt.nss) go to 400
                  do 500 id=1,ngs
                     ii=i0+(id-ncent)
                     if(ii.lt.1.or.ii.gt.nss) go to 500
                     sum=sum+hist(ii,jj,k)*w(id,jd)
  500                continue
  400             continue
               ghist(i,j,k)=sum
  300          continue
  200       continue
  100    continue
      return
      end
