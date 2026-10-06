      IMPLICIT REAL*8 (A-H,O-Z)
      dimension sroot(6)
      OPEN(UNIT=10, FILE='config.inp', FORM='FORMATTED')
      OPEN(UNIT=20, FILE='config.dat', FORM='FORMATTED')
      read(10,*)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,nb,(sroot(k),k=1,nb)
      write(*,*)" FORTRAN roots lie at :",(sroot(k),k=1,nb)
       write(20,*)b0 
       write(20,*)SigL
       write(20,*)SigR
       write(20,*)sl
       write(20,*)sh
       write(20,*)betl
       write(20,*)beth
       write(20,*)c0
       write(20,*)aa
       write(20,*)cs
       write(20,*)betc
       write(20,*)sigc
      END
