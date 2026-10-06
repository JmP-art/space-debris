      PROGRAM XANAL
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*8 (I-N)
      parameter (eps = 1.0d-10)
      parameter(NRMAX=6,NHMAX=100,NKS0=10,NKSIGS=8,X1=eps,X2=1.0d0-eps)
      dimension xrot(NRMAX)
! --------------------------------------------------------------------------------------------------------------------------------
903   format(3f6.2,f6.1,f6.2,5x,3f6.2,2f6.1,2f6.2,2x,i5,6f10.4)

      kdec = 0
      do while (kdec .eq. 0) ! i = 1 , 1000000000
        read(10,*,end=1,err=1)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,nb,(xrot(k),k=1,nb)
        if(s0 .lt. 0.1)then
          if(c0 .gt. b0)then
            if(c0 - b0 .gt. 0.15)then
              write(*,903)c0,aa,sigc,b0,SigL,SigR,betl,beth,sl,sh,s0,sigs,nb,(xrot(k),k=1,nb)
            end if
          end if
        end if
!       ------------------------------------------------------------------------------
      enddo
1     continue
      END
