      PROGRAM XHIST
! --------------------------------------------------------------------------------------------------------------------------------
!
! S(x) = s0 + (1-s0) x (1-x)^sigS
!
! 01 # 0.00  .le.   s0   .le. 0.50 ( delta.s0   = 0.05 ) / 11 configs
! 02 # 0.50  .le.  sigS  .le. 2.00 ( delta.sigS = 0.50 ) / 03 configs
! --------------------------------------------------------------------------------------------------------------------------------
! program to compute the ROOTS of benefit(x) - cost(x) (Space-Debris model) in the interval ]0,1[ from known roots of 
! benefit(s) - cost(s), given the scaling of s as a function of x defined above. 
! --------------------------------------------------------------------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*8 (I-N)
      parameter (eps = 1.0d-10)
      parameter(NRMAX=6,NHMAX=100,X1=eps,X2=1.0d0-eps)

      dimension kr(0:NRMAX)
      common / SR1   / s0,sigs
      common / RT1   / nb,kb,sroot(NRMAX),xrot(NRMAX)

      OPEN(UNIT=10, FILE='xhist-1-conf.dat', FORM='FORMATTED')
      OPEN(UNIT=21, FILE='xhist-1-conf-list.dat', FORM='FORMATTED')
! --------------------------------------------------------------------------------------------------------------------------------
! "l" stands for low (left, unstable) root whereas "h" stands for high (right, stable) root
!
! NOTE:
!    the program "xhist" (from which this program derives) computes x-roots in order (xH,xL) obtained from s-roots in order (sL,sH)
!    however, the notebook "DiskPlot.nb" plots a histogram array in order (xL,xH)
!    thus, to match the roots computed by the present code, we enter "DiskPlot.nb" values (iL,jH) which lead to (xL,xH) but compare 
!    the values of the ourput of the s -> x transformation in the reverse order. 

      read(10,*)krotl

      NKS0   = 10
      NKSIGS = 2
      ds0    = 0.05d0
      dsigs  = 0.50d0 
      dx    = 0.01d0

      xl0   = dfloat(krotl-1) * dx
      xl1   = dfloat(krotl) * dx
      if(krotl .eq. 1) xl0 = -eps
      write(*,"(' >>> root  :  ',3f10.3)")xl0,xl1,0.5d0*(xl0+xl1)
! --------------------------------------------------------------------------------------------------------------------------------
903   format(3f6.2,f6.1,f6.2,5x,3f6.2,2f6.1,2f6.2,2x,i5,6f10.4)           ! write s-roots
904   format(3f6.2,f6.1,f6.2,5x,3f6.2,2f6.1,2f6.2,5x,2f6.2,2x,i5,6f10.4)  ! write x-roots

      kdec = 0
      i = 0
      do while (kdec .eq. 0)
        do j = 1 , NRMAX
          sroot(j) = 0.0d0
        enddo
        read(20,*,end=1,err=1)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,nb,(sroot(k),k=1,nb)
        i = i + 1
        if(nb .lt. 2)GOTO 2
!       ------------------------------------------------------------------------------
        ii = 0
        do ks0 = 0 , NKS0                                        ! loop in s0
          s0 = 0.0d0 + dfloat(ks0) * ds0
!       ------------------------------------------------------------------------------
          do ksigs = 0 , NKSIGS                                  ! loop in sigS
            ii = ii + 1
            sigs = 0.0d0 + dfloat(ksigs) * dsigs
            do j = 1 , NRMAX
              xrot(j) = 0.0d0
            enddo
!       ------------------------------------------------------------------------------
            kb = 0
            do j = 1 , nb                                        ! loop over s-roots
              sr = dabs(sroot(j))
              if(s0 .lt. sr)then
                xr = xx(sr)
                if(xr .gt. X1 .and. xr .lt. X2)then
                  kb = kb + 1
                  xrot(kb) = xr
                end if
              end if
            enddo                                                 ! end_of: loop over s-roots
            if(kb .ne. 1)GOTO 3
            xr1 = xrot(1)
            if(xr1 .gt. xl0 .and. xr1 .le. xl1)then
              write(21,904)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,s0,sigs,kb,(xrot(k),k=kb,1,-1)
            end if
3           continue
          enddo                                                   ! end_of: loop in sigS
        enddo                                                     ! end_of: loop in s0
!       ------------------------------------------------------------------------------
2     continue
      enddo
1     continue
      END
!
! --------------------------------------------------------------------------------------------------------------------------------
! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! --------------------------------------------------------------------------------------------------------------------------------
      REAL*8 FUNCTION xx(s)
! --------------------------------------------------------------------------------------------------------------------------------
!                                                                                         inverse of function s(x) - function x(s)
      IMPLICIT REAL*8 (A-H,O-Z)
      parameter (eps = 1.0d-10)

      common / SR1   / s0,sigs

      sm1 = 1.0d0 / sigs
      xx = (s - s0) / (1.0d0 - s0)
      xx = 1.0d0 - xx**sm1
      return
      END
