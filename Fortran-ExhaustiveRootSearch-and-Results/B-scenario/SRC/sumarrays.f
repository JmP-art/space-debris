      PROGRAM SUMARRAYS
      IMPLICIT REAL*4 (A-H,O-Z)
      parameter (eps = 1.0d-10)
      parameter(NRMAX=6,NHMAX=100,X1=eps,X2=1.0d0-eps)

      dimension H31(NHMAX,NHMAX,NHMAX)
      dimension H32(NHMAX,NHMAX,NHMAX)
      dimension H33(NHMAX,NHMAX,NHMAX)

      dimension H41(NHMAX,NHMAX,NHMAX,NHMAX)
      dimension H42(NHMAX,NHMAX,NHMAX,NHMAX)
      dimension H43(NHMAX,NHMAX,NHMAX,NHMAX)

      read(10,*)idec
! ---------------------------------------------------------------------- idec = 1 -----------------
      if(idec .eq. 1)then
        do i = 1 , NHMAX
           read(11,"(f40.1)")h1old
           read(12,"(f40.1)")h1new
          write(13,"(f40.1)")h1old+h1new
        enddo
      end if

! ---------------------------------------------------------------------- idec = 2 -----------------
      if(idec .eq. 2)then
        do i = 1 , NHMAX
          do j = 1 , NHMAX
             read(21,"(f40.1)")h1old
             read(22,"(f40.1)")h1new
            write(23,"(f40.1)")h1old+h1new
          enddo
        enddo
      end if

! ---------------------------------------------------------------------- idec = 3 -----------------
      if(idec .eq. 3)then
        kdec = 0
        do i = 1 , NHMAX
        do j = 1 , NHMAX
        do k = 1 , NHMAX
          h31(i,j,k) = 0.0
          h32(i,j,k) = 0.0
          h33(i,j,k) = 0.0
        enddo
        enddo
        enddo
        do while (kdec .eq. 0) 
          read(31,*,end=1,err=1)k1,k2,k3,x
          h31(k1,k2,k3) = h31(k1,k2,k3) + x
        enddo
1       continue
        do while (kdec .eq. 0) 
          read(32,*,end=2,err=2)k1,k2,k3,x
          h32(k1,k2,k3) = h32(k1,k2,k3) + x
        enddo
2       continue
        do i = 1 , NHMAX
          do j = 1 , NHMAX
            do k = 1 , NHMAX
              h33(i,j,k) = h33(i,j,k) + h31(i,j,k) + h32(i,j,k)
              if(h33(i,j,k) .gt. 0.5)write(33,"(3i5,f40.1)")i,j,k,h33(i,j,k)
            enddo
          enddo
        enddo
      end if
! ---------------------------------------------------------------------- idec = 4 -----------------
      if(idec .eq. 4)then
        do i = 1 , NHMAX
        do j = 1 , NHMAX
        do k = 1 , NHMAX
        do l = 1 , NHMAX
          h41(i,j,k,l) = 0.0
          h42(i,j,k,l) = 0.0
          h43(i,j,k,l) = 0.0
        enddo
        enddo
        enddo
        enddo
        do while (kdec .eq. 0) 
          read(41,*,end=3,err=3)k1,k2,k3,k4,x
          h41(k1,k2,k3,k4) = h41(k1,k2,k3,k4) + x
        enddo
3       continue
        do while (kdec .eq. 0) 
          read(42,*,end=4,err=4)k1,k2,k3,k4,x
          h42(k1,k2,k3,k4) = h42(k1,k2,k3,k4) + x
        enddo
4       continue
        do i = 1 , NHMAX
          do j = 1 , NHMAX
            do k = 1 , NHMAX
              do l = 1 , NHMAX
                h43(i,j,k,l) = h43(i,j,k,l) + h41(i,j,k,l) + h42(i,j,k,l)
                if(h43(i,j,k,l) .gt. 0.5)write(43,"(4i5,f40.1)")i,j,k,l,h43(i,j,k,l)
              enddo
            enddo
          enddo
        enddo

      end if
      END
