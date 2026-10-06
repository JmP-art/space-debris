      PROGRAM RENORM
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*8 (I-N)
      parameter (eps = 1.0d-10)
      parameter(NRMAX=6,NHMAX=100,X1=eps,X2=1.0d0-eps)

      dimension H1(NHMAX)
      dimension H2(NHMAX,NHMAX)
      dimension H3(NHMAX,NHMAX,NHMAX)

      read(10,*)idec

      if(idec .eq. 1)then
        sum = 0.0d0
        do i = 1 , NHMAX
          read(11,"(f40.1)")h1(i)
          sum = sum + h1(i)
        enddo
        if(sum .lt. eps)sum = 1.0d0
        do i = 1 , NHMAX
          write(13,"(i10,f12.7)")i,h1(i)/sum
        enddo
      end if

      if(idec .eq. 2)then
        hmax = -1.0d0
        do i = 1 , NHMAX
          do j = 1 , NHMAX
             read(21,"(f40.1)")h2(i,j)
             hmax = max(hmax,h2(i,j))
          enddo
        enddo
        if(hmax .lt. eps)hmax = 1.0d0
          do i = 1 , NHMAX
          do j = 1 , NHMAX
            write(23,"(2i10,f12.7)")i,j,h2(i,j)/hmax
            write(24,"(2f10.2,f12.7)")dfloat(2*j-1)*dx/2.0d0,dfloat(2*i-1)*dx/2.0d0,h2(i,j)/hmax
          enddo
        enddo
      end if
      END
