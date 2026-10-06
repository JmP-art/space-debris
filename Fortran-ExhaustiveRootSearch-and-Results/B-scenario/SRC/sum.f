      IMPLICIT REAL*8 (A-H,O-Z)
      hmax = -1.0d0
      read(11,*)irank
      kdec = 0 
      hmax = -1.0d0
      ik = 0
      jk = 0
      kk = 0
      lk = 0
      sum = 0.0d0
      if(irank .eq. 1)then
        sum = 0.0d0
        do i = 1 , 100
          read(10,*)x
          sum = sum + x
        enddo
        write(*,*)" sum : ",sum
      end if
      if(irank .eq. 2)then
        sum = 0.0d0
        do i = 1 , 100
        do j = 1 , 100
          read(10,*)x
          sum = sum + x
        enddo
        enddo
        write(*,*)" sum : ",sum
      end if
      if(irank .eq. 3)then
        sum = 0.0d0
        do while (kdec .eq. 0) 
          read(10,*,end=3,err=3)k1,k2,k3,x
          sum = sum + x
        enddo
3       continue
        write(*,*)" sum : ",sum
      end if
      if(irank .eq. 4)then
        sum = 0.0d0
        do while (kdec .eq. 0) 
          read(10,*,end=4,err=4)k1,k2,k3,k4,x
          sum = sum + x
        enddo
4       continue
        write(*,*)" sum : ",sum
      end if
      END
