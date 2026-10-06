      IMPLICIT REAL*8 (A-H,O-Z)
      dimension ik(1000),jk(1000),kk(1000),lk(1000)
      hmax = -1.0d0
      kount = 0
      read(11,*)irank
      kdec = 0 
      hmax = -1.0d0
      do i = 1 , 1000
        ik(i) = 0
        jk(i) = 0
        kk(i) = 0
        lk(i) = 0
      enddo
      if(irank .eq. 1)then
        OPEN(UNIT=10, FILE='fort.10', FORM='FORMATTED')
        do i = 0 , 100
          read(10,*)x
          if(x .gt. hmax)hmax = x
        enddo
        CLOSE(UNIT=10, STATUS='KEEP')
        OPEN(UNIT=10, FILE='fort.10', FORM='FORMATTED')
        do i = 0 , 100
          read(10,*)x
          if(x .ge. hmax)then
            hmax = x
            kount = kount + 1
            ik(kount) = i
          end if
        enddo
        do i = 1 , kount
          write(*,*)" max at ",ik(i),hmax
        enddo
      end if
      if(irank .eq. 2)then
        OPEN(UNIT=10, FILE='fort.10', FORM='FORMATTED')
        do i = 0 , 100
        do j = 0 , 100
          read(10,*)x
          if(x .gt. hmax)hmax = x
        enddo
        enddo
        CLOSE(UNIT=10, STATUS='KEEP')
        OPEN(UNIT=10, FILE='fort.10', FORM='FORMATTED')
        do i = 0 , 100
        do j = 0 , 100
          read(10,*)x
          if(x .ge. hmax)then
            hmax = x
            kount = kount + 1
            ik(kount) = i
            jk(kount) = j
          end if
        enddo
        enddo
        do i = 1 , kount
          write(*,*)" max at ",ik(i),jk(i),hmax
        enddo
      end if
      if(irank .eq. 3)then
        OPEN(UNIT=10, FILE='fort.10', FORM='FORMATTED')
        do while (kdec .eq. 0) 
          read(10,*,end=1,err=1)k1,k2,k3,x
          if(x .gt. hmax)hmax = x
        enddo
1       continue
        CLOSE(UNIT=10, STATUS='KEEP')
        OPEN(UNIT=10, FILE='fort.10', FORM='FORMATTED')
        do while (kdec .eq. 0) 
          read(10,*,end=11,err=11)k1,k2,k3,x
          if(x .ge. hmax)then
            hmax = x
            kount = kount + 1
            ik(kount) = k1
            jk(kount) = k2
            kk(kount) = k3
          end if
        enddo
11      continue
        do i = 1 , kount
          write(*,*)" max at ",ik(i),jk(i),kk(i),hmax
        enddo
      end if
      if(irank .eq. 4)then
        OPEN(UNIT=10, FILE='fort.10', FORM='FORMATTED')
        do while (kdec .eq. 0) 
          read(10,*,end=2,err=2)k1,k2,k3,k4,x
          if(x .gt. hmax)hmax = x
        enddo
2       continue
        CLOSE(UNIT=10, STATUS='KEEP')
        OPEN(UNIT=10, FILE='fort.10', FORM='FORMATTED')
        do while (kdec .eq. 0) 
          read(10,*,end=22,err=22)k1,k2,k3,k4,x
          if(x .ge. hmax)then
            hmax = x
            kount = kount + 1
            ik(kount) = k1
            jk(kount) = k2
            kk(kount) = k3
            lk(kount) = k4
          end if
        enddo
22      continue
        do i = 1 , kount
          write(*,*)" max at ",ik(i),jk(i),kk(i),lk(i),hmax
        enddo
      end if
      END
