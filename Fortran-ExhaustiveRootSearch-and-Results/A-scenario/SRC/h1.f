      IMPLICIT REAL*8 (A-H,O-Z)
      do i = 1 , 1000
        read(10,*,err=1,end=1)x
        write(11,'(i5," ,",f40.1)')i,x
      enddo
1     continue
      END
