      PROGRAM SXHIST
      IMPLICIT REAL*8 (A-H,O-Z)
      parameter (eps = 1.0d-10)
      parameter(NRMAX=6,NHMAX=100)
      dimension kref(3)
      dimension sroot(NRMAX),xroot(NRMAX)
      dimension hs1(0:NHMAX),hs2(0:NHMAX,0:NHMAX)
      dimension hs3(0:NHMAX,0:NHMAX,0:NHMAX)
      dimension hs4(0:NHMAX,0:NHMAX,0:NHMAX,0:NHMAX)
!      common / HI1 / xl(NHMAX),xh(NHMAX)
903   format(3f6.2,f6.1,f6.2,5x,3f6.2,2f6.1,2f6.2,2x,i5,6i10)


      read(10,*)IUNIT

      if(IUNIT .EQ. 21)then
        OPEN(UNIT=21, FILE='res-s-1.dat', FORM='FORMATTED')
        OPEN(UNIT=31, FILE='hist-s-1.dat', FORM='FORMATTED')
      end if
      if(IUNIT .EQ. 22)then
        OPEN(UNIT=22, FILE='res-s-2.dat', FORM='FORMATTED')
        OPEN(UNIT=32, FILE='hist-s-2.dat', FORM='FORMATTED')
      end if
      if(IUNIT .EQ. 23)then
        OPEN(UNIT=23, FILE='res-s-3.dat', FORM='FORMATTED')
        OPEN(UNIT=33, FILE='hist-s-3.dat', FORM='FORMATTED')
      end if
      if(IUNIT .EQ. 24)then
        OPEN(UNIT=24, FILE='res-s-4.dat', FORM='FORMATTED')
        OPEN(UNIT=34, FILE='hist-s-3.dat', FORM='FORMATTED')
      end if

      do i = 0 , NHMAX
        hs1(i) = 0.0
      enddo
      do i = 0 , NHMAX
      do j = 0 , NHMAX
        hs2(i,j) = 0.0
      enddo
      enddo
      do i = 0 , NHMAX
      do j = 0 , NHMAX
      do k = 0 , NHMAX
        hs3(i,j,k) = 0.0
      enddo
      enddo
      enddo
      do i = 0 , NHMAX
      do j = 0 , NHMAX
      do k = 0 , NHMAX
      do l = 0 , NHMAX
        hs4(i,j,k,l) = 0.0
      enddo
      enddo
      enddo
      enddo
! --------------------------------------------------------------------------------------------------------------------------------
!                                                                                                                read-in data loop
      kdec = 0
      do while (kdec .eq. 0) 
        read(IUNIT,*,end=1,err=1)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,nb,(sroot(k),k=1,nb)
! --------------------------------------------------------------------------------------------------------------------------------
!                                                                                                                 ner of roots = 1
        if(nb .eq. 1)then 
          s1 = dabs(sroot(1))
          ks1 = 0
          CALL HISTBIN(s1,ks1)
          hs1(ks1) = hs1(ks1) + 1.0
        end if
! --------------------------------------------------------------------------------------------------------------------------------
!                                                                                                                 ner of roots = 2
        if(nb .eq. 2)then 
          ks1 = 0
          ks2 = 0
          s1 = dabs(sroot(1))
          s2 = dabs(sroot(2))
          CALL HISTBIN(s1,ks1)
          CALL HISTBIN(s2,ks2)
          hs2(ks1,ks2) = hs2(ks1,ks2) + 1.0
        end if
! --------------------------------------------------------------------------------------------------------------------------------
!                                                                                                                 ner of roots = 3
        if(nb .eq. 3)then 
          ks1 = 0
          ks2 = 0
          ks3 = 0
          s1 = dabs(sroot(1))
          s2 = dabs(sroot(2))
          s3 = dabs(sroot(3))
          CALL HISTBIN(s1,ks1)
          CALL HISTBIN(s2,ks2)
          CALL HISTBIN(s3,ks3)
          hs3(ks1,ks2,ks3) = hs3(ks1,ks2,ks3) + 1.0
        end if
! --------------------------------------------------------------------------------------------------------------------------------
!                                                                                                                 ner of roots = 4
        if(nb .eq. 4)then 
          ks1 = 0
          ks2 = 0
          ks3 = 0
          ks4 = 0
          s1 = dabs(sroot(1))
          s2 = dabs(sroot(2))
          s3 = dabs(sroot(3))
          s4 = dabs(sroot(4))
          CALL HISTBIN(s1,ks1)
          CALL HISTBIN(s2,ks2)
          CALL HISTBIN(s3,ks3)
          CALL HISTBIN(s4,ks4)
          hs4(ks1,ks2,ks3,ks4) = hs4(ks1,ks2,ks3,ks4) + 1.0
        end if
!                                                                                                       end of: read-in data loop
! --------------------------------------------------------------------------------------------------------------------------------
      enddo
1     continue
! --------------------------------------------------------------------------------------------------------------------------------
!                                                                                                                   write out data
      if(IUNIT .eq. 21)then
        do i = 0 , NHMAX
!          write(31,"(i5,f40.1)")i,hs1(i)
          write(31,"(f40.1)")hs1(i)
        enddo
      end if
      if(IUNIT .eq. 22)then
        do i = 0 , NHMAX
        do j = 0 , NHMAX
!          write(32,"(2i5,f40.1)")i,j,hs2(i,j)
          write(32,"(f40.1)")hs2(i,j)
        enddo
        enddo
      end if
      if(IUNIT .eq. 23)then
        do i = 0 , NHMAX
        do j = 0 , NHMAX
        do k = 0 , NHMAX
          if(hs3(i,j,k) .gt. 0.5) write(33,"(3i5,f40.1)")i,j,k,hs3(i,j,k)
        enddo
       enddo
        enddo
      end if
      if(IUNIT .eq. 24)then
        do i = 0 , NHMAX
          do j = 0 , NHMAX
            do k = 0 , NHMAX
              do l = 0 , NHMAX
                if(hs4(i,j,k,l) .gt. 0.5) write(34,"(4i5,f40.1)")i,j,k,l,hs4(i,j,k,l)
              enddo
            enddo
          enddo
        enddo
      end if
      END
!
! --------------------------------------------------------------------------------------------------------------------------------
! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! --------------------------------------------------------------------------------------------------------------------------------
      SUBROUTINE HISTBIN(XIN,KOUT)
      IMPLICIT REAL*8 (A-H,O-Z)
      tmp = (100.0d0 * XIN + 0.5d0)
      kout = int(tmp)
      return
      end

