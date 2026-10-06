      PROGRAM CONFIGS
      IMPLICIT REAL*8 (A-H,O-Z)
      parameter (eps = 1.0d-10)
      parameter(NRMAX=6,NHMAX=100)
      dimension sroot(NRMAX),xroot(NRMAX)
!      common / HI1 / xl(NHMAX),xh(NHMAX)


      read(10,*)IUNIT

      if(IUNIT .EQ. 21)then
        read(11,*) kr1
        OPEN(UNIT=21, FILE='res-s-1.dat', FORM='FORMATTED')
        OPEN(UNIT=31, FILE='configs-s-1.dat', FORM='FORMATTED')
        OPEN(UNIT=41, FILE='configs-x-1.dat', FORM='FORMATTED')
      end if
      if(IUNIT .EQ. 22)then
        read(11,*) kr1,kr2
        OPEN(UNIT=22, FILE='res-s-2.dat', FORM='FORMATTED')
        OPEN(UNIT=32, FILE='configs-s-2.dat', FORM='FORMATTED')
        OPEN(UNIT=42, FILE='configs-x-2.dat', FORM='FORMATTED')
      end if
      if(IUNIT .EQ. 23)then
        read(11,*) kr1,kr2,kr3
        OPEN(UNIT=23, FILE='res-s-3.dat', FORM='FORMATTED')
        OPEN(UNIT=33, FILE='configs-s-3.dat', FORM='FORMATTED')
        OPEN(UNIT=43, FILE='configs-x-3.dat', FORM='FORMATTED')
      end if
      if(IUNIT .EQ. 24)then
        read(11,*) kr1,kr2,kr3,kr4
        OPEN(UNIT=24, FILE='res-s-4.dat', FORM='FORMATTED')
        OPEN(UNIT=34, FILE='configs-s-4.dat', FORM='FORMATTED')
        OPEN(UNIT=44, FILE='configs-x-4.dat', FORM='FORMATTED')
      end if

!      dx = 0.01d0
!      kt = 0
!      do i = 1 , NHMAX
!        xl(i) = dfloat(i-1) * dx
!        xh(i) = dfloat(i) * dx 
!      enddo
!      xh(NHMAX) = 1.0d0
!903   format(3f6.2,f6.1,f6.2,5x,3f6.2,2f6.1,2f6.2,2x,i5,6f10.4)
901   format(f6.2,",",f6.2,",",f6.2,",",f6.1,",",f6.2,",",f6.2,",",f6.2,",",f6.2,",",f6.1,",",f6.1,",",
     .f6.2,",",f6.2,",",i2,",",f10.4)
902   format(f6.2,",",f6.2,",",f6.2,",",f6.1,",",f6.2,",",f6.2,",",f6.2,",",f6.2,",",f6.1,",",f6.1,",",
     .f6.2,",",f6.2,",",i2,",",f10.4,",",f10.4)
903   format(f6.2,",",f6.2,",",f6.2,",",f6.1,",",f6.2,",",f6.2,",",f6.2,",",f6.2,",",f6.1,",",f6.1,",",
     .f6.2,",",f6.2,",",i2,",",f10.4,",",f10.4,",",f10.4)
904   format(f6.2,",",f6.2,",",f6.2,",",f6.1,",",f6.2,",",f6.2,",",f6.2,",",f6.2,",",f6.1,",",f6.1,",",
     .f6.2,",",f6.2,",",i2,",",f10.4,",",f10.4,",",f10.4,",",f10.4)
905   format(f6.2,",",f6.2,",",f6.2,",",f6.1,",",f6.2,",",f6.2,",",f6.2,",",f6.2,",",f6.1,",",f6.1,",",
     .f6.2,",",f6.2,",",i2,",",f10.4,",",f10.4,",",f10.4,",",f10.4,",",f10.4)
906   format(f6.2,",",f6.2,",",f6.2,",",f6.1,",",f6.2,",",f6.2,",",f6.2,",",f6.2,",",f6.1,",",f6.1,",",
     .f6.2,",",f6.2,",",i2,",",f10.4,",",f10.4,",",f10.4,",",f10.4,",",f10.4,",",f10.4)

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
          if(ks1 .eq. kr1)then
            typ = 1.0d0
            if(sroot(1) .lt. 0.0d0)typ = -1.0d0
            xroot(1) = typ * (1.0d0 - dabs(sroot(1)))
            write(31,901)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,nb,(sroot(k),k=1,nb)
            write(41,901)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,nb,(xroot(k),k=1,nb)
          end if
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
          if(ks1 .eq. kr1 .and. ks2 .eq. kr2)then
            do kk = 1 , nb
              typ = 1.0d0
              if(sroot(kk) .lt. 0.0d0)typ = -1.0d0
              xroot(kk) = typ * (1.0d0 - dabs(sroot(kk)))
            enddo 
            write(32,902)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,nb,(sroot(k),k=1,nb)
            write(42,902)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,nb,(xroot(k),k=nb,1,-1)
          end if
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
          if(ks1 .eq. kr1 .and. ks2 .eq. kr2 .and. ks3 .eq. kr3)then
            do kk = 1 , nb
              typ = 1.0d0
              if(sroot(kk) .lt. 0.0d0)typ = -1.0d0
              xroot(kk) = typ * (1.0d0 - dabs(sroot(kk)))
            enddo 
            write(33,903)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,nb,(sroot(k),k=1,nb)
            write(43,903)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,nb,(xroot(k),k=nb,1,-1)
          end if
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
          if(ks1 .eq. kr1 .and. ks2 .eq. kr2 .and. ks3 .eq. kr3 .and. ks4 .eq. kr4)then
            do kk = 1 , nb
              typ = 1.0d0
              if(sroot(kk) .lt. 0.0d0)typ = -1.0d0
              xroot(kk) = typ * (1.0d0 - dabs(sroot(kk)))
            enddo 
            write(34,904)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,nb,(sroot(k),k=1,nb)
            write(44,904)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,nb,(xroot(k),k=nb,1,-1)
          end if
        end if
!                                                                                                       end of: read-in data loop
! --------------------------------------------------------------------------------------------------------------------------------
      enddo
1     continue
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

