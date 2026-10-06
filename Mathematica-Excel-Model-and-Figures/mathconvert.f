      IMPLICIT REAL*8 (A-H,O-Z)
      character*40  atext
      character*120 btext
      character*120 BLINE
      dimension sroot(6)
      OPEN(UNIT=10, FILE='config.inp', FORM='FORMATTED')
      OPEN(UNIT=20, FILE='config.dat', FORM='FORMATTED')
!------------------------------------------------------------------------------------------
!" B(s) = b . OmegaB^-1 . s^SL . (1 - s)^SR . FermiUP(S,tauL,betL) . FermiDO(S,tauR,betR) "
!" C(s) = OmegaC^-1 . [aa + (1 - aa) . FermiUP(s,tauC,betC) s^sC ]                        "
!" Sdot(x) = Ts . s . (1-s) .                                                             "
!" { (1-x) [pS . FermiDO(x,tauS,betaS) + s . FermiUP(x,tauS,betaS) ] - x . s^alfS }       "
!------------------------------------------------------------------------------------------
      read(10,*)BLINE
      write(*,*)BLINE
        read(10,*)btext
        write(*,*)btext
        read(10,*)btext
        write(*,*)btext
        read(10,*)btext
        write(*,*)btext
      read(10,*)BLINE
      write(*,*)BLINE

!                           benefit parameters
      read(10,*)BLINE
      write(*,*)BLINE
        read(10,*)btext
        write(*,*)btext
      read(10,*)BLINE
      write(*,*)BLINE
                           read(10,*)atext,b  
                           write(*,*)atext,b 
      read(10,*)BLINE
      write(*,*)BLINE
                            read(10,*)atext,SigL
                            write(*,*)atext,SigL
      read(10,*)BLINE
      write(*,*)BLINE
                            read(10,*)atext,SigR
                            write(*,*)atext,SigR
      read(10,*)BLINE
      write(*,*)BLINE
                            read(10,*)atext,tauL
                            write(*,*)atext,tauL
      read(10,*)BLINE
      write(*,*)BLINE
                            read(10,*)atext,tauR
                            write(*,*)atext,tauR
      read(10,*)BLINE
      write(*,*)BLINE
                            read(10,*)atext,betL
                            write(*,*)atext,betL
      read(10,*)BLINE
      write(*,*)BLINE
                            read(10,*)atext,betR
                            write(*,*)atext,betR

!                           cost parameters

      read(10,*)BLINE
      write(*,*)BLINE
        read(10,*)btext
        write(*,*)btext
      read(10,*)BLINE
      write(*,*)BLINE
                            read(10,*)atext,aa
                            write(*,*)atext,aa
      read(10,*)BLINE
      write(*,*)BLINE
                            read(10,*)atext,tauC
                            write(*,*)atext,tauC
      read(10,*)BLINE
      write(*,*)BLINE
                            read(10,*)atext,betC
                            write(*,*)atext,betC
      read(10,*)BLINE
      write(*,*)BLINE
                            read(10,*)atext,sC
                            write(*,*)atext,sC

!                           time scale parameter Ts

      read(10,*)BLINE
      write(*,*)BLINE
        read(10,*)btext
        write(*,*)btext
      read(10,*)BLINE
      write(*,*)BLINE
                            read(10,*)atext,ttts
                            write(*,*)atext,ttts

!                           sdot parameters

      read(10,*)BLINE
      write(*,*)BLINE
                            read(10,*)btext
                            write(*,*)btext
      read(10,*)BLINE
      write(*,*)BLINE
                            read(10,*)atext,tauS
                            write(*,*)atext,tauS
      read(10,*)BLINE
      write(*,*)BLINE
                            read(10,*)atext,betaS
                            write(*,*)atext,betaS
      read(10,*)BLINE
      write(*,*)BLINE
                            read(10,*)atext,pS
                            write(*,*)atext,pS
      read(10,*)BLINE
      write(*,*)BLINE
                            read(10,*)atext,alfS
                            write(*,*)atext,alfS
! -------------------------------------------------------------------------------------------------
!                           benefit parameters
      write(20,"(f20.6)")b 
      write(20,"(f20.6)")SigL
      write(20,"(f20.6)")SigR
      write(20,"(f20.6)")tauL
      write(20,"(f20.6)")tauR
      write(20,"(f20.6)")betL
      write(20,"(f20.6)")betR

!                           cost parameters
      write(20,"(f20.6)")aa
      write(20,"(f20.6)")tauC
      write(20,"(f20.6)")betC
      write(20,"(f20.6)")sC
!                           time scale parameter Ts

      write(20,"(f20.6)")ttts

!                           sdot parameters
      write(20,"(f20.6)")tauS
      write(20,"(f20.6)")betaS
      write(20,"(f20.6)")pS
      write(20,"(f20.6)")alfS
      END
