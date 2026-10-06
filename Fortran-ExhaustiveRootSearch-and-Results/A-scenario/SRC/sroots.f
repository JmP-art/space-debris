      PROGRAM SROOTS
!
! C (S) = 1.0 x Omega^-1 * [aa + (1 - aa) * FermiUP(S,CS,betC) S^sigC ]
!
! B(S)= b x Norm x s^SL x (1 - s)^SR * FermiUP(S,p,bet1) x FermiDO(S,q,bet2)
!
! --------------------------------------------------------------------------------------------------------------------------------
! program to compute the ROOTS of benefit(s) - cost(s) (Space-Debris model) in the interval [0,1]
! --------------------------------------------------------------------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      parameter (eps = 1.0d-10)
      parameter(N=100,NRMAX=6,NBMAX=20,X1=eps,X2=1.0d0-eps)
      parameter(Y1=0.0d0,Y2=1.0d0)
! common blocks
      common / IOP / IOPT
      common / BR1 / b0,SigL,SigR,sl,sh,betl,beth,bnorm
      common / CR1 / c0,aa,cs,betc,sigc,cnorm
      common / SR1 / s0,sigs
      common / ELS / tconfs(0:NRMAX)
      common / OPT / XKOUNT0,XKOUNT1,XKOUNT2,XKOUNT3
      EXTERNAL ubens
      EXTERNAL ucosts

      OPEN(UNIT=10, FILE='inp-s.dat', FORM='FORMATTED')
      OPEN(UNIT=21, FILE='res-s-1.dat', FORM='FORMATTED',ACCESS='APPEND')
      OPEN(UNIT=22, FILE='res-s-2.dat', FORM='FORMATTED',ACCESS='APPEND')
      OPEN(UNIT=23, FILE='res-s-3.dat', FORM='FORMATTED',ACCESS='APPEND')
      OPEN(UNIT=24, FILE='res-s-4.dat', FORM='FORMATTED',ACCESS='APPEND')
      OPEN(UNIT=25, FILE='res-s-5.dat', FORM='FORMATTED',ACCESS='APPEND')
      OPEN(UNIT=26, FILE='res-s-6.dat', FORM='FORMATTED',ACCESS='APPEND')
      do k = 0 , NRMAX
        tconfs(k) = 0.0d0
      enddo
! --------------------------------------------------------------------------------------------------------------------------------
! NOTE: the code always imposes that configurations for which 
!       [benefit(s) - cost(s)] > 0 when s -> 0 are ignored
!
! IOPT < 0 : impose that [benefit(s) - cost(s)] < 0 when s -> 1 
! IOPT > 0 : impose that [benefit(s) - cost(s)] > 0 when s -> 1 
! --------------------------------------------------------------------------------------------------------------------------------
!
! 01 # 0.00  .le.   aa   .le. 1.00 (delta.aa    =  0.10 ) / 11 configs
! 02 # 0.10  .le.   cs   .le. 0.50 (delta.cs    =  0.05 ) / 09 configs
! 03 # 5.00  .le.  betc  .le. 85.0 (delta.betc  = 10.00 ) / 09 configs
! 04 # 0.05  .le.  sigC  .le. 0.85 (delta.sigC  =  0.10 ) / 08 configs
!
! total number of cost    configs:         71 280
!
      daa   =  0.10d0
      dcs   =  0.05d0
      dbetc = 10.00d0
      dsigc =  0.10d0 

      aa_0   =  0.00d0
      cs_0   =  0.10d0
      betc_0 =  5.00d0
      sigc_0 =  0.05d0 
!
! ---------------------
!
! 05 # 0.10  .le.    b   .le. 1.00 (delta.b    = 0.15 ) / 07 configs
! 06 # 0.00  .le.  SigL  .le. 2.00 (delta.SL   = 0.05 ) / 05 configs
! 07 # 0.00  .le.  SigR  .le. 1.00 (delta.SR   = 1.00 ) / 02 configs
! 08 # 10.0  .le.  bet1  .le. 80.0 (delta.bet1 = 10   ) / 08 configs
! 09 # 10.0  .le.  bet2  .le. 80.0 (delta.bet2 = 10   ) / 08 configs
! 10 # 0.10  .le.    p   .le. 0.40 (delta.p    = 0.10 ) / 04 configs
! 11 # 0.75  .le.    q   .le. 0.85 (delta.q    = 0.10 ) / 03 configs (the 3rd conf is q = 2)
!
! total number of benefit configs:         53 760

      db0   =  0.15d0
      dSigL =  0.05d0
      dSigR =  1.00d0
      dbetl = 10.00d0
      dbeth = 10.00d0
      dsl   =  0.10d0
      dsh   =  0.10d0

      b0_0   =  0.10d0
      SigL_0 =  0.00d0
      SigR_0 =  0.00d0
      betl_0 =  10.0d0
      beth_0 =  10.0d0
      sl_0   =  0.10d0
      sh_0   =  0.75d0

!
! total number of         configs:  3 832 012 800
!
! --------------------------------------------------------------------------------------------------------------------------------
      read(10,*) IOPT
!      read(10,*) ic0 , iaa , ics , ibetc , isigc , ib0 , iSigL , iSigR , ibetl , ibeth , isl , ish
!      read(10,*) jc0 , jaa , jcs , jbetc , jsigc , jb0 , jSigL , jSigR , jbetl , jbeth , jsl , jsh
!      read(10,*) mc0 , maa , mcs , mbetc , msigc , mb0 , mSigL , mSigR , mbetl , mbeth , msl , msh
      read(10,*) iaa , ics , ibetc , isigc , ib0 , iSigL , iSigR , ibetl , ibeth , isl , ish
      read(10,*) jaa , jcs , jbetc , jsigc , jb0 , jSigL , jSigR , jbetl , jbeth , jsl , jsh
      read(10,*) maa , mcs , mbetc , msigc , mb0 , mSigL , mSigR , mbetl , mbeth , msl , msh
! --------------------------------------------------------------------------------------------------------------------------------
      IDEC = 1
      XKOUNT0 = 0.0d0
      XKOUNT1 = 0.0d0
      XKOUNT2 = 0.0d0
      XKOUNT3 = 0.0d0
      c0 = 1.0d0
! --------------------------------------------------------------------------------------------------------------------------------
!                                                                              !               loop over cost parameters
!      do kc0 = ic0 , jc0 , mc0                                                 ! loop in c0
!        c0 = c0_0 + dfloat(kc0-1) * dc0
        do kaa = iaa , jaa , maa                                               ! loop in aa
          write(*,"('    xKOUNTi :',4f40.0)")XKOUNT0,XKOUNT1,XKOUNT2,XKOUNT3
          aa = aa_0 + dfloat(kaa-1) * daa
            do kcs = ics , jcs , mcs                                           ! loop in cs
              cs = cs_0 + dfloat(kcs-1) * dcs
                do kbetc = ibetc , jbetc , mbetc                               ! loop in betc
                  betc = betc_0 + dfloat(kbetc-1) * dbetc
                    do ksigc = isigc , jsigc , msigc                           ! loop in sigc
                      sigc = sigc_0 + dfloat(ksigc-1) * dsigc
!                     ---------------------------------------------------------! normalizing the cost function
                      cnorm = 1.0d0
                      res = 0.0d0
                      CALL QSIMP(ucosts,Y1,Y2,res)
                      cnorm = res
! --------------------------------------------------------------------------------------------------------------------------------
!                                                                              !               loop over benefit parameters
                      do kb0 = ib0 , jb0 , mb0                                 ! loop in b0
                        b0 = b0_0 + dfloat(kb0) * db0
                        if(b0 .gt. c0) GOTO 1                                  ! enforce that <benefit(s)> <= <cost(s)>
                        do kSigL = iSigL , jSigL , mSigL                       ! loop in SigL
                          SigL = SigL_0 + dfloat(kSigL-1) * dSigL
                          do kbetl = ibetl , jbetl , mbetl                     ! loop in betL
                            betl = betl_0 + dfloat(kbetl-1) * dbetl
                            do ksl = isl , jsl , msl                           ! loop in sL
                              sl = sl_0 + dfloat(ksl-1) * dsl
                              do ksh = ish , jsh , msh                         ! loop in sH
                                sh = sh_0 + dfloat(ksh-1) * dsh
                                if(ksh .eq. jsh) then
!                                 ------------------------------------------------------------------------------------------------
                                  sh = 2.0d0
                                  beth = 10.0d0
                                  SigR = 0.0d0
!                                 ---------------------------------------------! normalizing the benefit function
                                  bnorm = 1.0d0
                                  res = 0.0d0
                                  CALL QSIMP(ubens,Y1,Y2,res)
                                  bnorm = res
                                  XKOUNT0 = XKOUNT0 + 1.0d0

                                  CALL GETROOTS
!                                 ------------------------------------------------------------------------------------------------
                                else
!                                 ------------------------------------------------------------------------------------------------
                                  do kSigR = iSigR , jSigR , mSigR             ! loop in SigR
                                    SigR = SigR_0 + dfloat(kSigR-1) * dSigR
                                    do kbeth = ibeth , jbeth , mbeth           ! loop in betH
                                      beth = beth_0 + dfloat(kbeth-1) * dbeth
!                                     -----------------------------------------! normalizing the benefit function
                                      bnorm = 1.0d0
                                      res = 0.0d0
                                      CALL QSIMP(ubens,Y1,Y2,res)
                                      bnorm = res
                                      XKOUNT0 = XKOUNT0 + 1.0d0

                                      CALL GETROOTS
                                    enddo                                      ! end_of: loop in betH
                                  enddo                                        ! end_of: loop in SigR
!                                 ------------------------------------------------------------------------------------------------
                                end if
                              enddo                                            ! end_of: loop in sH
                            enddo                                              ! end_of: loop in sL
                          enddo                                                ! end_of: loop in betL
                        enddo                                                  ! end_of: loop in SigL
1                       continue
                      enddo                                                    ! end_of: loop in b0
! --------------------------------------------------------------------------------------------------------------------------------
              enddo                                                            ! end_of: loop in sigc
            enddo                                                              ! end_of: loop in betc
          enddo                                                                ! end_of: loop in cs
        enddo                                                                  ! end_of: loop in aa
!      enddo                                                                    ! end_of: loop in c0
      write(*,"('    xKOUNTi :',4f40.0)")XKOUNT0,XKOUNT1,XKOUNT2,XKOUNT3
      write(*,*)"----------------------------------------------------------------------------------------------------"
      write(*,*)"----------------------------------------------------------------------------------------------------"
      write(*,*)"----------------------------------------------------------------------------------------------------"
      write(*,*)" DATA INFO FOR PROGRAM SROOTS TO BE WRITTEN TO NOHUP.OUT" 
      write(*,*)"----------------------------------------------------------------------------------------------------"
      write(*,'("number of configs attempted in SROOTS  : ",f18.0)')XKOUNT0
      write(*,'("ner of confs w/ b(s)-c(s) > 0 as s-> 0 : ",f18.0)')XKOUNT1
      if(IOPT .lt. 0)write(*,'("ner of confs w/ b(s)-c(s) > 0 as s-> 1 : ",f18.0)')XKOUNT2
      if(IOPT .gt. 0)write(*,'("ner of confs w/ b(s)-c(s) < 0 as s-> 1 : ",f18.0)')XKOUNT2
      write(*,'("number of configs checked   in SROOTS  : ",f18.0)')XKOUNT3
      xyz = 0.0d0
      do j = NRMAX , 0 , -1
        xyz = xyz + tconfs(j)
        if(tconfs(j) .gt. eps)write(*,'("number configs with",i3,"  roots          : ",f18.0)')j,tconfs(j)
      enddo
      END
!
! --------------------------------------------------------------------------------------------------------------------------------
! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! --------------------------------------------------------------------------------------------------------------------------------
      SUBROUTINE GETROOTS
      IMPLICIT REAL*8 (A-H,O-Z)
      parameter (eps = 1.0d-10)
      parameter(NI=100,NRMAX=6,X1=eps,X2=1.0d0-eps)
      dimension xb1(NRMAX),xb2(NRMAX),sroot(NRMAX)
      common / IOP / IOPT
      common / BR1 / b0,SigL,SigR,sl,sh,betl,beth,bnorm
      common / CR1 / c0,aa,cs,betc,sigc,cnorm
      common / SR1 / s0,sigs
      common / ELS / tconfs(0:NRMAX)
      common / OPT / XKOUNT0,XKOUNT1,XKOUNT2,XKOUNT3
      EXTERNAL benmcosts
! --------------------------------------------------------------------------------------------------------------------------------
! NOTE: the code always imposes that configurations for which 
!       [benefit(s) - cost(s)] > 0 when s -> 0 are ignored
!
      if(benmcosts(X1) .gt. 0.0d0) then
        XKOUNT1 = XKOUNT1 + 1.0d0
        RETURN
      end if
!
! IOPT < 0 : impose that [benefit(s) - cost(s)] < 0 when s -> 1 
! IOPT > 0 : impose that [benefit(s) - cost(s)] > 0 when s -> 1 
!
      if(IOPT .lt. 0 .and. benmcosts(X2) .gt. 0.0d0) then
        XKOUNT2 = XKOUNT2 + 1.0d0
        RETURN
      end if
      if(IOPT .gt. 0 .and. benmcosts(X2) .lt. 0.0d0) then
        XKOUNT2 = XKOUNT2 + 1.0d0
        RETURN
      end if
! --------------------------------------------------------------------------------------------------------------------------------
      XKOUNT3 = XKOUNT3 + 1.0d0      
! --------------------------------------------------------------------------------------------------------------------------------
!                                                                                              bracketing the roots of b(s) - c(s)
      nb = NRMAX
      do i = 1 , NRMAX
        xb1(i) = 0.0d0
        xb2(i) = 0.0d0
      enddo
!
      CALL zbrak(benmcosts,X1,X2,NI,xb1,xb2,nb)

! --------------------------------------------------------------------------------------------------------------------------------
!                                                                              determining the roots of b(s) - c(s) & their nature
      tconfs(nb) = tconfs(nb) + 1.0d0
      if(nb .gt. 0)then
         do j = 1 , NRMAX
           sroot(j) = 0.0d0
        enddo
        do i = 1 , nb                                      ! check the nature of each root :
          typ = -1.0d0                                     ! unstable: location is < 0; stable: location is > 0
          if( benmcosts(xb1(i)) .lt. 0.0d0 ) typ = 1.0d0
          xacc=(1.0d-6)*(xb1(i)+xb2(i))/2.0d0
          root = RTBIS(benmcosts,xb1(i),xb2(i),xacc)
          sroot(i) = root*typ
        enddo
        if(nb .eq. 1)write(21,903)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,nb,(sroot(k),k=1,nb)
        if(nb .eq. 2)write(22,903)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,nb,(sroot(k),k=1,nb)
        if(nb .eq. 3)write(23,903)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,nb,(sroot(k),k=1,nb)
        if(nb .eq. 4)write(24,903)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,nb,(sroot(k),k=1,nb)
        if(nb .eq. 5)write(25,903)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,nb,(sroot(k),k=1,nb)
        if(nb .eq. 6)write(26,903)c0,aa,cs,betc,sigc,b0,SigL,SigR,betl,beth,sl,sh,nb,(sroot(k),k=1,nb)
      end if
903   format(3f6.2,f6.1,f6.2,5x,3f6.2,2f6.1,2f6.2,2x,i5,6f10.4)
      RETURN
      END
!
! --------------------------------------------------------------------------------------------------------------------------------
! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! --------------------------------------------------------------------------------------------------------------------------------
      REAL*8 FUNCTION ubens(s)
! --------------------------------------------------------------------------------------------------------------------------------
!                                                                                                un-normalized function benefit(s)
      IMPLICIT REAL*8 (A-H,O-Z)
      parameter (eps = 1.0d-10)
! common blocks
      common / BR1 / b0,SigL,SigR,sl,sh,betl,beth,bnorm

      ferup = 1.0d0 / (1.0d0 + dexp(-betl*(s-sl))) 
      ferdo = 1.0d0 / (1.0d0 + dexp( beth*(s-sh))) 
      ubens = s**SigL * (1 - s)**SigR * ferup * ferdo
      return
      END
!
! --------------------------------------------------------------------------------------------------------------------------------
! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! --------------------------------------------------------------------------------------------------------------------------------
      REAL*8 FUNCTION bens(s)
! --------------------------------------------------------------------------------------------------------------------------------
!                                                                                                   normalized function benefit(s)
      IMPLICIT REAL*8 (A-H,O-Z)
      parameter (eps = 1.0d-10)
! common blocks
      common / BR1 / b0,SigL,SigR,sl,sh,betl,beth,bnorm
      bens = b0 * ubens(s) / bnorm
      return
      END
!
! --------------------------------------------------------------------------------------------------------------------------------
! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! --------------------------------------------------------------------------------------------------------------------------------
      REAL*8 FUNCTION ucosts(s)
! --------------------------------------------------------------------------------------------------------------------------------
!                                                                                                                 function cost(s)
      IMPLICIT REAL*8 (A-H,O-Z)
      parameter (eps = 1.0d-10)
! common blocks
      common / CR1 / c0,aa,cs,betc,sigc,cnorm

      ferup = 1.0d0 / (1.0d0 + dexp(-betc*(s-cs))) 
      ucosts = ( aa + (1.0d0 - aa) * ferup * s**sigc )
      return
      END
!
! --------------------------------------------------------------------------------------------------------------------------------
! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! --------------------------------------------------------------------------------------------------------------------------------
      REAL*8 FUNCTION costs(s)
! --------------------------------------------------------------------------------------------------------------------------------
!                                                                                                                 function cost(s)
      IMPLICIT REAL*8 (A-H,O-Z)
      parameter (eps = 1.0d-10)
! common blocks
      common / CR1 / c0,aa,cs,betc,sigc,cnorm

      costs = c0 * ucosts(s) / cnorm

      return
      END
!
! --------------------------------------------------------------------------------------------------------------------------------
! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! --------------------------------------------------------------------------------------------------------------------------------
      REAL*8 FUNCTION benmcosts(s)
! --------------------------------------------------------------------------------------------------------------------------------
!                                                                                                    function benefit(s) - cost(s)
      IMPLICIT REAL*8 (A-H,O-Z)
      parameter (eps = 1.0d-10)
      common / BR1 / b0,SigL,SigR,sl,sh,betl,beth,bnorm
      common / CR1 / c0,aa,cs,betc,sigc,cnorm
      benmcosts = bens(s) - costs(s)
      return
      END
!
! --------------------------------------------------------------------------------------------------------------------------------
! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! --------------------------------------------------------------------------------------------------------------------------------
      REAL*8 FUNCTION ss(x)
! --------------------------------------------------------------------------------------------------------------------------------
!                                                                                                                    function s(x)
      IMPLICIT REAL*8 (A-H,O-Z)
      parameter (eps = 1.0d-10)
      common / BR1 / b0,SigL,SigR,sl,sh,betl,beth,bnorm
      common / CR1 / c0,aa,cs,betc,sigc,cnorm
      common / SR1 / s0,sigs
      ss = s0 + (1.0 d0 - s0) * (1.0d0 - x)**sigs 
      return
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
      common / BR1 / b0,SigL,SigR,sl,sh,betl,beth,bnorm
      common / CR1 / c0,aa,cs,betc,sigc,cnorm
      common / SR1 / s0,sigs
      sm1 = 1.0d0 / sigs
      xx = (s - s0) / (1.0d0 - s0)
      xx = 1.0d0 - xx**sm1
      return
      END
! --------------------------------------------------------------------------------------------------------------------------------
! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! --------------------------------------------------------------------------------------------------------------------------------
!                                               NUMERICAL RECIPES BLOCK OF ROUTINES
! --------------------------------------------------------------------------------------------------------------------------------
! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! --------------------------------------------------------------------------------------------------------------------------------
      SUBROUTINE zbrak(fx,x1,x2,n,xb1,xb2,nb)
! Given a function fx defined on the interval [x1,x2] subdivide the interval into n equally spaced segments, and search for 
! zero crossings of the function. 
! nb is input as the maximum number of roots sought, and is reset to the number of bracketing pairs 
! xb1(1:nb), xb2(1:nb) that are found.
      INTEGER n,nb
      DOUBLE PRECISION x1,x2,xb1(nb),xb2(nb),fx
      EXTERNAL fx
      INTEGER i,nbb
      DOUBLE PRECISION dx,fc,fp,x
      nbb=0
      x=x1
      dx=(x2-x1)/n
      fp=fx(x)
      do 11 i=1,n
        x=x+dx
        fc=fx(x)
        if(fc*fp.le.0.d0) then
          nbb=nbb+1
          xb1(nbb)=x-dx
          xb2(nbb)=x
          if(nbb.eq.nb)goto 1
        endif
        fp=fc
11    continue
1     continue
      nb=nbb
      return
      END
!
! --------------------------------------------------------------------------------------------------------------------------------
! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! --------------------------------------------------------------------------------------------------------------------------------
      FUNCTION rtbis(func,x1,x2,xacc)
! Using bisection, find the root of a function func known to lie between x1 and x2. 
! The root, returned as rtbis, will be refined until its accuracy is ±xacc.
      INTEGER JMAX
      DOUBLE PRECISION rtbis,x1,x2,xacc,func
      EXTERNAL func
      PARAMETER (JMAX=40)
      INTEGER j,nb,kb
      DOUBLE PRECISION dx,f,fmid,xmid
      fmid=func(x2)
      f=func(x1)
      if(f*fmid.ge.0.d0)then
        write(*,*) "RTBIS : root must be bracketed",x1,x2,f,fmid

        STOP
      end if
      if(f.lt.0.d0)then
        rtbis=x1
        dx=x2-x1
      else
        rtbis=x2
        dx=x1-x2
      endif
      do 11 j=1,JMAX
        dx=dx*.5d0
        xmid=rtbis+dx
        fmid=func(xmid)
        if(fmid.le.0.d0)rtbis=xmid
        if(abs(dx).lt.xacc .or. fmid.eq.0.d0) return
11    continue
      write(*,*) "RTBIS : too many bisections"
      STOP
      END
!
! --------------------------------------------------------------------------------------------------------------------------------
! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! --------------------------------------------------------------------------------------------------------------------------------
      SUBROUTINE qtrap(func,a,b,s)
! Returns as s the integral of the function func from a to b. 
! The parameters EPS can be set to the desired fractional accuracy and JMAX so that 2 to the power JMAX-1 is the maximum allowed
! number of steps. Integration is performed by the trapezoidal rule.
      INTEGER JMAX
      DOUBLE PRECISION a,b,func,s,EPS
      EXTERNAL func
      PARAMETER (EPS=1.d-10, JMAX=20)
CU    USES trapzd
      INTEGER j
      DOUBLE PRECISION olds
      olds=0.d0
      do 11 j=1,JMAX
        call trapzd(func,a,b,s,j)
        if (j.gt.5) then
          if (abs(s-olds).lt.EPS*abs(olds).or.
     *(s.eq.0.d0.and.olds.eq.0.d0)) 
     *return
        endif
        olds=s
11    continue
      write(*,"('  QTRAP : value of integral after ',i5,' steps = ',f20.5)")JMAX,s
      STOP
      END
!
! --------------------------------------------------------------------------------------------------------------------------------
! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! --------------------------------------------------------------------------------------------------------------------------------
      SUBROUTINE qsimp(func,a,b,s)
! Returns as s the integral of the function func from a to b. 
! The parameters EPS can be set to the desired fractional accuracy and JMAX so that 2 to the power JMAX-1 is the maximum allowed 
! number of steps. 
! Integration is performed by Simpson’s rule
      INTEGER JMAX
      DOUBLE PRECISION a,b,func,s,EPS
      EXTERNAL func
corig      PARAMETER (EPS=1.d-10, JMAX=20)
      PARAMETER (EPS=1.d-6, JMAX=20)
CU    USES trapzd
      INTEGER j
      DOUBLE PRECISION os,ost,st
!debug      DOUBLE PRECISION res(JMAX)
      ost=0.d0
      os=0.d0
      do 11 j=1,JMAX
        call trapzd(func,a,b,st,j)
        s=(4.d0*st-ost)/3.d0
!debug        res(j) = s
        if (j.gt.5) then
          if (abs(s-os).lt.EPS*abs(os).or.(s.eq.0.d0.and.os.eq.0.d0))
     * return
        endif
        os=s
        ost=st
11    continue
      write(*,"('  QSIMP : value of integral after ',i5,' steps = ',f20.6)")JMAX,s
!debug      do 12 j = 1 , JMAX
!debug        write(*,"('  QSIMP : value of integral after ',i5,' steps = ',f50.16)")j,res(j)
!debug12    continue
      STOP
      END
!
! --------------------------------------------------------------------------------------------------------------------------------
! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! --------------------------------------------------------------------------------------------------------------------------------
      SUBROUTINE trapzd(func,a,b,s,n)
! This routine computes the nth stage of refinement of an extended trapezoidal rule. 
! func is input as the name of the function to be integrated between limits a and b, also input. 
! When called with n=1, the routine returns as s the crudest estimate of Integral_a^b f(x)dx. 
! Subsequent calls with n=2,3 (in that sequential order) will improve the accuracy of s by adding 2n-2 additional interior points. 
! s should not be modified between sequential calls.
      INTEGER n
      DOUBLE PRECISION a,b,s,func
      EXTERNAL func
      INTEGER it,j
      DOUBLE PRECISION del,sum,tnm,x
      if (n.eq.1) then
        s=0.5d0*(b-a)*(func(a)+func(b))
      else
        it=2**(n-2)
        tnm=it
        del=(b-a)/tnm
        x=a+0.5d0*del
        sum=0.d0
        do 11 j=1,it
          sum=sum+func(x)
          x=x+del
11      continue
        s=0.5d0*(s+(b-a)*sum/tnm)
      endif
      return
      END
