
*****************************************************************
*                                                               *
*     The following subroutines are written by N.S.Amelin,      *
*     Joint Institute for Nuclear Research, Dubna,              *
*     E-mail: amelin@sirius.jinr.ru                             *
*                                                               *
*****************************************************************

c************** last correction 16-11-95 05:31pm************
       SUBROUTINE READHH(LPRNT,ID1,AM1,PX1,PY1,PZ1,
     *ID2,AM2,PX2,PY2,PZ2,SIGTOT,SIGEX,SIGAN,SIGEL)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 AM1,PX1,PY1,PZ1,
     *       AM2,PX2,PY2,PZ2,SIGTOT,SIGEX,SIGAN,SIGEL
C
C          PRODUCED BY DR. N.S. AMELIN FROM LCTA OF JOINT INSTITUTE FOR
C          NUCLEAR RESEARCH. 1987-VERSION (QUARK GLUON STRING MODEL) FOR
C          LOW AND HIGH ENERGY HADRON COLLISION SIMULATION BY MONTE
C          CARLO METHOD
C
C
      COMMON/PRIMAR/SCM,HALFE,ECM,NJET,IDIN(2),NEVENT,NTRIES
      LOGICAL GH1H2
      COMMON/H1H2/ GH1H2(11)
      COMMON/COMTRE/ XMI,PTI,PTF,PTMAX,NSTART
      LOGICAL NOTRE
      COMMON/NOTRE/ NOTRE
      LOGICAL NODCAY,NOETA,NOPI0,NONUNU,NOEVOL,NOHADR,NOKA0
      COMMON/NODCAY/NODCAY,NOETA,NOPI0,NONUNU,NOEVOL,NOHADR,NOKA0
      COMMON/PRTOUT/NEVPRT,NJUMP
      LOGICAL MULTP
      COMMON/COMMUL/MULTP
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      LOGICAL LPRNT
      COMMON/COMENB/ ENBOU
      COMMON/COMFR/  ICMS
      COMMON/KEYHH/KEYHH
      LOGICAL KEYHH
      COMMON/COMQSE/QSEE,QVSEE
      LOGICAL  QSEE,QVSEE
C
       LPRNT=.FALSE.
       NOTRE=.TRUE.
       KEYHH=.TRUE.
       NODCAY=.FALSE.
       MULTP=.TRUE.
        QSEE =.FALSE.
C       IF LAB. FRAME
      ICMS=0
C       IF  C.M. FRAME
C     ICMS=1
      ENBOU= 4.4
      ID1=1120
      ID2=1120
      DO 1 I=1,11
1     GH1H2(I)=.TRUE.
      GH1H2(4)=.FALSE.
      AM1=AMASS(ID1)
      AM2=AMASS(ID2)
      PX1=0.
      PY1=0.
      PZ1=200.
      PX2=0.
      PY2=0.
      PZ2=0.
      INEL=0
      CALL CROSEC(0,ID1,ID2,PX1,PY1,PZ1,AM1,PX2,PY2,PZ2,
     *AM2,SIGEL,INEL)
      CALL CROSEC(1,ID1,ID2,PX1,PY1,PZ1,AM1,PX2,PY2,PZ2,
     *AM2,SIGTOT,INEL)
      CALL CROSEC(2,ID1,ID2,PX1,PY1,PZ1,AM1,PX2,PY2,PZ2,
     *AM2,SIGEX,INEL)
      CALL CROSEC(3,ID1,ID2,PX1,PY1,PZ1,AM1,PX2,PY2,PZ2,
     *AM2,SIGAN,INEL)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE   FORID(IDOLD,IDNEW)
      COMMON/ITAPES/ ITDKY,ITEVT,ITCOM,ITLIS
      DIMENSION IDENT(54)
      DATA IDENT/
     *     120 ,  -120 ,   130 ,  -130 ,   230 ,  -230 ,   110 ,
     *     220 ,   330 ,   121 ,  -121 ,   131 ,  -131 ,   231 ,
     *    -231 ,   111 ,   221 ,   331 ,
     *   -1120 , -1220 , -1130 , -2230 , -1230 , -2330 , -1330 ,
     *   -2130 , -1111 , -1121 , -2221 , -1221 , -1131 , -2231 ,
     *   -1231 , -2331 , -1331 , -3331 ,
     *    1120 ,  1220 ,  1130 ,  2230 ,  1230 ,  2330 ,  1330 ,
     *    2130 ,  1111 ,  1121 ,  2221 ,  1221 ,  1131 ,  2231 ,
     *    1231 ,  2331 ,  1331 ,  3331 /
      N=0
      DO  10  I=1,54
      IF(      I .NE.IDOLD)  GO TO 10
      N=I
      GO TO 20
   10 CONTINUE
   20 CONTINUE
c      IF(N.EQ.0) WRITE(ITLIS,991)IDOLD
  991 FORMAT(2X,'FORID:  IDOLD =',I5)
      IDNEW=IDENT(N)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE   BACKID(IDOLD,IDNEW)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/ITAPES/ ITDKY,ITEVT,ITCOM,ITLIS
C     REAL*8 PNAME,LAB
      CHARACTER*8 PNAME,LAB
      DIMENSION PNAME(54)
      DATA PNAME/
     *  'PI+  ','PI-  ','K+   ','K-   ','K0   ','AK0  ','PI0  ',
     *  'ETA  ','ETAP ','RHO+ ','RHO- ','K*+  ','K*-  ','K*0  ',
     *  'AK*0 ','RHO0 ','OMEG ','PHI  ',
     *  'AP   ','AN   ','AS+  ','AS-  ','AS0  ','AXI- ','AXI0 ',
     *  'AL   ','ADL++','ADL+ ','ADL- ','ADL0 ','AS*+ ','AS*- ',
     *  'AS*0 ','AXI*-','AXI*0','AOM- ',
     *  'P    ','N    ','S+   ','S-   ','S0   ','XI-  ','XI0  ',
     *  'L    ','DL++ ','DL+  ','DL-  ','DL0  ','S*+  ','S*-  ',
     *  'S*0  ','XI*- ','XI*0 ','OM-  '/
C
      CALL LABEL(LAB,IDOLD)
      N=0
      DO 100 I=1,54
      IF(LAB.NE.PNAME(I)) GO TO 100
      N=I
100   CONTINUE
      IDNEW=N
c      IF(N.EQ.0) WRITE(ITLIS,991) IDOLD
991   FORMAT(10X,'BACKID;IDOLD=',I6)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE MARK(IK01,IK02,KS)
      IMPLICIT REAL*8 (A-H,O-Z)
      IK1=IK01
      IK2=IK02
      IB1=IB(IK1)
      IB2=IB(IK2)
      IF(IB1.GE.0.OR.IB2.GE.0) GO TO 110
C     AB-AB -> B-B
      IK1=IABS(IK1)
      IK2=IABS(IK2)
      IF(IK2.EQ.1120.OR.IK2.EQ.1220) GO TO 112
      IF(IK1.NE.1120.AND.IK1.NE.1220) GO TO 111
      IK11=IK1
      IK1=IK2
      IK2=IK11
      GO TO 112
110   IF(IB1.NE.0.OR.IB2.GE.0) GO TO 111
C     M-AB -> AM-B
      IK2=IABS(IK2)
      IF(IK1/100.NE.MOD(IK1,100)/10) IK1=-IK1
111   IF(IK2.NE.1220) IK2=1120
112   IB1=IB(IK1)
      IB2=IB(IK2)
      IQ1=IDINT(CHARGE(IK1)*1.001)
      IQ2=IDINT(CHARGE(IK2)*1.001)
      MQ=IQ1+IQ2
      IF(IB1+IB2.LE.1) GO TO 1
C   NUCLEON-NUCLEON COLLISION
      IF(MQ-1) 3,4,3
C  MESON-NUCLEON COLLISION
 1    IF(MQ.EQ.2.OR.MQ.EQ.-1) GO TO 3
      IF(MQ.EQ.0) GO TO 2
      IF(IQ1-1) 5,4,5
 2    IF(IQ1+1) 5,4,5
 3    KS=1
      RETURN
 4    KS=2
      RETURN
 5    KS=3
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE COMP(PM,CT,FI,PX,PY,PZ)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 PM,CT,FI,PX,PY,PZ
      ST=DSQRT(1.D0-CT**2)
      PS=PM*ST
      PX=PS*DCOS(FI)
      PY=PS*DSIN(FI)
      PZ=PM*CT
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION COSDD(I)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 COSDD
 100  R1=RNDMD(-1)
      CT=2.*R1-1.
      IF(I.EQ.0) GO TO 2
      R2=RNDMD(-1)
      W=0.25+0.75*CT**2
      IF(W.LT.R2) GO TO 100
 2    COSDD=CT
      RETURN
       END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION DBLPCM(A,B,C)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 DBLPCM,A,B,C
      VAL=(A**2-B**2-C**2)**2-(2.D0*B*C)**2
      DBLPCM=0.
      IF(VAL.GT.0.D0)
     *DBLPCM=DSQRT(VAL)/(2.*A)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION AMASSF(ID)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 AMASSF
C          THIS FUNCTION RETURNS THE MASS OF THE PARTICLE WITH
C          IDENT CODE ID.
C          QUARK-BASED IDENT CODE
      DIMENSION AMMES0(10),AMMES1(10),AMBAR0(30),AMBAR1(30)
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      COMMON/QLMASS/ AMLEP(52),NQLEP,NMES,NBARY
      COMMON/ISOB3/ISOB3
C          0- MESON MASS TABLE
      DATA AMMES0/.13496,.13957,.5488,.49367,.49767,.9576,1.8633
     1,1.8683,2.030,2.976/
C          1- MESON MASS TABLE
      DATA AMMES1/.776,.776,.7826,.8881,.8922,1.0196,2.006,2.0086
     1,2.140,3.097/
C          1/2+ BARYON MASS TABLE
      DATA AMBAR0/-1.,.93828,.93957,2*-1.,1.1894,1.1925,1.1974
     1,1.1156,1.3149,1.3213,3*-1.,2.43,2.43,2.43,2.26
     2,2.50,2.50,2.60,2.40,2.40,3.55,3.55,3.70,4*-1./
C          3/2+ BARYON MASS TABLE
      DATA AMBAR1/1.232,1.232,1.232,1.232,-1.,1.3823,1.3820
     1,1.3875,-1.,1.5318,1.5350,1.6722,2*-1.
     2,2.63,2.63,2.63,-1.,2.70,2.70,2.80,2*-1.,3.75,3.75
     3,3.90,4.80,3*-1./
C          ENTRY
      CALL FLAVOR(ID,IFL1,IFL2,IFL3,JSPIN,INDEX)
      IF(ID.NE.0.AND.MOD(ID,100).EQ.0) GO TO 400
      IF(IABS(IFL1).GT.4.OR.IABS(IFL2).GT.4.OR.IABS(IFL3).GT.4)
     1GO TO 300
      IF(IFL2.EQ.0) GO TO 200
      IF(IFL1.EQ.0) GO TO 100
C          BARYONS
      INDEX=INDEX-109*JSPIN-36*NMES-NQLEP
      INDEX=INDEX-11
      AMASSF=(1-JSPIN)*AMBAR0(INDEX)+JSPIN*AMBAR1(INDEX)
      RETURN
C          MESONS
100   CONTINUE
      INDEX=INDEX-36*JSPIN-NQLEP
      INDEX=INDEX-11
      AMASSF=(1-JSPIN)*AMMES0(INDEX)+JSPIN*AMMES1(INDEX)
      RETURN
C          QUARKS AND LEPTONS
200   CONTINUE
      AMASSF=AMLEP(INDEX)
      RETURN
C          B AND T PARTICLES
300   CONTINUE
      AMASSF=AMLEP(IABS(IFL2))+AMLEP(IABS(IFL3))-.03+.04*JSPIN
      IF(IFL1.NE.0) AMASSF=AMASSF+AMLEP(IABS(IFL1))
      RETURN
C          DIQUARKS
400   AMASSF=AMLEP(IABS(IFL1))+AMLEP(IABS(IFL2))+0.5
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION AMASS(ID)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 AMASS
C          THIS FUNCTION RETURNS THE MASS OF THE PARTICLE WITH
C          IDENT CODE ID.
C          QUARK-BASED IDENT CODE
      DIMENSION AMMES0(10),AMMES1(10),AMBAR0(30),AMBAR1(30)
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      COMMON/QLMASS/ AMLEP(52),NQLEP,NMES,NBARY
      COMMON/ISOB3/ISOB3
      COMMON/INTTYP/ITYP
      COMMON/ITHEA/ITHEA(11)
C          0- MESON MASS TABLE
      DATA AMMES0/.13496,.13957,.5488,.49367,.49767,.9576,1.8633
     1,1.8683,2.030,2.976/
C          1- MESON MASS TABLE
      DATA AMMES1/.776,.776,.7826,.8881,.8922,1.0196,2.006,2.0086
     1,2.140,3.097/
C          1/2+ BARYON MASS TABLE
      DATA AMBAR0/-1.,.93828,.93957,2*-1.,1.1894,1.1925,1.1974
     1,1.1156,1.3149,1.3213,3*-1.,2.43,2.43,2.43,2.26
     2,2.50,2.50,2.60,2.40,2.40,3.55,3.55,3.70,4*-1./
C          3/2+ BARYON MASS TABLE
      DATA AMBAR1/1.232,1.232,1.232,1.232,-1.,1.3823,1.3820
     1,1.3875,-1.,1.5318,1.5350,1.6722,2*-1.
     2,2.63,2.63,2.63,-1.,2.70,2.70,2.80,2*-1.,3.75,3.75
     3,3.90,4.80,3*-1./
C          ENTRY
      CALL FLAVOR(ID,IFL1,IFL2,IFL3,JSPIN,INDEX)
      IF(ID.NE.0.AND.MOD(ID,100).EQ.0) GO TO 400
      IF(IABS(IFL1).GT.4.OR.IABS(IFL2).GT.4.OR.IABS(IFL3).GT.4)
     1GO TO 300
      IF(IFL2.EQ.0) GO TO 200
      IF(IFL1.EQ.0) GO TO 100
C          BARYONS
      INDEX=INDEX-109*JSPIN-36*NMES-NQLEP
      INDEX=INDEX-11
      AMASS=(1-JSPIN)*AMBAR0(INDEX)+JSPIN*AMBAR1(INDEX)
      IF(ISOB3.NE.1)  RETURN
      IF(ID.EQ.1111.OR.ID.EQ.1121.OR.ID.EQ.2221.OR.ID.EQ.1221)
     *GO  TO  1991
      RETURN
1991  CONTINUE
C     IF((ITHEA(8).EQ.1).OR.(ITYP.EQ.3))  THEN
C           CALL  MDELT1(AMD,GD)
C     ELSE
            CALL  MDELTA(AMD,GD)
C     ENDIF
      AMASS=AMD
      RETURN
C          MESONS
100   CONTINUE
      INDEX=INDEX-36*JSPIN-NQLEP
      INDEX=INDEX-11
      AMASS=(1-JSPIN)*AMMES0(INDEX)+JSPIN*AMMES1(INDEX)
      IF(ISOB3.NE.1)  RETURN
      IF(ID.EQ.10.OR.ID.EQ.11.OR.ID.EQ.16)
     *GO  TO  1992
      RETURN
 1992       CALL  MRHO(AMRHO,GD)
      AMASS=AMRHO
      RETURN
C          QUARKS AND LEPTONS
200   CONTINUE
      AMASS=AMLEP(INDEX)
      RETURN
C          B AND T PARTICLES
300   CONTINUE
      AMASS=AMLEP(IABS(IFL2))+AMLEP(IABS(IFL3))-.03+.04*JSPIN
      IF(IFL1.NE.0) AMASS=AMASS+AMLEP(IABS(IFL1))
      RETURN
C          DIQUARKS
400   AMASS=AMLEP(IABS(IFL1))+AMLEP(IABS(IFL2))+0.5
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
        FUNCTION GDM(X)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      REAL*8 GDM,X
        DATA AMN/0.940/,AMPI/0.140/,B/0.300/,GD0/0.110/,AMD0/1.232/
        AMD=X
        EN0=(AMD0**2+AMN**2-AMPI**2)/(2.*AMD0)
        Q0=DSQRT(EN0**2-AMN**2)
        EN =(AMD**2+AMN**2-AMPI**2)/(2.*AMD )
        QX=      EN**2-AMN**2
        IF(QX.LT.0.)  WRITE(ITLIS,*) 'GDM: QX,AMD=', QX,AMD
        Q =DSQRT(DABS(QX))
        V0=B**2/(B**2+Q0**2)
        V =B**2/(B**2+Q**2)
        GDM=(Q/Q0)**3*(AMD0/AMD)*(V/V0)**2*GD0
        RETURN
        END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
        SUBROUTINE MDELTA(AMDEL,GD)
      IMPLICIT REAL*8 (A-H,O-Z)
        REAL*8 AMDEL,GD
        DATA AMD0/1.232/
        DATA AMDMIN/1.081/,AMDMAX/1.700/
   10 CONTINUE
        AMD=AMDMIN+RNDMD(-1)*(AMDMAX-AMDMIN)
        GD=GDM(AMD)
        F=0.25*GD**2/((AMD-AMD0)**2+0.25*GD**2)
      IF(RNDMD(-1).GT.F)  GO  TO  10
        AMDEL=AMD
        RETURN
        END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           SUBROUTINE LABEL(LABEL1,ID)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C          RETURN THE LABEL FOR THE PARTICLE ID.
C          QUARK-BASED IDENT CODE.
C
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      COMMON/QLMASS/ AMLEP(52),NQLEP,NMES,NBARY
C
      CHARACTER*8   LABEL1, LLEP(104)
     *,LMES0(64),LMES1(64)
     *,LBAR0(109),LABAR0(109),LBAR1(109),LABAR1(109)
     *,LQQ(21),LAQQ(21)
C          DIQUARK LABELS
      DATA LQQ/
     1'UU0. ','UD0. ','DD0. ','US0. ','DS0. ','SS0. ','UC0. ','DC0. ',
     2'SC0. ','CC0. ','UB0. ','DB0. ','SB0. ','CB0. ','BB0. ','UT0. ',
     3'DT0. ','ST0. ','CT0. ','BT0. ','TT0. '/
      DATA LAQQ/
     1'AUU0.','AUD0.','ADD0.','AUS0.','ADS0.','ASS0.','AUC0.','ADC0.',
     2'ASC0.','ACC0.','AUB0.','ADB0.','ASB0.','ACB0.','ABB0.','AUT0.',
     3'ADT0.','AST0.','ACT0.','ABT0.','ATT0.'/
C          QUARK AND LEPTON LABELS
      DATA LLEP/
     $'     ','UP   ','UB   ','DN   ','DB   ','ST   ','SB   ','CH   ',
     $'CB   ','BT   ','BB   ','TP   ','TB   ','Y    ','YB   ','X    ',
     $'XB   ','GL   ','ERR  ','GM   ','ERR  ','NUE  ','ANUE ','E-   ',
     $'E+   ','NUM  ','ANUM ','MU-  ','MU+  ','NUT  ','ANUT ','TAU- ',
     $'TAU+ ','ERR  ','ERR  ','ERR  ','ERR  ','ERR  ','ERR  ','KS   ',
     $'ERR  ','ERR  ','KL   ',
     $'UPSS ','UBSS ','DNSS ','DBSS ','STSS ','SBSS ','CHSS ','CBSS ',
     $'BTSS ','BBSS ','TPSS ','TBSS ','ERR  ','ERR  ','ERR  ','ERR  ',
     $'GLSS ','ERR  ','GMSS ','ERR  ','NESS ','ANESS','E-SS ','E+SS ',
     $'NMSS ','ANMSS','MU-SS','MU+SS','NTSS ','ANTSS','T-SS ','T+SS ',
     $'ERR  ','ERR  ','ERR  ','ERR  ','W+SS ','W-SS ','Z0SS ','ERR  ',
     $'W+   ','W-   ','H10  ','AH10 ','H20  ','AH20 ','H30  ','AH30 ',
     $'H4+  ','H4-  ','H5+  ','H5-  ','H6+  ','H6-  ','H7++ ','H7-- ',
     $'H8++ ','H8-- ','H9++ ','H9-- ','Z0   '/
C          0- MESON LABELS
      DATA LMES0/
     1'PI0  ','PI+  ','ETA  ','PI-  ','K+   ','K0   ','ETAP ','AK0  ',
     2'K-   ','AD0  ','D-   ','F-   ','ETAC ','F+   ','D+   ','D0   ',
     2'UB.  ','DB.  ','SB.  ','CB.  ','BB.  ','BC.  ','BS.  ','BD.  ',
     3'BU.  ','UT.  ','DT.  ','ST.  ','CT.  ','BT.  ','TT.  ','TB.  ',
     4'TC.  ','TS.  ','TD.  ','TU.  ','UY.  ','DY.  ','SY.  ','CY.  ',
     5'BY.  ','TY.  ','YY.  ','YT.  ','YB.  ','YC.  ','YS.  ','YD.  ',
     6'YU.  ','UX.  ','DX.  ','SX.  ','CX.  ','BX.  ','TX.  ','YX.  ',
     7'XX.  ','XY.  ','XT.  ','XB.  ','XC.  ','XS.  ','XD.  ','XU.  '/
C          1- MESON LABELS
      DATA LMES1/
     1'RHO0 ','RHO+ ','OMEG ','RHO- ','K*+  ','K*0  ','PHI  ','AK*0 ',
     2'K*-  ','AD*0 ','D*-  ','F*-  ','JPSI ','F*+  ','D*+  ','D*0  ',
     3'UB*  ','DB*  ','SB*  ','CB*  ','UPSL ','BC*  ','BS*  ','BD*  ',
     4'BU*  ','UT*  ','DT*  ','ST*  ','CT*  ','BT*  ','TT*  ','TB*  ',
     5'TC*  ','TS*  ','TD*  ','TU*  ','UY*  ','DY*  ','SY*  ','CY*  ',
     6'BY*  ','TY*  ','YY*  ','YT*  ','YB*  ','YC*  ','YS*  ','YD*  ',
     7'YU*  ','UX*  ','DX*  ','SX*  ','CX*  ','BX*  ','TX*  ','YX*  ',
     8'XX*  ','XY*  ','XT*  ','XB*  ','XC*  ','XS*  ','XD*  ','XU*  '/
C          1/2+ BARYON LABELS
      DATA LBAR0/
     1'ERR  ','P    ','N    ','ERR  ','ERR  ','S+   ','S0   ','S-   ',
     2'L    ','XI0  ','XI-  ','ERR  ','ERR  ','ERR  ','SC++ ','SC+  ',
     3'SC0  ','LC+  ','USC. ','DSC. ','SSC. ','SDC. ','SUC. ','UCC. ',
     4'DCC. ','SCC. ','ERR  ','ERR  ','ERR  ','ERR  ','UUB. ','UDB. ',
     5'DDB. ','DUB. ','USB. ','DSB. ','SSB. ','SDB. ','SUB. ','UCB. ',
     6'DCB. ','SCB. ','CCB. ','CSB. ','CDB. ','CUB. ','UBB. ','DBB. ',
     7'SBB. ','CBB. ','ERR  ','ERR  ','ERR  ','ERR  ','ERR  ','UTT. ',
     8'UDT. ','DDT. ','DUT. ','UST. ','DST. ','SST. ','SDT. ','SUT. ',
     9'UCT. ','DCT. ','SCT. ','CCT. ','CST. ','CDT. ','CUT. ','UBT. ',
     1'DBT. ','SBT. ','CBT. ','BBT. ','BCT. ','BST. ','BDT. ','BUT. ',
     2'UTT. ','DTT. ','STT. ','CTT. ','BTT. ','ERR  ','ERR  ','ERR  ',
     3'ERR  ','ERR  ','ERR  ','UUY. ','UDY. ','DDY. ','DUY. ','USY. ',
     4'DSY. ','SSY. ','SDY. ','SUY. ','UUX. ','UDX. ','DDX. ','DUX. ',
     5'USX. ','DSX. ','SSX. ','SDX. ','SUX. '/
      DATA LABAR0/
     1'ERR  ','AP   ','AN   ','ERR  ','ERR  ','AS-  ','AS0  ','AS+  ',
     2'AL   ','AXI0 ','AXI+ ','ERR  ','ERR  ','ERR  ','ASC--','ASC- ',
     3'ASC0 ','ALC- ','AUSC.','ADSC.','ASSC.','ASDC.','ASUC.','AUCC.',
     4'ADCC.','ASCC.','ERR  ','ERR  ','ERR  ','ERR  ','AUUB.','AUDB.',
     5'ADDB.','ADUB.','AUSB.','ADSB.','ASSB.','ASDB.','ASUB.','AUCB.',
     6'ADCB.','ASCB.','ACCB.','ACSB.','ACDB.','ACUB.','AUBB.','ADBB.',
     7'ASBB.','ACBB.','ERR  ','ERR  ','ERR  ','ERR  ','ERR  ','AUTT.',
     8'AUDT.','ADDT.','ADUT.','AUST.','ADST.','ASST.','ASDT.','ASUT.',
     9'AUCT.','ADCT.','ASCT.','ACCT.','ACST.','ACDT.','ACUT.','AUBT.',
     1'ADBT.','ASBT.','ACBT.','ABBT.','ABCT.','ABST.','ABDT.','ABUT.',
     2'AUTT.','ADTT.','ASTT.','ACTT.','ABTT.','ERR  ','ERR  ','ERR  ',
     3'ERR  ','ERR  ','ERR  ','AUUY.','AUDY.','ADDY.','ADUY.','AUSY.',
     4'ADSY.','ASSY.','ASDY.','ASUY.','AUUX.','AUDX.','ADDX.','ADUX.',
     5'AUSX.','ADSX.','ASSX.','ASDX.','ASUX.'/
C          3/2+ BARYON LABELS
      DATA LBAR1/
     1'DL++ ','DL+  ','DL0  ','DL-  ','ERR  ','S*+  ','S*0  ','S*-  ',
     2'ERR  ','XI*0 ','XI*- ','OM-  ','ERR  ','ERR  ','UUC* ','UDC* ',
     3'DDC* ','ERR  ','USC* ','DSC* ','SSC* ','ERR  ','ERR  ','UCC* ',
     4'DCC* ','SCC* ','CCC* ','ERR  ','ERR  ','ERR  ','UUB* ','UDB* ',
     5'DDB* ','ERR  ','USB* ','DSB* ','SSB* ','ERR  ','ERR  ','UCB* ',
     6'DCB* ','SCB* ','CCB* ','ERR  ','ERR  ','ERR  ','UBB* ','DBB* ',
     7'SBB* ','CBB* ','BBB* ','ERR  ','ERR  ','ERR  ','ERR  ','UTT* ',
     8'UDT* ','DDT* ','ERR  ','UST* ','DST* ','SST* ','ERR  ','ERR  ',
     9'UCT* ','DCT* ','SCT* ','CCT* ','ERR  ','ERR  ','ERR  ','UBT* ',
     1'DBT* ','SBT* ','CBT* ','BBT* ','ERR  ','ERR  ','ERR  ','ERR  ',
     2'UTT* ','DTT* ','STT* ','CTT* ','BTT* ','TTT* ','ERR  ','ERR  ',
     3'ERR  ','ERR  ','ERR  ','UUY* ','UDY* ','DDY* ','ERR  ','USY* ',
     4'DSY* ','SSY* ','ERR  ','ERR  ','UUX* ','UDX* ','DDX* ','ERR  ',
     5'USX* ','DSX* ','SSX* ','ERR  ','ERR  '/
      DATA LABAR1/
     1'ADL--','ADL- ','ADL0 ','ADL+ ','ERR  ','AS*- ','AS*0 ','AS*+ ',
     2'ERR  ','AXI*0','AXI*+','AOM+ ','ERR  ','ERR  ','AUUC*','AUDC*',
     3'ADDC*','ERR  ','AUSC*','ADSC*','ASSC*','ERR  ','ERR  ','AUCC*',
     4'ADCC*','ASCC*','ACCC*','ERR  ','ERR  ','ERR  ','AUUB*','AUDB*',
     5'ADDB*','ERR  ','AUSB*','ADSB*','ASSB*','ERR  ','ERR  ','AUCB*',
     6'ADCB*','ASCB*','ACCB*','ERR  ','ERR  ','ERR  ','AUBB*','ADBB*',
     7'ASBB*','ACBB*','ABBB*','ERR  ','ERR  ','ERR  ','ERR  ','AUTT*',
     8'AUDT*','ADDT*','ERR  ','AUST*','ADST*','ASST*','ERR  ','ERR  ',
     9'AUCT*','ADCT*','ASCT*','ACCT*','ERR  ','ERR  ','ERR  ','AUBT*',
     1'ADBT*','ASBT*','ACBT*','ABBT*','ERR  ','ERR  ','ERR  ','ERR  ',
     2'AUTT*','ADTT*','ASTT*','ACTT*','ABTT*','ATTT*','ERR  ','ERR  ',
     3'ERR  ','ERR  ','ERR  ','AUUY*','AUDY*','ADDY*','ERR  ','AUSY*',
     4'ADSY*','ASSY*','ERR  ','ERR  ','AUUX*','AUDX*','ADDX*','ERR  ',
     5'AUSX*','ADSX*','ASSX*','ERR  ','ERR  '/
C          ENTRY
      CALL FLAVOR(ID,IFL1,IFL2,IFL3,JSPIN,INDEX)
C@@@@@@@@@@ SIVOCL @@@@@
      IF(ID.EQ.110.OR.ID.EQ.111.OR.ID.EQ.221.
     *OR.ID.EQ.220.OR.ID.EQ.330) THEN
      IDABS=IABS(ID)
      J=MOD(IDABS/100,10)
      K=MOD(IDABS/10,10)
      IFL1=0
      IFL2=ISIGN(J,ID)
      IFL3=ISIGN(K,-ID)
      ENDIF
C@@@@@@@@@@ SIVOCL @@@@@
      IF(IABS(ID).LT.100) GO TO 200
      IF(IABS(ID).LT.1000) GO TO 100
      IF(ID.NE.0.AND.MOD(ID,100).EQ.0) GO TO 300
C          BARYONS
      INDEX=INDEX-109*JSPIN-36*NMES-NQLEP
      INDEX=INDEX-11
      IF(JSPIN.EQ.0.AND.ID.GT.0) LABEL1=LBAR0(INDEX)
      IF(JSPIN.EQ.0.AND.ID.LT.0) LABEL1=LABAR0(INDEX)
      IF(JSPIN.EQ.1.AND.ID.GT.0) LABEL1=LBAR1(INDEX)
      IF(JSPIN.EQ.1.AND.ID.LT.0) LABEL1=LABAR1(INDEX)
      RETURN
C          MESONS
100   CONTINUE
      I=MAX0(IFL2,IFL3)
      J=-MIN0(IFL2,IFL3)
      INDEX=MAX0(I-1,J-1)**2+I+MAX0(I-J,0)
      IF(JSPIN.EQ.0) LABEL1=LMES0(INDEX)
      IF(JSPIN.EQ.1) LABEL1=LMES1(INDEX)
      RETURN
C          QUARKS, LEPTONS, ETC.
200   CONTINUE
      INDEX=2*INDEX
      IF(ID.LE.0) INDEX=INDEX+1
      LABEL1=LLEP(INDEX)
      RETURN
300   I=IABS(IFL1)
      J=IABS(IFL2)
      INDEX=I+J*(J-1)/2
      IF(ID.GT.0) LABEL1=LQQ(INDEX)
      IF(ID.LT.0) LABEL1=LAQQ(INDEX)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION CHARGE(ID)
      REAL*8 CHARGE
C
C          COMPUTE CHARGE OF PARTICLE WITH IDENT CODE ID
C          ICHRG MUST BE DIMENSIONED NQLEP+12
C
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      DIMENSION ICHRG(53),IFL(3)
      DATA ICHRG/0,2,-1,-1,2,-1,2,-1,2,0,0,0,-3,0,-3,0,-3,0,-3
     $,0,0,0,2,-1,-1,2,-1,2,-1,2,0,0,0,-3,0,-3,0,-3,0,-3,3,0
     $,3,0,0,0,3,3,3,6,6,6,0/
      IDABS=IABS(ID)
      CALL FLAVOR(ID,IFL(1),IFL(2),IFL(3),JSPIN,INDEX)
c      write(16,*) 'IFL(3)=', IFL
      if(IDABS.ge.100) then
        ISUM=0
        DO I=1,3
          IS1=1
          if(IFL(I).ne.0)  IS1=ISIGN(1,IFL(I))
          ISUM=ISUM+ICHRG(IABS(IFL(I))+1)*IS1
c         ISUM=ISUM+ICHRG(IABS(IFL(I))+1)*ISIGN(1,IFL(I))
c          write(16,*) 'I,ISUM,IS1,IFL(I)=',I,ISUM,IS1,IFL(I)
           ISUM=ISUM    !!!! Lena
        ENDDO
      else
        ISUM=ICHRG(INDEX+1)*ISIGN(1,ID)
      endif
      CHARGE=ISUM/3.d0
c      write(16,*) 'ID,ISUM,CHARGE=', ID,ISUM,CHARGE
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE DECAY(IP,IPOINT)
      IMPLICIT REAL*8 (A-H,O-Z)
C          THIS SUBROUTINE DECAYS PARTICLE IP FROM /PARTCL/ USING THE
C          BRANCHING RATIOS FROM /DKYTAB/ AND ADDS THE DECAY PRODUCTS
C          TO /PARTCL/ WITH IORIG=IP.
C          QUARK-BASED IDENT CODE
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      COMMON/PRINTS/IPRINT
      LOGICAL IPRINT
      COMMON/WCON/SIN2W,WMASS(4),WGAM(4),AQ(12,4),BQ(12,4),COUT(4),
     1MATCH(25,4),WCBR(25,4),CUTOFF,CUTPOW,TBRWW(4,2),RBRWW(12,4,2),EZ,
     2AQDP(12,4),BQDP(12,4),EZDP
      COMMON/PARTCL/PPTCL(9,499),NPTCL,IORIG(499),IDENT(499)
     *,IDCAY(499)
      COMMON/COMLID/PLIDER(499)
      COMMON/PARORD/IORDP(499)
C     LOOK MUST BE DIMENSIONED TO THE MAXIMUM VALUE OF INDEX
      COMMON/DKYTAB/LOOK(400),CBR(600),MODE(5,600)
      LOGICAL NODCAY,NOETA,NOPI0,NONUNU,NOEVOL,NOHADR,NOKA0
      COMMON/NODCAY/NODCAY,NOETA,NOPI0,NONUNU,NOEVOL,NOHADR,NOKA0
      COMMON/JETSET/PJSET(5,100),NJSET,JORIG(100),JTYPE(100),
     $JDCAY(100)
      COMMON/JWORK/ZZC(100),P1CM(4),E1CM,E2CM,E3CM,E4CM,E5CM,
     1J1,J2,J3,J4,J5,JMATCH(100),TNEW
      LOGICAL TNEW
      DIMENSION JJ(5),EE(5)
      EQUIVALENCE (J1,JJ(1)),(E1CM,EE(1))
      COMMON/CONST/PI,SQRT2,ALF,GF,UNITS
      DIMENSION PGEN(5,5),RND(5),U(3),BETA(3),ROT(3,3),PSAVE(3)
     1,REDUCE(5), PSUM(5)
      DATA REDUCE/1.,1.,2.,5.,15./
      DATA PSUM/5*0./
      DATA TWOME/1.022006E-3/
C          FUNCTION DEFINITIONS
C          PCM AND DOT MUST BE CALCULATED IN DOUBLE PRECISION
C          ON 32-BIT MACHINES.
CC    PCM(A,B,C)=DSQRT((A**2-B**2-C**2)**2-(2.*B*C)**2)/(2.*A)
C          SELECT DECAY MODE
 92   MXPTCL=499
      IPACK=1000
      IDPACK=10000
      MXJSET=100
      JPACK=1000
      IDLV1=IDENT(IP)
      CALL FLAVOR(IDLV1,IFL1,IFL2,IFL3,JSPIN,INDEX)
C     IF(IABS(IFL1).GT.4.OR.IABS(IFL2).GT.4.OR.IABS(IFL3).GT.4)GOTO101
      IF(NOPI0.AND.IDLV1.EQ.110) GO TO 101
      IF(NOETA.AND.IDLV1.EQ.220) GO TO 101
      IF(NOKA0.AND.(IDLV1.EQ.230.OR.IDLV1.EQ.-230)) GO TO 101
      GO TO 102
101   IPOINT=-1
      RETURN
102   CONTINUE
1     CONTINUE
      IPOINT=LOOK(INDEX)-1
      IF(IPOINT.LT.0) RETURN
      IF(IPRINT) WRITE(ITLIS,2000) IDENT(IP),(PPTCL(J,IP),J=1,4)
2000  FORMAT(' DECAY OF ',I6,' WITH MOM.',3F10.3,'& ENERGY',F10.3)
      TRY=RNDMD(-1)
100   IPOINT=IPOINT+1
      IF(TRY.GT.CBR(IPOINT)) GO TO 100
      NADD=0
      SUM=0.
      NSTART=NPTCL+1
      DO 110 I=1,5
      IF(MODE(I,IPOINT).EQ.0) GO TO 110
      IF(NPTCL+NADD+1.GT.MXPTCL) GO TO 9999
      NADD=NADD+1
      NEW=NPTCL+NADD
      IDENT(NEW)=MODE(I,IPOINT)
      IDLV1=IDENT(NEW)
      PPTCL(5,NEW)=AMASSF(IDLV1)
      SUM=SUM+PPTCL(5,NEW)
110   CONTINUE
       IF(SUM.GT.PPTCL(5,IP)) GO TO 1991
      NADD1=NADD-1
      DO 120 J=1,5
      PGEN(J,1)=PPTCL(J,IP)
120   CONTINUE
      PGEN(5,NADD)=PPTCL(5,NPTCL+NADD)
      IF(NADD.EQ.1) GO TO 700
      IF(NADD.EQ.2) GO TO 400
C          USE KROLL-WADA DISTRIBUTION FOR DALITZ DECAYS.
      IF(.NOT.((IDENT(IP).EQ.110.OR.IDENT(IP).EQ.220).AND.
     1IABS(IDENT(NPTCL+2)).EQ.12)) GO TO 130
125   AMEE=TWOME*(PPTCL(5,IP)/TWOME)**RNDMD(-1)
      REE=(TWOME/AMEE)**2
      WTEE=(1.-(AMEE/PPTCL(5,IP))**2)**3*DSQRT(1.-REE)*(1.+.5*REE)
      DRND=RNDMD(-1)
      IF(WTEE.LT.DRND) GO TO 125
      PGEN(5,2)=AMEE
      GO TO 400
130   CONTINUE
C          CALCULATE MAXIMUM PHASE-SPACE WEIGHT
      WTMAX=1./REDUCE(NADD)
      SUM1=PGEN(5,1)
      SUM2=SUM-PPTCL(5,NPTCL+1)
      DO 200 I=1,NADD1
      WTMAX=WTMAX*DBLPCM(SUM1,SUM2,PPTCL(5,NPTCL+I))
      SUM1=SUM1-PPTCL(5,NPTCL+I)
      SUM2=SUM2-PPTCL(5,NPTCL+I+1)
200   CONTINUE
C          GENERATE UNIFORM NADD-BODY PHASE SPACE
300   CONTINUE
      RND(1)=1.
      DO 310 I=2,NADD1
      RNEW=RNDMD(-1)
      I1=I-1
      DO 320 JJ1=1,I1
      J=I-JJ1
      JSAVE=J+1
      IF(RNEW.LE.RND(J)) GO TO 310
      RND(JSAVE)=RND(J)
320   CONTINUE
310   RND(JSAVE)=RNEW
      RND(NADD)=0.
      WT=1.
      SUM1=SUM
      DO 330 I=2,NADD
      SUM1=SUM1-PPTCL(5,NPTCL+I-1)
      PGEN(5,I)=SUM1+RND(I)*(PGEN(5,1)-SUM)
      WT=WT*DBLPCM(PGEN(5,I-1),PGEN(5,I),PPTCL(5,NPTCL+I-1))
330   CONTINUE
      IF(WT.LT.RNDMD(-1)*WTMAX) GO TO 300
C          CARRY OUT TWO-BODY DECAYS IN PGEN FRAMES
400   CONTINUE
      DO 410 I=1,NADD1
      QCM=DBLPCM(PGEN(5,I),PGEN(5,I+1),PPTCL(5,NPTCL+I))
      U(3)=2.*RNDMD(-1)-1.
      PHI=2.*PI*RNDMD(-1)
      U(1)=DSQRT(1.-U(3)**2)*DCOS(PHI)
      U(2)=DSQRT(1.-U(3)**2)*DSIN(PHI)
      DO 420 J=1,3
      PPTCL(J,NPTCL+I)=QCM*U(J)
      PGEN(J,I+1)=-PPTCL(J,NPTCL+I)
420   CONTINUE
      PPTCL(4,NPTCL+I)=DSQRT(QCM**2+PPTCL(5,NPTCL+I)**2)
      PGEN(4,I+1)=DSQRT(QCM**2+PGEN(5,I+1)**2)
410   CONTINUE
      DO 430 J=1,4
      PPTCL(J,NPTCL+NADD)=PGEN(J,NADD)
430   CONTINUE
C          BOOST PGEN FRAMES TO LAB FRAME
      DO 500 II=1,NADD1
      I=NADD-II
      DO 510 J=1,3
      BETA(J)=PGEN(J,I)/PGEN(4,I)
510   CONTINUE
      GAMMA=PGEN(4,I)/PGEN(5,I)
      DO 520 K=I,NADD
      K1=NPTCL+K
      BP=BETA(1)*PPTCL(1,K1)+BETA(2)*PPTCL(2,K1)+BETA(3)*PPTCL(3,K1)
      DO 530 J=1,3
      PPTCL(J,K1)=PPTCL(J,K1)+GAMMA*BETA(J)*(PPTCL(4,K1)
     1+BP*GAMMA/(GAMMA+1.))
530   CONTINUE
      PPTCL(4,K1)=GAMMA*(PPTCL(4,K1)+BP)
520   CONTINUE
500   CONTINUE
C          MATRIX ELEMENTS
      IF(NADD.EQ.3.AND.(IDENT(IP).EQ.221.OR.IDENT(IP).EQ.331)) GO TO 610
      IF(NADD.EQ.3.AND.IABS(IDENT(NPTCL+1)).LT.20.AND.
     1IDENT(NPTCL+1).NE.10) GO TO 620
      GO TO 800
C          OMEG AND PHI DECAY
610   WT=(PPTCL(5,NPTCL+1)*PPTCL(5,NPTCL+2)*PPTCL(5,NPTCL+3))**2
     1-(PPTCL(5,NPTCL+1)*DOT(NPTCL+2,NPTCL+3))**2
     2-(PPTCL(5,NPTCL+2)*DOT(NPTCL+1,NPTCL+3))**2
     3-(PPTCL(5,NPTCL+3)*DOT(NPTCL+1,NPTCL+2))**2
     4+2.*DOT(NPTCL+1,NPTCL+2)*DOT(NPTCL+2,NPTCL+3)*DOT(NPTCL+1,NPTCL+3)
      IF(WT.LT.RNDMD(-1)*PPTCL(5,IP)**6/108.) GO TO 300
      GO TO 800
C          SEMILEPTONIC AND QUARK DECAYS
C          INCLUDE W PROPAGATOR
620   WT=DOT(IP,NPTCL+2)*DOT(NPTCL+1,NPTCL+3)
      S12=PPTCL(5,NPTCL+1)**2+PPTCL(5,NPTCL+2)**2
     1+2.*DOT(NPTCL+1,NPTCL+2)
      S12MAX=PPTCL(5,IP)**2
      WT=WT*WPROP(S12MAX)/WPROP(S12)
      IF(WT.LT.RNDMD(-1)*PPTCL(5,IP)**4/16.) GO TO 300
      GO TO 800
C          ONE-PARTICLE DECAYS
700   CONTINUE
      DO 710 J=1,5
      PPTCL(J,NPTCL+1)=PPTCL(J,IP)
710   CONTINUE
C          SWAP PARTICLES AND ANTIPARTICLES IF IDENT(IP)<0
800   CONTINUE
      IF(IDENT(IP).GE.0.OR.IABS(IDENT(IP)).EQ.20) GO TO 900
      DO 810 I=1,NADD
      IDABS=IABS(IDENT(NPTCL+I))
      IFL1=IDABS/1000
      IFL2=MOD(IDABS/100,10)
      IFL3=MOD(IDABS/10,10)
      IF(IFL1.EQ.0.AND.IFL2.NE.0.AND.IFL2.EQ.IFL3) GO TO 810
      IF(IDABS.EQ.9.OR.IDABS.EQ.10.OR.IDABS.EQ.20) GO TO 810
      IF(IDABS.EQ.29.OR.IDABS.EQ.30.OR.IDABS.EQ.40) GO TO 810
      IDENT(NPTCL+I)=-IDENT(NPTCL+I)
810   CONTINUE
C          REMOVE QUARKS FROM /PARTCL/ AND TRANSFORM BACK TO REST FRAME
900   CONTINUE
      NPTCL=NPTCL+NADD
      NQK=0
      IF(IABS(IDENT(NPTCL)).GE.10.AND.MOD(IDENT(NPTCL),100).NE.0)
     1GO TO 1000
      NOFF=NPTCL-NSTART+1
      DO 910 II=1,NOFF
      I=NPTCL+1-II
      IF(IABS(IDENT(NPTCL)).LT.10.OR.MOD(IDENT(NPTCL),100).NE.0)
     1NQK=NQK+1
      BP=BETA(1)*PPTCL(1,I)+BETA(2)*PPTCL(2,I)+BETA(3)*PPTCL(3,I)
      DO 911 J=1,3
      PPTCL(J,I)=PPTCL(J,I)-GAMMA*BETA(J)*(PPTCL(4,I)
     1-BP*GAMMA/(GAMMA+1.))
911   CONTINUE
      PPTCL(4,I)=GAMMA*(PPTCL(4,I)-BP)
910   CONTINUE
C          COPY DECAY PRODUCTS INTO /JETSET/
      IF(NJSET+NADD.GT.MXJSET) GO TO 9998
      NJSAVE=NJSET
      NPTCL=NPTCL-NADD
      DO 920 I=1,NADD
      NJSET=NJSET+1
      DO 921 K=1,5
921   PJSET(K,NJSET)=PPTCL(K,NPTCL+I)
      JORIG(NJSET)=0
      JTYPE(NJSET)=IDENT(NPTCL+I)
      JDCAY(NJSET)=0
      JMATCH(NJSET)=JPACK*(NJSAVE+1)+NJSAVE+NADD
C          QCD EVOLUTION STARTS FROM PARENT MASS
C          BUT USE NADD*ENERGY TO PRESERVE TP --> W+ BT
      IF(IABS(JTYPE(NJSET)).GE.10.AND.MOD(JTYPE(NJSET),100).NE.0)
     1GO TO 920
      JDCAY(NJSET)=-1
      PJSET(5,NJSET)=DMIN1(PPTCL(5,IP),NADD*PJSET(4,NJSET))
920   CONTINUE
C          PERFORM QCD JET EVOLUTION
c-Sob      PRINT*,'DECAY, IDENT(IP)= ',IDENT(IP)
      CALL QCDFRG(NJSAVE+1)
C          DECAY QUARKS AND ROTATE TO PROPER ANGLES
C          HADRONIZE JETS
      NJ1=NJSAVE+1
      DO 931 I=NJ1,NJSET
      IF(JDCAY(I).NE.0) GO TO 931
      IF(IABS(JTYPE(I)).GE.10.AND.MOD(JTYPE(I),100).NE.0)
     1GO TO 935
      NEXT=NPTCL+1
      PJET=DSQRT(PJSET(1,I)**2+PJSET(2,I)**2+PJSET(3,I)**2)
      CTHQK=PJSET(3,I)/PJET
      STHQK=DSQRT(1.-CTHQK**2)
      CPHIQK=PJSET(1,I)/(PJET*STHQK)
      SPHIQK=PJSET(2,I)/(PJET*STHQK)
      CALL JETGEN(I)
      IF(NEXT.GT.NPTCL) GO TO 931
      ROT(1,1)=CPHIQK*CTHQK
      ROT(2,1)=SPHIQK*CTHQK
      ROT(3,1)=-STHQK
      ROT(1,2)=-SPHIQK
      ROT(2,2)=CPHIQK
      ROT(3,2)=0.
      ROT(1,3)=CPHIQK*STHQK
      ROT(2,3)=SPHIQK*STHQK
      ROT(3,3)=CTHQK
      DO 932 K=NEXT,NPTCL
      DO 933 J=1,3
      PSAVE(J)=PPTCL(J,K)
      PPTCL(J,K)=0.
933   CONTINUE
      DO 932 J=1,3
      DO 932 JJ1=1,3
      PPTCL(J,K)=PPTCL(J,K)+ROT(J,JJ1)*PSAVE(JJ1)
932   CONTINUE
      GOTO 931
C          ADD LEPTON TO /PARTCL/
  935 NPTCL=NPTCL+1
      DO 936 K=1,5
  936 PPTCL(K,NPTCL)=PJSET(K,I)
      IDENT(NPTCL)=JTYPE(I)
931   CONTINUE
C          RESET NJSET SO DECAY JETS DO NOT APPEAR IN /JETSET/
      NJSET=NJSAVE
C          CHECK FOR AT LEAST TWO PARTICLES
      IF(NPTCL.GT.NSTART) GO TO 939
      NPTCL=NSTART-1
      GO TO 1
939   CONTINUE
C          CONSERVE CHARGE
      SUMQ=0.
      DO 960 I=NSTART,NPTCL
      IDLV1=IDENT(I)
      SUMQ=SUMQ+CHARGE(IDLV1)
960   CONTINUE
      IDLV1=IDENT(IP)
      SUMQ=SUMQ-CHARGE(IDLV1)
      IF(SUMQ.EQ.0.) GO TO 970
      DO 961 I=NSTART,NPTCL
      ID1=IDENT(I)
      IF(IABS(ID1).GT.1000) GO TO 961
      I1=MOD(IABS(ID1)/100,10)
      I2=MOD(IABS(ID1)/10,10)
      I3=MOD(IABS(ID1),10)
      IF(I1.EQ.1.AND.I2.GT.2.AND.SUMQ*ID1.GT.0.) GO TO 962
      IF(I1.EQ.2.AND.I2.GT.2.AND.SUMQ*ID1.LT.0.) GO TO 963
      IF(I1.EQ.1.AND.I2.EQ.2.AND.SUMQ*ID1.GT.0.) GO TO 964
      IF(I1.EQ.1.AND.I2.EQ.1) GO TO 965
      GO TO 961
962   IDENT(I)=ISIGN(200+10*I2+I3,ID1)
      GO TO 969
963   IDENT(I)=ISIGN(100+10*I2+I3,ID1)
      GO TO 969
964   IDENT(I)=110+I3
      GO TO 969
965   IDENT(I)=(120+I3)*(-DSIGN(1.D0,SUMQ))
969   SUMQ=DSIGN(DABS(SUMQ)-1.,SUMQ)
      IDLV1=IDENT(I)
      PPTCL(5,I)=AMASSF(IDLV1)
      PPTCL(4,I)=DSQRT(PPTCL(1,I)**2+PPTCL(2,I)**2+PPTCL(3,I)**2
     1+PPTCL(5,I)**2)
      IF(SUMQ.EQ.0.) GO TO 970
961   CONTINUE
      NPTCL=NSTART-1
      GO TO 1
C          RESCALE MOMENTA FOR CORRECT MASS
970   CONTINUE
      PSUM(4)=PPTCL(5,IP)
      PSUM(5)=PSUM(4)
      NPTLV1=NPTCL
      CALL RESCAL(NSTART,NPTLV1,PSUM,IFAIL)
      IF(IFAIL.EQ.0) GO TO 940
      NPTCL=NSTART-1
      GO TO 1
940   CONTINUE
C          BOOST BACK TO LAB FRAME
      DO 950 I=NSTART,NPTCL
      BP=BETA(1)*PPTCL(1,I)+BETA(2)*PPTCL(2,I)+BETA(3)*PPTCL(3,I)
      DO 951 J=1,3
      PPTCL(J,I)=PPTCL(J,I)+GAMMA*BETA(J)*(PPTCL(4,I)
     1+BP*GAMMA/(GAMMA+1.))
951   CONTINUE
      PPTCL(4,I)=GAMMA*(PPTCL(4,I)+BP)
950   CONTINUE
C          SET IORIG AND IDCAY
1000  CONTINUE
      IDCAY(IP)=IDPACK*NSTART+NPTCL
      IF(IPRINT) WRITE(ITLIS,2001)
2001  FORMAT(' PRODUCTS OF DECAY:   ID     .    PX    .  PY    ',
     *'    PZ    .   E       ')
      DO 1010 I=NSTART,NPTCL
      IF(IPRINT) WRITE(ITLIS,2002) IDENT(I),(PPTCL(J,I),J=1,4)
2002  FORMAT(18X,I6,4F10.3)
C      IORIG(I)=IP + IORIG(IP)*IPACK
       IORIG(I)=IP
      IDCAY(I)=0
      PLIDER(I)=PLIDER(IP)
      IORDP(I)=IORDP(IP)
      PPTCL(6,I)=PPTCL(6,IP)
      PPTCL(7,I)=PPTCL(7,IP)
      PPTCL(8,I)=PPTCL(8,IP)
      PPTCL(9,I)=PPTCL(9,IP)
1010  CONTINUE
      RETURN
C
9999  WRITE(ITLIS,10) NPTCL
10    FORMAT(//5X,25HERROR IN DECAY...NPTCL > ,I5)
      RETURN
C
1991  WRITE(ITLIS,1992)  SUM,PPTCL(5,IP)
      WRITE(17,1992)  SUM,PPTCL(5,IP)
1992  FORMAT(1X,'ERROR IN DECAY:  SUMM > MD ',2(F7.3,1X))
      PPTCL(5,IP)=SUM
      GO  TO  92
C
9998  WRITE(ITLIS,20) NJSET
20    FORMAT(//5X,25HERROR IN DECAY...NJSET > ,I5)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION DOT(I1,I2)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 DOT
      COMMON/PARTCL/PPTCL(9,499),NPTCL,IORIG(499),IDENT(499)
     *,IDCAY(499)
      DOT=PPTCL(4,I1)*PPTCL(4,I2)-PPTCL(1,I1)*PPTCL(1,I2)
     *   -PPTCL(2,I1)*PPTCL(2,I2)-PPTCL(3,I1)*PPTCL(3,I2)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE  JETGEN(I)
      K=I
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION WPROP(Z)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 WPROP,Z
      COMMON/WCON/SIN2W,WMASS(4),WGAM(4),AQ(12,4),BQ(12,4),COUT(4),
     1MATCH(25,4),WCBR(25,4),CUTOFF,CUTPOW,TBRWW(4,2),RBRWW(12,4,2),EZ,
     2AQDP(12,4),BQDP(12,4),EZDP
C          CHARGED W PROPAGATOR.
      WPROP=(Z-WMASS(2)**2)**2+(WMASS(2)*WGAM(2))**2
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE  QCDFRG(I)
      K=I
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION IB(ID)
C
C    COMPUTE BARYON NUMBER
C
      IB=0
      IF(IABS(ID)/1000.NE.0) IB=ISIGN(1,ID)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION IS(ID)
C
C   COMPUTE STRANGENESS
C
      CALL FLAVOR(ID,IF1,IF2,IF3,JSPIN,INDEX)
      IS=0
      IF(IABS(IF1).EQ.3) IS=IS+ISIGN(1,-IF1)
      IF(IABS(IF2).EQ.3) IS=IS+ISIGN(1,-IF2)
      IF(IABS(IF3).EQ.3) IS=IS+ISIGN(1,-IF3)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE CLUSTR(IFL1,IFL2,AMCTR)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 AMCTR
C  HADRONS PRODUCTION BY MEANS CLUSTER BREAKING
C  WITH QUARK AND ANTIQUARK OR QUARK AND DIQUARK OR DIQUARK AND
C  ANTIDIQUARK IFL1 AND IFL2 ON ENDS
C  AMCTR IS MASS OF CLUSTER
C
      COMMON/PRIMAR/SCM,HALFE,ECM,NJET,IDIN(2),NEVENT,NTRIES
      COMMON/ITAPES/ ITVKY,ITEVT,ITCOM,ITLIS
      COMMON/PRINTS/IPRINT
      LOGICAL IPRINT
      COMMON/PARTCL/PPTCL(9,499),NPTCL,IORIG(499),IDENT(499)
     *,IDCAY(499)
      COMMON/FRGCPA/ PUDC,PUDCC,PSPINC,PJSPNC,PMIX1C(3,2),PMIX2C(3,2),
     *PBARC
      COMMON/CONST/PI,SQRT2,ALF,GF,UNITS
      COMMON/COMNPT/ NOPTC
      LOGICAL NOPTC
      COMMON/COLRET/ LRET
      LOGICAL LRET
      DIMENSION IFL(2),U(3)
      LOGICAL SPINT
      COMMON/COMWTI/ WTIME
      LOGICAL WTIME
      MXPTCL=499
C     IF(.NOT.WTIME) GO TO 99
C     NOPTC=.FALSE.
C     CALL CLUSTT(IFL1,IFL2,AMCTR)
C     RETURN
99    NFIX=NPTCL
      NREP=0
      LRET=.FALSE.
 100  I=NFIX
      IF(NREP.LT.NTRIES) GO TO 101
      LRET=.TRUE.
      IF(IPRINT) WRITE(ITLIS,1200) NREP,IFL1,IFL2,AMCTR
1200  FORMAT(3X,' IN CLUSTR NREP GT ',3I8,' AMCTR=',F12.4)
      RETURN
101   CONTINUE
      KSPIN=0
      IFL(1)=IFL1
      IFL(2)=IFL2
      SPINT=.FALSE.
      I=I+2
      IF(I.GT.MXPTCL) GO TO 9999
C  CHOOSE SIDE OF BREAK
      JSIDE=1
C  IF ANY IFL IS A DIQUARK
      IF(MOD(IFL(1),100).EQ.0.OR.MOD(IFL(2),100).EQ.0) GO TO 150
C  IFL(1) AND IFL(2) ARE QUARKS
C  SELECT Q,QBARPAIR OR QQ,QQBAR PAIR
      DRND=RNDMD(-1)
      IF(DRND.LT.PBARC) GO TO 140
C  Q,QBAR PAIR
      IFLN=ISIGN(IDINT(RNDMD(-1)/PUDC)+1,-IFL(JSIDE))
      IF(IABS(IFLN).EQ.3)
     *IFLN=ISIGN(IDINT(RNDMD(-1)/PUDCC)+3,-IFL(JSIDE))
      GO TO 200
C  QQ,QQBAR PAIR
140   IQ1=IDINT(RNDMD(-1)/PUDC)+1
      IF(IQ1.EQ.3) IQ1=IDINT(RNDMD(-1)/PUDCC)+3
      IQ2=IDINT(RNDMD(-1)/PUDC)+1
      IF(IQ2.EQ.3) IQ2=IDINT(RNDMD(-1)/PUDCC)+3
      IF(IQ1.LE.IQ2) GO TO 145
      ISWAP=IQ1
      IQ1=IQ2
      IQ2=ISWAP
145   IFQQ=1000*IQ1+100*IQ2
      IFLN=ISIGN(IFQQ,IFL(JSIDE))
      GO TO 200
C  IFL(1) OR IFL(2) IS DIQUARK
C Q,QBAR PAIR
150   IPSIGN=IFL(JSIDE)
      IF(MOD(IFL(JSIDE),100).EQ.0) GO TO 130
      IPSIGN=-IFL(JSIDE)
130   IFLN=ISIGN(IDINT(RNDMD(-1)/PUDC)+1,IPSIGN)
      IF(IABS(IFLN).EQ.3)
     *IFLN=ISIGN(IDINT(RNDMD(-1)/PUDCC)+3,IPSIGN)
C  IDENTS AND MASSES OF PARTICLES
 200  IDENT(I-1)=IDPARC(IFL(JSIDE),IFLN,SPINT,KSPIN)
      IDENT(I)=IDPARC(IFL(3-JSIDE),-IFLN,SPINT,KSPIN)
      PPTCL(5,I-1)=AMASS(IDENT(I-1))
      PPTCL(5,I)=AMASS(IDENT(I))
C  IF TOO LOW MASS,START ALL OVER
      IF(AMCTR.GT.PPTCL(5,I-1)+PPTCL(5,I)) GO TO 102
      NREP=NREP+1
      GO TO 100
102   CONTINUE
      PA=DBLPCM(AMCTR,PPTCL(5,I-1),PPTCL(5,I))
C      PROB=2.*PA/AMCTR
C   PROB IS TWO-BODY PHASE SPACE FACTOR
C      DRND=RNDMD(-1)
C      IF(DRND.GT.PROB) GO TO 100
      U(3)=COSDD(0)
      PHI=2.*PI*RNDMD(-1)
      ST=DSQRT(1.-U(3)**2)
      U(1)=ST*DCOS(PHI)
      U(2)=ST*DSIN(PHI)
      PPTCL(1,I-1)=PA*U(1)
      PPTCL(1,I)=-PA*U(1)
      PPTCL(2,I-1)=PA*U(2)
      PPTCL(2,I)=-PA*U(2)
      PPTCL(3,I-1)=PA*U(3)
      PPTCL(3,I)=-PA*U(3)
      PA2=PA**2
      PPTCL(4,I-1)=DSQRT(PA2+PPTCL(5,I-1)**2)
      PPTCL(4,I)=DSQRT(PA2+PPTCL(5,I)**2)
      IDCAY(I-1)=0
      IDCAY(I)=0
      NPTCL=I
      RETURN
9999  WRITE(ITLIS,9998) I
9998  FORMAT(//10X,40H...STOP IN CLUSTR..NPTCL TOO HIGH NPTCL=,I5)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION IDPARC(IFL01,IFL02,SPINT,IR)
      IMPLICIT REAL*8 (A-H,O-Z)
C   CONSTRUCT MESON FROM QUARK AND ANTIQUARK WITH FLAVORS IFL01,IFL02
C   OR CONSTRUCT BARION FROM DIQUARK AND QUARK OR ANTIDIQUARK AND
C  ANTIQUARK WITH FLAVORS IFL01,IFL02
      COMMON/FRGCPA/ PUDC,PUDCC,PSPINC,PJSPNC,PMIX1C(3,2),PMIX2C(3,2),
     *PBARC
      LOGICAL SPINT
      IFL1=IFL01
      IFL2=IFL02
C  CONSTRUCT MESON WITH ACCOUNT FLAVOR MIXING
      IF(MOD(IFL1,100).EQ.0) GO TO 420
      IF(MOD(IFL2,100).EQ.0) GO TO 425
      JSPIN=IDINT(PSPINC+RNDMD(-1))
      IF(SPINT.AND.IR.EQ.2) JSPIN=0
      IF(SPINT.AND.IR.EQ.1) JSPIN=1
      ID1=IFL1
      ID2=IFL2
      IF(ID1+ID2.NE.0) GO TO 400
      RND=RNDMD(-1)
      ID1=IABS(ID1)
      ID1=IDINT(PMIX1C(ID1,JSPIN+1)+RND)+
     +IDINT(PMIX2C(ID1,JSPIN+1)+RND)+1
      ID2=-ID1
 400  IF(IABS(ID1).LE.IABS(ID2)) GO TO 410
      ISAVE=ID1
      ID1=ID2
      ID2=ISAVE
 410  IDHAD=ISIGN(100*IABS(ID1)+10*IABS(ID2)+JSPIN,ID1)
      GO TO 470
C  CONSTRUCT BARYON IDENT
 420  ID3=ISIGN(MOD(IFL1/100,10),IFL1)
      ID2=IFL1/1000
      ID1=IFL2
      GO TO 430
 425  ID3=ISIGN(MOD(IFL2/100,10),IFL2)
      ID2=IFL2/1000
      ID1=IFL1
 430  IF(IABS(ID1).LE.IABS(ID2)) GO TO 431
      ISWAP=ID1
      ID1=ID2
      ID2=ISWAP
 431  IF(IABS(ID2).LE.IABS(ID3)) GO TO 432
      ISWAP=ID2
      ID2=ID3
      ID3=ISWAP
 432  IF(IABS(ID1).LE.IABS(ID2)) GO TO 440
      ISWAP=ID1
      ID1=ID2
      ID2=ISWAP
 440  JSPIN=1
      IF(ID1.EQ.ID2.AND.ID2.EQ.ID3) GO TO 450
      JSPIN=IDINT(RNDMD(-1)+PJSPNC)
      IF(SPINT.AND.IR.EQ.2) JSPIN=0
      IF(SPINT.AND.IR.EQ.1) JSPIN=1
 450  IF(JSPIN.EQ.1.OR.ID1.EQ.ID2.OR.ID2.EQ.ID3) GO TO 460
      DRND=RNDMD(-1)
      IF(DRND.GT.PJSPNC) GO TO 460
      ISWAP=ID1
      ID1=ID2
      ID2=ISWAP
 460  IDHAD=1000*IABS(ID1)+100*IABS(ID2)+10*IABS(ID3)+JSPIN
      IDHAD=ISIGN(IDHAD,IFL1)
 470  IDPARC=IDHAD
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE STRING(IFL1,IFL2,AMSTR)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 AMSTR
C  HADRONS PRODUCTION BY MEANS STRING BREAKING
C  WITH QUARK AND ANTIQUARK OR QUARK AND DIQUARK OR DIQUARK AND
C  ANTIDIQUARK IFL1 AND IFL2 ON ENDS
C  AMSTR IS MASS OF STRING
      COMMON/CONST/PI,SQRT2,ALF,GF,UNITS
      COMMON/ITAPES/ ITDKY,ITEVT,ITCOM,ITLIS
      COMMON/PRIMAR/SCM,HALFE,ECM,NJET,IDIN(2),NEVENT,NTRIES
      COMMON/PARTCL/PPTCL(9,499),NPTCL,IORIG(499),IDENT(499)
     *,IDCAY(499)
      COMMON/COMLID/ PLIDER(499)
      COMMON/COMQSE/ QSEE,QVSEE
      LOGICAL QSEE,QVSEE
      COMMON/FRGSPA/ PUDS,PUDSC,PSPINS,PJSPNS,PMIX1S(3,2),PMIX2S(3,2),
     *SIGQTS,WENDM,WENDB,PBARS,PRIQS(9),PARDBS,PARQLS,PARRS,PUNDS
      COMMON /PARCUT/ SWMAX
      DIMENSION PX1L(2),PY1L(2),PX1(2),PY1(2),PMTS(2),W(2),IFL(2)
      LOGICAL DIQBR,SPINT,BACK
      LOGICAL MESON
      DIMENSION VC(3)
      DIMENSION PPTL(5,200),PPTR(5,200),IDENTL(200),IDENTR(200)
      COMMON/KAPPA/ XAP
      COMMON /CINSID/ INSIDE
      COMMON/COMWTI/ WTIME
      LOGICAL WTIME
      COMMON/PRINTS/IPRINT
      LOGICAL IPRINT
      COMMON/COLRET/ LRET
      COMMON/NCASCA/ NCAS,NCPRI
      LOGICAL LRET
      DIMENSION P8(500),P9(500)
      LRET=.FALSE.
      NREP = 0
      MXPTCL=499
C     IF(.NOT.WTIME) GO TO 99
C     CALL STRINT(IFL1,IFL2,AMSTR)
C     RETURN
99    DIQBR=.TRUE.
      NFIX=NPTCL
      BACK=.TRUE.
 100  I=NFIX
      NPR=0
      NPL=0
      NPTCL=NFIX
      NREP=NREP+1
c-Sob      IF(NCAS.GE.NCPRI) write(*,*) 'STRING:Nrep,Ntries=', NREP,NTRIES
      IF(NREP.LT.NTRIES) GO TO 102
      LRET=.TRUE.
      IF(IPRINT) WRITE(ITLIS,1200) NREP
1200  FORMAT(3X,' IN STRING NREP GT ',I8)
      RETURN
102   CONTINUE
      IFL(1)=IFL1
      IFL(2)=IFL2
      DO 110 J=1,2
 110  W(J)=AMSTR
      DO 120 J=1,2
      PX1L(J)=0.
      PY1L(J)=0.
      PX1(J)=0.
 120  PY1(J)=0.
C  WILL BE ONLY ONE BREAK OR NOT
      SPINT=.TRUE.
      KSPIN=1
      IF(MOD(IFL(1),100).EQ.0.AND.MOD(IFL(2),100).EQ.0) GO TO 131
      IDR=IDPARS(IFL(1),IFL(2),SPINT,KSPIN)
      PARC1=0.35
      IF(IABS(IFL(1)).EQ.3.OR.IABS(IFL(2)).EQ.3) PARC1=0.5
      IF(IABS(IFL(1)).GE.4.AND.IABS(IFL(1)).LT.6) PARC1=0.7
      IF(IABS(IFL(2)).GE.4.AND.IABS(IFL(2)).LT.6) PARC1=0.7
      WEND=(AMASS(IDR)+PARC1)**2
      GO TO 151
131   IFCN=1
      IF(RNDMD(-1).GT.0.5) IFCN=2
      IFLC1=IFCN
      IF(IFL(1).LT.0) IFLC1=-IFCN
      IDR1=IDPARS(IFL(1),IFLC1,SPINT,KSPIN)
      IDR2=IDPARS(IFL(2),-IFLC1,SPINT,KSPIN)
      WEND=(AMASS(IDR1)+AMASS(IDR2)+SWMAX)**2
151   SPINT=.FALSE.
      KSPIN=0
      IF(W(1)*W(2).LE.WEND) GO TO 225
 130  I=I+1
      IF(I.GT.MXPTCL) GO TO 9999
      MESON=.FALSE.
C  CHOOSE SIDE OF BREAK
      JSIDE=IDINT(1.+2.*RNDMD(-1))
      IF(JSIDE.EQ.1) NPR=NPR+1
      IF(JSIDE.EQ.2) NPL=NPL+1
      IF(NPR.GT.200.OR.NPL.GT.200) GO TO 9999
C  IF IFL(JSIDE) A QUARK OR A DIQUARK
      IF(MOD(IFL(JSIDE),100).EQ.0) GO TO 150
C  IFL(JSIDE) IS A QUARK
C SELECT Q,QBAR PAIR OR QQ,QQBAR PAIR
      DIQBR=.FALSE.
      DRND=RNDMD(-1)
      IF(DRND.LT.PBARS) GO TO 140
C  Q,QBAR PAIR
      IFLN=ISIGN(IDINT(RNDMD(-1)/PUDS)+1,-IFL(JSIDE))
      IF(IABS(IFLN).EQ.3)
     *IFLN=ISIGN(IDINT(RNDMD(-1)/PUDSC)+3,-IFL(JSIDE))
      GO TO 200
C  QQ,QQBAR PAIR
140   IQ1=IDINT(RNDMD(-1)/PUDS)+1
      IF(IQ1.EQ.3) IQ1=IDINT(RNDMD(-1)/PUDSC)+3
      IQ2=IDINT(RNDMD(-1)/PUDS)+1
      IF(IQ2.EQ.3) IQ2=IDINT(RNDMD(-1)/PUDSC)+3
      IF(IQ1.LE.IQ2) GO TO 145
      ISWAP=IQ1
      IQ1=IQ2
      IQ2=ISWAP
145   IFQQ=1000*IQ1+100*IQ2
      IFLN=ISIGN(IFQQ,IFL(JSIDE))
      GO TO 200
C  IFL(JSIDE) IS A DIQUARK
C  CAN DIQUARK BREAK OR NOT
 150  DRND=RNDMD(-1)
      IF(DRND.LE. PARDBS) GO TO 190
C DIQUARK BREAK
      MESON=.TRUE.
      CALL FLAVOR(IFL(JSIDE),IFLD1,IFLD2,IFLD3,JSPIN,INDEX)
      IFLL=IFLD1
      IFL(JSIDE)=IFLD2
      DRND=RNDMD(-1)
      IF(DRND.GE.PARQLS) GO TO 160
      IFLL=IFLD2
      IFL(JSIDE)=IFLD1
 160  DIQBR=.TRUE.
C  LEADING QUARK TRANSFERSE MOMENTUM
C     CALL PTDGET(PXL,PYL,SIGQTS)
      CALL GAUSPT(PTL0,SIGQTS)
      PHI=2.*PI*RNDMD(-1)
      PXL=PTL0*DCOS(PHI)
      PYL=PTL0*DSIN(PHI)
      PX1L(JSIDE)=PX1(JSIDE)
      PY1L(JSIDE)=PY1(JSIDE)
      PX1(JSIDE)=-PXL
      PY1(JSIDE)=-PYL
C Q,QBAR PAIR
      IFLN=ISIGN(IDINT(RNDMD(-1)/PUDS)+1,-IFL(JSIDE))
      IF(IABS(IFLN).EQ.3)
     *IFLN=ISIGN(IDINT(RNDMD(-1)/PUDSC)+3,-IFL(JSIDE))
      GO TO 200
C DIQUARK DOES NOT BREAK
C Q,QBAR PAIR
 190  IFLN=ISIGN(IDINT(RNDMD(-1)/PUDS)+1,IFL(JSIDE))
      IF(IABS(IFLN).EQ.3)
     *IFLN=ISIGN(IDINT(RNDMD(-1)/PUDSC)+3,IFL(JSIDE))
      DIQBR=.FALSE.
C  IDENT,MASS AND TRANSFERSE MOMENTUM OF PARTICLE
 200  IDENT(I)=IDPARS(IFL(JSIDE),IFLN,SPINT,KSPIN)
      PPTCL(5,I)=AMASS(IDENT(I))
C     CALL PTDGET(PX2,PY2,SIGQTS)
      CALL GAUSPT(PT2,SIGQTS)
      PHI=2.*PI*RNDMD(-1)
      PX2=PT2*DCOS(PHI)
      PY2=PT2*DSIN(PHI)
      PPTCL(1,I)=PX1(JSIDE)+PX2
      PPTCL(2,I)=PY1(JSIDE)+PY2
C GENERATE Z
      PMTS(3-JSIDE)=AMASS(IABS(IFL(3-JSIDE)))**2
      PTS=PPTCL(1,I)**2+PPTCL(2,I)**2
      PMTS(JSIDE)=PPTCL(5,I)**2+PTS
      IF(PMTS(JSIDE)+PMTS(3-JSIDE).GE.PARRS*W(1)*W(2)) GO TO 100
      ZMIN=PMTS(JSIDE)/(W(1)*W(2))
      ZMAX=1.-PMTS(3-JSIDE)/(W(1)*W(2))
      IF(ZMIN.GE.ZMAX) GO TO 100
C  WARNING. VERY IMPORTANT THE ORDER OF IFL AND IFLN IN ZFRAGS
c-Sob      if(NCAS.GE.NCPRI) write(*,*) 'to ZFRAG'
ca     Z=ZFRAG0(IFL(JSIDE),IFLN,MESON,PTS,ZMIN,ZMAX)
      Z=ZFRAGS(IFL(JSIDE),IFLN,PTS,ZMIN,ZMAX)
c-Sob      if(NCAS.GE.NCPRI) write(*,*) 'from ZFRAG'
      PPTCL(3,I)=0.5*(Z*W(JSIDE)-PMTS(JSIDE)/(Z*W(JSIDE)))
     **(-1.)**(JSIDE+1)
      PPTCL(4,I)=0.5*(Z*W(JSIDE)+PMTS(JSIDE)/(Z*W(JSIDE)))
      IDCAY(I)=0
      IF(.NOT.(JSIDE.EQ.1)) GO TO 282
      IDENTR(NPR)=IDENT(I)
      PPTR(1,NPR)=PPTCL(1,I)
      PPTR(2,NPR)=PPTCL(2,I)
      PPTR(3,NPR)=PPTCL(3,I)
      PPTR(4,NPR)=PPTCL(4,I)
      PPTR(5,NPR)=PPTCL(5,I)
 282  IF(.NOT.(JSIDE.EQ.2)) GO TO 283
      IDENTL(NPL)=IDENT(I)
      PPTL(1,NPL)=PPTCL(1,I)
      PPTL(2,NPL)=PPTCL(2,I)
      PPTL(3,NPL)=PPTCL(3,I)
      PPTL(4,NPL)=PPTCL(4,I)
      PPTL(5,NPL)=PPTCL(5,I)
 283  IF(DIQBR) GO TO 210
      IFL(JSIDE)=-IFLN
      PX1(JSIDE)=-PX2
      PY1(JSIDE)=-PY2
      GO TO 220
C  NEW DIQUARK CREATION
210   ID1=IABS(IFLL)
      ID2=IABS(IFLN)
      IF(ID1.LE.ID2) GO TO 215
      ISWAP=ID1
      ID1=ID2
      ID1=ISWAP
215   IFL(JSIDE)=ISIGN(1000*ID1+100*ID2,IFLL)
      PX1L(JSIDE)=PX1L(JSIDE)+PXL-PX2
      PY1L(JSIDE)=PY1L(JSIDE)+PYL-PY2
      PX1(JSIDE)=PX1L(JSIDE)
      PY1(JSIDE)=PY1L(JSIDE)
 220  W(1)=W(1)-PPTCL(4,I)-PPTCL(3,I)
      W(2)=W(2)-PPTCL(4,I)+PPTCL(3,I)
      SPINT=.TRUE.
      KSPIN=2
      PARC=0.2
      IF(MOD(IFL(1),100).EQ.0.AND.MOD(IFL(2),100).EQ.0) GO TO 240
      IDB=IDPARS(IFL(1),IFL(2),SPINT,KSPIN)
      IF(IABS(IFL(1)).GE.4.AND.IABS(IFL(1)).LT.6) PARC=0.7
      IF(IABS(IFL(2)).GE.4.AND.IABS(IFL(2)).LT.6) PARC=0.7
      IF(IABS(IFL(1)).EQ.3.OR.IABS(IFL(2)).EQ.3) PARC=0.5
      AMB=AMASS(IDB)+PARC
      GO TO 211
240   IFCN=1
      IF(RNDMD(-1).GT.0.5) IFCN=2
      IFLC1=-IFCN
      IF(IFL(1).GT.0) IFLC1=IFCN
      IFLC2=-IFLC1
      IKH1=IDPARS(IFL(1),IFLC1,SPINT,KSPIN)
      IKH2=IDPARS(IFL(2),IFLC2,SPINT,KSPIN)
      AMB=AMASS(IKH1)+AMASS(IKH2)+PARC
211   P1X=PX1(1)+PX1(2)
      P1Y=PY1(1)+PY1(2)
      PT12=P1X**2+P1Y**2
      W12=W(1)*W(2)
      AMS2=W12-PT12
      IF(AMS2.LT.AMB**2) GO TO 100
      SPINT=.TRUE.
      KSPIN=1
      PARC1=0.2
      IF(MOD(IFL(1),100).EQ.0.AND.MOD(IFL(2),100).EQ.0) GO TO 231
      IF(IABS(IFL(1)).EQ.3.OR.IABS(IFL(2)).EQ.3) PARC1=0.5
      IF(IABS(IFL(1)).GE.4.AND.IABS(IFL(1)).LT.6) PARC1=0.7
      IF(IABS(IFL(2)).GE.4.AND.IABS(IFL(2)).LT.6) PARC1=0.7
      IDR=IDPARS(IFL(1),IFL(2),SPINT,KSPIN)
      WEND=(AMASS(IDR)+PARC1)**2
      GO TO 232
231   IKHR1=IDPARS(IFL(1),IFLC1,SPINT,KSPIN)
      IKHR2=IDPARS(IFL(2),IFLC2,SPINT,KSPIN)
      WEND=(AMASS(IKHR1)+AMASS(IKHR2)+PARC1)**2
232   SPINT=.FALSE.
      KSPIN=0
      IF(W(1)*W(2).GE.WEND) GO TO 130
         GO TO 230
225   P1X=PX1(1)+PX1(2)
      P1Y=PY1(1)+PY1(2)
      PT12=P1X**2+P1Y**2
      W12=W(1)*W(2)
      AMS2=W12-PT12
C  LAST BREAK OF STRING
 230  NPTCL=I
      AMC=DSQRT(AMS2)
      EC=(W(1)+W(2))/2.0
      VC(1)=P1X/EC
      VC(2)=P1Y/EC
      VC(3)=(W(1)-W(2))/(2.0*EC)
      NIN1=NPTCL+1
      CALL CLUSTR(IFL(1),IFL(2),AMC)
      IF(LRET) GO TO 100
      NFIN1=NPTCL
      CALL LORTR(VC,NIN1,NFIN1,BACK)
      NPR=NPR+1
      NPL=NPL+1
      IF(NPR.GT.200.OR.NPL.GT.200) GO TO 9999
      IDENTL(NPL)=IDENT(NFIN1)
      PPTL(1,NPL)=PPTCL(1,NFIN1)
      PPTL(2,NPL)=PPTCL(2,NFIN1)
      PPTL(3,NPL)=PPTCL(3,NFIN1)
      PPTL(4,NPL)=PPTCL(4,NFIN1)
      PPTL(5,NPL)=PPTCL(5,NFIN1)
      IDENTR(NPR)=IDENT(NIN1)
      PPTR(1,NPR)=PPTCL(1,NIN1)
      PPTR(2,NPR)=PPTCL(2,NIN1)
      PPTR(3,NPR)=PPTCL(3,NIN1)
      PPTR(4,NPR)=PPTCL(4,NIN1)
      PPTR(5,NPR)=PPTCL(5,NIN1)
      JJ=NFIX
      DO 284 J=1,NPR
      JJ=JJ+1
      IDENT(JJ)=IDENTR(J)
      PPTCL(1,JJ)=PPTR(1,J)
      PPTCL(2,JJ)=PPTR(2,J)
      PPTCL(3,JJ)=PPTR(3,J)
      PPTCL(4,JJ)=PPTR(4,J)
      PPTCL(5,JJ)=PPTR(5,J)
284   CONTINUE
      JJ=NFIX+NPR
      DO 285 J=1,NPL
      JJ=JJ+1
      K=NPL-J+1
      IDENT(JJ)=IDENTL(K)
      PPTCL(1,JJ)=PPTL(1,K)
      PPTCL(2,JJ)=PPTL(2,K)
      PPTCL(3,JJ)=PPTL(3,K)
      PPTCL(4,JJ)=PPTL(4,K)
      PPTCL(5,JJ)=PPTL(5,K)
285   CONTINUE
      N1=NFIX+1
      N2=NFIX+NPR+NPL-1
      IF(INSIDE.NE.0)   GO  TO  385
C------------------------------------------------------C
C----- CONSTITUENT      TIME           ----------------C
C------------------------------------------------------C
      DO 286 J=N1,N2
      P3S=0.
      ES=0.
      DO 287 L=N1,J
      P3S=P3S+PPTCL(3,L)
 287  ES=ES+PPTCL(4,L)
      TI=(AMSTR-2.*P3S)/(2.*XAP)
      ZI=(AMSTR-2.*ES)/(2.*XAP)
      IF(J.NE.N2) GO TO 288
      TII=TI
      ZII=ZI
 288  PPTCL(6,J)=0.
      PPTCL(7,J)=0.
      PPTCL(8,J)=ZI
      PPTCL(9,J)=TI
 286  CONTINUE
      PPTCL(6,N2+1)=0.
      PPTCL(7,N2+1)=0.
      PPTCL(8,N2+1)=ZII
      PPTCL(9,N2+1)=TII
C]]]]]]]]]]]]
      IF(N2.LE.1) GO TO 485
      LN11=N1+1
      DO 1289 L=LN11,N2
      P8(L)=0.5*(PPTCL(8,L-1)+PPTCL(8,L))
      P9(L)=0.5*(PPTCL(9,L-1)+PPTCL(9,L))
1289  CONTINUE
      DO 1389 L=LN11,N2
      PPTCL(8,L)=P8(L)
      PPTCL(9,L)=P9(L)
1389  CONTINUE
C]]]]]]]]]]]]
      GO  TO  485
C------------------------------------------------------C
C-----  INSIDE-OUTSIDE  TIME           ----------------C
C------------------------------------------------------C
 385  CONTINUE
      DO 386 J=N1,NPTCL
      P3S=0.
      ES=0.
      NJ=J-1
      IF(NJ.EQ.0) GO TO 389
      DO 387 L=N1,NJ
      P3S=P3S+PPTCL(3,L)
 387  ES=ES+PPTCL(4,L)
 389  TI=(AMSTR-2.*P3S+PPTCL(4,J)-PPTCL(3,J))/(2.*XAP)
      ZI=(AMSTR-2.*ES-PPTCL(4,J)+PPTCL(3,J))/(2.*XAP)
      PPTCL(6,J)=0.
      PPTCL(7,J)=0.
      PPTCL(8,J)=ZI
      PPTCL(9,J)=TI
 386  CONTINUE
 485  CONTINUE
C------------------------------------------------------C
      DO 486 J=N1,NPTCL
 486  PLIDER(J)=0.
      IF(QSEE) RETURN
      IB1=IB(IDENT(N1))
      IB2=IB(IDENT(NPTCL))
      PLIDER(N1)=.667
      PLIDER(NPTCL)=.667
      IF(IB1.EQ.0) PLIDER(N1)=.5
      IF(IB2.EQ.0) PLIDER(NPTCL)=.5
      IF(PLIDER(N1).GT.0.6.AND.MOD(IFL1,100).NE.0) PLIDER(N1)=0.333
      IF(PLIDER(NPTCL).GT..6.AND.MOD(IFL2,100).NE.0)PLIDER(NPTCL)=.333
      IF(.NOT.QVSEE) RETURN
      IF(IB1.EQ.0.AND.IB2.EQ.0) GO TO 1387
      IF(IB1.EQ.0) PLIDER(N1)=0.
      IF(IB2.EQ.0) PLIDER(NPTCL)=0.
      IF(PLIDER(N1).GT.0.6.AND.MOD(IFL1,100).NE.0) PLIDER(N1)=0.333
      IF(PLIDER(NPTCL).GT..6.AND.MOD(IFL2,100).NE.0)PLIDER(NPTCL)=.333
      RETURN
1387  RM=RNDMD(-1)
      IF(RM.GT.0.5) PLIDER(N1)=0.
      IF(RM.LE.0.5) PLIDER(NPTCL)=0.
      RETURN
9999  WRITE(ITLIS,9998) I
9998  FORMAT(//10X,40H...STOP IN STRING..NPTCL TOO HIGH NPTCL=,I5)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION IDPARS(IFL01,IFL02,SPINT,IR)
      IMPLICIT REAL*8 (A-H,O-Z)
C   CONSTRUCT MESON FROM QUARK AND ANTIQUARK WITH FLAVORS IFL01,IFL02
C   OR CONSTRUCT BARION FROM DIQUARK AND QUARK OR ANTIDIQUARK AND
C  ANTIQUARK WITH FLAVORS IFL01,IFL02
      COMMON/FRGSPA/ PUDS,PUDSC,PSPINS,PJSPNS,PMIX1S(3,2),PMIX2S(3,2),
     *SIGQTS,WENDM,WENDB,PBARS,PRIQS(9),PARDBS,PARQLS,PARRS,PUNDS
      LOGICAL SPINT
      IFL1=IFL01
      IFL2=IFL02
C  CONSTRUCT MESON WITH ACCOUNT FLAVOR MIXING
      IF(MOD(IFL1,100).EQ.0) GO TO 420
      IF(MOD(IFL2,100).EQ.0) GO TO 425
      JSPIN=IDINT(PSPINS+RNDMD(-1))
      IF(SPINT.AND.IR.EQ.2) JSPIN=0
      IF(SPINT.AND.IR.EQ.1) JSPIN=1
      ID1=IFL1
      ID2=IFL2
      IF(ID1+ID2.NE.0) GO TO 400
      RND=RNDMD(-1)
      ID1=IABS(ID1)
      ID1=IDINT(PMIX1S(ID1,JSPIN+1)+RND)+
     +IDINT(PMIX2S(ID1,JSPIN+1)+RND)+1
      ID2=-ID1
 400  IF(IABS(ID1).LE.IABS(ID2)) GO TO 410
      ISAVE=ID1
      ID1=ID2
      ID2=ISAVE
 410  IDHAD=ISIGN(100*IABS(ID1)+10*IABS(ID2)+JSPIN,ID1)
      GO TO 470
C  CONSTRUCT BARYON IDENT
 420  ID3=ISIGN(MOD(IFL1/100,10),IFL1)
      ID2=IFL1/1000
      ID1=IFL2
      GO TO 430
 425  ID3=ISIGN(MOD(IFL2/100,10),IFL2)
      ID2=IFL2/1000
      ID1=IFL1
 430  IF(IABS(ID1).LE.IABS(ID2)) GO TO 431
      ISWAP=ID1
      ID1=ID2
      ID2=ISWAP
 431  IF(IABS(ID2).LE.IABS(ID3)) GO TO 432
      ISWAP=ID2
      ID2=ID3
      ID3=ISWAP
 432  IF(IABS(ID1).LE.IABS(ID2)) GO TO 440
      ISWAP=ID1
      ID1=ID2
      ID2=ISWAP
  440 JSPIN=1
      IF(ID1.EQ.ID2.AND.ID2.EQ.ID3) GO TO 450
      JSPIN=IDINT(RNDMD(-1)+PJSPNS)
      IF(SPINT.AND.IR.EQ.2) JSPIN=0
      IF(SPINT.AND.IR.EQ.1) JSPIN=1
  450 IF(JSPIN.EQ.1.OR.ID1.EQ.ID2.OR.ID2.EQ.ID3) GO TO 460
      DRND=RNDMD(-1)
      IF(DRND.GT.PJSPNS) GO TO 460
      ISWAP=ID1
      ID1=ID2
      ID2=ISWAP
 460  IDHAD=1000*IABS(ID1)+100*IABS(ID2)+10*IABS(ID3)+JSPIN
      IDHAD=ISIGN(IDHAD,IFL1)
 470  IDPARS=IDHAD
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE LORTR(V,NIN,NFIN,BACK)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 V
C
C   LORENTZ BOOST ENERGY AND MOMENTUM OF HADRON
C
      COMMON/PARTCL/PPTCL(9,499),NPTCL,IORIG(499),IDENT(499)
     *,IDCAY(499)
      DIMENSION V(3)
      LOGICAL BACK
      L=1
      IF(BACK) L=-1
      VV=V(1)*V(1)+V(2)*V(2)+V(3)*V(3)
      GA=1.D0/DSQRT(DABS(1.D0-VV))
      DO 100 J=NIN,NFIN
      VP=V(1)*PPTCL(1,J)+V(2)*PPTCL(2,J)+V(3)*PPTCL(3,J)
      GAVP=GA*(GA*VP/(1.+GA)-FLOAT(L)*PPTCL(4,J))
      PPTCL(1,J)=PPTCL(1,J)+GAVP*V(1)
      PPTCL(2,J)=PPTCL(2,J)+GAVP*V(2)
      PPTCL(3,J)=PPTCL(3,J)+GAVP*V(3)
      PMAS=PPTCL(5,J)
      PPTCL(4,J)=DSQRT(PPTCL(1,J)**2+PPTCL(2,J)**2+PPTCL(3,J)**2+
     +PMAS**2)
100   CONTINUE
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE LORCO(V,NIN,NFIN,BACK)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 V
C
C   LORENTZ BOOST TIME AND COORDINATES OF HADRON
C
      COMMON/PARTCL/PPTCL(9,499),NPTCL,IORIG(499),IDENT(499)
     *,IDCAY(499)
      DIMENSION V(3)
      LOGICAL BACK
      L=1
      IF(BACK) L=-1
      VV=V(1)*V(1)+V(2)*V(2)+V(3)*V(3)
      GA=1.D0/DSQRT(DABS(1.D0-VV))
      DO 100 J=NIN,NFIN
      VR=V(1)*PPTCL(6,J)+V(2)*PPTCL(7,J)+V(3)*PPTCL(8,J)
      PPTCL(6,J)=PPTCL(6,J)+GA*V(1)*(VR*GA/(GA+1.)-FLOAT(L)*PPTCL(9,J))
      PPTCL(7,J)=PPTCL(7,J)+GA*V(2)*(VR*GA/(GA+1.)-FLOAT(L)*PPTCL(9,J))
      PPTCL(8,J)=PPTCL(8,J)+GA*V(3)*(VR*GA/(GA+1.)-FLOAT(L)*PPTCL(9,J))
      PPTCL(9,J)=GA*(PPTCL(9,J)-FLOAT(L)*VR)
100   CONTINUE
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE FLAVOR(ID,IFL1,IFL2,IFL3,JSPIN,INDEX)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C          THIS SUBROUTINE UNPACKS THE IDENT CODE ID=+/-IJKL
C
C          MESONS--
C          I=0, J<=K, +/- IS SIGN FOR J
C          ID=110 FOR PI0, ID=220 FOR ETA, ETC.
C
C          BARYONS--
C          I<=J<=K IN GENERAL
C          J<I<K FOR SECOND STATE ANTISYMMETRIC IN (I,J), EG. L = 2130
C
C          OTHER--
C          ID=1,...,6 FOR QUARKS
C          ID=9 FOR GLUON
C          ID=10 FOR PHOTON
C          ID=11,...,16 FOR LEPTONS
C          ID=20 FOR KS, ID=-20 FOR KL
C
C          I=21...26 FOR SCALAR QUARKS
C          I=29 FOR GLUINO
C          I=30 FOR PHOTINO
C          I=31...36 FOR SCALAR LEPTONS
C          I=39 FOR WINO
C          I=40 FOR ZINO
C
C          ID=80 FOR W+
C          ID=81,...,89 FOR HIGGS MESONS
C          ID=90 FOR Z0
C
C          DIQUARKS--
C          ID=+/-IJ00, I<J FOR DIQUARK COMPOSED OF I,J.
C
C          INDEX IS A SEQUENCE NUMBER USED INTERNALLY
C
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      COMMON/QLMASS/ AMLEP(52),NQLEP,NMES,NBARY
*@@@@@@@@@@   SIVOKL  @@@@@@@@@@
      COMMON/FLACOM/NFLA,NFL1,NFL2,NFL3,NSPIN,NNDEX
*@@@@@@@@@@@@@@@@@@@@
      IDABS=IABS(ID)
      I=IDABS/1000
      J=MOD(IDABS/100,10)
      K=MOD(IDABS/10,10)
      JSPIN=MOD(IDABS,10)
      IF(ID.NE.0.AND.MOD(ID,100).EQ.0) GO TO 300
      IF(J.EQ.0) GO TO 200
      IF(I.EQ.0) GO TO 100
C          BARYONS
C         ONLY X,Y BARYONS ARE QQX, QQY, Q=U,D,S.
      IFL1=ISIGN(I,ID)
      IFL2=ISIGN(J,ID)
      IFL3=ISIGN(K,ID)
      IF(.NOT.(K.LE.6)) GO TO 1
        INDEX=MAX0(I-1,J-1)**2+I+MAX0(I-J,0)+(K-1)*K*(2*K-1)/6
     1  +109*JSPIN+36*NMES+NQLEP+11
      GO TO 2
1       CONTINUE
        INDEX=MAX0(I-1,J-1)**2+I+MAX0(I-J,0)+9*(K-7)+91
     1  +109*JSPIN+36*NMES+NQLEP+11
2     CONTINUE
      RETURN
C          MESONS
100   CONTINUE
      IFL1=0
      IFL2=ISIGN(J,ID)
      IFL3=ISIGN(K,-ID)
      INDEX=J+K*(K-1)/2+36*JSPIN+NQLEP
      INDEX=INDEX+11
*@@@@@@@@@@@@@ SIVOKL - TONEEV @@@@@
      IF(ID.EQ.110.OR.ID.EQ.111.OR.ID.EQ.221) GO TO 13
      IF(ID.EQ.220.OR.ID.EQ.330)GOTO 12
      RETURN
 12   IFL2=2+IDINT(0.25+RNDMD(-1))
      IF(IFL2.EQ.2) IFL2=1+IDINT(0.5+RNDMD(-1))
      IFL2=ISIGN(IFL2,ID)
      IFL3=ISIGN(IFL2,-ID)

      IF(NFLA.EQ.-1) THEN
      NFL1=IFL1
      NFL2=IFL2
      NFL3=IFL3
      NSPIN=JSPIN
      NNDEX=INDEX
      NFLA=ID
      ENDIF
      RETURN
 13   IFL2= 1+IDINT(0.5+RNDMD(-1))
      IFL2=ISIGN(IFL2,ID)
      IFL3=ISIGN(IFL2,-ID)

      IF(NFLA.EQ.-1) THEN
      NFL1=IFL1
      NFL2=IFL2
      NFL3=IFL3
      NSPIN=JSPIN
      NNDEX=INDEX
      NFLA=ID
c      RETURN
c in the next line NFLB was not defined 11.16.94 V.T. !
c      ELSE IF(NFLB.EQ.-1) THEN
c      MFL1=IFL1
c      MFL2=IFL2
c      MFL3=IFL3
c      MSPIN=JSPIN
c      MNDEX=INDEX
c      NFLB=ID
      ENDIF
*@@@@@@@@@@@@@@@@@@@@
      RETURN
200   CONTINUE
      IFL1=0
      IFL2=0
      IFL3=0
      JSPIN=0
      INDEX=IDABS
      IF(IDABS.LT.20) RETURN
C          DEFINE INDEX=20 FOR KS, INDEX=21 FOR KL
      INDEX=IDABS+1
      IF(ID.EQ.20) INDEX=20
C          INDEX=NQLEP+1,...,NQLEP+11 FOR W+, HIGGS, Z0
      IF(IDABS.LT.80) RETURN
      INDEX=NQLEP+IDABS-79
      RETURN
300   IFL1=ISIGN(I,ID)
      IFL2=ISIGN(J,ID)
      IFL3=0
      JSPIN=0
      INDEX=0
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE GAUSPT(PT0,SIGQT)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 PT0,SIGQT
C  GENERATE PT WITH 1/DSQRT(PI*SIGQT**2)*DEXP(-PT**2/SIGQT**2)
C  DISTRIBUTION
      RND=RNDMD(-1)
      PT0=SIGQT*DSQRT(-DLOG(RND))
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE FLAVOB(IDB,IFLQ,IFLQQ)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  COMPUTE FLAVOUR OF INTERACTING QUARK
C
      CALL FLAVOR(IDB,IFL01,IFL02,IFL03,JSPIN,INDEX)
      IF(IFL01.NE.0) GO TO 150
      IFLQ=IFL02
      IFLQQ=IFL03
      RETURN
150   INR=IDINT(1.+3.*RNDMD(-1))
      IF(INR.EQ.1) GO TO 100
      IF(INR.EQ.2) GO TO 200
      IFLQ=IFL03
      ID1=IABS(IFL01)
      ID2=IABS(IFL02)
      ISWAP=ID1
      GO TO 300
100   IFLQ=IFL01
      ID1=IABS(IFL02)
      ID2=IABS(IFL03)
      ISWAP=ID1
      GO TO 300
200   IFLQ=IFL02
      ID1=IABS(IFL01)
      ID2=IABS(IFL03)
      ISWAP=ID1
300   IF(ID1.LE.ID2) GO TO 400
      ID1=ID2
      ID2=ISWAP
400   IFLQQ=ISIGN((1000*ID1+100*ID2),IFLQ)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
          SUBROUTINE XDISP2(XMIN,X1,X2)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XMIN,X1,X2
      COMMON/CONST/PI,SQRT2,ALF,GF,UNITS
C         U(X)=1./DSQRT(X1*X2)*DELTA(1.-X1-X2)
 100      X1=0.5+0.5*DSIN(PI*(RNDMD(-1)-0.5))
          IF(X1.LE.XMIN.OR.X1.GT.1.-XMIN) GO TO 100
          X2=1.-X1
          RETURN
           END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION XDISTP(XMIN,IB1,IS1,IFL1)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XDISTP,XMIN
      COMMON/COMABM/ ALFAM,BETAM
      COMMON/COMABB/ ALFAB,BETAB
      IFL=IABS(IFL1)
      IF(IABS(IB1).EQ.1) GO TO 3
      IF(IS1.NE.0) GO TO 33
 2    CALL SBETA(X,ALFAM,BETAM)
      IF(RNDMD(-1).GE.0.5) X=1.-X
      IF(X.LE.XMIN.OR.X.GT.1.-XMIN ) GO TO 2
      XDISTP=X
      RETURN
 33   BETAN=1.
      GO TO 1
 3    BETAN=BETAB
      IF(IFL.GT.1) BETAN=BETAB+1
 1    CALL SBETA(X,ALFAB,BETAN)
      IF(X.LE.XMIN.OR.X.GT.1.-XMIN ) GO TO 1
      XDISTP=X
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
          SUBROUTINE XDIST3(X1,X2,X3)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 X1,X2,X3
      COMMON/CONST/PI,SQRT2,ALF,GF,UNITS
C         U(X)=1/DSQRT(X1*X2*X3)*DELTA(1.-X1-X2-X3)
          X1=RNDMD(-1)**2
          X2=(1.-X1)*DSIN(PI*RNDMD(-1)/2.)**2
          X3=1.-X1-X2
          RETURN
           END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION GAMHE(ID)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 GAMHE
C
C    THIS FUNCTION RETURNS THE WIDTH OF THE PARTICLE
C    WITH IDENT CODE ID
C    QUARK-BASED IDENT OF PARTICLE
C
      COMMON/QLMASS/ AMLEP(52),NQLEP,NMES,NBARY
      COMMON/ITAPES/ ITDKY,ITEVT,ITCOM,ITLIS
      DIMENSION GAMES0(10),GAMES1(10),GAMBR0(30),GAMBR1(30)
C    0-MESON WIDTH TABLE
C-------------- GAM(ETA) MUST BE 1.05 KEV (0.000001 GEV) ------C
      DATA GAMES0/ 2*0.,.83D-6,2*0.,0.00029,4*0./
C    1-MESON WIDTH TABLE
      DATA GAMES1/2*0.152,0.01,0.049,0.049,0.004,4*0./
C    1/2-BARION MASS TABLE
      DATA GAMBR0/-1.,2*0.,2*-1.,6*0.,3*-1.,12*0.,4*-1./
C    3/2-BARION MASS TABLE
      DATA GAMBR1/0.122,0.122,0.122,0.122,-1.,0.035,0.034,
     *0.042,-1.,0.0106,0.0091,0.,2*-1.,3*0.,-1.,3*0.,2*-1.,
     *4*0.,3*-1./
C    ENTRY
      CALL FLAVOR(ID,IFL1,IFL2,IFL3,JSPIN,INDEX)
      IF(IFL1.EQ.0) GO TO 100
C     BARYONS
      INDEX=INDEX-109*JSPIN-36*NMES-NQLEP
      INDEX=INDEX-11
      GAMHE=(1-JSPIN)*GAMBR0(INDEX)+JSPIN*GAMBR1(INDEX)
      RETURN
C    MESONS
100   CONTINUE
      INDEX=INDEX-36*JSPIN-NQLEP
      INDEX=INDEX-11
      GAMHE=(1-JSPIN)*GAMES0(INDEX)+JSPIN*GAMES1(INDEX)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE XSDIS(X,XMIN,XMAX)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 X,XMIN,XMAX
C
C   COMPUTE XSEE FROM 1./XSEE*(1.-XSEE)**(BETA-1) DISTRIBUTION
C
      COMMON/COMIND/ PUD,SIGMA,ALFA,BETA
      BETAO=BETA
      BETA=3.
      IF(XMAX.GT.0.99999999) XMAX=0.99999999
      PE1=(1.-XMIN)**(BETA+1.)/(BETA+1.)-(1.-XMAX)**(BETA+1.)/
     *(BETA+1.)
      PE=PE1+DLOG(XMAX/XMIN)
      PE1=PE1/PE
100   RND=RNDMD(-1)
      IF(RNDMD(-1).GT.PE1) GO TO 200
      X=1.-((1.-XMIN)**(BETA+1.)*(1.-RND)+(1.-XMAX)**(BETA+1.)*
     *RND)**(1./(BETA+1.))
      GO TO 300
200   X=XMIN*(XMAX/XMIN)**RND
300   PPE1=(1.-X)**BETA
      PPE2=1./X
      PPE1=PPE1+PPE2
      PPE2=PPE1*PPE2
      IF(PPE1*RNDMD(-1).GT.PPE2) GO TO 100
      BETA=BETAO
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE X2DIST(X1,X2,IFL1,IFL2)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 X1,X2
      COMMON/COMABB/ ALFAB,BETAB
      BETA=BETAB-1.
      ALFA=BETAB-1.
      IF(IABS(IFL1).GT.1) ALFA=BETAB+1.
      IF(IABS(IFL2).GT.1) BETA=BETAB+1.
      XMAX=(BETA-.5)/(ALFA+BETA-1.)
      FMAX=XMAX**(BETA-.5)*(1.-XMAX)**(ALFA-.5)
 100  X1=RNDMD(-1)
      FX1=X1**(BETA-.5)*(1.-X1)**(ALFA-.5)
      IF(FMAX*RNDMD(-1).GT.FX1) GO TO 100
      X2=1.-X1
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE X3DIST(X1,X2,X3,IFL1,IFL2,IFL3)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 X1,X2,X3
      COMMON/COMABB/ ALFAB,BETAB
      BETA1=BETAB
      IF(IABS(IFL1).GT.1) BETA1=BETAB+1.
      CALL SBETA(X1,ALFAB,BETA1)
      CALL X2DIST(X2,X3,IFL2,IFL3)
      XS=1.-X1
      X2=XS*X2
      X3=XS*X3
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
       SUBROUTINE XCORR(IFL1,IFL2,PX1,PY1,PX2,PY2,X1,X2,
     *PSIGN,NPRODS,RETU)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 PX1,PY1,PX2,PY2,X1,X2,PSIGN
C
C   DECAY STRING OR CLUSTER
C
      COMMON/ITAPES/ ITDKY,ITEVT,ITCOM,ITLIS
      COMMON/PARTCL/PPTCL(9,499),NPTCL,IORIG(499),IDENT(499)
     *,IDCAY(499)
      COMMON/COMLID/PLIDER(499)
      COMMON/PRIMP0/ P0
       DIMENSION V(3)
       DIMENSION PPX1(3),PPX2(3),PRX1(3),PRX2(3)
      COMMON/PRIMAR/SCM,HALFE,ECM,NJET,IDIN(2),NEVENT,NTRIES
      COMMON/MASQUA/ AMQ21,AMQ22
      COMMON /PARCUT/ SWMAX
      COMMON /NEEDR/ NRET
      COMMON /UNCYS/ NUNCY
      COMMON/COLRET/ LRET
      LOGICAL LRET
      COMMON/COMQSE/ QSEE,QVSEE
      LOGICAL QSEE,QVSEE
      COMMON/KAPPA/ XAP
      COMMON/KEYPLA/ KEYPLA
      COMMON/NCASCA/ NCAS,NCPRI
      LOGICAL KEYPLA
      LOGICAL BACK
      LOGICAL SPINT
      LOGICAL RETU
C     INITIALIZE
      NREP=0
      SWMAX0=SWMAX
1     CONTINUE
      SWMAX=SWMAX0
      NREP=NREP+1
c-Sob      if(NCAS.GE.NCPRI) WRITE(*,*) 'XCORR: Nrep,Ntries=',NREP,NTRIES
      IF(NREP.LT.NTRIES) GO TO 101
      RETU=.TRUE.
      SWMAX=SWMAX0
c-Sob      WRITE(ITLIS,1200) NREP,NTRIES
c-Sob1200  FORMAT(3X,' IN XCORR  NREP(=',I8,') GT NTRIES(=',I8,')')
      RETURN
101   CONTINUE
      NUNCY=0
      MXPTCL=499
      NPRODS=0
      NFIX=NPTCL
      BACK=.TRUE.
      SPINT=.TRUE.
      RETU=.FALSE.
      PTX=PX1+PX2
      PTY=PY1+PY2
      PT12=PX1**2+PY1**2
      PT22=PX2**2+PY2**2
      P1=X1*P0
      P2=X2*P0*PSIGN
      E12=P1**2+PT12+AMQ21
      IF(E12.GE.0.) GO TO 200
      RETU=.TRUE.
      RETURN
200   E22=P2**2+PT22+AMQ22
      IF(E22.GE.0.) GO TO 210
      RETU=.TRUE.
      RETURN
210   E1=DSQRT(E12)
      E2=DSQRT(E22)
      AMSS12=(E1+E2)**2-(P1+P2)**2-PTX**2-PTY**2
C   ARE THERE ANTIDIQUARK AND DIQUARK
      IF(MOD(IFL1,100).EQ.0.AND.MOD(IFL2,100).EQ.0) GO TO 100
       IKHR1=IDPARS(IFL1,IFL2,SPINT,2)
      PARBE=0.31
      IF(KEYPLA) PARBE=0.01
C     IF(IABS(IFL1).EQ.3.OR.IABS(IFL2).EQ.3) PARBE=0.5
      AMHR=AMASS(IKHR1)
       AMHRB=AMHR+PARBE
      IKHR=IDPARS(IFL1,IFL2,SPINT,1)
      AMHR1=AMASS(IKHR)
      IF(AMSS12.GT.AMHR**2) GO   TO  220
      RETU=.TRUE.
      RETURN
220   CONTINUE
       IF(AMSS12.GE.AMHRB**2) GO TO 400
      IF(NRET.EQ.1) GO TO 420
      NUNCY=1
      NPTCL=NPTCL+1
      IF(NPTCL.GT.MXPTCL) GO TO 9999
      IDENT(NPTCL)=IKHR1
      PPTCL(1,NPTCL)=PTX
      PPTCL(2,NPTCL)=PTY
      PPTCL(3,NPTCL)=P1+P2
      PPTCL(4,NPTCL)=E1+E2
C   !!!!!!!!!!!!!!!!!!!
C     AMHR=DSQRT(AMSS12)     14.08.91 !!!
C   !!!!!!!!!!!!!!!!!!!
      PPTCL(5,NPTCL)=AMHR
      IDCAY(NPTCL)=0
      PPTCL(6,NPTCL)=0.
      PPTCL(7,NPTCL)=0.
      PPTCL(8,NPTCL)=PPTCL(4,NPTCL)/XAP
      AMT=DSQRT(PTX**2+PTY**2+AMHR**2)
      PPTCL(9,NPTCL)=DSQRT(2.D0)*AMT/XAP*PPTCL(4,NPTCL)/PPTCL(5,NPTCL)
      PLIDER(NPTCL)=1.
      IF(QSEE.OR.QVSEE) PLIDER(NPTCL)=0.
      NPRODS=1
      IF(DSQRT(AMSS12).GE.AMHR) GO TO 419
      PPTCL(4,NPTCL)=DSQRT(AMHR**2+PPTCL(1,NPTCL)**2+
     + PPTCL(2,NPTCL)**2+PPTCL(3,NPTCL)**2)
 419    RETURN
 420   RETU=.TRUE.
       RETURN
100   IFCN=1
      IF(RNDMD(-1).GT.0.5) IFCN=2
      IFLC1=-IFCN
      IF(IFL1.GT.0) IFLC1=IFCN
      IFLC2=-IFLC1
      IF(KEYPLA) SWMAX=0.
      IKH1=IDPARS(IFL1,IFLC1,SPINT,2)
      IKH2=IDPARS(IFL2,IFLC2,SPINT,2)
      AMB2=(AMASS(IKH1)+AMASS(IKH2)+SWMAX)**2
      IF(AMSS12.GE.AMB2) GO TO 350
      IF(NRET.EQ.1) GO TO 430
      NUNCY=1
      AMSS12=AMB2
      E1PE2=AMB2+(P1+P2)**2+PTX**2+PTY**2
      E2   =DSQRT(E1PE2)-E1
      GO TO 350
430    RETU=.TRUE.
      SWMAX=SWMAX0
       RETURN
350   IKHR1=IDPARS(IFL1,IFLC1,SPINT,1)
      IKHR2=IDPARS(IFL2,IFLC2,SPINT,1)
      IF(AMSS12.GT.(AMASS(IKHR1)+AMASS(IKHR2)+SWMAX)**2) GO TO 500
      AMSS1=DSQRT(AMSS12)
      PZ1=P1+P2
      ESS1=E1+E2
      V(1)=PTX/ESS1
      V(2)=PTY/ESS1
      V(3)=PZ1/ESS1
      NIN1=NPTCL+1
c-Sob      if(NCAS.GE.NCPRI) WRITE(*,*) 'to CLUSTR1',IFL1,IFL2,AMSS1
      CALL CLUSTR(IFL1,IFL2,AMSS1)
c-Sob      if(NCAS.GE.NCPRI) WRITE(*,*) 'from CLUSTR1'

      IF(LRET) GO TO 1
      NFIN1=NPTCL
      CALL TIFILL(NIN1,NFIN1,AMSS1,IFL1,IFL2)
      PPX1(1)=PX1
      PPX1(2)=PY1
      PPX1(3)=P1
      BACK=.FALSE.
      CALL LORLC(V,PPX1,E1,BACK)
      CALL ANGLE(PPX1,CT,ST,CFI,SFI)
      DO 610 J=NIN1,NFIN1
      PRX1(1)=PPTCL(6,J)
      PRX1(2)=PPTCL(7,J)
      PRX1(3)=PPTCL(8,J)
      CALL ROTR(CT,ST,CFI,SFI,PRX1,PRX2,BACK)
      PPTCL(6,J)=PRX2(1)
      PPTCL(7,J)=PRX2(2)
      PPTCL(8,J)=PRX2(3)
610   CONTINUE
      BACK=.TRUE.
      CALL LORTR(V,NIN1,NFIN1,BACK)
      CALL LORCO(V,NIN1,NFIN1,BACK)
      NPRODS=NPTCL-NFIX
      SWMAX=SWMAX0
      RETURN
400   AMSS1=DSQRT(AMSS12)
      PZ1=P1+P2
      ESS1=E1+E2
      V(1)=PTX/ESS1
      V(2)=PTY/ESS1
      V(3)=PZ1/ESS1
      NIN1=NPTCL+1
      IF(AMSS1.GE.AMHR1+SWMAX) GO TO 600
c-Sob      if(NCAS.GE.NCPRI) WRITE(*,*) 'to CLUSTR2',IFL1,IFL2,AMSS1
      CALL CLUSTR(IFL1,IFL2,AMSS1)
c-Sob      if(NCAS.GE.NCPRI) WRITE(*,*) 'from CLUSTR2'
      IF(LRET) GO TO 1
      NFIN1=NPTCL
      CALL TIFILL(NIN1,NFIN1,AMSS1,IFL1,IFL2)
      PPX1(1)=PX1
      PPX1(2)=PY1
      PPX1(3)=P1
      BACK=.FALSE.
      CALL LORLC(V,PPX1,E1,BACK)
      CALL ANGLE(PPX1,CT,ST,CFI,SFI)
      DO 710 J=NIN1,NFIN1
      PRX1(1)=PPTCL(6,J)
      PRX1(2)=PPTCL(7,J)
      PRX1(3)=PPTCL(8,J)
      CALL ROTR(CT,ST,CFI,SFI,PRX1,PRX2,BACK)
      PPTCL(6,J)=PRX2(1)
      PPTCL(7,J)=PRX2(2)
      PPTCL(8,J)=PRX2(3)
710   CONTINUE
      BACK=.TRUE.
      CALL LORTR(V,NIN1,NFIN1,BACK)
      CALL LORCO(V,NIN1,NFIN1,BACK)
      NPRODS=NPTCL-NFIX
      SWMAX=SWMAX0
      RETURN
500   AMSS1=DSQRT(AMSS12)
c-Sob      if(NCAS.GE.NCPRI) WRITE(*,*) 'to STRING(1)'
      PZ1=P1+P2
      ESS1=E1+E2
      V(1)=PTX/ESS1
      V(2)=PTY/ESS1
      V(3)=PZ1/ESS1
      NFIX=NPTCL
      NIN1=NPTCL+1
600   continue
c-Sob      if(NCAS.GE.NCPRI) WRITE(*,*) 'to STRING',IFL1,IFL2,AMSS1
      CALL STRING(IFL1,IFL2,AMSS1)
c-Sob      if(NCAS.GE.NCPRI) WRITE(*,*) 'from STRING'
      IF(LRET) GO TO 1
       NFIN1=NPTCL
      PPX1(1)=PX1
      PPX1(2)=PY1
      PPX1(3)=P1
      BACK=.FALSE.
      CALL LORLC(V,PPX1,E1,BACK)
      CALL ANGLE(PPX1,CT,ST,CFI,SFI)
      DO 510 J=NIN1,NFIN1
      PPX1(1)=PPTCL(1,J)
      PPX1(2)=PPTCL(2,J)
      PPX1(3)=PPTCL(3,J)
      CALL ROTR(CT,ST,CFI,SFI,PPX1,PPX2,BACK)
      PPTCL(1,J)=PPX2(1)
      PPTCL(2,J)=PPX2(2)
      PPTCL(3,J)=PPX2(3)
      PRX1(1)=PPTCL(6,J)
      PRX1(2)=PPTCL(7,J)
      PRX1(3)=PPTCL(8,J)
      CALL ROTR(CT,ST,CFI,SFI,PRX1,PRX2,BACK)
      PPTCL(6,J)=PRX2(1)
      PPTCL(7,J)=PRX2(2)
      PPTCL(8,J)=PRX2(3)
510   CONTINUE
      BACK=.TRUE.
      CALL LORTR(V,NIN1,NFIN1,BACK)
      CALL LORCO(V,NIN1,NFIN1,BACK)
      NPRODS=NPTCL-NFIX
      SWMAX=SWMAX0
       RETURN
9999  WRITE(ITLIS,9998) NPTCL
9998  FORMAT(//10X,39H...STOP IN XCORR..NPTCL TOO HIGH NPTCL=,I5)
      RETURN
       END
      FUNCTION CRINT(T,I)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CRINT,T
C     CALCULATION OF THE CROSS SECTION BY MEANS OF INTERPOLATION
      COMMON/TABLE/ SIGMA(30,21),ENERGY(30,4)
      COMMON/KSI/ KSI(4)
      COMMON/INTERP/ F(6)
      S=2.*T
      IF(I .GT. 6) GO TO 1
C     COMPUTE THE ENERGY TABLE NUMBER
      J=2
            GO TO 3
 1    IF(I .GT. 10) GO TO 2
      J=1
            GO TO 3
 2    IF(I.GT.19) GO TO 14
      J=3
      GO TO 3
14    J=4
C     IF  30  < T< 220 CRINT = CONST <=================
3     IF(.NOT.(T.GE.ENERGY(30,J).AND.T.LT.220.)) GO TO 33
      CRINT=SIGMA(30,I)
      RETURN
C     COMPUTE THREE POINTS FOR THE INTERPOLATION
33    L=1
 4    IF(T-ENERGY(L,J))6,5,11
 5    CRINT=SIGMA(L,I)
                         RETURN
C     (INTERPOLATION IS UNNECESSARY)
 6    IF(L .GT. 1) GO TO 7
      CRINT=SIGMA(L,I)
                 RETURN
 7    IF(L .GE. 29) GO TO 8
      L1=L-2
               GO TO 9
 8    L1=27
 9    DO 10 K=1,3
      LL1=L1+K
                 F(K)=SIGMA(LL1,I)
                                      F(K+3)=ENERGY(LL1,J)
 10   CONTINUE
                 GO TO 12
 11   L=L+1
C     IF(L.EQ.30)GOTO 13
      IF(L.EQ.30)GOTO 15
      GOTO 4
 12   CRINT=SINTER(T)
      RETURN
C13   IF(I.LE.19) GO TO 15
C     CRINT=SIGMA(30,I)
C     RETURN
15    IF(KSI(1).EQ.5) GO TO 21
      IF(IABS(KSI(2)).EQ.2) GO TO 22
      COEF=SIGMA(30,I)/(1.+0.0023*DLOG(1.16*ENERGY(L,J))**2)
      CRINT=COEF*(1.+0.0023*DLOG(1.16*T)**2)
      RETURN
21    IB=-1.
      IF(I.GT.20) GO TO 23
C     ANTI-BARYON-BARYON CROSS SECTION
C     TOTAL CROSS SECTION
      CRINT=PPCRSE(S,IB)
      RETURN
C     ELASTIC CROSS SECTION
23    IF(I.GT.21) GO TO 24
      CRINT=PPELSE(S,IB)
      RETURN
24    CRINT=0.
      RETURN
C     BARYON-BARYON CROSS SECTION
22    IB=1.
      IF(.NOT.(I.EQ.7.OR.I.EQ.9)) GO TO 26
C     TOTAL CROSS SECTION
      CRINT=PPCRSE(S,IB)
      RETURN
26    IF(.NOT.(I.EQ.8.OR.I.EQ.10)) GO TO 27
C     ELASTIC CROSS SECTION
      CRINT=PPELSE(S,IB)
      RETURN
27    CRINT=0.
      RETURN
               END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION PPCRSE(S,IB)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8   PPCRSE,S
C
C   COMPUTE PP-TOTAL CROSS SECTION
C
C   M.M. BLOCK AND R.N. CAHN REV.MOD.PHYS.V.57,N2(1985) (SET 2)
C
      COMMON/CONST/PI,SQRT2,ALF,GF,UNITS
      COMMON/RHOPP/ RHOPP
      COMMON/CREGGE/ CREGGE
      DATA A/41.30/,D/-40.51/,BETA/0.62/,ALFA/0.47/,S0/293.46/
      DATA C/8.40/,AMU/0.5/
C
      SIGN=1.
      IF(IB.EQ.-1.) SIGN=-1.
      CREGGE=D*DCOS(PI*ALFA/2.)*S**(ALFA-1.)
      PPCRSE=A+BETA*(DLOG(S/S0)**2-PI**2/4.)+
     +C*DSIN(PI*AMU/2.0)*S**(AMU-1.)+SIGN*CREGGE
      RHOPP=(PI*BETA*DLOG(S/S0)-C*DCOS(PI*AMU/2.)*S**(AMU-1.)+SIGN*
     *D*DSIN(PI*ALFA/2.)*S**(ALFA-1.))/PPCRSE
      RETURN
      END
C**********************************************************************
      FUNCTION PPELSE(S,IB)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8   PPELSE,S
C
C   COMPUTE PP-ELASTIC CROSS SECTION
C
C   M.M. BLOCK AND R.N. CAHN REV.MOD.PHYS.V.57,N2(1985) (SET 1)
C
      COMMON/CONST/PI,SQRT2,ALF,GF,UNITS
      COMMON/CPPTOT/ CPPTOT
      COMMON/RHOPP/ RHOPP
      DATA CP/10.90/,DP/-0.08/,EP/0.043/,CM/23.27/,DM/0.93/
C
C  COMPUTE SLOPE BPP
      BPPLUS=CP+DP*DLOG(S)+EP*DLOG(S)**2
      BPMIN=CM+DM*DLOG(S)
C  COMPUTE ELASTIC CROSS SECTION
      IBB=-1
      CPPTPL=PPCRSE(S,IBB)
      IF(.NOT.(IB.EQ.IBB)) GO TO 100
      RHO=RHOPP
      CPPTOT=CPPTPL
100   IBB=1
      CPPTMI=PPCRSE(S,IBB)
      IF(.NOT.(IB.EQ.IBB)) GO TO 200
      RHO=RHOPP
      CPPTOT=CPPTPL
200   SIGMAP=CPPTPL+CPPTMI
      SIGMAM=CPPTPL-CPPTMI
      BPP=BPPLUS+(SIGMAM/SIGMAP)*(BPMIN-BPPLUS)
      PPELSE=CPPTOT**2*(1.+RHO**2)/(16.*PI*BPP*0.389)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE SMARK(IK01,IK02)
      IMPLICIT REAL*8 (A-H,O-Z)
C  COMPUTE CROSS SECTION TYPE KSI(1),SUMMARY BARION
C  NUMBER KSI(2),SUMMARY CHARGE KSI(3),SUMMARY STRANGE KSI(4)
C  ----------IK2-BARYON & MESON ------
      COMMON/KSI/ KSI(4)
      IK1=IK01
      IK2=IK02
      IB1=IB(IK1)
      IB2=IB(IK2)
      IF(IB1.GE.0.OR.IB2.GE.0) GO TO 110
C     AB-AB -> B-B
      IK1=IABS(IK1)
      IK2=IABS(IK2)
      IF(IK2.EQ.1120.OR.IK2.EQ.1220) GO TO 112
      IF(IK1.NE.1120.AND.IK1.NE.1220) GO TO 111
      IK11=IK1
      IK1=IK2
      IK2=IK11
      GO TO 112
110   IF(IB1.NE.0.OR.IB2.GE.0) GO TO 111
C     M-AB -> AM-B
      IK2=IABS(IK2)
      IF(IK1/100.NE.MOD(IK1,100)/10) IK1=-IK1
111   IF(IK2.NE.1220) IK2=1120
112   IB1=IB(IK1)
      IB2=IB(IK2)
      IQ1=IDINT(CHARGE(IK1)*1.01)
      IQ2=IDINT(CHARGE(IK2)*1.01)
      IS1=IS(IK1)
      IS2=IS(IK2)
      KSI(2)=IB1+IB2
      KSI(3)=IQ1+IQ2
      KSI(4)=IS1+IS2
      IF(KSI(4).EQ.0) GO TO 10
      IF(KSI(2).LE.1) GO TO 7
C  STRANGE BARYON NUCLEON COLLISION
      IF(KSI(4)+2)1,2,3
C  OMEGA NUCLEON COLLISION
 1    KSI(1)=1
                 RETURN
C  KSI NUCLEON COLLISION
 2    KSI(1)=1
      IF(KSI(3).EQ.-1)KSI(1)=2
      RETURN
C  SIGMA OR LAMBDA NUCLEON COLLISION
 3    IF(KSI(3).EQ.0.OR.KSI(3).EQ.1) GO TO 4
      KSI(1)=1
                 RETURN
 4    IF(IQ1)6,5,6
 5    KSI(1)=3
                 RETURN
 6    KSI(1)=2
                 RETURN
7     IF(IB1.EQ.0.OR.IB2.EQ.0) GO TO 17
C    ANTIBARYON NUCLEON COLLISION
      KSI(1)=5
      RETURN
C  STRANGE MESON NUCLEON COLLISION
 17   IF(KSI(4)-1)9,8,9
 8    KSI(1)=1
      IF(IQ1.EQ.1) RETURN
      KSI(1)=3
                 RETURN
 9    KSI(1)=2
      IF(KSI(3).EQ.0)RETURN
      KSI(1)=3
      K=IK1
      NF=0
      IF(K.EQ.-230.OR.K.EQ.-231) NF=1
      IF(KSI(3).EQ.-1.OR.NF.EQ.1)KSI(1)=4
      RETURN
C  NONSTRANGE PARTICLE COLLISION
 10   IF(KSI(2).EQ.2) GO TO 13
      IF(IB1.EQ.0.OR.IB2.EQ.0) GO TO 18
C   ANTIBARYON NUCLEON COLLISION
      KSI(1)=5
      RETURN
C  MESON NUCLEON COLLISION
18    IK=KSI(3)+2
      GOTO(11,12,12,11),IK
 11   KSI(1)=1
                 RETURN
 12   KSI(1)=2
      IF(IQ1.EQ.0) KSI(1)=3
      RETURN
C  BARION NUCLEON COLLISION
 13   K=IABS(IK1)
      IF(K.EQ.1120.OR.K.EQ.1220) GO TO 14
      IF(KSI(3).EQ.3.OR.KSI(3).EQ.-1) GO TO 16
      KSI(1)=2
      IF(KSI(3).NE.1) RETURN
      KSI(1)=3
                 RETURN
 14   KSI(1)=1
      IF(KSI(3).EQ.1)KSI(1)=2
      RETURN
 16   KSI(1)=1
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION SFICRI(T,IKS)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 SFICRI,T
C  CALCULATION OF CROSS SECTION FOR FI0 NUCLEON INTERACTION
      COMMON/DATA10/ PAROM,PARKSI,PARSIG,PARF0
      GO TO (1,2,3,3,4,4,5),IKS
 1    SIF=(CRINT(T,1)+CRINT(T,3))/2.
                                      GOTO 6
 2    SIF=(CRINT(T,2)+CRINT(T,4)-CRINT(T,5))/2.
                                                  GO TO 6
 3    SIF=CRINT(T,IKS+2)
                           GO TO 6
 4    SIF=(CRINT(T,IKS+10)+CRINT(T,IKS+12))/2.
                                                 GOTO 6
 5    SIF=(CRINT(T,15)+CRINT(T,16)+CRINT(T,17)+
     * CRINT(T,18)+CRINT(T,19))/2.
 6    SFICRI=SIF*PARF0
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION SBCRI(T,IKS)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 SBCRI,T
C  CALCULATION OF CROSS SECTION FOR STRANGE BARYON NUCLEON INTERACTION
      COMMON/DATA10/ PAROM,PARKSI,PARSIG,PARF0
      COMMON/KSI/ KSI(4)
      IF(KSI(4)+2)3,2,1
 1    CS=PARSIG
                  GO TO 4
 2    CS=PARKSI
                  GO TO 4
 3    CS=PAROM
 4    IF(KSI(1)-2)5,9,12
C  POSITIVE SIGMA PROTON REACTION
 5    GO TO(6,6,7,7,6,6,8),IKS
 6    SIG=CRINT(T,IKS+6)
                           GO TO 14
 7    SIG=0.
               GO TO 14
 8    SIG=CRINT(T,11)+CRINT(T,12)
                                    GO TO 14
C  NEGATIVE SIGMA PROTON REACTION
 9    GOTO(10,10,7,7,10,10,11),IKS
 10   SIG=CRINT(T,IKS+8)
                           GO TO 14
 11   SIG=2.*CRINT(T,13)+CRINT(T,14)
                                       GO TO 14
C  NEUTRAL SIGMA PROTON REACTION
 12   GOTO(13,13,7,7,13,13,11),IKS
 13   SIG=(CRINT(T,IKS+8)+CRINT(T,IKS+6))/2.
 14   SBCRI=SIG*CS
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION SCRIK(T1,IKS)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 SCRIK,T1
C  CALCULATION OF CROSS SECTION FOR STRANGE KAON NUCLEON REACTION
      COMMON/KSI/ KSI(4)
      T=T1
      P0=DSQRT(T*T+2.*0.497*T)
      IF(T1.GE.20.0.AND.IKS.EQ.3) GO TO 4
      IF(T.GE.49.5) T=49.5
      IK=KSI(1)
      GO TO(1,5,9,13),IK
C  POSITIVE KAON PROTON REACTION
 1    GO TO(2,3,4,4,4,4,4),IKS
 2    IF(P0.GE.10.) SCRIK=17.042+0.0188*T+0.000093*T**2
      IF(P0.LT.0.8) SCRIK = 13.0
      IF(P0.GE.0.8.AND.P0.LT.10.)
     * SCRIK=23.4/(P0+1.)+15.-7.5/(P0-0.3)
               GO TO 16
 3    IF(P0.GE.10.) SCRIK=3.77-0.047*T+0.00035*T**2
      IF(P0.LT.0.8) SCRIK = 13.0
      IF(P0.GE.0.8.AND.P0.LT.10.) SCRIK=23.4/(P0+1.)
               GO TO 16
 4    SCRIK=0.
               GO TO 16
C  NEGATIVE KAON PROTON REACTION CROSS SECTION
 5    GO TO(6,7,8,4,4,4,4),IKS
 6    IF(P0.GE.4.) SCRIK=23.97-0.179*T+0.0022*T**2
      IF(P0.LT.0.25) SCRIK=60.
      IF(P0.GE.0.25.AND.P0.LT.0.8) SCRIK=15./P0 + 13./P0
      IF(P0.GE.0.8.AND.P0.LT.4.)
     *SCRIK=15./P0+9.-1.2/(P0-.6)+13./DSQRT(P0)
               GO TO 16
 7    IF(P0.GE.4.) SCRIK=4.45-0.103*T+0.0012*T**2
      IF(P0.LT.0.25) SCRIK=60.
      IF(P0.GE.0.25.AND.P0.LT.4.) SCRIK=15./P0
               GO TO 16
 8    SCRIK=0.23-0.022*T+0.00053*T**2
               GO TO 16
C  POSITIVE KAON NEUTRON REACTION CROSS SECTION
 9    GO TO(10,11,12,4,4,4,4),IKS
 10   IF(P0.GE.10.) SCRIK=17.49+0.0192*T+0.000039*T**2
      IF(P0.LT.0.8) SCRIK = 13.0
      IF(P0.GE.0.8.AND.P0.LT.10.)
     * SCRIK=23.4/(P0+1.)+15.-7.5/(P0-0.3)
               GO TO 16
 11   IF(P0.GE.10.) SCRIK=3.77-0.047*T+0.00035*T**2
      IF(P0.LT.0.8) SCRIK = 13.0
      IF(P0.GE.0.8.AND.P0.LT.10.) SCRIK=23.4/(P0+1.)
               GO TO 16
 12   SCRIK=0.9487-0.176*T+0.0084*T**2
               GO TO 16
C  NEGATIVE MESON NEUTRON REACTION
 13   GO TO(14,15,4,4,4,4,4),IKS
 14   IF(P0.GE.4.) SCRIK=21.53-0.114*T+0.0014*T**2
      IF(P0.LT.0.25) SCRIK=60.
      IF(P0.GE.0.25.AND.P0.LT.0.8) SCRIK=15./P0
      IF(P0.GE.0.8.AND.P0.LT.4.)
     *SCRIK=15./P0+9.-1.2/(P0-.6)+13./DSQRT(P0)
               GO TO 16
 15   IF(P0.GE.4.) SCRIK=19.51-10.2*T+1.52*T**2
      IF(P0.LT.0.25) SCRIK=60.
      IF(P0.GE.0.25.AND.P0.LT.4.) SCRIK=15./P0
 16   IF(T1.LE.49.5)  RETURN
      COEF=SCRIK/(1.+0.0023*DLOG(1.16*T)**2)
      T=T1
      SCRIK=COEF*(1.+0.0023*DLOG(1.16*T)**2)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION SIGMAT(T,IKS,IK1)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 SIGMAT,T
C     CALCULATION OF THE CROSS-SECTION
      COMMON/KSI/ KSI(4)
      IF(KSI(1).EQ.5) GO TO 26
      IS1=IS(IK1)
C  SEPARATION STRANGE PARTICLES
      IF(IS1.NE.0) GOTO 24
C     SEPARATION OF THE MESON AND NUCLEON REACTION
      IF(KSI(2) .GT. 1) GO TO 17
C     MESON-NUCLEON REACTIONS
      IF(KSI(1)-2)1,7,11
C     POSITIVE MESON+PROTON
 1    GO TO (2,2,3,4,5,5,6),IKS
 2    SIGMAT=CRINT(T,IKS)
                            RETURN
 3    SIGMAT=0.
                  RETURN
 4    SIGMAT=CRINT(T,6)
                          RETURN
 5    SIGMAT=CRINT(T,IKS+10)
                               RETURN
 6    SIGMAT=CRINT(T,15)+CRINT(T,16)
                                       RETURN
C     NEGATIVE MESON+PROTON
 7    GO TO (8,8,8,8,9,9,10),IKS
 8    SIGMAT=CRINT(T,IKS+2)
                              RETURN
 9    SIGMAT=CRINT(T,IKS+12)
                               RETURN
 10   SIGMAT=CRINT(T,17)+CRINT(T,18)+CRINT(T,19)
                                                   RETURN
C     NEUTRAL MESON+PROTON
 11   KN=IABS(IK1)
      IF(KN.NE.331) GO TO 36
C  NEUTRAL FI-MESON NUCLEON REACTION
      SIGMAT=SFICRI(T,IKS)
                             RETURN
 36   GOTO(12,13,14,14,15,15,16),IKS
 12   SIGMAT=(CRINT(T,1)+CRINT(T,3))/2.
                                          RETURN
 13   SIGMAT=(CRINT(T,2)+CRINT(T,4)-CRINT(T,5))/2.
                                                     RETURN
 14   SIGMAT=CRINT(T,IKS+2)
                              RETURN
 15   SIGMAT=(CRINT(T,IKS+10)+CRINT(T,IKS+12))/2.
                                                    RETURN
 16   SIGMAT=(CRINT(T,15)+CRINT(T,16)
     *+CRINT(T,17)+CRINT(T,18)+CRINT(T,19))/2.
                                                 RETURN
C     NUCLEON-NUCLEON REACTIONS
C
 17   IF(KSI(1)-2)18,21,21
C     PROTON+PROTON
 18   GO TO (19,19,3,3,19,19,20),IKS
 19   SIGMAT=CRINT(T,IKS+6)
                              RETURN
 20   SIGMAT=CRINT(T,11)+CRINT(T,12)
                                       RETURN
C     NEUTRON +PROTON
 21   GO TO (22,22,3,3,22,22,23),IKS
 22   SIGMAT=CRINT(T,IKS+8)
                              RETURN
 23   SIGMAT=2.*CRINT(T,13)+CRINT(T,14)
                                          RETURN
C  SEPARATION OF MESON AND BARYON REACTION
 24   IF(KSI(2).GT.1)GO TO 25
C  STRANGE MESON NUCLEON REACTION
      SIGMAT=SCRIK(T,IKS)
                            RETURN
C  STRANGE BARYON NUCLEON REACTION
 25   SIGMAT=SBCRI(T,IKS)
                            RETURN
C    ANTIBARYON NUCLEON REACTION
26    SIGMAT=APPCRI(T,IKS)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION SINTER(X)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 SINTER,X
C   INTERPOLATION BY MEANS OF LAGRANGE POLTNOMS
      COMMON/INTERP/ F(6)
      SINTER=0.
      DO 2 I=1,3
      R=1.0
      K=3+I
      DO 1 J=1,3
      IF(I.EQ.J) GO TO 1
      L=3+J
      R=R*(X-F(L))/(F(K)-F(L))
 1    CONTINUE
 2    SINTER=SINTER+R*F(I)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION  CRNNDN(S)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CRNNDN,S
C
C  NN--NDELTA CROSS SECTION
C
      SQS=DSQRT(S)
      IF(SQS.GT.2.015) GO TO 100
      CRNNDN=0.
      RETURN
 100  CRNNDN=20.*(SQS-2.015)**2/(0.015+(SQS-2.015)**2)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION CRNDNN(S,IE)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CRNDNN,S
C       X2=Mnucl+Mpion
      DATA X2 /1.080/
C
C  N+DELTA->N+N' CROSS SECTION (NEW) with D-B correction 04.12.92
C
      T=(S-(2.*.940)**2)/1.88
      P0=DSQRT(T*(T+1.88))
      PIN=(S+0.63)**2/(4.*S)-1.51
      PF=S/4.-0.88
c------------- degeneration factor ------------------
      G=4.D0
      IF(IE.EQ.1)    G=2.D0
c-------------Danielewitz-Bertsch correction (appr.) -----------
      X1=DSQRT(S)-.940
      GD1=GDM(X1)
      ARG1=(X1**2-1.232**2)/(1.232*GD1)
      ARG2=(X2**2-1.232**2)/(1.232*0.118)
      COR=1./3.1415927*(DATAN(ARG1)-DATAN(ARG2))
c---------------------------------------------------------------
c     CRNDNN=PF*CRNNDN(S)/(8.*PIN)    Amel
      CRNDNN=PF*CROSS(P0,1,5)/(G*PIN*COR)
      if(CRNDNN.LE.0.) CRNDNN=0.
      RETURN
      END
C******************************************************************
      SUBROUTINE CROSEC(ITIN,IK01,IK02,PX01,PY01,PZ01,AM01,
     *PX02,PY02,PZ02,AM02,SIGMA,INEL)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 PX01,PY01,PZ01,AM01,
     *PX02,PY02,PZ02,AM02,SIGMA
C  CALCULATION OF TOTAL(ITIN=1) OR ELASTIC(ITIN=0)
C  OR CHARGE-EXCHANGE (ITIN=2) OR ANNIHILATION(ITIN=3)
C  HADRON-HADRON CROSS SECTION
C   IF INEL=1 ==> SIGTOT=SIGTOT-SIGEL
C   IF INEL=2 ==> SIGTOT AND SIGEL ARE CONSTANTS
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      COMMON/PRINTS/ IPRINT
      LOGICAL IPRINT
      ITOT=ITIN
      IB1=IB(IK01)
      IB2=IB(IK02)
      IB12=IB1*IB2
      IF(IB1.EQ.0.AND.IB2.EQ.0) GO TO 10
      IF((IK01.EQ.1120.OR.IK01.EQ.1220) .AND.
     *    IK02.NE.1120.AND.IK02.NE.1220) GO TO 13
      IF(IB1.GT.0.AND.IB2.LE.0) GO TO 13
      IF(IB1.LT.0.AND.IB2.EQ.0) GO TO 13
 10   IK1=IK01
      IK2=IK02
      PX1=PX01
      PY1=PY01
      PZ1=PZ01
      AM1=AM01
      PX2=PX02
      PY2=PY02
      PZ2=PZ02
      AM2=AM02
      GO TO 20
13    IK1=IK02
      IK2=IK01
      PX1=PX02
      PY1=PY02
      PZ1=PZ02
      AM1=AM02
      PX2=PX01
      PY2=PY01
      PZ2=PZ01
      AM2=AM01
 20   CONTINUE
      SIGMA=0.
      IF(ITOT.EQ.3.AND.IB12.GE.0) RETURN
      IB1=IB(IK1)
      IB2=IB(IK2)
      IS1=IABS(IS(IK1))
      IS2=IABS(IS(IK2))
C
      AM1N=0.139
      IF(IS1.NE.0) AM1N=0.497
      IF(IB1.NE.0) AM1N=0.939
      IF(IB1.NE.0.AND.IS1.NE.0) AM1N=1.1156
C
      COEFMS=1.
C--- IF MESON-MESON COLL. SIGMA=(2/3)*SIGMA
      IF(IB2.EQ.0) COEFMS=0.667
      IF(IS2.NE.0.AND.IB12.GE.0) COEFMS=COEFMS*0.81**IS2
C--- IF STRANGE HADRONS   SIGMA=0.81**(IS1+IS2)*SIGMA
      IF(IB12.LT.0) COEFMS=COEFMS*0.81**(IS1+IS2)
      E1=SQRT(AM1**2+PX1**2+PY1**2+PZ1**2)
      E2=SQRT(AM2**2+PX2**2+PY2**2+PZ2**2)
      S=AM1**2+AM2**2+2.*E1*E2-2.*(PX1*PX2+PY1*PY2+PZ1*PZ2)
      SQR=SQRT(S)
c !!!
      TKINM=(S-AM1**2-AM2**2)/(2.*AM2)-AM1
c !!!
      DSR=0.20
C     IF(IS1.EQ.0.AND.IS2.EQ.0.AND.IB1.EQ.1.AND.IB2.EQ.1) DSR=0.17
C
      IF(IB1.EQ.0.AND.IB2.EQ.0) GO TO 21
      AM2N=0.93900000
      TKIN=(S-AM1N**2-AM2N**2)/(2.*AM2N)-AM1N
C
      IF(TKIN.LE.0.0001.AND.IB12.GE.0) RETURN
C
C  M--N AND N--N
      IF(SQR-AM1N-AM2N.LT.DSR.AND.IB12.GE.0) ITOT=0
C
      IKS=4
      GO TO 22
C PI---PI
21    IF(TKINM.LE.0.0001) RETURN
      IF(SQR-AM1-AM2.LT.DSR) ITOT=0
C
22    IF(ITOT.EQ.0) IKS=2
      IF(ITOT.EQ.1) IKS=1
      IF(ITOT.EQ.2) IKS=3
      IF(ITOT.EQ.0) SIGCON=7.
      IF(ITOT.EQ.1) SIGCON=39.
      IF(IB1.EQ.0) SIGCON=0.6667*SIGCON
      IF(ITOT.EQ.3) GO TO 35
      IF(IB12.GE.0) GO TO 40
C
      IF(TKIN.LT.0.0001.AND.(IKS.EQ.2.OR.IKS.EQ.3)) RETURN
C]]]]]]]]]PBAR+P]]]]]]]]]]]]]]
35    IK1=-1120
      IK2=1120
40    SIGMA=SIGCON
      IF(IB1.EQ.0.AND.IB2.EQ.0) GO TO 500
      IF(IB1.GE.0.AND.IB2.EQ.1.AND.AM2.GT.0.96) GO TO 500
      CALL SMARK(IK1,IK2)
      IF(IK1.EQ.-1120.AND.IK2.EQ.1120)  GO TO 1825
      IF(INEL.NE.2) GO TO 400
      SIGMA=SIGCON
      GO TO 500
400   SIGMA=SIGMAT(TKIN,IKS,IK1)
C
c !!!         20.10.92T
      IF(ITOT.NE.1.OR.SQR.GT.3.0)       GO TO 500
      IF(IK1.EQ.1111.OR.IK1.EQ.1121.OR.IK1.EQ.2221.OR.IK1.EQ.1221) THEN
        IE1=IDINT(CHARGE(IK1)*1.001)
        IE2=IDINT(CHARGE(IK2)*1.001)
        IE12=IE1+IE2
        IF(IE12.EQ.3.OR.IE12.EQ.-1)      GO TO 500
        P0=DSQRT(TKIN*(TKIN+2.*AM2))
        SBIN=CROSS(P0,1,5)
        SBIND=CRNDNN(S,IE12)
        SIGMA=SIGMA-SBIN+SBIND
      END IF
c !!!
500   SIGMA=SIGMA*COEFMS
C   FOR LOW-ENERGY PION-PION COLLISIONS (A LA WOLF)
      IF(AM1.LT.0.141.AND.AM2.LT.0.141) THEN
        IF(SQR-AM1N-AM2N.LT.1.000.AND.ITIN.LE.1) SIGMA=30.
        go to 1827
      ENDIF
C   FOR LOW-ENERGY PION-PION COLLISIONS (A LA Bao-An Li)
C       IF(AM1.LT.0.141.AND.AM2.LT.0.141) THEN
C         IF(SQR-AM1N-AM2N.LT.0.800.AND.ITIN.LE.1)
C     &   SIGMA=PIPICS(IK1,IK2,SQR,ITOT)
C         GO TO  1827
C       ENDIF
C
      GO TO 1826
1825  SIGMA=SIGMAT(TKIN,IKS,IK1)*COEFMS
1826  CONTINUE
      IF(IB12.LT.0.AND.TKIN.LT.0.02) TKIN=0.02
C]]]]]]]]]]]PBAR+P ]]]]]]]]]]]]
      IF(INEL.EQ.1.AND.ITOT.EQ.1)
     *SIGMA=SIGMA-SIGMAT(TKIN,2,IK1)*COEFMS
1827  CONTINUE
      IF(IPRINT.AND.ITOT.EQ.1) WRITE(ITLIS,2001) SIGMA
     *,IK01,IK02,PX01,PY01,PZ01,PX02,PY02,PZ02
      IF(IPRINT.AND.ITOT.EQ.0) WRITE(ITLIS,2000) SIGMA
     *,IK01,IK02,PX01,PY01,PZ01,PX02,PY02,PZ02
      IF(IPRINT.AND.ITOT.EQ.3) WRITE(ITLIS,2002) SIGMA
     *,IK01,IK02,PX01,PY01,PZ01,PX02,PY02,PZ02
2000  FORMAT('  SIGMA ELASTIC =',F10.4,2I6,6F12.4)
2001  FORMAT('  SIGMA TOTAL   =',F10.4,2I6,6F12.4)
2002  FORMAT('  SIGMA ANNIH.  =',F10.4,2I6,6F12.4)
      RETURN
      END

C     *********************************************************
      FUNCTION PIPICS(ID1,ID2,SS,ICS)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 PIPICS,SS,MPI
      DATA MPI/0.139/,PI/3.141592/
C  to transforme 1/GeV**2 -> mb
      C=10./(5.06**2)
      Q=DSQRT((SS/2.)**2-MPI**2)
      GR=0.095*Q*((Q/MPI)/(1.+(Q/0.77)**2))**2
      D00=DATAN(1.03*Q/(5.8*MPI-SS))
      D11=DATAN(GR/2./(0.77-SS))
      D20=-0.12*Q/MPI
      S00=8.*C*PI/Q/Q*1.*DSIN(D00)**2
      S11=8.*C*PI/Q/Q*3.*DSIN(D11)**2
      S20=8.*C*PI/Q/Q*1.*DSIN(D20)**2
      PIPICS=0.
      IF(ICS.EQ.-1)    THEN
C         ISOSPIN AVERAGED CROSS SECTION
          PIPICS=1./9.*S00+1./3.*S11+5./9.*S20
          RETURN
      ELSEIF(ICS.EQ.1.OR.ICS.EQ.0) THEN
C         TOTAL=ELASTIC CROSS SECTION
C        PI+ PI-  OR PI- PI-
          IF((ID1.EQ.120.AND.ID2.EQ.120).OR.
     &       (ID1.EQ.-120.AND.ID2.EQ.-120))
     &    PIPICS=S20
C        PI+ PI0  OR PI- PI0 OR PI0 PI- OR PI0 PI+
          IF((IABS(ID1).EQ.120.AND.ID2.EQ.110).OR.
     &       (ID1.EQ.110.AND.IABS(ID1).EQ.120))
     &    PIPICS=S20/2.+S11/2.
C        PI+ PI-  OR PI- PI+
          IF((ID1.EQ.120.AND.ID2.EQ.-120).OR.
     &       (ID1.EQ.-120.AND.ID2.EQ.120))
     &    PIPICS=S20/6.+S11/2.+S00/3.
C        PI0 PI0
          IF(ID1.EQ.110.AND.ID2.EQ.110)
     &    PIPICS=S00/3.+2.*S20/3.
      ELSEIF(ICS.EQ.2) THEN
C CHARGE-EXCHANGE PI+PI- <-> PI0PI0
          IF((ID1.EQ.120.AND.ID2.EQ.-120).OR.
     &       (ID1.EQ.-120.AND.ID2.EQ.120).OR.
     &       (ID1.EQ.110.AND.ID2.EQ.110))
     &    PIPICS=S00/3.+S20/3.
      ELSEIF(ICS.EQ.3) THEN
C         RHO FORMATION CROSS SECTION
        PIPICS=S11/2.
      ENDIF
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE SIGFIH(SIG,I1,I2,I3,I4)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 SIG
C
C      FILL /HADSIG/
C
C   I1 IS ABSOLUTE VALUE OF PROJECTILE IDENT
C   I2 IS ABSOLUTE VALUE OF TARGET IDENT
C   I3 MARK REACTION TYPE
C   I4 IS SIGN OF PROJECTILE IDENT
C
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      COMMON/PRINTS/IPRINT
      LOGICAL IPRINT
      COMMON/HADSIG/SIGS(100),SIGEVT,NSIGS,INOUT(2,100)
      COMMON/COMMUL/ MULTP
      LOGICAL MULTP
      COMMON/CSIGSU/ SIGSUM
      COMMON/CPRSIG/ ISGCOL
C     REAL*8      AGH1H2(5,11)
      CHARACTER*8 AGH1H2(5,11)
      DATA AGH1H2 /
     * 'TRIPLE P','OMERON V','ERTEX DI','FRACTION',' DIAGRAM',
     * 'PLANAR (','ONE REGG','EON) DIA','GRAM    ','        ',
     * 'UNDEVELO','PED CYLI','NDER DIA','GRAM    ','        ',
     * 'ELASTIC ','SCATTERI','NG DIAGR','AM      ','        ',
     * 'ANNIHILA','TION DIA','GRAM    ','        ','        ',
     * 'SMALL MA','SS DIFRA','CTION DI','AGRAM   ','        ',
     * 'MULTI PO','MERON SC','ATTER.(C','YLINDER)',' DIAGRAM',
     * 'TWO PART','ICLE REA','CTION   ','        ','        ',
     * 'MULTI PO','MERON SC','ATTER.(C','HAINS  )',' DIAGRAM',
     * 'TRIPLE R','EGGEON V','ERTEX DI','AGRAM   ','        ',
     * 'DOUBLE D','IFRACTIO','N DIAGRA','M       ','        '/
      IOPAK=10000
      NSIGS=NSIGS+1
      SIGS(NSIGS)=SIG
      SIGSUM=SIGSUM+SIG
C   @@@@@@@@@@@@@@@@@@@@
C     INOUT1(1,NSIGS)=I1
C     INOUT2(1,NSIGS)=I2
C     INOUT1(2,NSIGS)=I3
C     INOUT2(2,NSIGS)=I4
      INOUT(1,NSIGS)=I1+IOPAK*I2
      INOUT(2,NSIGS)=I3+IOPAK*I4
C   @@@@@@@@@@@@@@@@@@@@
      K3=I3
      IF(I3.EQ.7.AND.MULTP) K3=9
      IF(ISGCOL.EQ.0.AND.IPRINT)
     *WRITE(ITLIS,1000) I3,SIG,(AGH1H2(J1,K3),J1=1,5)
1000  FORMAT(10X,17HREACTION TYPE I3=,I2,25H  REACTION CROSS SECTION=,
     *E10.4,5HMB - ,5A8)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION APPCRI(T,IKS)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 APPCRI,T
C
C    COMPUTE ANTIPROTON-PROTON CROSS SECTION
C
      GO TO (100,200,300,400),IKS
C     TOTAL CROSS SECTION
100   INUM=20
      APPCRI=CRINT(T,INUM)
      RETURN
C   ELASTIC CROSS SECTION
200   INUM=21
      APPCRI=CRINT(T,INUM)
      RETURN
C    CHARGE-EXCHANGE CROSS SECTION
300   APPCRI=0.
      RETURN
C   ANNIHILATION CROSS SECTION
400   IF(T.GE.0.9) GO TO 500
      IF(T.GE.0.02) GO TO 502
      APPCRI=162.0
      RETURN
502   PMOD=DSQRT(T*(T+1.88))
      APPCRI=61.6/PMOD**0.6
      RETURN
500   INUM=20
      APPCRI=CRINT(T,INUM)
      INUM=7
      PPCRI=CRINT(T,INUM)
      APPCRI=APPCRI-PPCRI
      IF(APPCRI.LE.0.) APPCRI=0.
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE PLANAR(IRET)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     COMPUTE PLANAR DIAGRAM
C
C     DECAY Q-QBAR OR QQ-QQBAR STRING
C
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      COMMON/PRIMAR/SCM,HALFE,ECM,NJET,IDIN(2),NEVENT,NTRIES
      COMMON/PARTCL/PPTCL(9,499),NPTCL,IORIG(499),IDENT(499)
     *,IDCAY(499)
      COMMON/PARORD/ IORDP(499)
      COMMON/PRIMP0/ P0
      COMMON /NEEDR/ NRET
      LOGICAL RETU
      COMMON/COMANN/ DIQAN
      LOGICAL DIQAN
      COMMON/KEYPLA/ KEYPLA
      LOGICAL KEYPLA
      COMMON/MASQUA/ AMQ21,AMQ22
      COMMON/COMASS/ AM1,AM2
      COMMON/NCASCA/ NCAS,NCPRI
      DIMENSION IFORD(3),IFORDP(3)
      DIMENSION IMQ1(2),IMQ2(2)
      COMMON/FLACOM/NFLA,NFL1,NFL2,NFL3,NSPIN,NNDEX
      ZER=0.0
      NREP=0
      KEYPLA=.TRUE.
      NPOLD=NPTCL
      P0OLD=P0
1111  CONTINUE
      P0=P0OLD
      NPTCL=NPOLD
      NREP=NREP+1
      IF(NREP.LE.NTRIES) GO TO 201
C     WRITE(ITLIS,1200) IDIN(1),IDIN(2),P0
1200  FORMAT(1X,'IN PLANAR:NREP > NTRIES,IK1,IK2,P0=',
     *2I5,1X,F7.3)
      IRET=1
      RETURN
201   CONTINUE
      IPACK=1000
      NRET=1
C    INITIALIZE
 110  RETU=.FALSE.
      IRET=0
      PSIGN=-1.
      P0=HALFE
      IKA=IDIN(1)
      IKB=IDIN(2)
      IF(IB(IKA).EQ.0.AND.IB(IKB).EQ.0) NRET=0
      I1=IABS(IDIN(1))
      IFL=I1/1000
      IF(IFL.NE.0) GO TO 260
C
C    INITIAL MESON
c-Sob      IF(NCAS.GE.NCPRI) WRITE(*,*) 'PLANAR:', IKA,IKB
      IQ=0
      JFL=MOD(I1/100,10)
      KFL=MOD(I1/10,10)
      J=ISIGN(JFL,IDIN(1))
      K=ISIGN(KFL,-IDIN(1))
*@@@@@@@@@@@@@@@@@ SIVOKL @@@@@@
      IF(IKA.EQ.110.OR.IKA.EQ.111.OR.IKA.EQ.221.
     *OR.IKA.EQ.220.OR.IKA.EQ.330) THEN
      J=NFL2
      K=NFL3
      ENDIF
*@@@@@@@@@@@@@@@@@
      IFLU=J
      IFLQM=K
      IF(K.GT.0) GO TO 150
      IFLQM=J
      IFLU=K
150   IAFLU=IABS(IFLU)
*@@@@@@@@@@@@@@@@@ SIVOKL @@@@@@
      IF(IKB.EQ.110.OR.IKB.EQ.111.OR.IKB.EQ.221.
     *OR.IKB.EQ.220.OR.IKB.EQ.330) THEN
      IFORD(2)=NFL2
      IFORD(3)=NFL3
      ELSE
      CALL FLAVOR(IKB,IFORD(1),IFORD(2),IFORD(3),JSPIN,INDEX)
      ENDIF
*@@@@@@@@@@@@@@@@@
C     CALL FLAVOR(IKB,IFORD(1),IFORD(2),IFORD(3),JSPIN,INDEX)
      IF(IB(IKB).EQ.0) GO TO 101
      ISIG=ISIGN(1,IKB)
      IF(ISIG.LT.0) GO TO 255
      DO 155 I=1,3
      IF(IAFLU.NE.IFORD(I)) GO TO 155
      IQ=I
      GO TO 156
155   CONTINUE
      GO TO 156
255   CONTINUE
      DO 256 I=1,3
      IF(IFLQM.NE.IABS(IFORD(I))) GO TO 256
      IFLQM0=IFLQM
      IFLQM =IFLU
      IFLU  =IFLQM0
       IQ=I
       GO TO 156
256   CONTINUE
156   IF(IQ-2) 10,20,30
10    ID1=IFORD(2)
      ID2=IFORD(3)
      GO TO 40
20    ID1=IFORD(1)
      ID2=IFORD(3)
      GO TO 40
30    ID1=IFORD(1)
      ID2=IFORD(2)
40    IF(IABS(ID1).LE.IABS(ID2)) GO TO 50
      ISWAP=ID1
      ID1=ID2
      ID2=ISWAP
 50     IFLQ=ID1*1000+ID2*100
        IFLQ=ISIGN(IFLQ,IKB)
      GO TO 100
C
C    INITIAL BARION
260   IF(DIQAN) GO TO 400
      CALL FLAVOR(IKB,IFORD(1),IFORD(2),IFORD(3),JSPIN,INDEX)
      CALL FLAVOR(IKA,IFORDP(1),IFORDP(2),IFORDP(3),JSPIN1,INDEX1)
170   INR=IDINT(1.+3.*RNDMD(-1))
      IFLU=IFORDP(INR)
      IAFLU=IABS(IFLU)
      IQ=0
      DO 160 I=1,3
      IF(IAFLU.NE.IFORD(I)) GO TO 160
      IQ=I
      IFL=IFORD(I)
      GO TO 165
160   CONTINUE
      IF(IQ.EQ.0) GO TO 170
165   IF(INR-2) 175,176,177
175   ID1=IABS(IFORDP(2))
      ID2=IABS(IFORDP(3))
      GO TO 180
176   ID1=IABS(IFORDP(1))
      ID2=IABS(IFORDP(3))
      GO TO 180
177   ID1=IABS(IFORDP(1))
      ID2=IABS(IFORDP(2))
180   IF(ID1.LE.ID2) GO TO 185
      ISWAP=ID1
      ID1=ID2
      ID2=ISWAP
185   IFQM=ID1*1000+ID2*100
      IFLQM=ISIGN(IFQM,IKA)
      IF(IQ-2) 195,196,197
195   ID1=IFORD(2)
      ID2=IFORD(3)
      GO TO 190
196   ID1=IFORD(1)
      ID2=IFORD(3)
      GO TO 190
197   ID1=IFORD(1)
      ID2=IFORD(2)
190   IF(ID1.LE.ID2) GO TO 194
      ISWAP=ID1
      ID1=ID2
      ID2=ISWAP
194   IFLQ=ID1*1000+ID2*100
      GO TO 100
C     COMPUTE ANNIHILATION DIAGRAM
400   CALL FLAVOR(IKA,IFORDP(1),IFORDP(2),IFORDP(3),JSPIN,INDEX)
      IFT1=IFORDP(1)*1000+IFORDP(2)*100
      IFT2=IFORDP(2)*1000+IFORDP(3)*100
      IFT3=IFORDP(1)*1000+IFORDP(3)*100
410   CALL FLAVOB(IKB,IFL1,IFL2)
      IFLQM=IFORDP(3)
      IF(IABS(IFT1).EQ.IABS(IFL2)) GO TO 420
      IFLQM=IFORDP(1)
      IF(IABS(IFT2).EQ.IABS(IFL2)) GO TO 420
      IFLQM=IFORDP(2)
      IF(IABS(IFT3).EQ.IABS(IFL2)) GO TO 420
      GO TO 410
420   IFLQ=IFL1
      GO TO 100
C---  MESON-MESON COLLISION ---------------------------
101   CONTINUE
      IAN=0
      IF(.NOT.(IFORD(2).GT.0.AND.IFORD(2).EQ.IAFLU)) GO TO 102
      IAN=IAN+1
      IMQ1(IAN)=IFORD(3)
      IMQ2(IAN)=IFLQM
102   IF(.NOT.(IFORD(2).LT.0.AND.IABS(IFORD(2)).EQ.IFLQM))GO TO 103
      IAN=IAN+1
      IMQ2(IAN)=IFLU
      IMQ1(IAN)=IFORD(3)
103   IF(.NOT.(IFORD(3).GT.0.AND.IFORD(3).EQ.IAFLU)) GO TO 104
      IAN=IAN+1
C     IMQ1(IAN)=IFORD(3)
C     IMQ2(IAN)=IFLU
      IMQ1(IAN)=IFORD(2)
      IMQ2(IAN)=IFLQM
104   IF(.NOT.(IFORD(3).LT.0.AND.IABS(IFORD(3)).EQ.IFLQM))GO TO 105
      IAN=IAN+1
C     IMQ2(IAN)=IFLQM
C     IMQ1(IAN)=IFORD(3)
      IMQ2(IAN)=IFLU
      IMQ1(IAN)=IFORD(2)
105   IFLQM=IMQ2(1)
      IFLQ =IMQ1(1)
      IF(IAN.EQ.1) GO TO 100
      IF(RNDMD(-1).GT.0.5) GO TO 100
      IFLQM=IMQ2(2)
      IFLQ =IMQ1(2)
100   NIN=NPTCL+1
C  ******
c-Sob      IF(NCAS.GE.NCPRI.AND.IB(IKA).EQ.0.AND.IB(IKB).EQ.0)
c-Sob     *       WRITE(*,*) 'IKA,IKB,IFLQM,IFLQ=',IKA,IKB,IFLQM,IFLQ
C  ******
      AMQ21=0.
      AMQ22=0.
      CALL XCORR(IFLQM,IFLQ,ZER,ZER,ZER,ZER,
     *1.D0,1.D0,PSIGN,NPRODS,RETU)
c-Sob      IF(NCAS.GE.NCPRI.AND.IB(IKA).EQ.0.AND.IB(IKB).EQ.0)
c-Sob     *       WRITE(*,*) 'from XCORR',RETU
      IF(RETU) GO TO 1111
      IF((NPTCL-NPOLD).EQ.2.AND.((IDENT(NPTCL).EQ.IDIN(1).AND.
     *IDENT(NPTCL-1).EQ.IDIN(2)).OR.(IDENT(NPTCL).EQ.IDIN(2).AND.
     *IDENT(NPTCL-1).EQ.IDIN(1)))) GO TO 1111
      NFIN=NPTCL
      IRD=0
      DO 300 I=NIN,NFIN
      IORDP(I)=IRD
      IORIG(I)=2
      IF(.NOT.DIQAN) GO TO 300
      IORIG(I)=15
300   CONTINUE
      P0=P0OLD
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE REACTL
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  SELECT REACTION TYPE AT LOW ENERGY IN H-N AND HBAR-N COLLISIONS
C
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      LOGICAL GH1H2
      COMMON/H1H2/ GH1H2(11)
      COMMON/PRIMAR/SCM,HALFE,ECM,NJET,IDIN(2),NEVENT,NTRIES
      COMMON/HADSIG/SIGS(100),SIGEVT,NSIGS,INOUT(2,100)
      DIMENSION INITYP(2),IREATY(2)
      COMMON/COMANN/ DIQAN
      COMMON/PRIMP0/ P0
      LOGICAL DIQAN
      COMMON/COMMUL/ MULTP
      LOGICAL MULTP
      COMMON /YESELA/YESELA
      LOGICAL YESELA
      COMMON/COMCOL/ NAC(100,4),NBC(100,4),NCOL
      COMMON /PRINTS/ IPRINT
      COMMON /CPRSIG/ ISGCOL
      LOGICAL IPRINT
C     INITIALIZE
  1   SIGEVT=0.
      NCOL=0
      DIQAN=.FALSE.
      IOPAK=10000
      IF(.NOT.MULTP) GO TO 109
      CALL REACTH
      RETURN
109   SIGTOT=0.
      DO 110 I=1,11
110   GH1H2(I)=.FALSE.
      DO 100 ISIGN=1,NSIGS
100   SIGTOT=SIGTOT+SIGS(ISIGN)
      IF(SIGTOT.EQ.0.) GO TO 9999
C    FIND REACTION
      TRY=RNDMD(-1)
      SUM=0.
      DO 200 I=1,NSIGS
      ISIGS=I
      SUM=SUM+SIGS(I)/SIGTOT
      IF(SUM.GT.TRY) GO TO 300
200   CONTINUE
300   SIGEVT=SIGS(ISIGS)
      IF(IPRINT)
     *  WRITE(ITLIS,1001) NSIGS,(SIGS(K),K=1,NSIGS)
1001  FORMAT(1X,'RL:',1X,I3,11(1X,E10.3))
      IF(IPRINT) WRITE(ITLIS,1002) ISIGS,SUM,TRY
1002  FORMAT(1X,'RL: ITLIS,SUM,TRY=',I3,2(1X,E13.6))
C   @@@@@@@@@@@@@@@@@@
C     INITYP(1)=INOUT1(1,ISIGS)
C     INITYP(2)=INOUT2(1,ISIGS)
C     IREATY(1)=INOUT1(2,ISIGS)
C     IREATY(2)=INOUT2(2,ISIGS)
      I1=INOUT(1,ISIGS)
      DO 400 K=1,2
      INITYP(K)=MOD(I1,IOPAK)
  400 I1=I1/IOPAK
      I2=INOUT(2,ISIGS)
      DO 500 K=1,2
      IREATY(K)=MOD(I2,IOPAK)
  500 I2=I2/IOPAK
      IF(IREATY(2).EQ.2.OR.IREATY(2).EQ.4) INITYP(1)=-INITYP(1)
      IF(IREATY(2).EQ.3.OR.IREATY(2).EQ.4) INITYP(2)=-INITYP(2)
C   @@@@@@@@@@@@@@@@@@
      GH1H2(IREATY(1))=.TRUE.
      IF(IPRINT) WRITE(ITLIS,1003) INITYP,IREATY
1003  FORMAT(1X,'RL: INITYP,IREATY=',4(I6,1X))
      IF(YESELA)  RETURN
      IF(GH1H2(4).AND.NSIGS.GT.1)  GO  TO  1
      RETURN
9999  WRITE(ITLIS,1000) SIGEVT
1000  FORMAT(/10X,38H..STOP IN REACTL.. CHECK YOUR INPUT...,
     *7HSIGEVT=,E10.4/)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE TWOSHE(IRET)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C       COMPUTE TWO SHEETS ANNIHILATION DIAGRAM
C
      COMMON/CONST/PI,SQRT2,ALF,GF,UNITS
      COMMON/PRIMAR/SCM,HALFE,ECM,NJET,IDIN(2),NEVENT,NTRIES
      COMMON/PARTCL/PPTCL(9,499),NPTCL,IORIG(499),IDENT(499)
     *,IDCAY(499)
      COMMON/PARORD/ IORDP(499)
      COMMON/COMIND/ PUD,SIGMA,ALFA,BETA
      COMMON/MASQUA/ AMQ21,AMQ22
      COMMON/PRIMP0/ P0
      COMMON/NEEDR/ NRET
      COMMON/COMASS/ AM1,AM2
      DIMENSION IFORD(3),IFORDP(3)
      DIMENSION PSUM(5)
      LOGICAL RETU
C      INITIALIZE
      IPACK=1000
      NRET=0
      DO 151 I=1,3
151   PSUM(I)=0.
      PSUM(4)=ECM
      PSUM(5)=ECM
100   IRET=0
      RETU=.FALSE.
      PSIGN=-1.
      IKA=IDIN(1)
      IKB=IDIN(2)
      AMA=AM1
      AMB=AM2
      CALL FLAVOR(IKA,IFORDP(1),IFORDP(2),IFORDP(3),JSPIN,INDEX)
      CALL FLAVOR(IKB,IFORD(1),IFORD(2),IFORD(3),JSPIN,INDEX)
110   INR=IDINT(1.+3.*RNDMD(-1))
      IFLN=IFORDP(INR)
      IFL1=IABS(IFLN)
      IQ=0
      DO 155 I=1,3
      IF(IFL1.NE.IFORD(I)) GO TO 155
      IQ=I
      GO TO 156
155   CONTINUE
      IF(IQ.EQ.0) GO TO 110
156   IF(IQ-2) 10,20,30
10    IFL01=IFORD(2)
      IFL02=IFORD(3)
      IF(RNDMD(-1).GE.0.5) GO TO 40
      IFL01=IFORD(3)
      IFL02=IFORD(2)
      GO TO 40
20    IFL01=IFORD(1)
      IFL02=IFORD(3)
      IF(RNDMD(-1).GE.0.5) GO TO 40
      IFL01=IFORD(3)
      IFL02=IFORD(1)
      GO TO 40
30    IFL01=IFORD(1)
      IFL02=IFORD(2)
      IF(RNDMD(-1).GE.0.5) GO TO 40
      IFL01=IFORD(2)
      IFL02=IFORD(1)
40    CONTINUE
      IQ=0
      DO 50 I=1,3
      IF(IFL1.NE.IABS(IFORDP(I))) GO TO 50
      IQ=I
      GO TO 60
50    CONTINUE
60    IF(IQ-2) 70,80,90
70    IFL03=IFORDP(2)
      IFL04=IFORDP(3)
      IF(RNDMD(-1).GE.0.5) GO TO 95
      IFL03=IFORDP(3)
      IFL04=IFORDP(2)
      GO TO 95
80    IFL03=IFORDP(1)
      IFL04=IFORDP(3)
      IF(RNDMD(-1).GE.0.5) GO TO 95
      IFL03=IFORDP(3)
      IFL04=IFORDP(1)
      GO TO 95
90    IFL03=IFORDP(1)
      IFL04=IFORDP(2)
      IF(RNDMD(-1).GE.0.5) GO TO 95
      IFL03=IFORDP(2)
      IFL04=IFORDP(1)
95    CONTINUE
C    COMPUTE X VALUES FOR PARTONS
      CALL X2DIST(X1,X2,IFL01,IFL02)
      CALL X2DIST(X3,X4,IFL03,IFL04)
C     PT VALUES FOR PARTONS
      PHI=2.*PI*RNDMD(-1)
      CALL GAUSPT(PT1,SIGMA)
      AMZER2=AMB**2
      PZER2=P0**2
      AMQ21=AMZER2*(AMZER2+4.*X1*X2*PZER2)/(4.*(AMZER2+PZER2))-PT1**2
      PX1=PT1*DCOS(PHI)
      PY1=PT1*DSIN(PHI)
      PX2=-PX1
      PY2=-PY1
      PHI=2.*PI*RNDMD(-1)
      CALL GAUSPT(PT3,SIGMA)
      AMZER2=AMA**2
      AMQ22=AMZER2*(AMZER2+4.*X3*X4*PZER2)/(4.*(AMZER2+PZER2))-PT3**2
      PX3=PT3*DCOS(PHI)
      PY3=PT3*DSIN(PHI)
      PX4=-PX3
      PY4=-PY3
      NIN=NPTCL+1
      CALL XCORR(IFL03,IFL01,PX3,PY3,PX1,PY1,X3,X1,
     *PSIGN,NPRODS,RETU)
      NPRD=NPRODS
      IF(RETU) GO TO 100
      CALL XCORR(IFL04,IFL02,PX4,PY4,PX2,PY2,X4,X2,
     *PSIGN,NPRODS,RETU)
      NPRD=NPRD+NPRODS
      IF(.NOT.RETU) GO TO 140
      NPTCL=NPTCL-NPRD
      GO TO 100
140   NFIN=NPTCL
      IRD=0
      DO 200 I=NIN,NFIN
      IORDP(I)=IRD
200   IORIG(I)=25
      CALL RESCAL(NIN,NFIN,PSUM,IFAIL)
      IF(IFAIL.EQ.0) RETURN
      NPTCL=NPTCL-NPRD
      GO TO 100
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE THREES(IRET)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     COMPUTE THREE SHEETS ANNIHILATION DIAGRAM
C
      COMMON/CONST/PI,SQRT2,ALF,GF,UNITS
      COMMON/PRIMAR/SCM,HALFE,ECM,NJET,IDIN(2),NEVENT,NTRIES
      COMMON/PRIMP0/ P0
      COMMON/PARTCL/PPTCL(9,499),NPTCL,IORIG(499),IDENT(499)
     *,IDCAY(499)
      COMMON/PARORD/ IORDP(499)
      COMMON/COMIND/ PUD,SIGMA,ALFA,BETA
      COMMON/NEEDR/ NRET
      COMMON/MASQUA/ AMQ21,AMQ22
      COMMON/COAUX1/ AMA2,XI2(20),INUM
      COMMON/COMASS/AM1,AM2
      DIMENSION IFORD(3),IFORD1(3)
      DIMENSION PSUM(5)
      LOGICAL RETU
      EXTERNAL FMQ2
      DATA AM/0./,BM/15.0/,MAXFUN/200/
      DATA EPSI/0.001/
C       INITIALIZE
      IPACK=1000
      NRET=0
      DO 151 I=1,3
151   PSUM(I)=0.
      PSUM(4)=ECM
      PSUM(5)=ECM
150   IRET=0
      RETU=.FALSE.
      PSIGN=-1.
      P02=P0**2
      INUM=0
      IKA=IDIN(1)
      IKB=IDIN(2)
      AMA=AM1
      AMB=AM2
      CALL FLAVOR(IKB,IFORD1(1),IFORD1(2),IFORD1(3),JSPIN,INDEX)
      CALL FLAVOR(IKA,IFORD(1),IFORD(2),IFORD(3),JSPIN,INDEX)
      INR=IDINT(1.+3.*RNDMD(-1))
      IFL11=IFORD(INR)
      IF(INR-2) 10,20,30
10    IFL22=IFORD(2)
      IFL33=IFORD(3)
      IF(RNDMD(-1).GT.0.5) GO TO 160
      IFL22=IFORD(3)
      IFL33=IFORD(2)
      GO TO 160
20    IFL22=IFORD(1)
      IFL33=IFORD(3)
      IF(RNDMD(-1).GT.0.5) GO TO 160
      IFL22=IFORD(3)
      IFL33=IFORD(1)
      GO TO 160
30    IFL22=IFORD(1)
      IFL33=IFORD(2)
      IF(RNDMD(-1).GT.0.5) GO TO 160
      IFL22=IFORD(2)
      IFL33=IFORD(1)
160   INR=IDINT(1.+3.*RNDMD(-1))
      IFT11=IFORD1(INR)
      IF(INR-2) 40,50,60
40    IFT22=IFORD1(2)
      IFT33=IFORD1(3)
      IF(RNDMD(-1).GE.0.5) GO TO 260
      IFT22=IFORD1(3)
      IFT33=IFORD1(2)
      GO TO 260
50    IFT22=IFORD1(1)
      IFT33=IFORD1(3)
      IF(RNDMD(-1).GE.0.5) GO TO 260
      IFT22=IFORD1(3)
      IFT33=IFORD1(1)
      GO TO 260
60    IFT22=IFORD1(1)
      IFT33=IFORD1(2)
      IF(RNDMD(-1).GE.0.5) GO TO 260
      IFT22=IFORD1(2)
      IFT33=IFORD1(1)
C    COMPUTE X VALUES FOR PARTONS
260   CALL X3DIST(X11,X12,X13,IFL11,IFL22,IFL33)
      AMA2=AMA**2
      INUM=3
      XI2(1)=X11**2
      XI2(2)=X12**2
      XI2(3)=X13**2
      CALL RZERO(AM,BM,AMQ1P,RES,EPSI,MAXFUN,FMQ2)
      IF(RES.LT.0.) GO TO 260
C  COMPUTE PT VALUES FOR PARTONS
      CALL GAUSPT(PT11,SIGMA)
      PHI1=2.*PI*RNDMD(-1)
      CALL GAUSPT(PT12,SIGMA)
      PHI2=2.*PI*RNDMD(-1)
      PT11X=PT11*DCOS(PHI1)
      PT11Y=PT11*DSIN(PHI1)
      PT12X=PT12*DCOS(PHI2)
      PT12Y=PT12*DSIN(PHI2)
      PT13X=-(PT11X+PT12X)
      PT13Y=-(PT11Y+PT12Y)
C  COMPUTE X VALUES FOR PARTONS
261   CALL X3DIST(X21,X22,X23,IFT11,IFT22,IFT33)
      AMA2=AMB**2
      INUM=3
      XI2(1)=X21**2
      XI2(2)=X22**2
      XI2(3)=X23**2
      CALL RZERO(AM,BM,AMQ2P,RES,EPSI,MAXFUN,FMQ2)
      IF(RES.LT.0.) GO TO 261
C  COMPUTE PT VALUES FOR PARTONS
      CALL GAUSPT(PT21,SIGMA)
      PHI1=2.*PI*RNDMD(-1)
      CALL GAUSPT(PT22,SIGMA)
      PHI2=2.*PI*RNDMD(-1)
      PT21X=PT21*DCOS(PHI1)
      PT21Y=PT21*DSIN(PHI1)
      PT22X=PT22*DCOS(PHI2)
      PT22Y=PT22*DSIN(PHI2)
      PT23X=-(PT21X+PT22X)
      PT23Y=-(PT21Y+PT22Y)
      NIN1=NPTCL+1
      AMQ21=AMQ1P-PT11**2
      AMQ22=AMQ2P-PT21**2
      CALL XCORR(IFL11,IFT11,PT11X,PT11Y,PT21X,PT21Y,
     *X11,X21,PSIGN,NPRODS,RETU)
      IF(RETU) GO TO 150
      NPRD=NPRODS
      AMQ21=AMQ1P-PT12**2
      AMQ22=AMQ2P-PT22**2
      CALL XCORR(IFL22,IFT22,PT12X,PT12Y,PT22X,PT22Y,
     *X12,X22,PSIGN,NPRODS,RETU)
      NPRD=NPRD+NPRODS
      IF(.NOT.RETU) GO TO 130
      NPTCL=NPTCL-NPRD
      GO TO 150
130   AMQ21=AMQ1P-(PT13X**2+PT13Y**2)
      AMQ22=AMQ2P-(PT23X**2+PT23Y**2)
      CALL XCORR(IFL33,IFT33,PT13X,PT13Y,PT23X,PT23Y,
     *X13,X23,PSIGN,NPRODS,RETU)
      IF(.NOT.RETU) GO TO 140
      NPTCL=NPTCL-NPRD
      GO TO 150
140   NFIN1=NPTCL
      DO 500 I=NIN1,NFIN1
      IORDP(I)=0
500   IORIG(I)=35
      NPRD=NPRD+NPRODS
      CALL RESCAL(NIN1,NFIN1,PSUM,IFAIL)
      IF(IFAIL.EQ.0) GO TO 501
      NPTCL=NPTCL-NPRD
      GO TO 150
501   RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE BINAR(IRET)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  SIMULATION TWO PARTICLE REACTION
C
      COMMON /COMKI1/ HLA2,HLB2,W,INUMA
      COMMON/COMLID/PLIDER(499)
      COMMON /COMKI2/ELA,ELB,PLALB
      COMMON /CALC/HA,HB,HA2,HB2
      COMMON /BEES/B,BFOR
      COMMON/COMASS/ AM1,AM2
      COMMON/ITAPES/ ITDKY,ITEVT,ITCOM,ITLIS
      COMMON/PARTCL/PPTCL(9,499),NPTCL,IORIG(499),IDENT(499)
     *,IDCAY(499)
      COMMON/PARORD/ IORDP(499)
      COMMON/PRIMAR/SCM,HALFE,ECM,NJET,IDIN(2),NEVENT,NTRIES
      DIMENSION PA(3)
      LOGICAL SPINT
C  INITIALIZE
      INUMA=1
      NREP =0
      IK1=IDIN(1)
      IK2=IDIN(2)
      SPINT=.FALSE.
      IRET=0
      IB1=IB(IK1)
      IB2=IB(IK2)
       W= ECM
      HLA=AM1
      HLB=AM2
      PARBE=0.2
      IF(ECM.GT.HLA+HLB+PARBE) GO TO 999
      IRET=1
      RETURN
999   HLA2=HLA*HLA
      HLB2=HLB*HLB
      PLALB=DSQRT(ALAMB(SCM,HLA2,HLB2))/(2.0*ECM)
      ELA=(SCM+HLA2-HLB2)/(2.0*ECM)
      ELB=(SCM+HLB2-HLA2)/(2.0*ECM)
C   SELECT INTERACTIVE QUARKS
105   CALL FLAVOB(IK1,IFL1,IFL2)
      CALL FLAVOB(IK2,IFL3,IFL4)
      NREP=NREP+1
      IF(NREP.LT.NTRIES) GO TO 106
C     WRITE(ITLIS,1200) IDIN(1),IDIN(2),PLAB
1200  FORMAT(1X,'IN BINAR:NREP > NTRIES,IK1,IK2,PLAB=',
     *2I4,1X,F7.3)
      IRET=1
      RETURN
106   CONTINUE
      IREP1=0
C  HADRONS GENERATE BY MEANS QUARKS EXCHANGE
      IF(IFL1)1,1,2
1     IF11=IFL2
      IF22=IFL1
      GO TO 3
2     IF11=IFL1
      IF22=IFL2
3     CONTINUE
      IF(IB2.EQ.1) GO TO 102
      IF(IFL3) 101,101,102
101   IF44=IFL3
      IF33=IFL4
      GO TO 103
102   IF33=IFL3
      IF44=IFL4
103   CONTINUE
104   IKH2=IDPARS(IF11,IF44,SPINT,0)
      IKH1=IDPARS(IF33,IF22,SPINT,0)
      IREP1=IREP1+1
      IF(IREP1.GT.NTRIES) GO TO 105
C  SELECT ELASTIC COLLISION
C     IF(IKH1.EQ.IK1.AND.IKH2.EQ.IK2.AND.IREP1.LE.NTRIES) GO TO 104
      IF(IKH1.EQ.IK1.AND.IKH2.EQ.IK2.AND.IREP1.LE.NTRIES) GO TO 105
C  SELECT TABLE MASSES AND TABLE WIDTH OF HADRONS
      AMH1=AMASSF(IKH1)
      AMH2=AMASSF(IKH2)
      GAMH1=GAMHE(IKH1)
      GAMH2=GAMHE(IKH2)
C  COMPUTE MASSES OF PARTICLES
      IREP3=0
205   CONTINUE
      IREP3=IREP3+1
C     IF(IREP3.GT.NTRIES) GO TO 104
      IF(IREP3.GT.NTRIES) GO TO 105
      GAM1=WIDTH(GAMH1)
      GAM2=WIDTH(GAMH2)
      AMP1=AMH1+GAM1
      AMP2=AMH2+GAM2
C  CHECK ENERGY THRESHOLD
      IF(W.LT.AMP1+AMP2) GO TO 205
      HA=AMP1
      HA2=AMP1**2
      HB=AMP2
      HB2=AMP2**2
C  COMPUTE SCATTERING ANGLE
      IF(IB1.EQ.0.OR.IB2.EQ.0) INUMA=0
      IF(IB1.EQ.0.AND.IB2.EQ.0) INUMA=2
      CALL SLOPEB(IB1,IB2,PLALB,B)
      CALL HADGENANG(TFOR,TBACK,T,Z,PHI)
      PAMOD=DSQRT(ALAMB(SCM,HA2,HB2))/(2.0*ECM)
      PAN=PAMOD*DSQRT(1.-Z**2)
      PA(1)=PAN*DCOS(PHI)
      PA(2)=PAN*DSIN(PHI)
      PA(3)=PAMOD*Z
      NPTCL=NPTCL+1
      IDENT(NPTCL)=IKH1
      PPTCL(1,NPTCL)=PA(1)
      PPTCL(2,NPTCL)=PA(2)
      PPTCL(3,NPTCL)=PA(3)
      PPTCL(4,NPTCL)=DSQRT(PAMOD**2+HA2)
      PPTCL(5,NPTCL)=AMP1
      PPTCL(6,NPTCL)=0.
      PPTCL(7,NPTCL)=0.
      PPTCL(8,NPTCL)=0.
      PPTCL(9,NPTCL)=0.
      PLIDER(NPTCL)=1.
      IDCAY(NPTCL)=0
      IORIG(NPTCL)=8
      IORDP(NPTCL)=0
      NPTCL=NPTCL+1
      IDENT(NPTCL)=IKH2
      PPTCL(1,NPTCL)=-PA(1)
      PPTCL(2,NPTCL)=-PA(2)
      PPTCL(3,NPTCL)=-PA(3)
      PPTCL(4,NPTCL)=DSQRT(PAMOD**2+HB2)
      PPTCL(5,NPTCL)=AMP2
      PPTCL(6,NPTCL)=0.
      PPTCL(7,NPTCL)=0.
      PPTCL(8,NPTCL)=0.
      PPTCL(9,NPTCL)=0.
      PLIDER(NPTCL)=1.
      IORIG(NPTCL)=8
      IDCAY(NPTCL)=0
      IORDP(NPTCL)=0
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION WIDTH(GAM)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 WIDTH,GAM
C
C   COMPUTE WIDTH OF PARTICLE
C
        COMMON /CONST/ PI,SQRT2,ALF,GF,UNITS
100   DRND=RNDMD(-1)
      GT=GAM*DSQRT(-DLOG(DRND))
      PHI=2.*PI*RNDMD(-1)
      WIDTH=GT*DCOS(PHI)
      IF(DABS(WIDTH).GT.GAM) GO TO 100
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION COSAN(T)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 COSAN,T
C
C    COMPUTE ANGULAR DISTRIBUTION
C
C    FOR AP P COLLISION
C
      COMMON/TABLE1/ B12(30,3),ENER(30)
      LOGICAL SWANG
      RAN=RNDMD(-1)
      IF(T.LE.0.084) GO TO 200
      AMP=0.939
      PP=APP(T,AMP)
      WCOS=0.0105+0.0165*PP
      IF(T.GE.59.0) WCOS=1.
      SWANG=.FALSE.
      IF(RNDMD(-1).LE.WCOS) SWANG=.TRUE.
      IF(T.LE.0.226) GO TO 300
      IF(SWANG) GO TO 100
      B1=ANGINT(T,1)
      CANG0=ANGINT(T,3)
      COSAN=DLOG(RAN*DEXP(B1)+(1.-RAN)*DEXP(B1*CANG0))/B1
      RETURN
100   B2=ANGINT(T,2)
      CANG0=ANGINT(T,3)
      COSAN=DLOG(RAN*DEXP(B2*CANG0)+(1.-RAN)*DEXP(-B2))/B2
      RETURN
200   B1=ANGINT(T,1)
      COSAN=1.+DLOG(RAN*(1.-DEXP(-2.*B1))+DEXP(-2.*B1))/B1
      RETURN
300   IF(SWANG) GO TO 400
      B1=ANGINT(T,1)
      CANG0=ANGINT(T,3)
      COSAN=DLOG(RAN*DEXP(B1)+(1.-RAN)*DEXP(B1*CANG0))/B1
      RETURN
400   CANG0=ANGINT(T,3)
      COSAN=RAN*(CANG0+1.)-1.
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION APP(T,W)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 APP,T,W
C     CALCULATION OF THE MOMENTUM
      APP=DSQRT(T*(T+2.*W))
      RETURN
               END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION ANGINT(T,I)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 ANGINT,T
C
C    COMPUTE CONSTANTS BY INTERPOLATION
C    FOR AP P ELASTIC SCATTERING
C
      COMMON/TABLE1/ B12(30,3),ENER(30)
      COMMON/INTERP/ F(6)
C   COMPUTE OF THREE POINTS FOR INTERPOLATION
      L=1
4     IF(T-ENER(L)) 6,5,11
5     ANGINT=B12(L,I)
      RETURN
6     IF(L.GT.1) GO TO 7
      ANGINT=B12(L,I)
      RETURN
7     IF(L.GE.29) GO TO 8
      L1=L-2
      GO TO 9
8     L1=27
9     DO 10 K=1,3
      LL1=L1+K
      F(K)=B12(LL1,I)
      F(K+3)=ENER(LL1)
10    CONTINUE
      GO TO 12
11    L=L+1
      IF(L.EQ.30) GO TO 13
      GO TO 4
12    ANGINT=SINTER(T)
      RETURN
13    ANGINT=B12(30,I)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE ELAST(IRET)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     MONTE CARLO SIMULATION ELASTIC HADRON NUCLEON COLLISION
C
      COMMON/COMKI1/ HLA2,HLB2,W,INUMA
      COMMON/COMKI2/ ELA,ELB,PLALB
      COMMON/CALC/ HA,HB,HA2,HB2
      COMMON/BEES/ B,BFOR
      COMMON/PRIMAR/SCM,HALFE,ECM,NJET,IDIN(2),NEVENT,NTRIES
      COMMON/PARTCL/PPTCL(9,499),NPTCL,IORIG(499),IDENT(499)
     *,IDCAY(499)
      COMMON/PARORD/ IORDP(499)
      COMMON /ORDER/ IRD1,IRD2
      COMMON/COMLID/ PLIDER(499)
      COMMON/COMELX/ SIGEL
      COMMON/COMCRO/ SIGTOT
      COMMON/COMASS/ AM1,AM2
      COMMON/NPTCLZ/ NPTCLZ
      COMMON/PRIMP0/ P0
      DIMENSION PA(3),P1(3),P2(3)
      IRET=0
      SIGEL0=SIGEL
      P02=P0**2
      IEXE=0
      IK01=IDIN(1)
      IK02=IDIN(2)
      HLA=AM1
      HLB=AM2
      HLA2=HLA*HLA
      HLB2=HLB*HLB
C   W= CENTRE OF MASS (C.M.) ENERGY
      W=ECM
C  TKIN=KINETIC ENERGY OF PROJECTILE IN TARGET REST FRAME
      TKIN=(SCM-HLA2-HLB2)/(2.0*HLB)-HLA
C   PLALB=CM MOMENTUM OF A OR B IN ELASTIC EVENTS
      PLALB=DSQRT(ALAMB(SCM,HLA2,HLB2))/(2.0*W)
C   ELA=CM ENERGY OF A IN ELASTIC EVENT *** ELB=SAME FOR B
      ELA=(SCM+HLA2-HLB2)/(2.0*W)
      ELB=(SCM+HLB2-HLA2)/(2.0*W)
      IK1=IK01
      IK2=IK02
      IB1=IB(IK1)
      IB2=IB(IK2)
      HA=HLA
      HB=HLB
      INUMA=1
      IF(IB1.EQ.0.OR.IB2.EQ.0) INUMA=0
      IF(IB1.EQ.0.AND.IB2.EQ.0) INUMA=2
      HA2=HA*HA
      HB2=HB*HB
      TOBR=10.0
      IF(IB1.NE.0) GOTO 71
        TOBR=2.4
      IF(IB2.NE.0) GO TO 71
      TOBR=0.
      IF(TKIN.GT.2.5) GO TO 71
      IF(AM1.GT.0.50.OR.AM2.GT.0.50) GO TO 71
C  COMPUTE OF RESONANCE PARAMETERS
C   PI+PI--RHO,OMEGA,  PI+K--K*,   K+K--PHI
C
      QSUM = CHARGE(IK01)+CHARGE(IK02)
      IF(DABS(QSUM).GE.2.) GO TO 71
      ISOB=0
      P1(1)=0.
      P1(2)=0.
      P1(3)=P0
      P2(1)=0.
      P2(2)=0.
      P2(3)=-P0
      CALL FOROM(IK1,P1,AM1,IK2,P2,AM2,SIGEL0,
     *IKD,PXD,PYD,PZD,DMAS,ISOB)
      IF(ISOB.EQ.0) GO TO 71
      NPTCL=1
      IORDP(NPTCL)=0
      IORIG(NPTCL)=0
      IDCAY(NPTCL)=0
      IDENT(NPTCL)=IKD
      PPTCL(5,NPTCL)=DMAS
      PPTCL(1,NPTCL)=PXD
      PPTCL(2,NPTCL)=PYD
      PPTCL(3,NPTCL)=PZD
      PPTCL(4,NPTCL)=DSQRT(DMAS**2+PXD**2+PYD**2+PZD**2)
      PPTCL(6,NPTCL)=0.
      PPTCL(7,NPTCL)=0.
      PPTCL(8,NPTCL)=0.
      PPTCL(9,NPTCL)=0.
      PLIDER(NPTCL)=1.
      RETURN
 71   IF(TKIN-TOBR) 72,72,73
 72   CALL ELZPHI(IK01,IK02,TKIN,Z,PHI,IEXE)
      GO TO 74
 73   CONTINUE
C   SLOPE CALCULATES THE DIFFRACTIVE SLOPES FOR THE CHOSEN MASSES
      CALL SLOPE(B,BFOR)
C   ANG CALCULATES THE TWO-BODY SCATTERING ANGLES (AZIMUTHAL ANGLE PHI
C   AND POLAR ANGLE THETA,WHERE Z=COS(THETA)
      IB1=IB(IK1)
      IB2=IB(IK2)
C     IF(IB1.EQ.-1.AND.IB2.NE.-1) B=11.0
      CALL HADGENANG(TFOR,TBACK,T,Z,PHI)
74    IF(IEXE.EQ.0) GO TO 76
      HA=AMASS(IK1)
      HB=AMASS(IK2)
      HA2=HA*HA
      HB2=HB*HB
 76   PAMOD=DSQRT(ALAMB(SCM,HA2,HB2))/(2.0*W)
      PAN=PAMOD*DSQRT(DABS(1.-Z**2))
      PA(1)=PAN*DCOS(PHI)
      PA(2)=PAN*DSIN(PHI)
      PA(3)=PAMOD*Z
      EA=DSQRT(PAMOD**2+HA2)
      EB=DSQRT(PAMOD**2+HB2)
      NPTCL=NPTCL+1
      IDCAY(NPTCL)=0
      IORIG(NPTCL)=4
      IORDP(NPTCL)=IRD1
      IDENT(NPTCL)=IK1
      PPTCL(1,NPTCL)=PA(1)
      PPTCL(2,NPTCL)=PA(2)
      PPTCL(3,NPTCL)=PA(3)
      PPTCL(4,NPTCL)=EA
      PPTCL(5,NPTCL)=HA
      PPTCL(6,NPTCL)=0.
      PPTCL(7,NPTCL)=0.
      PPTCL(8,NPTCL)=0.
      PPTCL(9,NPTCL)=0.
      PLIDER(NPTCL)=1.
      NPTCL=NPTCL+1
      IDCAY(NPTCL)=0
      IORIG(NPTCL)=4
      IORDP(NPTCL)=IRD2
      IDENT(NPTCL)=IK2
      PPTCL(1,NPTCL)=-PA(1)
      PPTCL(2,NPTCL)=-PA(2)
      PPTCL(3,NPTCL)=-PA(3)
      PPTCL(4,NPTCL)=EB
      PPTCL(5,NPTCL)=HB
      PPTCL(6,NPTCL)=0.
      PPTCL(7,NPTCL)=0.
      PPTCL(8,NPTCL)=0.
      PPTCL(9,NPTCL)=0.
      PLIDER(NPTCL)=1.
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
        SUBROUTINE ELZPHI(IK1,IK2,TKIN,Z,PHI,IEXE)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 TKIN,Z,PHI
        COMMON /CONST/ PI,SQRT2,ALF,GF,UNITS
      PHI=2.*PI*RNDMD(-1)
      IF(IEXE.EQ.0) GO TO 1
       Z=COSP(TKIN,12)
       RETURN
1     IB1=IB(IK1)
      IB2=IB(IK2)
      IF(IB1.LT.0.AND.IB2.GT.0) GO TO 2
      CALL MARK(IK1,IK2,KS)
      IBP=IB(IK1)
      Z=COSAM(IBP,TKIN,KS)
      RETURN
2     Z=COSAN(TKIN)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION FMQ2(AMQ2)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 FMQ2,AMQ2
      COMMON/PRIMP0/ P0
      COMMON/COAUX1/ AMA2,XI2(20),INUM
      P02=P0**2
      ENER=DSQRT(AMA2+P02)
      SUME=0.
      DO 100 I=1,INUM
      SUME=SUME+DSQRT(AMQ2+XI2(I)*P02)
100   CONTINUE
      FMQ2=ENER-SUME
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE REACTH
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  SELECT REACTION TYPE AT HIGH ENERGY IN H-N AND HBAR-N COLLISIONS
C
      LOGICAL GH1H2
      COMMON/H1H2/ GH1H2(11)
      COMMON/ITAPES/ ITDKY,ITEVT,ITCOM,ITLIS
      COMMON/PRIMAR/SCM,HALFE,ECM,NJET,IDIN(2),NEVENT,NTRIES
      COMMON/HADSIG/SIGS(100),SIGEVT,NSIGS,INOUT(2,100)
      COMMON/COMPOM/ POMGEN(15)
      COMMON/COMCOL/ NAC(100,4),NBC(100,4),NCOL
      DIMENSION INITYP(2),IREATY(2)
      COMMON/COMPLI/ LIMP
      COMMON/YESELA/YESELA
      COMMON /PRINTS/ IPRINT
      COMMON /CPRSIG/ ISGCOL
      LOGICAL YESELA
      LOGICAL IPRINT
C      INITIALIZE
  1   SIGEVT=0.
      IOPAK=10000
      SIGTOT=0.
      DO 110 I=1,11
110   GH1H2(I)=.FALSE.
      DO 100 ISIGN=1,NSIGS
100   SIGTOT=SIGTOT+SIGS(ISIGN)
      IF(SIGTOT.EQ.0.) GO TO 9999
C   FIND REACTION
      TRY=RNDMD(-1)
      SUM=0.
      DO 200 I=1,NSIGS
      ISIGS=I
      SUM=SUM+SIGS(I)/SIGTOT
      IF(SUM.GT.TRY) GO TO 300
200   CONTINUE
300   SIGEVT=SIGS(ISIGS)
      IF(IPRINT)
     *  WRITE(ITLIS,1001) NSIGS,(SIGS(K),K=1,NSIGS)
1001  FORMAT(1X,'RH:',1X,I3,11(1X,E10.3))
      IF(IPRINT) WRITE(ITLIS,1002) ISIGS,SUM,TRY
1002  FORMAT(1X,'RH: ITLIS,SUM,TRY=',I3,2(1X,E13.6))
C   @@@@@@@@@@@@@@@@@
C     INITYP(1)=INOUT1(1,ISIGS)
C     INITYP(2)=INOUT2(1,ISIGS)
C     IREATY(1)=INOUT1(2,ISIGS)
C     IREATY(2)=INOUT2(2,ISIGS)
      I1=INOUT(1,ISIGS)
      DO 400 K=1,2
      INITYP(K)=MOD(I1,IOPAK)
  400 I1=I1/IOPAK
      I2=INOUT(2,ISIGS)
      DO 500 K=1,2
      IREATY(K)=MOD(I2,IOPAK)
  500 I2=I2/IOPAK
      IF(IREATY(2).EQ.2.OR.IREATY(2).EQ.4) INITYP(1)=-INITYP(1)
      IF(IREATY(2).EQ.3.OR.IREATY(2).EQ.4) INITYP(2)=-INITYP(2)
C   @@@@@@@@@@@@@@@@@
      GH1H2(IREATY(1))=.TRUE.
      IF(IPRINT) WRITE(ITLIS,1003) INITYP,IREATY
1003  FORMAT(1X,'RH: INITYP,IREATY=',4(I6,1X))
      IF(IREATY(1).NE.7) GO TO 910
C  SELECT NUMBER OF POMERONS
      TRY=RNDMD(-1)
      DO 700 NPOM=1,LIMP
      NC=NPOM
      IF(POMGEN(NPOM).GT.TRY) GO TO 800
700   CONTINUE
800   CONTINUE
      NCOL=NC
      DO 900 J=1,NCOL
      NAC(J,3)=0
      NBC(J,3)=0
      NAC(J,1)=1
      NBC(J,1)=1
      NAC(J,4)=0
      NBC(J,4)=0
      NAC(J,2)=INITYP(1)
      NBC(J,2)=INITYP(2)
900   CONTINUE
910   CONTINUE
      IF(YESELA)  RETURN
      IF(GH1H2(4))  GO  TO  1
      RETURN
9999  WRITE(ITLIS,1000) SIGEVT
1000  FORMAT(//10X,28H...CHECK YOUR INPUT..SIGEVT=,E10.4)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION FP02(PNEW2)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 FP02,PNEW2
      COMMON/PRIMP0/ P0
      COMMON/COAUX1/ AMA2,XI2(20),INUM
      COMMON/COAUX2/ X12,X22,X32,PT12,PT22,PT32
      P02=P0**2
      ENER=DSQRT(P02+AMA2)
      SUME=DSQRT(AMA2+X32*PNEW2+PT32)+DSQRT(X22*PNEW2+PT22)+
     *DSQRT(X12*PNEW2+PT12)
      FP02=ENER-SUME
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION RNFAC(K)
      REAL*8 RNFAC
C
C      RETURN N FACTORIAL
C
      IF(K.GT.1) GO TO 1
      RNFAC=1.
      RETURN
1     RNFAC=FLOAT(K)
      DO 2 J=2,K
2     RNFAC=RNFAC*FLOAT(K-J+1)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION FUNIT(Z)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 FUNIT,Z
C
C    RETURN UNITARIZATION FUNCTION FOR POMERON
C
      DATA LIMP2/12/
      FUNIT=1.
      DO 100 J=2,LIMP2
100   FUNIT=FUNIT+(-Z)**(J-1)/(FLOAT(J)*RNFAC(J))
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE SIGIN
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  COMPUTE DIAGRAM WEIGHTS FOR INELASTIC
C  HADRON BARYON COLLISIONS
C
C  SIGMA    =CROSS SETION SUMMED OVER TYPES ALLOWED BY GH1H2
C  SIGS(I)  =PARTIAL CROSS SECTION FOR DUAL TYPE DIAGRAM
C
C     REAL*8 LAB1,LAB2
      CHARACTER*8 LAB1,LAB2
      LOGICAL GH1H2
      LOGICAL GH
      LOGICAL IPRINT
      LOGICAL MULTP
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      COMMON/HADSIG/SIGS(100),SIGEVT,NSIGS,INOUT(2,100)
      COMMON/CONST/PI,SQRT2,ALF,GF,UNITS
      COMMON/PRIMAR/SCM,HALFE,ECM,NJET,IDIN(2),NEVENT,NTRIES
      COMMON/REACOE/ COEF(11),COEF1(11)
      COMMON/COMPLI/ LIMP
      COMMON/COMECB/ ECMB
      COMMON/H1H2/ GH1H2(11)
      COMMON/COMPOM/ POMGEN(15)
      COMMON/CANPOM/ POAGEN(15)
      COMMON/CSIGSU/ SIGSUM
      COMMON/PRIMPL/ PL
      COMMON/CSIGA/ SIGAN
      COMMON/COMELX/ SIGEL
      COMMON/COMCRO/ SIGTOT
      COMMON/COMASS/ AM1,AM2
      COMMON/CPRSIG/ ISGCOL
      COMMON/PRINTS/IPRINT
      COMMON/COMENB/ ENBOU
      COMMON /SIGDIA/ CROSD(5),DST
      COMMON/FRGCPA/ PUDC,PUDCC,PSPINC,PJSPNC,PMIX1C(3,2),PMIX2C(3,2),
     *PBARC
      COMMON/FRGSPA/ PUDS,PUDSC,PSPINS,PJSPNS,PMIX1S(3,2),PMIX2S(3,2),
     *SIGQTS,WENDM,WENDB,PBARS,PRIQS(9),PARDBS,PARQLS,PARRS,PUNDS
      COMMON/COMMUL/ MULTP
*@@@@@@@@@@
      COMMON/FLACOM/NFLA,NFL1,NFL2,NFL3,NSPIN,NNDEX
*@@@@@@@@@@
      DIMENSION GH(11)
      DIMENSION COEF01(11)
      DIMENSION IFL1(3),IFL2(3)
C
      DATA ALFR/0.5/
      DATA BPOM/2.05/,BPOMA/2.0/,APOM/3.5/,APOMA/3.5/
C
      DO 1 I=1,11
 1    COEF01(I)=COEF1(I)
C
C   INITIALIZE CROSS SECTION
C             SCALE=0.389
      ISGCOL=0
      SIGSUM=0.
      NSIGS=0
      SIGM =0.
      SIGPS=0.
      SIG=0.
      IPL=0
       PJSC=PJSPNC
       PSPC=PSPINC
       PJSS=PJSPNS
       PSPS=PSPINS
      DO 99 I=1,11
99    GH(I)=.TRUE.
*@@@@@@@@@@
      NFLA=-1
*@@@@@@@@@@
      CALL FLAVOR(IDIN(1),IFL1(1),IFL1(2),IFL1(3),JSPIN1,INDEX1)
      CALL FLAVOR(IDIN(2),IFL2(1),IFL2(2),IFL2(3),JSPIN2,INDEX2)
*@@@@@@@@@@
      IF(NFLA.EQ.-1)  NFLA=-2
*@@@@@@@@@@
      IB1=IB(IDIN(1))
      IB2=IB(IDIN(2))
      IF(IB1.GT.0.AND.IB2.GT.0) GO TO 5
      IF(IB1.LT.0.AND.IB2.LT.0) GO TO 5
      L1=1
      IF(IB1.EQ.0.AND.IB2.EQ.0) L1=2
      DO 3 I=L1,3
      I1=IFL1(I)
      DO 3 J=L1,3
      I2=-IFL2(J)
      IF(I1.NE.I2) GO TO 39
      IPL=1
       GO TO 4
 39   CONTINUE
 3    CONTINUE
 4    IF(IPL.EQ.1) GO TO 77
 5    GH(2) = .FALSE.
      GH1H2(2)=.FALSE.
77    IF(IB1.LT.0.AND.IB2.GT.0) GO TO 6
      GH(5)=.FALSE.
      GH1H2(5)=.FALSE.
      GO TO 7
  6   GH(8)=.FALSE.
      GH1H2(8)=.FALSE.
 7    CONTINUE
      IF(IB1.EQ.0.AND.IB2.EQ.0) CALL  GHGH00(GH)
 8    PARBE=0.38
      DSM=ECM-AM1-AM2
      IF(ECM.GE.ENBOU) GO TO 33
      PJSPNC=.75
      PSPINC=.75
      PJSPNS=.75
      PSPINS=.75
      GH1H2(3)=.FALSE.
      GH(3)=.FALSE.
      GH1H2(11)=.FALSE.
      GH(11)=.FALSE.
C ONLY PLAN,DIFSMA,DIFTRI,ANNIH ALLOWED FOR ECM < ENBOW
      MULTP=.FALSE.
      IF(IB1.EQ.0.AND.IB2.LT.0) GO TO 30
      IF(IB1.LE.0.AND.IB2.LE.0) GO TO 30
      GH1H2(7)=.FALSE.
      GH1H2(8)=.FALSE.
      GH1H2(10)=.FALSE.
      GH(7)=.FALSE.
      GH(8)=.FALSE.
      GH(10)=.FALSE.
      GO TO 33
 30   IF(DSM.GE.PARBE) GO TO 33
      GH1H2(1)=.FALSE.
C    **** PLANAR DIAG. SHOULD NOT BE FORBIDDEN  ***  SIVOKL.01.08.91
      GH1H2(2)=.FALSE.
      GH1H2(6)=.FALSE.
      GH1H2(7)=.FALSE.
C    **** BINAR  DIAG. SHOULD NOT BE FORBIDDEN  ***  SIVOKL.01.08.91
      GH1H2(8)=.FALSE.
      GH1H2(10)=.FALSE.
      GH(1)=.FALSE.
      GH(2)=.FALSE.
      GH(6)=.FALSE.
      GH(7)=.FALSE.
      GH(8)=.FALSE.
      GH(10)=.FALSE.
33    I1=IABS(IDIN(1))
      I2=IABS(IDIN(2))
      I4=1
C   @@@@@@@@@@@@@@@
      IF(IDIN(1).LT.0) I4=2
      IF(IDIN(2).LT.0) I4=3
      IF(IDIN(1).LT.0.AND.IDIN(2).LT.0) I4=4
C   @@@@@@@@@@@@@@@
      IF(IB1.EQ.0.AND.IB2.EQ.0) MULTP=.FALSE.
      CALL LABEL(LAB1,IDIN(1))
      CALL LABEL(LAB2,IDIN(2))
C     IF(ISGCOL.EQ.0.AND.IPRINT) WRITE(ITLIS,1000) LAB1,LAB2,SCM
1000  FORMAT(//15X,47HI SELECT THE NEXT REACTIONS WITH CROSS SECTIONS
     *,/20X,3HFOR,1X,A8,A8,18H COLLISION AT SCM=,E10.4,7H GEV**2/)
      IFL=IDIN(1)/1000
      IF(IFL.NE.0) GO TO 107
      JFL=MOD(I1/100,10)
      KFL=MOD(I1/10,10)
      IF(JFL.NE.3.AND.KFL.NE.3) GO TO 106
       COEF01(2)= COEF1(2)*0.4
       COEF01(3)= COEF1(3)*0.33
       COEF01(8)= COEF1(8)*0.33
       COEF01(10)= COEF1(10)*0.33
      GO TO 107
106   CONTINUE
       COEF01(2)= COEF1(2)*0.4
       COEF01(3)= COEF1(3)*0.33
       COEF01(8)= COEF1(8)*0.33
       COEF01(10)= COEF1(10)*0.33
107   CONTINUE
      IF(ECM.GE.ECMB) GO TO 400
C  TWO PARTICLE REACTION CROSS SECTION
C
      SIG     = COEF01(8)/SCM
      IF(ECM.LT.ENBOU) SIG=CROSD(1)
      IF(GH1H2(8)) GO TO 155
      IF(.NOT.GH(8)) GO TO 200
      SIGM=SIGM+SIG
      IF(ISGCOL.EQ.0.AND.IPRINT) WRITE(ITLIS,80) SIG
80    FORMAT(10X,34HI DO NOT USE BINAR REACTION.SIGMA=,E10.4)
       GO TO 200
155   CALL SIGFIH(SIG,I1,I2,8,I4)
200   CONTINUE
C    PLANAR TYPE DIAGRAM (Q,QBAR-ANNIHILATION)
      SIG     = COEF01(2)*SCM**(ALFR-1.)
      IF(IB1.EQ.0.AND.IB2.EQ.0) GO TO 292
      IF(ECM.LT.ENBOU.AND.IB1*IB2.EQ.-1) SIG = CROSD(3)
      IF(ECM.LT.ENBOU.AND.IB1*IB2.NE.-1) SIG = CROSD(4)
      IF(ECM.LT.ENBOU.AND..NOT.GH1H2(2)) GH(2)=.TRUE.
292   IF(GH1H2(2)) GO TO 291
      IF(.NOT.GH(2)) GO TO 400
      SIGM=SIGM+SIG
      IF(ISGCOL.EQ.0.AND.IPRINT) WRITE(ITLIS,82) SIG
 82   FORMAT(10X,35HI DO NOT USE PLANAR REACTION.SIGMA=,E10.4)
      GO TO 400
291   CALL SIGFIH(SIG,I1,I2,2,I4)
C      ELASTIC SCATTERING DIAGRAM
400   SIG=SIGEL
      IF(GH1H2(4)) GO TO 491
      SIGM=SIGM+SIG
      IF(ISGCOL.EQ.0.AND.IPRINT) WRITE(ITLIS,84) SIG
 84   FORMAT(10X,36HI DO NOT USE ELASTIC REACTION.SIGMA=,E10.4)
      GO TO 500
491   CALL SIGFIH(SIG,I1,I2,4,I4)
500   CONTINUE
      IF(ECM.GT.ECMB) GO TO 600
C     ANNIHILATION DIAGRAM
      SIG=SIGAN
      IF(GH1H2(5)) GO TO 591
      IF(.NOT.GH(5)) GO TO 600
      SIGM=SIGM+SIG
      IF(ISGCOL.EQ.0.AND.IPRINT) WRITE(ITLIS,85) SIG
 85   FORMAT(10X,41HI DO NOT USE ANNIHILATION REACTION.SIGMA=,E10.4)
      GO TO 600
591   CALL SIGFIH(SIG,I1,I2,5,I4)
      BPOMA=2.35-0.25*DLOG(ECM)
      SIGPA= 0.
      DO 605 I=1,LIMP
      POAGEN(I)=(1.+APOMA*I**2)*DEXP(-BPOMA*I)
 605  SIGPA=SIGPA+POAGEN(I)
      PSUM=0.
      DO 606 I=1,LIMP
      PSUM=PSUM+POAGEN(I)/SIGPA
      POAGEN(I)=PSUM
 606  CONTINUE
C   HIGH MASS DIFFRACTION
600   SIG = COEF01(1)*SIGEL
      IF(ECM.LT.ENBOU.AND.IB1*IB2.EQ.-1) SIG = CROSD(2)
      IF(ECM.LT.ENBOU.AND.IB1*IB2.NE.-1) SIG = CROSD(2)*DST
      IF(GH1H2(1)) GO TO 160
      IF(.NOT.GH(1)) GO TO 802
      SIGM=SIGM+SIG
      IF(ISGCOL.EQ.0.AND.IPRINT) WRITE(ITLIS,81) SIG
81    FORMAT(10X,37HI DO NOT USE DIFR.OF HIGH MASS.SIGMA=,E10.4)
      GO TO 802
160   CALL SIGFIH(SIG,I1,I2,1,I4)
C   TRIPLE REGGEON DIAGRAM
802   SIG = COEF01(10)*SCM**(ALFR-1.)
      IF(ECM.LT.ENBOU) SIG = CROSD(3)
      IF(GH1H2(10)) GO TO 162
      IF(.NOT.GH(10)) GO TO 800
      SIGM=SIGM+SIG
      IF(ISGCOL.EQ.0.AND.IPRINT) WRITE(ITLIS,181) SIG
181   FORMAT(10X,39HI DO NOT USE TRIPLE REGGEON DIAG.SIGMA=,E10.4)
      GO TO 800
162   CALL SIGFIH(SIG,I1,I2,10,I4)
C   DOUBLE DIFRACTION DIAGRAM
C
800    CONTINUE
      SIG =SIGEL* COEF01(11)
      IF(GH1H2(11)) GO TO 172
      IF(.NOT.GH(11)) GO TO 890
      SIGM=SIGM+SIG
      IF(ISGCOL.EQ.0.AND.IPRINT) WRITE(ITLIS,191) SIG
191   FORMAT(10X,42HI DO NOT USE DOUBLE DIFRACTION DIAG.SIGMA=,E10.4)
      GO TO 890
172   CALL SIGFIH(SIG,I1,I2,11,I4)
C    SMALL MASS DIFFRACTION
890    CONTINUE
      SIG =SIGEL* COEF01(6)
      IF(IB1.EQ.0.AND.IB2.EQ.0) GO TO 692
      IF(ECM.LT.ENBOU.AND.IB1*IB2.EQ.-1) SIG = CROSD(1)
      IF(ECM.LT.ENBOU.AND.IB1*IB2.NE.-1) SIG = CROSD(2)*(1.-DST)
      GO TO 693
692   SIG=SIG*1.9
693   IF(GH1H2(6)) GO TO 691
      IF(.NOT.GH(6)) GO TO 300
      SIGM=SIGM+SIG
      IF(ISGCOL.EQ.0.AND.IPRINT) WRITE(ITLIS,86) SIG
 86   FORMAT(10X,39HI DO NOT USE DIFR. OF SMALL MASS.SIGMA=,E10.4)
      GO TO 300
691   CALL SIGFIH(SIG,I1,I2,6,I4)
300   IF(ECM.GT.ECMB) GO TO 700
C           UNDEVELOPED CYLINDER TYPE DIAGRAM
      SIG      = COEF01(3)*SCM**(ALFR-1.)
      IF(GH1H2(3)) GO TO 391
      IF(.NOT.GH(3)) GO TO 700
      SIGM=SIGM+SIG
      IF(ISGCOL.EQ.0.AND.IPRINT) WRITE(ITLIS,83) SIG
 83   FORMAT(10X,39HI DO NOT USE UNCYLINDER REACTION.SIGMA=,E10.4)
      GO TO 700
391   CALL SIGFIH(SIG,I1,I2,3,I4)
C      MULTI POMERON SCATTERING DIAGRAM
 700  CONTINUE
      IF(ECM.GE.ECMB) BPOM=2.840-0.215*DLOG(ECM)
      IF(ECM.LT.ECMB) BPOM=2.731-0.4500*DLOG(ECM)
       DO 805 I=1,LIMP
      POMGEN(I)=(1.+APOM*I**2)*DEXP(-BPOM*I)
805   SIGPS=SIGPS+POMGEN(I)
      PSUM=0.
      DO 810 I=1,LIMP
      PSUM=PSUM+POMGEN(I)/SIGPS
      POMGEN(I)=PSUM
810   CONTINUE
      IF(.NOT.GH1H2(7)) GO TO 900
      SIG=SIGTOT-SIGSUM-SIGM
      IF(SIG.LT.0.)  SIG=0.
      CALL SIGFIH(SIG,I1,I2,7,I4)
900   CONTINUE
      ISGCOL=1
      PJSPNC=PJSC
      PSPINC=PSPC
      PJSPNS=PJSS
      PSPINS=PSPS
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE GHGH00(GH)
      LOGICAL GH1H2
      LOGICAL GH
      COMMON/H1H2/ GH1H2(11)
      DIMENSION GH(11)
      GH(1)=.FALSE.
      GH(3)=.FALSE.
      GH(5)=.FALSE.
      GH(8)=.FALSE.
      GH(10)=.FALSE.
      GH(11)=.FALSE.
      GH1H2(1)=.FALSE.
      GH1H2(3)=.FALSE.
      GH1H2(5)=.FALSE.
      GH1H2(8)=.FALSE.
      GH1H2(10)=.FALSE.
      GH1H2(11)=.FALSE.
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE RZERO(A,B,X,R,ETA,MAXFUN,FCN)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 A,B,X,R,ETA,FCN
C
C     DATA TETA/1.E-12/
      DATA TETA/1.D-6/
C     TETA,MACHINE DEPENDENT,IS THE COMPUTER PRECISION
      EPSI=ETA
      IF(EPSI.LE.TETA) EPSI=TETA
      FLOW=1.D30
      E=1.
      MC=0
      XA=DMIN1(A,B)
      XB=DMAX1(A,B)
      I=1
c      FA=FCN(XA,I)
      FA=FCN(XA)    ! sobol, 16.04.2002
      MC=MC+1
      I=2
c      FB=FCN(XB,I)
      FB=FCN(XB)    ! sobol
      IF(FA*FB.GT.0.) GO TO 16
      MC=MC+1
C
    4 X=0.5*(XA+XB)
      R=X-XA
      EE=DABS(X)+E
      IF(R.LE.EE*EPSI) GO TO 18
      F1=FA
      X1=XA
      F2=FB
      X2=XB
    1 CONTINUE
      MC=MC+1
      IF(MC.GT.MAXFUN) GO TO 17
c      FX=FCN(X,I)
      FX=FCN(X)    ! sobol
C
      IF(FX*FA.GT.0) GO TO 2
      FB=FX
      XB=X
      GO TO 3
    2 XA=X
      FA=FX
    3 CONTINUE
C
C     PARABOLA ITERATION
C
      F3=FX
      X3=X
      IF(DABS(F1-F2).GE.FLOW*DABS(X1-X2)) GO TO 4
      U1=(F1-F2)/(X1-X2)
      IF(DABS(F2-FX).GE.FLOW*DABS(X2-X)) GO TO 4
      U2=(F2-FX)/(X2-X)
      CA=U1-U2
      CB=(X1+X2)*U2-(X2+X)*U1
      CC=(X1-X)*F1-X1*(CA*X1+CB)
      IF(DABS(CB).GE.FLOW*DABS(CA)) GO TO 8
      U3=0.5*CB/CA
      IF(DABS(CC).GE.FLOW*DABS(CA)) GO TO 4
      U4=U3**2-CC/CA
      IF(U4.LT.0.) GO TO4
      U5=DSQRT(U4)
      IF(X.GE.-U3) GO TO 10
      X=-U3-U5
      GO TO9
   10 X=-U3+U5
      GO TO 9
    8 IF(DABS(CC).GE.FLOW*DABS(CB)) GO TO 4
      X=-CC/CB
    9 CONTINUE
      IF(X.LT.XA) GO TO 4
      IF(X.GT.XB) GO TO 4
C
C     TEST FOR OUTPUT
C
      R=DABS(X-X3)
      R1=DABS(X-X2)
      IF(R.GT.R1) R=R1
      EE=DABS(X)+E
      IF(R/EE.GT.EPSI) GO TO 5
      MC=MC+1
      IF(MC.GT.MAXFUN) GO TO 17
c      FX=FCN(X,I)
      FX=FCN(X)    ! sobol
      IF(FX.EQ.0.) GO TO 18
      IF(FX*FA.LT.0.) GO TO 7
      XX=X+EPSI*EE
      IF(XX.GE.XB) GO TO 18
      MC=MC+1
      IF(MC.GT.MAXFUN) GO TO 17
c      FF=FCN(XX,I)
      FF=FCN(XX)    ! sobol
      FA=FF
      XA=XX
      GO TO 6
    7 XX=X-EPSI*EE
      IF(XX.LE.XA) GO TO 18
      MC=MC+1
c      FF=FCN(XX,I)
      FF=FCN(XX)    ! sobol
      FB=FF
      XB=XX
    6 IF(FX*FF.GT.0.) GO TO 14
   18 CONTINUE
      R=EPSI*EE
      I=3
c      FF=FCN(X,I)
      FF=FCN(X)    ! sobol, 16.04.2002
      RETURN
   14 F1=F3
      X1=X3
      F2=FX
      X2=X
      X=XX
      FX=FF
      GO TO 3
C
    5 CONTINUE
      F1=F2
      X1=X2
      F2=F3
      X2=X3
      GO TO 1
C
   16 CONTINUE               ! sobol
c-Sob   16 WRITE(16,301)
c-Sob  301 FORMAT(5X,'RZERO    FCN(A,I)  AND FCN(B,I)  HAVE THE SAME SIGN ')
      R=-2.*(XB-XA)
      X=0.
      RETURN
C
   17 CONTINUE               ! sobol
c-Sob   17 WRITE(16,300) MC
c-Sob  300 FORMAT(10X,'RZERO  ',I5,' CALLS OF THE FUNCTION'/10X,
c-Sob     1'CALL LIMIT EXCEEDED'///)
      R=-0.5*DABS(XB-XA)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION ZFRAGS(IFL,IFLN,PT2,ZMIN,ZMAX)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 ZFRAGS,PT2,ZMIN,ZMAX
C  RETURN FRACTION Z FROM E+PZ FOR QUARK OR DIQUARK
C  SIMULATION OF U(Z) DISTRIBUTION FROM A.B.KAIDALOV
C         ITEP-116
C
      PARAMETER(ALFT=0.5,ARHO=0.5,APHI=0.,APSI=-2.)
      PARAMETER(AN=-0.5,ALA=-0.75,ALAC=-1.75)
      PARAMETER(AKSI=-1.0,AUSC=-2.0,AUCC=-2.0)
C
      ID1=IABS(IFL)
      ID2=IABS(IFLN)
      IF(MOD(ID2,100).EQ.0) GO TO 15
      GO TO(1,2,3,4),ID2
C  UU-TRAJECTORY
1     ZFRAGS=ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=(1.0-ZFRAGS)**(ALFT-ARHO)
      IF(RNDMD(-1).LE.YF) RETURN
      GO TO 1
C DD-TRAJECTORY
2     ZFRAGS=ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=(1.-ZFRAGS)**(ALFT-ARHO)
      IF(RNDMD(-1).LE.YF) RETURN
      GO TO 2
C SS-TRAJECTORY
3     ZFRAGS=ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=(1.-ZFRAGS)**(ALFT-APHI)
      IF(RNDMD(-1).LE.YF) RETURN
      GO TO 3
C CC-TRAJECTORY
4     ZFRAGS=ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=(1.-ZFRAGS)**(ALFT-APSI)
      IF(RNDMD(-1).LE.YF) RETURN
      GO TO 4
C
15    CALL FLAVOR(ID2,IFL2,IFL3,IFL1,ISPIN,INDEX)
*      IF(.NOT.(IFL2.EQ.1.AND.IFL3.EQ.1)) GO TO 16
*      IF(.NOT.(IFL2.EQ.1.AND.IFL3.EQ.2)) GO TO 7
*      IF(.NOT.(IFL2.EQ.1.AND.IFL3.EQ.3)) GO TO 18
*      IF(.NOT.(IFL2.EQ.1.AND.IFL3.EQ.4)) GO TO 19
*      IF(.NOT.(IFL2.EQ.2.AND.IFL3.EQ.2)) GO TO 20
*      IF(.NOT.(IFL2.EQ.2.AND.IFL3.EQ.3)) GO TO 21
*      IF(.NOT.(IFL2.EQ.2.AND.IFL3.EQ.4)) GO TO 22
*      IF(.NOT.(IFL2.EQ.3.AND.IFL3.EQ.3)) GO TO 23
*      IF(.NOT.(IFL2.EQ.3.AND.IFL3.EQ.4)) GO TO 24
*      IF(.NOT.(IFL2.EQ.4.AND.IFL3.EQ.4)) GO TO 25  ! 16.11.95 amelin
      IF(IFL2.EQ.1.AND.IFL3.EQ.1) GO TO 16
      IF(IFL2.EQ.1.AND.IFL3.EQ.2) GO TO 17
      IF(IFL2.EQ.1.AND.IFL3.EQ.3) GO TO 18
      IF(IFL2.EQ.1.AND.IFL3.EQ.4) GO TO 19
      IF(IFL2.EQ.2.AND.IFL3.EQ.2) GO TO 20
      IF(IFL2.EQ.2.AND.IFL3.EQ.3) GO TO 21
      IF(IFL2.EQ.2.AND.IFL3.EQ.4) GO TO 22
      IF(IFL2.EQ.3.AND.IFL3.EQ.3) GO TO 23
      IF(IFL2.EQ.3.AND.IFL3.EQ.4) GO TO 24
      IF(IFL2.EQ.4.AND.IFL3.EQ.4) GO TO 25

C UUUU-TRAJECTORY
16    ZFRAGS=ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=(1.-ZFRAGS)**(ALFT-(2.*AN-ARHO))
      IF(RNDMD(-1).LE.YF) RETURN
      GO TO 16
C UDUD-TRAJECTORY
17    ZFRAGS=ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=(1.-ZFRAGS)**(ALFT-(2.*AN-ARHO))
      IF(RNDMD(-1).LE.YF) RETURN
      GO TO 17
C USUS-TRAJECTORY
18    ZFRAGS=ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=(1.-ZFRAGS)**(ALFT-(2.*ALA-ARHO))
      IF(RNDMD(-1).LE.YF) RETURN
      GO TO 18
C UCUC-TRAJECTORY
19    ZFRAGS=ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=(1.-ZFRAGS)**(ALFT-(2.*ALAC-ARHO))
      IF(RNDMD(-1).LE.YF) RETURN
      GO TO 19
C DDDD-TRAJECTORY
20    ZFRAGS=ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=(1.-ZFRAGS)**(ALFT-(2.*AN-ARHO))
      IF(RNDMD(-1).LE.YF) RETURN
      GO TO 16
C DSDS-TRAJECTORY
21    ZFRAGS=ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=(1.-ZFRAGS)**(ALFT-(2.*ALA-ARHO))
      IF(RNDMD(-1).LE.YF) RETURN
      GO TO 21
C DCDC-TRAJECTORY
22    ZFRAGS=ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=(1.-ZFRAGS)**(ALFT-(2.*ALAC-ARHO))
      IF(RNDMD(-1).LE.YF) RETURN
      GO TO 22
C SSSS-TRAJECTORY
23    ZFRAGS=ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=(1.-ZFRAGS)**(ALFT-(2.*AKSI-ARHO))
      IF(RNDMD(-1).LE.YF) RETURN
      GO TO 23
C SCSC-TRAJECTORY
24    ZFRAGS=ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=(1.-ZFRAGS)**(ALFT-(2.*AUSC-ARHO))
      IF(RNDMD(-1).LE.YF) RETURN
      GO TO 24
C CCCC-BARYON
25    ZFRAGS=ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=(1.-ZFRAGS)**(ALFT-(2.*AUCC-ARHO))
      IF(RNDMD(-1).LE.YF) RETURN
      GO TO 25
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION ZFRAG0(IFL,IFLN,MESON,PT2,ZMIN,ZMAX)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 ZFRAG0,PT2,ZMIN,ZMAX
C  RETURN FRACTION Z FROM E+PZ FOR QUARK OR DIQUARK
C  SIMULATION OF U(Z) DISTRIBUTION FROM A.B.KAIDALOV
C         ITEP-116
C
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      LOGICAL MESON
      DATA BSLOPE/1.5/
C
      ID1=IABS(IFL)
      ID2=IABS(IFLN)
      IF(MOD(ID1,100).EQ.0) GO TO 400
      IF(MESON) GO TO 601
      IF(MOD(ID2,100).EQ.0) GO TO 600
C     IFL QUARK FRAGMENTS INTO MESON:
      IF(ID1.EQ.3) GO TO 200
      IF(ID2.EQ.3) GO TO 100
      IF(ID1.GE.4.OR.ID2.GE.4) GO TO 700
C     NONSTRANGE QUARK AND NONSTRANGE ANTIQUARK:
C       U(Z)=(1.-Z)**(1.6*PT2-0.5)
      ALFT=1.6*PT2
      IF(ALFT.GE.2.5) ALFT=2.5
50    ZFRAG0=1.-(RNDMD(-1)*(DSQRT(1.-ZMAX)-DSQRT(1.-ZMIN))+
     *DSQRT(1.-ZMIN))**2
         YF=(1.-ZFRAG0)**(ALFT-0.5)
         YP=1./DSQRT(1.-ZFRAG0)
       IF(YP*RNDMD(-1).LE.YF) RETURN
         GO TO 50
C
C     NONSTRANGE QUARK AND STRANGE ANTIQUARK:
C       U(Z)=(1.-Z)**ALFT
100   ALFT=1.6*PT2 - 0.45
      IF(ALFT.GE.2.5) ALFT=2.5
      ZFRAG0=ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=(1.-ZFRAG0)**ALFT
       IF(RNDMD(-1).LE.YF) RETURN
        GO TO 100
 200  IF(ID2.EQ.3) GO TO 300
C     STRANGE QUARK AND NONSTRANGE ANTIQUARK:
C       U(Z)=(1.-Z)**(1.6*PT2-0.5)
      ALFT=1.6*PT2
      IF(ALFT.GE.2.5) ALFT=2.5
51    ZFRAG0=1.-(RNDMD(-1)*(DSQRT(1.-ZMAX)-DSQRT(1.-ZMIN))+
     *DSQRT(1.-ZMIN))**2
         YF=(1.-ZFRAG0)**(ALFT-0.5)
         YP=1./DSQRT(1.-ZFRAG0)
       IF(YP*RNDMD(-1).LE.YF) RETURN
         GO TO 51
C
C        STRANGE QUARK AND STRANGE ANTIQUARK:
C       U(Z)=(1.-Z)**ALFT
300   ALFT=1.6*PT2 - 0.45
      IF(ALFT.GE.2.5) ALFT=2.5
      ZFRAG0=ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=(1.-ZFRAG0)**ALFT
       IF(RNDMD(-1).LE.YF) RETURN
        GO TO 300
C     IFL DIQUARK FRAGMENTS INTO BARION%
 400  IF(ID2.EQ.3) GO TO 510
C       DIQUARK AND NONSTRANGE QUARK:
C        U(Z)=Z**1.5
610   ZFRAG0=ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=ZFRAG0**1.5
      IF(RNDMD(-1).LE.YF) RETURN
        GO TO 610
C       DIQUARK AND    STRANGE QUARK:
C        U(Z)=Z**2*DSQRT(1.-Z)
510   ZFRAG0=ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=ZFRAG0**2*DSQRT(1.-ZFRAG0)
      IF(RNDMD(-1).LE.YF) RETURN
        GO TO 510
C     IFL   QUARK FRAGMENTS INTO BARION (IN CASE OF DIQUARK SPLITTING)
C        U(Z)=(1.-Z)**0
 601  ZFRAG0= ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
        RETURN
C     IFL   QUARK FRAGMENTS INTO BARION!
C        U(Z)=(1.-Z)**2
 600  ZFRAG0= ZMIN+RNDMD(-1)*(ZMAX-ZMIN)
      YF=3.*(1.-ZFRAG0)**2
      IF(3.*RNDMD(-1).LE.YF) RETURN
        GO TO 600
C      HEAVY QUARK OR ANTIQUARK
 700   GAMMA=BSLOPE*PT2
       ZM=GAMMA
       IF(ZM.LE.ZMIN) ZM=ZMIN
       IF(ZM.GE.ZMAX) ZM=ZMAX
        UMAX=DEXP(-GAMMA/ZM)/ZM
 710    ZFRAG0=RNDMD(-1)*(ZMAX-ZMIN)+ZMIN
        UF=DEXP(-GAMMA/ZFRAG0)/ZFRAG0
        IF(RNDMD(-1)*UMAX.GT.UF) GO TO 710
        RETURN
        END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE ROTR(CT,ST,CFI,SFI,PX1,PX2,BACK)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CT,ST,CFI,SFI,PX1,PX2
C  ROTATE OF VECTOR PX1
      DIMENSION ROT(3,3),PX1(3),PX2(3)
      LOGICAL BACK
      ROT(1,1)=CT*CFI
      ROT(1,2)=-SFI
      ROT(1,3)=ST*CFI
      ROT(2,1)=CT*SFI
      ROT(2,2)=CFI
      ROT(2,3)=ST*SFI
      ROT(3,1)=-ST
      ROT(3,2)=0.
      ROT(3,3)=CT
      IF(BACK) GO TO 2
      DO 1 I=1,3
 1    PX2(I)=ROT(I,1)*PX1(1)+ROT(I,2)*PX1(2)+ROT(I,3)*PX1(3)
      RETURN
 2    DO 3 I=1,3
 3    PX2(I)=ROT(1,I)*PX1(1)+ROT(2,I)*PX1(2)+ROT(3,I)*PX1(3)
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE LORLC(V,PX,E,BACK)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 V,PX,E
C  LORENTZ TRANSFORMATION OF PX MOMENTUM COMPONENTS
      DIMENSION V(3),PX(3)
      LOGICAL BACK
      REAL L
      L=1.
      IF(BACK) L=-1.
      VV=V(1)*V(1)+V(2)*V(2)+V(3)*V(3)
      GA=1.D0/DSQRT(DABS(1.D0-VV))
      BEP=SP(V,PX)
      GABEP=GA*(GA*BEP/(1.+GA)-L*E)
      DO 1 I=1,3
 1    PX(I)=PX(I)+GABEP*V(I)
        RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE FISOB(IK01,P01,AM01,IK02,P02,AM02,SIGEL,
     *IKD,PXD,PYD,PZD,DMAS,ISOB)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 P01,AM01,P02,AM02,SIGEL,
     *PXD,PYD,PZD,DMAS
C  FORM ISOBAR FROM PION AND NUCLEON
      DIMENSION P01(3),P02(3),P1(3),P2(3)
      ISOB=0
      E1=DSQRT( SP(P01,P01)+AM01**2)
      E2=DSQRT( SP(P02,P02)+AM02**2)
      S=AM01**2+AM02**2+2.*E1*E2-2.* SP(P01,P02)
      PXC=DSQRT(ALAMB(S,AM01**2,AM02**2))/(2.*DSQRT(S))
      PT=5.067*PXC
      DM=DSQRT(S)
      IK1=IK01
      AM1=AM01
      IK2=IK02
      AM2=AM02
      DO 1 J=1,3
      P1(J)=P01(J)
      P2(J)=P02(J)
 1    CONTINUE
      IF(IBLE(IK2).NE.0) GO TO 10
      IK1=IK02
      AM1=AM02
      IK2=IK01
      AM2=AM01
      DO 2 J=1,3
      P1(J)=P02(J)
      P2(J)=P01(J)
 2    CONTINUE
 10   IQ1=IQLE(IK1)
      IQ2=IQLE(IK2)
      IKS=IQ1+IQ2+2
      GO TO (3,4,5,6),IKS
 3    AK=1.
      IKD=47
      GO TO 7
 4    AK=0.3334
      IF(IQ1.EQ.0) AK=0.6667
      IKD=48
      GO TO 7
 5    AK=0.3334
      IF(IQ1.EQ.0) AK=0.6667
      IKD=46
      GO TO 7
 6    AK=1.
      IKD=45
C  COMPUTE RESONANCE CROSS SECTION
 7    SIGR=SGR(DM,SIGEL,PT)
C     PR=AK*SIGR/SIGEL
      PR=SIGR/SIGEL
      IF(RNDMD(-1).GE.PR) RETURN
C  ISOBAR PARAMETERS
      PXD=P1(1)+P2(1)
      PYD=P1(2)+P2(2)
      PZD=P1(3)+P2(3)
      DMAS=DM
      ISOB=1
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      FUNCTION SGR(DM,SIGEL,PT)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 SGR,DM,SIGEL,PT
C  CALCULATION OF RESONANCE CROSS SECTION
      DM0=1.23000000
      GM=0.12700000
      DMM0=(DM**2-DM0**2)**2
      DMG=(DM0*GM)**2
      PT2=PT**2
      ANORM=SIGEL*PT2
      SGR=ANORM*DMG/(PT2*(DMG+DMM0))
C     SGR=(251.32740000*DMG)/((PT**2)*(DMG+DMM0))
      RETURN
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
       SUBROUTINE XQUARK(IAB,XMIN,XMAX,ALFA,BETA,IB1)
      IMPLICIT REAL*8 (A-H,O-Z)
       REAL*8 XMIN,XMAX,ALFA,BETA
C
C   COMPUTE PART OF HADRON MOMENTUM FOR PARTONS
C
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      COMMON/COMFLA/MNASEA(12),MNBSEA(12),IFLAS(12),IFLBS(12)
     * ,NUAVAL,
     * NUBVAL,IFLQA1,IFLQB1,IFLQA2,IFLQB2,IFAQQ,IFBQQ
       COMMON/COMVA/ XAVAL1,XAVAL2,XAQQ,XASEA1(12),
     * XASEA2(12),NPOMA
       COMMON/COMVB/ XBVAL1,XBVAL2,XBQQ,XBSEA1(12),
     * XBSEA2(12),NPOMB
      COMMON/COMDIF/ NDIFA,NDIFB
      LOGICAL DIQAN
      COMMON/COMANN/ DIQAN
       IF(IAB.EQ.1) GO TO 10
       NPA=NPOMA
      IF(NPA.EQ.1.AND.NDIFA.EQ.1) GO TO 500
  2    JS=1
       NPAJ=2*NPA-1-JS
       BETA1=BETA
       IF(IABS(IFLQA1).GT.1) BETA1=BETA+1.
C ONLY FOR STRANGE QUARK
       IF(IABS(IFAQQ).EQ.3)  BETA1=0.
       ALFA1=-ALFA
       BETAV=FLOAT(NPAJ)*(1.-ALFA)+BETA1
  3    RNDAV=RNDMD(-1)**(1./(1.+ALFA1))
       RNDBV=RNDMD(-1)**(1./(1.+BETAV))
       RNDV=RNDAV+RNDBV
       IF(RNDV.GE.1.) GO TO 3
       XAVAL1=RNDAV/RNDV
       IF(XAVAL1.LT.XMIN.OR.XAVAL1.GT.XMAX) GO TO 3
       XAQQ=XAVAL1
      XAVAL2=0.
      GO TO 501
500   CALL XSDIS(XAVAL1,XMIN,XMAX)
      CALL XSDIS(XAVAL2,XMIN,XMAX)
      XAQQ=XAVAL1+XAVAL2
      IF(XAQQ.GE.1.0) GO TO 500
501    IF(NPOMA.EQ.1) GO TO 1
       NSA=NPOMA-1
      IF(NSA.GE.13) WRITE(ITLIS,999) NSA
999   FORMAT(/10X,'NUMBER OF COLLISIONS TOO HIGH,NSA=',I5)
      IF(XMIN*FLOAT(2*NSA+1).LT.1.0) GO TO 20
      WRITE(ITLIS,995) XMIN,NSA
995   FORMAT(/10X,'...STOP IN XQUARK..XMIN=',E10.4,'NSA=',I3)
      RETURN
20    CONTINUE
       DO 6 JS1=1,NSA
       JS=JS+1
       NPAJ=2*NPA-1-JS
       BETAV=FLOAT(NPAJ)*(1.-ALFA)+BETA1
       XB=1.-XAQQ
       IF(XB.LE.XMIN) GO TO 2
  4    RNDAV=RNDMD(-1)**(1./(1.+ALFA1))
       RNDBV=RNDMD(-1)**(1./(1.+BETAV))
       RNDV=RNDAV+RNDBV
       IF(RNDV.GE.1.) GO TO 4
       XASEA1(JS1)=RNDAV/RNDV*(XB-XMIN)+XMIN
       IF(XASEA1(JS1).GT.XMAX) GO TO 4
       XAQQ=XAQQ+XASEA1(JS1)
       JS=JS+1
       NPAJ=2*NPA-1-JS
       BETAV=FLOAT(NPAJ)*(1.-ALFA)+BETA1
       XB=1.-XAQQ
       IF(XB.LE.XMIN) GO TO 2
  5    RNDAV=RNDMD(-1)**(1./(1.+ALFA1))
       RNDBV=RNDMD(-1)**(1./(1.+BETAV))
       RNDV=RNDAV+RNDBV
       IF(RNDV.GE.1.) GO TO 5
       XASEA2(JS1)=RNDAV/RNDV*(XB-XMIN)+XMIN
       IF(XASEA2(JS1).GT.XMAX) GO TO 5
       XAQQ=XAQQ+XASEA2(JS1)
  6    CONTINUE
  1    CONTINUE
       IF(IB1.EQ.0.AND.NDIFA.EQ.0) GO TO 502
      IF(.NOT.DIQAN) RETURN
      IFL1=IFLQA2
      IFL2=IFAQQ
      CALL X2DIST(X1,X2,IFL1,IFL2)
      XAVAL2=(1.-XAQQ)*X1
      XAQQ  =(1.-XAQQ)*X2
      IF(XAVAL2.LT.XMIN.OR.XAQQ.LT.XMIN) GO TO 2
       RETURN
 502   IF(IABS(IFAQQ).EQ.3) RETURN
       IF(RNDMD(-1).GE.0.5) RETURN
       XSWAP=XAVAL1
       XAVAL1=1.-XAQQ
      XAQQ=1.-XSWAP
       RETURN
  10   CONTINUE
       NPB=NPOMB
      IF(NPB.EQ.1.AND.NDIFB.EQ.1) GO TO 600
  12   JS=1
       NPBJ=2*NPB-1-JS
       BETA1=BETA
       ALFA1=-ALFA
       IF(IABS(IFLQB1).GT.1) BETA1=BETA+1
       BETAV=FLOAT(NPBJ)*(1.-ALFA)+BETA1
  13   RNDAV=RNDMD(-1)**(1./(1.+ALFA1))
       RNDBV=RNDMD(-1)**(1./(1.+BETAV))
       RNDV=RNDAV+RNDBV
       IF(RNDV.GE.1.) GO TO 13
       XBVAL1=RNDAV/RNDV
       IF(XBVAL1.LT.XMIN.OR.XBVAL1.GT.XMAX) GO TO 13
       XBQQ=XBVAL1
      XBVAL2=0.
      GO TO 601
600   CALL XSDIS(XBVAL1,XMIN,XMAX)
      CALL XSDIS(XBVAL2,XMIN,XMAX)
      XBQQ=XBVAL1+XBVAL2
      IF(XBQQ.GE.1.0) GO TO 600
601    IF(NPOMB.EQ.1) GO TO 11
       NSB=NPOMB-1
      IF(NSB.GE.13) WRITE(ITLIS,998) NSB
998   FORMAT(/10X,'NUMBER OF COLLISIONS TOO HIGH,NSB=',I5)
      IF(XMIN*FLOAT(2*NSB+1).LT.1.0) GO TO 30
      WRITE(ITLIS,996) XMIN,NSB
996   FORMAT(/10X,'..STOP IN XQUARK...XMIN=',E10.4,'NSB=',I3)
      RETURN
30    CONTINUE
       DO 16 JS2=1,NSB
       JS=JS+1
       NPBJ=2*NPB-1-JS
       BETAV=FLOAT(NPBJ)*(1.-ALFA)+BETA1
       XB=1.-XBQQ
       IF(XB.LE.XMIN) GO TO 12
  14   RNDAV=RNDMD(-1)**(1./(1.+ALFA1))
       RNDBV=RNDMD(-1)**(1./(1.+BETAV))
       RNDV=RNDAV+RNDBV
       IF(RNDV.GE.1.) GO TO 14
       XBSEA1(JS2)=RNDAV/RNDV*(XB-XMIN)+XMIN
       IF(XBSEA1(JS2).GT.XMAX) GO TO 14
       XBQQ=XBQQ+XBSEA1(JS2)
       JS=JS+1
       NPBJ=2*NPB-1-JS
       BETAV=FLOAT(NPBJ)*(1.-ALFA)+BETA1
       XB=1.-XBQQ
       IF(XB.LE.XMIN) GO TO 12
  15   RNDAV=RNDMD(-1)**(1./(1.+ALFA1))
       RNDBV=RNDMD(-1)**(1./(1.+BETAV))
       RNDV=RNDAV+RNDBV
       IF(RNDV.GE.1.) GO TO 15
       XBSEA2(JS2)=RNDAV/RNDV*(XB-XMIN)+XMIN
       IF(XBSEA2(JS2).GT.XMAX) GO TO 15
       XBQQ=XBQQ+XBSEA2(JS2)
  16   CONTINUE
  11   CONTINUE
      IF(.NOT.DIQAN) RETURN
      IFL1=IFLQB2
      IFL2=IFBQQ
      CALL X2DIST(X1,X2,IFL1,IFL2)
      XBVAL2=(1.-XBQQ)*X1
      XBQQ  =(1.-XBQQ)*X2
      IF(XBVAL2.LT.XMIN.OR.XBQQ.LT.XMIN) GO TO 10
       RETURN
       END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
       SUBROUTINE PTQUAR(KEY)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C    COMPUTE PARTON TRANSFERSE MOMENTA
C
C        TRANSFERSE MOMENTUM OF EACH DIQUARK IS EQUAL SUM
C        MOMENTA VALENCE AND SEA QUARKS
       COMMON/COMVA/ XAVAL1,XAVAL2,XAQQ,XASEA1(12),
     * XASEA2(12),NPOMA
       COMMON/COMVB/ XBVAL1,XBVAL2,XBQQ,XBSEA1(12),
     * XBSEA2(12),NPOMB
       COMMON/COMPXA/ PXAV1,PXAV2,PXAQQ,
     *PXAS1(12),PXAS2(12)
       COMMON/COMPYA/ PYAV1,PYAV2,PYAQQ,
     *PYAS1(12),PYAS2(12)
       COMMON/COMPXB/ PXBV1,PXBV2,PXBQQ,
     *PXBS1(12),PXBS2(12)
       COMMON/COMPYB/ PYBV1,PYBV2,PYBQQ,
     *PYBS1(12),PYBS2(12)
      COMMON/PRIMAR/SCM,HALFE,ECM,NJET,IDIN(2),NEVENT,NTRIES
      COMMON/CONST/PI,SQRT2,ALF,GF,UNITS
      COMMON/COMIND/ PUD,SIGMA,ALFA,BETA
      COMMON/COMQMA/ AMQUA1,AMQUA2,AMQQA,
     *AMQAS1(12),AMQAS2(12)
      COMMON/COMQMB/ AMQUB1,AMQUB2,AMQQB,
     *AMQBS1(12),AMQBS2(12)
      COMMON/PRIMP0/ P0
      COMMON/COMDIF/ NDIFA,NDIFB
      COMMON/COAUX1/ AMA2,XI2(20),INUM
      COMMON/COAUX2/ X12,X22,X32,PT12,PT22,PT32
      COMMON/COMASS/ AM1,AM2
      LOGICAL DIQAN
      COMMON/COMANN/ DIQAN
      EXTERNAL FMQ2
      EXTERNAL FP02
      DATA AM/0./,BM/15./,MAXFUN/200/,EPSI/0.001/
      DATA PAM2/0./
      DATA SIGMDQ/0.45/
      P02=P0**2
      AMA=AM1
      AMB=AM2
26    IF(KEY.EQ.1) GO TO 20
      AMA2=AMA**2
      XQQ=1.-XAQQ
       NPA=NPOMA
      IF(NPA.EQ.1.AND.NDIFA.EQ.1) GO TO 500
      IF(.NOT.DIQAN) GO TO 27
      XQQ=XAQQ
27     NS=NPA-1
      PTQXS=0.
      PTQYS=0.
      PTQXS0=0.
      PTQYS0=0.
      INUM=0
      IF(NPA.EQ.1) GO TO 5
      DO 4 JS=1,NS
      CALL GAUSPT(PTS1,SIGMA)
      PHI1=2.*PI*RNDMD(-1)
      CALL GAUSPT(PTS2,SIGMA)
      PHI2=2.*PI*RNDMD(-1)
       PXAS1(JS)=PTS1*DCOS(PHI1)
       PYAS1(JS)=PTS1*DSIN(PHI1)
       PXAS2(JS)=PTS2*DCOS(PHI2)
       PYAS2(JS)=PTS2*DSIN(PHI2)
      PTQXS=PTQXS+PXAS1(JS)+PXAS2(JS)
      PTQYS=PTQYS+PYAS1(JS)+PYAS2(JS)
      INUM=INUM+2
      XI2(INUM-1)=XASEA1(JS)**2
      XI2(INUM)=XASEA2(JS)**2
4     CONTINUE
      PTQXS0=PTQXS
      PTQYS0=PTQYS
5     CALL GAUSPT(PTV1,SIGMA)
      PHI1=2.*PI*RNDMD(-1)
      PXAV1=PTV1*DCOS(PHI1)
      PYAV1=PTV1*DSIN(PHI1)
      PXAV2=0.
      PYAV2=0.
      IF(.NOT.DIQAN) GO TO 15
      CALL GAUSPT(PTV2,SIGMDQ)
      PHI2=2.*PI*RNDMD(-1)
      PXAV2=PTV2*DCOS(PHI2)
      PYAV2=PTV2*DSIN(PHI2)
 15   PTQXS=PTQXS0
      PTQYS=PTQYS0
      PTQXS=PTQXS+PXAV1+PXAV2
      PTQYS=PTQYS+PYAV1+PYAV2
      PXAQQ=-PTQXS
      PYAQQ=-PTQYS
      IF(DIQAN) GO TO 28
      INUM=INUM+2
      XI2(INUM-1)=XAVAL1**2
      XI2(INUM)=XQQ**2
      GO TO 29
 28   INUM=INUM+3
      XI2(INUM-2)=XAVAL1**2
      XI2(INUM-1)=XAVAL2**2
      XI2(INUM  )=XQQ**2
29    FA=FMQ2(AM)
      FB=FMQ2(BM)
      IF(FA*FB.GE.0.) GO TO 26
      CALL RZERO(AM,BM,AMQ2,RES,EPSI,MAXFUN,FMQ2)
      IF(RES.LT.0.) GO TO 26
      IF(NPA.EQ.1) GO TO 3
      DO 2 JS=1,NS
      AMQAS1(JS)=AMQ2-PTS1**2
      AMQAS2(JS)=AMQ2-PTS2**2
2     CONTINUE
3     AMQUA1=AMQ2-PTV1**2
      AMQUA2=0.
      AMQQA=AMQ2-PXAQQ**2-PYAQQ**2
      IF(.NOT.DIQAN) GO TO 1
      AMQUA2=AMQ2-PTV2**2
      GO TO 1
500   CONTINUE
      PBM2=P02+25.
      CALL GAUSPT(PTV1,SIGMA)
      PHI1=2.*PI*RNDMD(-1)
      PXAV1=PTV1*DCOS(PHI1)
      PYAV1=PTV1*DSIN(PHI1)
      CALL GAUSPT(PTV2,SIGMA)
      PHI2=2.*PI*RNDMD(-1)
      PXAV2=PTV2*DCOS(PHI2)
      PYAV2=PTV2*DSIN(PHI2)
      PXAQQ=-(PXAV1+PXAV2)
      PYAQQ=-(PYAV1+PYAV2)
      X12=XAVAL1**2
      X22=XAVAL2**2
      X32=XQQ**2
      PT12=PTV1**2
      PT22=PTV2**2
      PT32=PXAQQ**2+PYAQQ**2
      ENER=DSQRT(P02+AMA2)
      ESUM=DSQRT(AMA2+PT32)+PTV1+PTV2
      IF(ESUM.GE.ENER) GO TO 500
      FA=FP02(PAM2)
      FB=FP02(PBM2)
      IF(FA*FB.GE.0.) GO TO 500
      CALL RZERO(PAM2,PBM2,PNEW2,RES,EPSI,MAXFUN,FP02)
      IF(RES.LT.0.) GO TO 500
      AMQUA1=0.
      AMQUA2=0.
      AMQQA=AMA**2
      PNR=DSQRT(PNEW2)/P0
      XAVAL1=XAVAL1*PNR
      XAVAL2=XAVAL2*PNR
      XAQQ=XAVAL1+XAVAL2
1     CONTINUE
      RETURN
  20   CONTINUE
      AMA2=AMB**2
      INUM=0
       XQQ=1.-XBQQ
       NPB=NPOMB
      IF(NPB.EQ.1.AND.NDIFB.EQ.1) GO TO 600
      IF(.NOT.DIQAN) GO TO 37
      XQQ=XBQQ
 37    NS=NPB-1
      PTQXS=0.
      PTQYS=0.
      PTQXS0=0.
      PTQYS0=0.
      IF(NPB.EQ.1) GO TO 7
       DO 6 JS=1,NS
      INUM=INUM+2
      XI2(INUM-1)=XBSEA1(JS)**2
      XI2(INUM)=XBSEA2(JS)**2
      CALL GAUSPT(PTS1,SIGMA)
      PHI1=2.*PI*RNDMD(-1)
      CALL GAUSPT(PTS2,SIGMA)
      PHI2=2.*PI*RNDMD(-1)
       PXBS1(JS)=PTS1*DCOS(PHI1)
       PYBS1(JS)=PTS1*DSIN(PHI1)
       PXBS2(JS)=PTS2*DCOS(PHI2)
       PYBS2(JS)=PTS2*DSIN(PHI2)
       PTQXS=PTQXS+PXBS1(JS)+PXBS2(JS)
      PTQYS=PTQYS+PYBS1(JS)+PYBS2(JS)
 6    CONTINUE
      PTQXS0=PTQXS
      PTQYS0=PTQYS
 7    CALL GAUSPT(PTV1,SIGMA)
      PHI1=2.*PI*RNDMD(-1)
      PXBV1=PTV1*DCOS(PHI1)
      PYBV1=PTV1*DSIN(PHI1)
      PXBV2=0.
      PYBV2=0.
      IF(.NOT.DIQAN) GO TO 35
      CALL GAUSPT(PTV2,SIGMDQ)
      PHI2=2.*PI*RNDMD(-1)
      PXBV2=PTV2*DCOS(PHI2)
      PYBV2=PTV2*DSIN(PHI2)
 35   PTQXS=PTQXS0
      PTQYS=PTQYS0
      PTQXS=PTQXS+PXBV1+PXBV2
      PTQYS=PTQYS+PYBV1+PYBV2
      PXBQQ=-PTQXS
      PYBQQ=-PTQYS
      IF(DIQAN) GO TO 38
      INUM=INUM+2
      XI2(INUM-1)=XBVAL1**2
      XI2(INUM)=XQQ**2
      GO TO 39
 38   INUM=INUM+3
      XI2(INUM-2)=XBVAL1**2
      XI2(INUM-1)=XBVAL2**2
      XI2(INUM  )=XQQ**2
39    FA=FMQ2(AM)
      FB=FMQ2(BM)
      IF(FA*FB.GE.0.) GO TO 20
      CALL RZERO(AM,BM,AMQ2,RES,EPSI,MAXFUN,FMQ2)
      IF(RES.LT.0.) GO TO 20
      IF(NPB.EQ.1) GO TO 13
      DO 12 JS=1,NS
      AMQBS1(JS)=AMQ2-PTS1**2
      AMQBS2(JS)=AMQ2-PTS2**2
12    CONTINUE
13    AMQUB1=AMQ2-PTV1**2
      AMQUB2=0.
      AMQQB=AMQ2-PXBQQ**2-PYBQQ**2
      IF(.NOT.DIQAN) GO TO 9
      AMQUB2=AMQ2-PTV2**2
      GO TO 9
600   CONTINUE
      CALL GAUSPT(PTV1,SIGMA)
      PHI1=2.*PI*RNDMD(-1)
      PXBV1=PTV1*DCOS(PHI1)
      PYBV1=PTV1*DSIN(PHI1)
      CALL GAUSPT(PTV2,SIGMA)
      PHI2=2.*PI*RNDMD(-1)
      PXBV2=PTV2*DCOS(PHI2)
      PYBV2=PTV2*DSIN(PHI2)
      PXBQQ=-(PXBV1+PXBV2)
      PYBQQ=-(PYBV1+PYBV2)
      AMQUB1=0.
      AMQUB2=0.
      AMQQB=AMB**2
      X12=XBVAL1**2
      X22=XBVAL2**2
      X32=XQQ**2
      PT12=PTV1**2
      PT22=PTV2**2
      PT32=PXBQQ**2+PYBQQ**2
      PBM2=P02+25.
      ENER=DSQRT(P02+AMA2)
      ESUM=DSQRT(AMA2+PT32)+PTV1+PTV2
      IF(ESUM.GE.ENER) GO TO 600
      FA=FP02(PAM2)
      FB=FP02(PBM2)
      IF(FA*FB.GE.0.) GO TO 600
      CALL RZERO(PAM2,PBM2,PNEW2,RES,EPSI,MAXFUN,FP02)
      IF(RES.LT.0.) GO TO 600
      PNR=DSQRT(PNEW2)/P0
      XBVAL1=XBVAL1*PNR
      XBVAL2=XBVAL2*PNR
      XBQQ=XBVAL1+XBVAL2
9     CONTINUE
      RETURN
       END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      SUBROUTINE PRTEVT(IPRT)
      IMPLICIT REAL*8 (A-H,O-Z)
C     REAL*8 LABE
      CHARACTER*8 LABE
C
C          IF IPRT IS SELECTED BY NEVPRT AND NJUMP.
C
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      COMMON/PRINTS/IPRINT
      LOGICAL IPRINT
      LOGICAL NODCAY,NOETA,NOPI0,NONUNU,NOEVOL,NOHADR,NOKA0
      COMMON/NODCAY/NODCAY,NOETA,NOPI0,NONUNU,NOEVOL,NOHADR,NOKA0
      COMMON/IDRUN/IDVER,IDG(2),IEVT
      COMMON/PRIMAR/SCM,HALFE,ECM,NJET,IDIN(2),NEVENT,NTRIES
      COMMON/HADSIG/SIGS(100),SIGEVT,NSIGS,INOUT(2,100)
      COMMON/PARTCL/PPTCL(9,499),NPTCL,IORIG(499),IDENT(499)
     *,IDCAY(499)
      COMMON/COMLID/PLIDER(499)
      COMMON/LSTPRT/LSTPRT
      COMMON/PRTOUT/NEVPRT,NJUMP
      LOGICAL NOTRE
      COMMON/NOTRE/ NOTRE
      COMMON/COMTRE/ XMI,PTI,PTF,PTMAX,NSTART
      COMMON/P0INIT/ P0INIT
      COMMON/PLINIT/ PLINIT
      COMMON/COMFR/ ICMS
      COMMON/CONST/PI,SQRT2,ALF,GF,UNITS
      COMMON/COMCOL/ NAC(100,4),NBC(100,4),NCOL
      LOGICAL GH1H2
      DIMENSION IK0(499),AM0(499),PX0(499),PY0(499),PZ0(499)
      COMMON/H1H2/ GH1H2(11)
      NP0=0
C
      IPOINT=-1
      IF(IPRT.GT.1) IPRINT=.FALSE.
C
      IOPAK=100
      JNUMP=1000
C     IF(IPRT.GT.NJUMP*NEVPRT) RETURN
C     IF(IPRT.GT.1.AND.MOD(IPRT,NJUMP).NE.0) RETURN
C     IF(IEVT.EQ.LSTPRT) RETURN
      IF(NPTCL.EQ.0) GO TO 300
      IF(.NOT.NOTRE) GO TO 370
      WRITE(ITLIS,85) SIGEVT
85    FORMAT(/15X,30HREACTION CROSS SECTION SIGEVT=,E10.4,2HMB)
      IF(GH1H2(7)) WRITE(ITLIS,87) NCOL
87    FORMAT(/20X,41HNUMBER OF COLLISIONS INSIDE NUCLEON NCOL=,I5)
      WRITE(ITLIS,80)
80    FORMAT(//10X,26HPRODUCED HADRON PARAMETERS//
     *6X,1HI,8X,2HPX,8X,2HPY,8X,2HPZ,8X,2HP0,6X,4HMASS,2X,5HIDENT
     *,7X,5HIORIG,5X,5HIDCAY,4X,5HLABEL)
370   NDEC1=1
      NCOR1=1
      NCOR2=NPTCL
      MULT=0
      SUM1=0.
      SUM2=0.
      SUM3=0.
      SUM4=0.
365   NDEC2=NPTCL
      DO 360 I=NDEC1,NDEC2
      IF(NODCAY) GO TO 361
      CALL DECAY(I,IPOINT)
361   CALL LABEL(LABE,IDENT(I))
      IF(IPOINT.GE.0) GO TO 360
      if(dabs(CHARGE(IDENT(I))).gt.1.e-6.and.IDCAY(I).EQ.0)
     &                     MULT=MULT+1
      SUM1=SUM1+PPTCL(1,I)
      SUM2=SUM2+PPTCL(2,I)
      SUM3=SUM3+PPTCL(3,I)
      SUM4=SUM4+PPTCL(4,I)
360   IF(NOTRE) WRITE(ITLIS,81) I,(PPTCL(K,I),K=1,5),
     *IDENT(I),IORIG(I),IDCAY(I),LABE
81    FORMAT(1X,I6,5F10.2,I7,I12,I10,4X,A5)
      IF(NPTCL.EQ.NDEC2) GO TO 803
      NDEC1=NDEC2+1
      GO TO 365
803   IF(NOTRE) GO TO 804
C_____________________________
      DO 805 I=1,NPTCL
      IF(IDCAY(I).NE.0) GO TO 805
      NP0=NP0+1
      RNDR=RNDMD(-1)
      IF((IDENT(I).EQ.20.OR.IDENT(I).EQ.-20).AND.RNDR.GT.0.5)
     *IDENT(I)=230
      IF((IDENT(I).EQ.20.OR.IDENT(I).EQ.-20).AND.RNDR.LE.0.5)
     *IDENT(I)=-230
      IK0(NP0)=IDENT(I)
      AM0(NP0)=PPTCL(5,I)
      PX0(NP0)=PPTCL(1,I)
      PY0(NP0)=PPTCL(2,I)
      PZ0(NP0)=PPTCL(3,I)
805   CONTINUE
C______________________________
804   IF(NOTRE) WRITE(ITLIS,801) SUM1,SUM2,SUM3,SUM4,MULT
801   FORMAT(/10X,6HSUMPX=,E10.4,7H SUMPY=,E10.4,7H SUMPZ=,E10.4,
     *8H SUMS0 =,E10.4,8H  MULT =,I8)
      IF(NOTRE) WRITE(ITLIS,90)
90    FORMAT(//10X,27HPRODUCED HADRON COORDINATES//
     *6X,1HI,8X,2HRX,8X,2HRY,8X,2HRZ,6X,4HTIME,6X,5HIDENT
     *,7X,5HLIDER,5X,5HIDCAY,4X,5HLABEL)
      DO 460 I=NCOR1,NCOR2
      CALL LABEL(LABE,IDENT(I))
460   IF(NOTRE) WRITE(ITLIS,91) I,
     *(PPTCL(K,I),K=6,9),IDENT(I),PLIDER(I),IDCAY(I),LABE
91    FORMAT(1X,I6,4(1X,E10.4),I7,F12.3,I10,4X,A5)
      IF(NOTRE) WRITE(ITLIS,20) NOTRE,NOETA,NODCAY,NOKA0
 20   FORMAT(5X,' NOTRE=',L5,' NOETA=',L5,' NODCAY=',L5,' NOKA0=',L5)
      IF(NOTRE) GO TO 300
      P0HIS=P0INIT
      IF(ICMS.EQ.0) P0HIS=PLINIT
C     CALL HISACT(P0HIS,XMI,PTI,PTF,PTMAX,NSTART,
C    *IK0,AM0,PX0,PY0,PZ0,NP0)
300   RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE PARCRO(ITOT,IK,IKS,IK01,IK02,PX01,PY01,
     *PZ01,AM01,PX02,PY02,PZ02,AM02)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 PX01,PY01,
     *PZ01,AM01,PX02,PY02,PZ02,AM02
C
C  CALCULATION OF THE DIAGRAM CROSS SECTION
C  ATTENTION: IK2 - IS BARYON ONLY
C  ITOT=0  COMPUTE ALL POSSIBLE DIAGRAMS
C
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      COMMON/P0LAB1/P0,DSM,TKIN
      COMMON/SIGDIA/ CROSD(5),DST
      COMMON/PRINTS/IPRINT
      LOGICAL IPRINT
      DIMENSION IK(5)
      IB1=IB(IK01)
      IB2=IB(IK02)
      IF(IB1.NE.0.OR.IB2.NE.0) GO TO 1
      WRITE(ITLIS, 30) IK01,IK02
 30   FORMAT(10X,'YOU CAN HADRON-NUCLEON CROSS SECTION TREAT'/
     *,' COLLIDING PARTICLES ARE ',I6,' AND ',I6)
      RETURN
 1    IF((IK01.EQ.1120.OR.IK01.EQ.1220) .AND.
     *    IK02.NE.1120.AND.IK02.NE.1220) GO TO 13
      IF(IB1.GT.0.AND.IB2.LE.0.OR.(IB1.LT.0.AND.IB2.EQ.0))GOTO13
C     IF INCIDENT PARTICLE IS MESON OR
C     IF TARGET PARTICLE IS NUCLEON OR
C     IF INCIDENT AND TARGET PARTICLES ARE NOT NUCLEONS
C
      IK1=IK01
      PX1=PX01
      PY1=PY01
      PZ1=PZ01
      AM1=AM01
      IK2=IK02
      PX2=PX02
      PY2=PY02
      PZ2=PZ02
      AM2=AM02
      GO TO 20
C      ELSE  EXCHANGE 1->2  2->1
13    IK1=IK02
      PX1=PX02
      PY1=PY02
      PZ1=PZ02
      AM1=AM02
      IK2=IK01
      PX2=PX01
      PY2=PY01
      PZ2=PZ01
      AM2=AM01
 20   CALL SMARK(IK1,IK2)
      E1=DSQRT(AM1**2+PX1**2+PY1**2+PZ1**2)
      E2=DSQRT(AM2**2+PX2**2+PY2**2+PZ2**2)
      S=AM1**2+AM2**2+2.*E1*E2-2.*(PX1*PX2+PY1*PY2+PZ1*PZ2)
      DSM=DSQRT(S)-AM1-AM2
      TKIN=(S-AM1**2-AM2**2)/(2.*AM2)-AM1
      IF(TKIN.LE.0.02) TKIN=0.02
      P0 = DSQRT(TKIN*(TKIN+2*AM1))
C]]]]]]]]
C TO TAKE INTO ACCOUNT NON-MESON AND NON-NUCLEON INELASTIC
C REACTION MORE CAREFULLY I CHANGE MASSES
      IB1=IB(IK1)
      IB2=IB(IK2)
      IS1=IABS(IS(IK1))
      IS2=IABS(IS(IK2))
      AM1N=0.139
      AM2N=0.939
      IF(IS1.NE.0) AM1N=0.497
      IF(IB1.NE.0) AM1N=0.939
      IF(IB1.NE.0.AND.IS1.NE.0) AM1N=1.1156
      DSM=SQRT(ABS(S))-AM1N-AM2N
      TKIN=(S-AM1N**2-AM2N**2)/(2.*AM2N)-AM1N
      IF(TKIN.LE.0.02) TKIN=0.02
      P0 = SQRT(TKIN*(TKIN+2*AM1N))
C]]]]]]]
      DO 99 I = 1,5
99    IK(I) = 0
      IF(ITOT.NE.0) GO TO 40
      IK(1)=1
      IK(2)=2
      IK(3)=3
      IK(4)=4
      IK(5)=5
      CALL CRODIA(P0,ITOT,IK,IK1)
      IF(IPRINT) WRITE(ITLIS,333) ITOT,IK01,IK02,P0,CROSD
 333  FORMAT(4X,' ##### PARCRO: ITOT=',I4,' IK01,IK02==>',2I6,' P0='
     *,E10.4,' CROSD(5)=',5E10.4)
      RETURN
40    GOTO(50,60,70,80,90),IKS
50    IK(1)=1
      GO TO 100
60    IK(2)=2
      GO TO 100
70    IK(3)=3
      GO TO 100
80    IK(4)=4
      GO TO 100
90    IK(5)=5
100   CALL CRODIA(P0,ITOT,IK,IK1)
      RETURN
      END
C-------------------------------------------------------------C
      SUBROUTINE CRODIA(P0,ITOT,IK,IK1)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 P0
C
C     CALCULATION OF THE DIAGRAM CROSS-SECTION
C
      COMMON/KSI/ KSI(4)
      COMMON/COMELX/ SIGEL
      COMMON/COMCRO/ SIGTOT
      COMMON/CSIGA/ SIGAN
      COMMON/DATA10/ PAROM,PARKSI,PARSIG,PARF0
      COMMON/SIGDIA/ CROSD(5),DST
      COMMON/P0LAB1/P00,DSM,TKIN
      DIMENSION IK(5)
      SINEL = SIGTOT-SIGEL
      DO 80 I=1,5
80    CROSD(I)=0.
      DST=0.5
      IF(P0.GE.4.0) DST=0.75
      IF(P0.LT.1.5) DST=0.
      IF(SINEL.LT.0.0001.AND.SIGAN.LT.0.0001) RETURN
      IF(KSI(1).EQ.5) GO TO 50
      IS1=IS(IK1)
C  SEPARATION STRANGE PARTICLES
      IF(IS1.NE.0) GOTO 26
C     SEPARATION OF THE MESON AND NUCLEON REACTION
      IF(KSI(2) .GT. 1) GO TO 19
C     MESON-NUCLEON REACTIONS
      IF(KSI(1)-2)1,7,13
C     POSITIVE MESON+PROTON
 1    IF(IK(1).EQ.0) GO TO 2
      CROSD(1) = CROSS(P0,1,1)
 2    IF(IK(2).EQ.0) GO TO 3
      IF(DSM.LT.0.3) GO TO 3
      CROSD(2) = CROSS(P0,2,1)
 3    IF(IK(3).EQ.0) GO TO 4
      CROSD(3) = CROSS(P0,3,1)
 4    IF(IK(4).EQ.0) GO TO 5
      CROSD(4) = CROSS(P0,4,1)
 5    IF(IK(5).EQ.0) GO TO 6
      CROSD(5)=SINEL-CROSD(1)-CROSD(2)-CROSD(3)-CROSD(4)
 6      GO TO 500
C     NEGATIVE MESON+PROTON
 7    IF(IK(1).EQ.0) GO TO 8
      CROSD(1) = CROSS(P0,1,2)
 8    IF(IK(2).EQ.0) GO TO 9
      IF(DSM.LT.0.3) GO TO 9
      CROSD(2) = CROSS(P0,2,2)
 9    IF(IK(3).EQ.0) GO TO 10
      CROSD(3) = CROSS(P0,3,2)
 10   IF(IK(4).EQ.0) GO TO 11
      CROSD(4) = CROSS(P0,4,2)
 11   IF(IK(5).EQ.0) GO TO 12
      CROSD(5)=SINEL-CROSD(1)-CROSD(2)-CROSD(3)-CROSD(4)
 12       GO TO 500
C     NEUTRAL MESON+PROTON
 13   KN=IABS(IK1)
      COEF = 0.5
      IF(KN.NE.331) GO TO 36
C  NEUTRAL FI-MESON NUCLEON REACTION
      COEF = PARF0/2.
 36   CONTINUE
 14   IF(IK(1).EQ.0) GO TO 15
      CROSD(1) =(CROSS(P0,1,1)+CROSS(P0,1,2))*COEF
 15   IF(IK(2).EQ.0) GO TO 16
      IF(DSM.LT.0.3) GO TO 16
      CROSD(2) =(CROSS(P0,2,1)+CROSS(P0,2,2))*COEF
 16   IF(IK(3).EQ.0) GO TO 17
      CROSD(3) =(CROSS(P0,3,1)+CROSS(P0,3,2))*COEF
 17   IF(IK(4).EQ.0) GO TO 18
      CROSD(4) =(CROSS(P0,4,1)+CROSS(P0,4,2))*COEF
 18   IF(IK(5).EQ.0) GO TO 181
      IF(DSM.LT.0.3) GO TO 181
      CROSD(5)=SINEL-CROSD(1)-CROSD(2)-CROSD(3)-CROSD(4)
 181       GO TO 500
C     NUCLEON-NUCLEON REACTIONS
C
 19   CONTINUE
      IF(KSI(1)-2) 20,200,200
C  PROTON - PROTON REACTION
 20   IF(IK(1).EQ.0) GO TO 21
      CROSD(1) = CROSS(P0,1,5)
 21   IF(IK(2).EQ.0) GO TO 22
      IF(DSM.LT.0.3) GO TO 22
      CROSD(2) = CROSS(P0,2,5)
 22   IF(IK(3).EQ.0) GO TO 23
      CROSD(3) = CROSS(P0,3,5)
 23   IF(IK(4).EQ.0) GO TO 24
      CROSD(4) = CROSS(P0,4,5)
 24   IF(IK(5).EQ.0) GO TO 25
      CROSD(5)=SINEL-CROSD(1)-CROSD(2)-CROSD(3)-CROSD(4)
 25     GO TO 500
C  NEUTRON - PROTON REACTION
 200  IF(IK(1).EQ.0) GO TO 210
      CROSD(1) = CROSS(P0,1,5)
 210  IF(IK(2).EQ.0) GO TO 220
      IF(DSM.LT.0.3) GO TO 220
      CROSD(2) = CROSS(P0,2,5)
 220  IF(IK(3).EQ.0) GO TO 230
      CROSD(3) = CROSS(P0,3,5)
 230  IF(IK(4).EQ.0) GO TO 240
      CROSD(4) = CROSS(P0,4,5)
 240  IF(IK(5).EQ.0) GO TO 250
      CROSD(5)=SINEL-CROSD(1)-CROSD(2)-CROSD(3)-CROSD(4)
 250    GO TO 500
C  SEPARATION OF MESON AND BARYON REACTION
 26   IF(KSI(2).GT.1)GO TO 40
C  STRANGE MESON NUCLEON REACTION
      KSIGO=KSI(1)
      GOTO (27,32,27,32),KSIGO
C  STRANGE MESON NUCLEON ---> K0 N, K+ P, K+ N
 27   IF(IK(1).EQ.0) GO TO 28
      CROSD(1) = CROSS(P0,1,3)
 28   IF(IK(2).EQ.0) GO TO 29
      IF(DSM.LT.0.3) GO TO 29
      CROSD(2) = CROSS(P0,2,3)
 29   IF(IK(3).EQ.0) GO TO 30
      CROSD(3) = CROSS(P0,3,3)
 30   IF(IK(4).EQ.0) GO TO 31
      CROSD(4) = CROSS(P0,4,3)
 31   IF(IK(5).EQ.0) GO TO 500
      CROSD(5)=SINEL-CROSD(1)-CROSD(2)-CROSD(3)-CROSD(4)
       GO TO 500
C  STRANGE MESON NUCLEON --->AK0 N, K- P, K- N
 32   IF(IK(1).EQ.0) GO TO 33
      CROSD(1) = CROSS(P0,1,4)
 33   IF(IK(2).EQ.0) GO TO 34
      IF(DSM.LT.0.3) GO TO 34
      CROSD(2) = CROSS(P0,2,4)
 34   IF(IK(3).EQ.0) GO TO 35
      CROSD(3) = CROSS(P0,3,4)
 35   IF(IK(4).EQ.0) GO TO 37
      CROSD(4) = CROSS(P0,4,4)
 37   IF(IK(5).EQ.0) GO TO 500
      CROSD(5)=SINEL-CROSD(1)-CROSD(2)-CROSD(3)-CROSD(4)
       GO TO 500
C  STRANGE BARYON NUCLEON REACTION
 40   IF(2 - IABS(KSI(4))) 41,42,43
 41   COEF = PARSIG
      GO TO 44
 42   COEF = PARKSI
      GO TO 44
 43   COEF = PAROM
 44   IF(IK(1).EQ.0) GO TO 45
      CROSD(1) = CROSS(P0,1,5)*COEF
 45   IF(IK(2).EQ.0) GO TO 46
      IF(DSM.LT.0.3) GO TO 46
      CROSD(2) = CROSS(P0,2,5)*COEF
 46   IF(IK(3).EQ.0) GO TO 47
      CROSD(3) = CROSS(P0,3,5)*COEF
 47   IF(IK(4).EQ.0) GO TO 48
      CROSD(4) = CROSS(P0,4,5)*COEF
 48   IF(IK(5).EQ.0) GO TO 49
      CROSD(5)=SINEL-CROSD(1)-CROSD(2)-CROSD(3)-CROSD(4)
 49      GO TO 500
C    ANTIBARYON NUCLEON REACTION
 50   IF(IK(1).EQ.0) GO TO 51
      IF(TKIN.LT.0.0001) GO TO 53
      CROSD(1) = CROSS(P0,1,6)
 51   IF(IK(2).EQ.0) GO TO 52
      IF(TKIN.LT.0.0001) GO TO 53
      CROSD(2) = CROSS(P0,2,6)
 52   IF(IK(3).EQ.0) GO TO 53
      IF(TKIN.LT.0.0001) GO TO 53
      CROSD(3)=SINEL-SIGAN-CROSD(1)-CROSD(2)
 53   IF(IK(4).EQ.0) GO TO 54
      CROSD(4) = CROSS(P0,3,6)
 54   IF(IK(5).EQ.0) GO TO 500
      CROSD(5) = CROSS(P0,4,6)
 500  CONTINUE
      IF(ITOT.NE.0) RETURN
      IF(CROSD(5).LT.0) CROSD(5)=0.
      SUM=CROSD(1)+CROSD(2)+CROSD(3)+CROSD(4)+CROSD(5)
      IF(SUM.LE.0.00001)ACOE=0.
      IF(SUM.GT.0.00001) ACOE=SINEL/SUM
      DO 501 I=1,5
 501  CROSD(I)=CROSD(I)*ACOE
      RETURN
      END
C-------------------------------------------------------------C
      FUNCTION CROSS(P01,NDIAGR,ITYP)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CROSS,P01
C
C     CALCULATION OF THE DIAGRAM CROSS SECTION
C         BY MEANS OF INTERPOLATION
C
      COMMON /SIGDGR/ SDGR(50,24)
      COMMON /INTERP/ F(6)
      I = NDIAGR+(ITYP-1)*4
      P0 = P01
C     COMPUTE THREE POINTS FOR THE INTERPOLATION
 3    L=1
 4    IF(P0-0.2*L+0.1)6,5,11
 5    CROSS=SDGR(L,I)
      RETURN
C     (INTERPOLATION IS UNNECCESSARY)
 6    IF(L .GT. 1) GO TO 7
      CROSS=SDGR(L,I)
      RETURN
 7    IF(L .GE. 49) GO TO 8
      L1=L-2
      GO TO 9
 8    L1=47
 9    DO 10 K=1,3
      LL1=L1+K
      F(K)=SDGR(LL1,I)
      F(K+3)= 0.2*(LL1-1)
 10   CONTINUE
                 GO TO 12
 11   L=L+1
      IF(L.EQ.50)GOTO 13
      GOTO 4
 12   CROSS=SINTER(P0)
      RETURN
 13   CROSS=SDGR(50,I)
      RETURN
      END
C-------------------------------------------------------------C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      BLOCK DATA SGDGR
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON /SIGDGR/ SDGR(50,24)
      DIMENSION SDGPIP(200),SDGPIM(200),SDGKP(200),
     * SDGKM(200),SDGPP(200),SDGAPP(200)
      EQUIVALENCE (SDGR( 1, 1),SDGPIP(1))
     *      ,     (SDGR( 1, 5),SDGPIM(1))
     *      ,     (SDGR( 1, 9),SDGKP (1))
     *      ,     (SDGR( 1,13),SDGKM (1))
     *      ,     (SDGR( 1,17),SDGPP (1))
     *      ,     (SDGR( 1,21),SDGAPP(1))
C=================  BINAR  PI+ P ============
C                 ----,--,---,---,1.0,---,---,---,--,2.0,---,
      DATA SDGPIP/2*0.,.8,2.5,6.8,5.0,4.4,3.4,2.8,2.,1.6,1.2,
C     --,--,--,3.0--,-------->
     :.8,.6,.4,.2,.1,33*0.,
C------------------ DIFRACTION  PI+ P -------
     :0.,0.,.4,1.
C     ----,--,-1.0,----,----,----,----,2.0,---,----,----,----,
     :    ,6.,11.3,17.2,17.9,17.2,18.9,20.,20.,19.8,19.6,19.2,
C      3.0,----,----,----,----,-4.0,----,----,----,---,5.0,--,
     :18.4,17.4,16.2,15.2,14.2,13.2,12.3,11.4,10.6,9.8,9.,8.2,
C     ---,--,---,6.0---,--,---,---,7.0---,---,---,---,8.0,---,
     :7.4,7.,6.6,6.,5.4,5.,4.6,4.3,4.,3.9,3.7,3.6,3.4,3.3,3.2,
C     ---,---,---,9.0,---,---,---,9.8
     :3.0,2.9,2.8,2.7,2.6,2.6,2.6,2.5,
C------------------ UNCYLINDR   PI+ P -------
C     ----,--,---,2.0,---,----,---,----,3.0 ,----,----,----,----,
     :8*0.,.5,2.7,5.5,9.5,13.5,16.,17.6,18.6,19.2,18.9,18.2,17.4,
C     -4.0,----,----,----,---,5.0,---,---,--,--,-6.--,---,---,--,
     :16.6,15.5,14.8,13.9,13.,12.,11.,9.9,9.,8.,7.,6.,5.2,4.5,4.,
C     7.0,---,---,---,---,8.0,---,--,---,---,9.0,-------
     :3.7,3.5,3.2,2.8,2.5,2.4,2.2,2.,1.8,1.7,1.6,4*1.5,
C------------------ PLANAR   PI+ P   --------
C     ----,-----,1.,--,---,---,---,2.,---,---,---,---,3. ,---,---,
     :4*0.,.5,2.,3.,4.,3.6,3.4,3.2,3.,2.8,2.7,2.6,2.5,2.4,2.2,2.1,
C     ---,---,4. ,---,---,---,-----------
     :1.9,1.8,1.7,1.6,1.5,1.5,25*1.4/
C=================  BINAR  PI-P  ============
C                   --,--,---,----,----, 1.0,----,----,----,----, 2.0,
      DATA SDGPIM/0.,120.,24.,24. ,24.1,26.8,22.1,21.8,21.1,20.1,16.8,
C     ---,---,--,--,3.0,---,---,--,---,3.0,---,---,---,---,5.0----
     :9.9,8.8,6.,5.,3.5,2.9,2.6,2.,1.8,1.7,1.5,1.4,1.3,1.2,25*1.,
C------------------ DIFRACTION  PI-P  -------
C     --,--,--,---,---,1.0,------------------->
     :0.,0.,.2,1.0,1.8,2.2,44*2.4,
C------------------ UNCYLINDR   PI-P  -------
C     ----,--,---,2.0,---,----,---,----, 3.0,----,----,----,----,
     :8*0.,.5,2.7,5.5,9.5,13.5,16.,17.6,18.6,19.2,18.9,18.2,17.4,
C     4.0 ,----,----,----,---,5.0,---,---,--,--,6.,--,---,---,--,
     :16.6,15.5,14.8,13.9,13.,12.,11.,9.9,9.,8.,7.,6.,5.2,4.5,4.,
C     7.0,---,---,---,---,8.0,---,--,---,---,9.0,------------
     :3.7,3.5,3.2,2.8,2.5,2.4,2.2,2.,1.8,1.7,1.6,4*1.5,
C------------------ PLANAR   PI- P   --------
C      ---,---,---,---,---,1.0,---,--,---,--,2.,--,--,--,--,3.,--,
     :  0.,20.,10.,7.5,5.8,3.4,2.5,2.,1.5,1.,.9,.9,.8,.7,.6,.5,.4,
C       --,--,--,4.,--,--,--,--------->
     :  .4,.3,.3,.2,.2,.1,.1,26*0./
C=================  BINAR  K+ P  ============
C                 ---1.,---,--,---,---,2.,--,--,
      DATA SDGKP/6*13.4,8.9,6.,2.5,1.3,.8,.4,.2,
     :37*0.,
C------------------ DIFRACTION  K+ P  -------
C     ----,--,1.,--,--,--,---,2.0,--,---,---,---,3.0,---,---,--,
     :4*0.,1.,2.,3.,4.,5.,5.2,5.4,5.,4.8,4.6,4.2,3.8,3.2,2.5,2.,
C     ---,4.0,---------->
     :1.6,1.5,29*1.4,
C------------------ UNCYLINDR   K+ P  -------
C     ----,1.,--,---,---,---,2.,---,---,---,---,3. ,---,---,---,
     :5*0.,1.,3.,5.4,6.6,7.4,8.,8.3,8.5,8.7,9.2,9.5,9.7,9.8,9.9,
C     ---, 4. ,------,----,6. ,---,---,---,---, 7.,---,---,---,---,
     :10.,10.1,8*10.2,10.1,10.,9.9,9.8,9.7,9.6,9.6,9.5,9.5,9.4,9.3,
C      8.,--,---,---,---,9. ,--,---,---,9.8,
     :9.2,9.,7.8,7.6,7.4,7.2,7.,6.8,6.6,6.4,
C------------------ PLANAR   K+  P  - NOT ALLOWED --------
     :50*0./
C=================  BINAR  K- P  ============
C                0 ,--,--,--,----, 1. ,----,----,----,---, 2.,
      DATA SDGKM/0.,0.,6.,9.,11.6,12.2,12.2,11.6,10.4,9.4,8.2,
C     ---,---,---,---, 3.,---,--,---,---, 4.,---,---,---,---, 5.,
     :7.4,6.9,6.6,6.3,5.6,5.3,5.,4.7,4.4,4.1,3.8,3.7,3.5,3.4,3.2,
C     --,---,---,---, 6.,---,---,---,---, 7.,---,------------
     :3.,2.9,2.8,2.7,2.6,2.5,2.4,2.3,2.2,2.1,2.1,13*2.,
C------------------ DIFRACTION  K- P  -------
C     ----,--,1.,--,---,---,---, 2.,---,--,-----------
     :4*0.,.2,.4,.8,1.3,1.5,1.7,1.8,1.9,2.,37*2.0,
C------------------ UNCYLINDR   K- P  -------
C     ----,1.,--,---,--,--, 2.,----,----,---,----, 3. ,---,----,
     :5*0.,2.,5.,6.8,8.,9.,9.8,10.4,10.7,11.,11.1,11.2,11.,10.7,
C     ----,---,4. ,---,---,--,---, 5.,---,---,---,--, 6.,---,
     :10.3,9.8,9.2,8.8,8.4,8.,7.8,7.5,7.3,7.2,7.1,7.,6.9,6.8,
C     ---,---,---, 7.,---,---,---,--, 8.,---,---,---,---, 9.,---,
     :6.7,6.6,6.5,6.4,6.3,6.2,6.1,6.,5.9,5.8,5.7,5.6,5.5,5.4,5.3,
C     ---,---,9.8,
     :5.2,5.1,5.0,
C------------------ PLANAR   K-  P   --------
C       0. ,---,---,----,---,1. ,---,---,---,---, 2.,---,--,
     :  20.,20.,13.,10.4,8.3,6.7,5.7,4.9,4.3,3.8,3.4,3.2,3.,
C     ---,---, 3.,---,---,---,---, 4.,---,---,--,--,5.,--,--,
     :2.8,2.6,2.4,2.3,2.1,1.9,1.7,1.5,1.3,1.1,.9,.7,.5,.3,.1,
     :22*0./
C=================  BINAR  PP    ============
C                ----, 1.,----,----,----,----, 2. ,----,----,
      DATA SDGPP/4*0.,2*2.8,11.6,21.5,22.8,21.6,23.6,24.8,25.3,
C     ----,----, 3. ,----,----,---,----, 4. ,----,--,---,---,
     :25.5,25.1,22.8,20.4,19.4,15.,12.6,11.4,10.1,9.,7.8,7.2,
C      5.,--,---,---,---,6.,---,---,---,--, 7.,---,---,---,--,
     :6.6,6.,5.4,4.9,4.5,4.,3.7,3.4,3.2,3.,2.8,2.5,2.3,2.2,2.,
C      8.,---,---,---,---, 9.,---,--,--,--,
     :1.9,1.7,1.6,1.5,1.4,1.3,1.2,1.,.9,.8,
C------------------ DIFRACTION   PP   -------
C        ----,1.,--,--,--,--,2.,---,---,-----,3. ,---,---,---,---,
     :4*0.,2*.0,.2,.4,.6,1.,1.2,1.4,1.6,7*1.8,1.9,2.0,2.1,2.2,2.3,
C     4. ,---,---,---,---, 5.,---,---,---,---, 6.,---,---,---,---,
     :2.4,2.5,2.6,2.7,2.8,2.9,3.0,3.1,3.2,3.3,3.4,3.5,3.6,3.7,3.8,
C      7.,---,---,---,---, 8.,---,---,---,---,
     :3.9,4.0,4.1,4.2,4.3,4.4,4.5,4.6,4.7,4.8,
C------------------ UNCYLINDR     PP  -------
C                                 1.0                2.0
     :14*0.,.7,4.,6.6,9.,10.8,12.6,13.4,14.,14.6,15.2,15.6,15.8,
C                                 1.0                2.0
     :16.2,16.3,16.4,16.4,16.3,16.3,16.1,15.6,15.2,14.5,13.8,13.2,
C                                 1.0                2.0
     :12.4,11.8,11.3,10.5,9.9,9.5,8.8,8.3,7.9,7.5,7.3,7.,
C------------------ PLANAR   PP  - IS ABSENT ------
     :50*0./
C=================  DIFSMA APP   ============
C                                 1.0                2.0
      DATA SDGAPP/5*0.,.0,.2,.3,.5,.8,1.2,1.4,1.8,2.4,
C                                 1.0                2.0
     :2.8,3.2,3.4,3.6,3.7,3.8,4.,4.,4.,3.9,3.8,3.7,3.6,
C                                 1.0                2.0
     :3.5,3.4,3.3,3.2,3.1,3.,2.9,2.9,2.8,2.8,2.7,2.7,2.6,
C                                 1.0                2.0
     :2.6,2.5,2.5,2.4,2.4,2.3,2.3,2.3,2.3,2.3,
C------------------ DIFTRI APP --------------
C                                 1.0                2.0
     :12*0.,.2,.4,.8,1.,1.5,2.,2.5,3.4,4.4,5.3,6.3,7.3,
C                                 1.0                2.0
     :8.3,9.2,9.8,10.3,10.8,11.2,11.5,11.9,12.2,12.5,12.8,
C                                 1.0                2.0
     :13.,13.1,13.2,23.3,13.4,13.5,13.6,13.7,13.8,13.9,5*14.,
C------------------ ANNIH I-SHEET -----------
     :50*0.,
C------------------ ANNIH II-SHEET ----------
C                                 1.0                2.0
     :150.,100.,80.,60.,40.,30.,20.,16.,14.,12.,10.,39*9./
C--------------------- PLANAR FILLS IN SUBR."CRODIA" ----C
      END
C-------------------------------------------------------------C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      BLOCK DATA BDAM1
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON /DATA2/PUD,PS1,SIGMA,CX2
      COMMON /DATA1/CMIX(6,2),PMASM(18),PMASB(18),PGAMM(18),
     *PGAMB(18),MESO(9,2)
      COMMON /DATA4/QMAS(9)
      COMMON /DATA6/POD810(3,6),PODSA(8,3),KBAR(18,2)
      COMMON /DATA5/IFLM(18,2),IFLB(18,3),DQQ(3,3)
      COMMON /COMCHA/ ICHAM(18),ICHAB(18)
      COMMON /COMSTR/ ISTR(36)
      COMMON /DATA3/ POPB(10)
      DATA POPB/1.0,0.89,1.0,0.89,0.89,0.89,0.67,0.67,0.67,1.0/
      DATA ISTR /2*0,1,-1,1,-1,5*0,1,-1,1,-1,5*0,3*-1,
     *2*-2,-1,4*0,3*-1,2*-2,-3/
      DATA ICHAM/1,-1,1,-1,0,0,0,0,0,1,-1,1,-1,0,0,0,0,0/
      DATA ICHAB/1,0,1,-1,0,-1,0,0,2,1,-1,0,1,-1,0,-1,0,-1/
      DATA PUD/0.425/,PS1/0.75/,SIGMA/0.51/,CX2/0.12/
      DATA MESO/7,1,3,2,8,5,4,6,9,7,2,4,1,8,6,3,5,9/
      DATA CMIX/2*0.5,1.,2*0.5,1.,2*0.25,0.5,2*0.,1./
      DATA PMASM/2*0.14,4*0.492,0.14,0.549,0.958,2*0.770,2*0.896,
     *          2*0.896,0.770,0.783,1.020/
      DATA PMASB/2*0.94,3*1.189,2*1.315,1.116,4*1.232,
     *3*1.385,2*1.530,1.672/
      DATA PGAMM/7*0.,0.83D-6,0.29D-3,2*0.152,4*0.049,0.152,0.01,0.004/
      DATA PGAMB/8*0.,4*0.122,0.035,0.042,0.035,0.0091,0.01,0./
      DATA DQQ   /4.,6.,7.,
     *                     6.,5.,8.,
     *                     7.,8.,9./
      DATA QMAS/9*0./
      DATA IFLM  /1,2,1,3,2,3,3*1,1,2,1,3,2,3,2*1,3,
     *    -2,-1,-3,-1,-3,-2,3*-1,-2,-1,-3,-1,-3,
     *-2,2*-1,-3/
      DATA IFLB   /3*1,2,1,2,4*1,2,2*1,2,1,2,1,3,
     *                      1,2,1,2,2,2*3,2,2*1,2*2,1,2*2,3*3,
     *                       2*2,6*3,1,3*2,6*3 /
      DATA PODSA /0.12,1.0,0.25,0.,0.25,0.,1.0,0.75,
     *                       1.0,0.12,0.,0.25,0.25,1.,0.,0.75,
     *                       0.,0.,1.,1.,1.,0.25,0.25,1./
      DATA KBAR    /0,37,39,38,0,40,37,38,41,39,44,43,44,
     *40,42,43,42,0,
     *             45,46,49,48,47,50,46,48,51,49,51,53,51,
     *50,52,53,52,54/
      DATA POD810 /1.,3*0.667,1.,0.667,   2*0.89,0.667,0.89,
     *             0.667,0.89,0.667,2*0.89,2*0.667,1./
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      BLOCK DATA BDAM2
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON /CUTOF2/BA,BB,BINF,POW,ISL,ISLFOR
      COMMON /COMCUT/ PARM,PARB,SWMAX
C
      DATA PARM/1.4/,PARB/2.0/,SWMAX/0.35/
C   BA,BB FIX THE ELASTIC  SLOPE AT INFINITE ENERGY
      DATA BA/7.90/
      DATA BB/7.90/
C   BINF FIXES THE DIFFRACTIVE SLOPE FOR LARGE MASS CLUSTERS
      DATA BINF/4.0/
      DATA POW/2.0/
C   ISL=1,2,3 FIXES THE TYPE OF MASS DEPENDENCE OF THE DIFFRACTIVE SLOPE
       DATA ISL/2/
C   ISLFOR=1 OR 2 DETERMINES THE PRESENCE OR ABSENCE OF A T(FORWARD)
C   CUTOFF ON THE MASS SPECTRUM
      DATA ISLFOR/1/
C
      END
C-------------------------------------------------------------C
      BLOCK DATA BDAM3
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON /PARCUT/ SWMAX
      COMMON/COMABM/ ALFAM,BETAM
      COMMON/COMABB/ ALFAB,BETAB
C
      DATA ALFAM/0.5/,BETAM/2.5/,ALFAB/0.5/,BETAB/2.5/
      DATA SWMAX/0.35/
C
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      BLOCK DATA BDAM4
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/FRGCPA/ PUDC,PUDCC,PSPINC,PJSPNC,PMIX1C(3,2),PMIX2C(3,2),
     *PBARC
      COMMON/FRGSPA/ PUDS,PUDSC,PSPINS,PJSPNS,PMIX1S(3,2),PMIX2S(3,2),
     *SIGQTS,WENDM,WENDB,PBARS,PRIQS(9),PARDBS,PARQLS,PARRS,PUNDS
      COMMON/KAPPA/ XAP
      COMMON/CONSTI/ CONSTI
      LOGICAL CONSTI
      COMMON/VALON/ VALON
      LOGICAL VALON
C
C     INPUT TYPE OF CROSS SECTION PARAMETER
      DATA VALON/.TRUE./
C
C     INPUT TYPE OF TIME PARAMETER
      DATA CONSTI/.TRUE./
C
C     INPUT CHARM PARTICLES
      DATA PUDCC,PUDSC/1.0,1.0/
C  IF YOU WANT TO PRODUCE CHARM PARTICLES THEN
CCCC             DATA PUDCC,PUDSC/0.995,0.995  /
      DATA PJSPNC/.50/
      DATA PMIX1C/.25,.25,.5,.5,.5,1./,PMIX2C/.5,.5,1.,0.,0.,1./
      DATA PMIX1S/.25,.25,.5,.5,.5,1./,PMIX2S/.5,.5,1.,0.,0.,1./
      DATA PJSPNS/.50/
C  AT ENERGY SQRT(S)>540 GEV IT WILL BE BETTER TO PUT SIGQTS=0.40
C  AND PUDC,PUDS=0.435
C     DATA SIGQTS/0.40/
C   BELOW THE DUBNA PARAMETER SET IS GIVEN
C     DATA PUDC/0.425/,PSPINC/0.50/,PBARC/0.120/
C     DATA PUDS/0.425/,PSPINS/0.50/,PBARS/0.120/
C     DATA SIGQTS/0.51/
      DATA SIGQTS/0.55/
      DATA PUDC/0.420/,PSPINC/0.50/,PBARC/0.080/
      DATA PUDS/0.420/,PSPINS/0.50/,PBARS/0.080/
      DATA PARDBS/0.90/
C     DATA PARDBS/0.60/
      DATA PUNDS/0.27/,PARQLS/0.5/,PARRS/0.9/
C  INPUT STRING TENSION (FERMI/GEV)
      DATA XAP/.9/
       END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      BLOCK DATA BDAM5
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/REAPOM/ ALFP,ALFPP,R2PN,R2PI,R2PK,
     *GAMPN,GAMPPI,GAMKP,S0N,S0PI,S0K
      COMMON/REAREG/ ALFR,ALFRP,R2R,GAMR
      COMMON/COMSMD/ CN,CPI,CK
      COMMON/COMPLI/ LIMP
      COMMON/REACOE/ COEF(11),COEF1(11)
      COMMON/CPRSIG/ ISGCOL
      COMMON/COMLIA/ LIMPA
      DATA LIMPA/3/
      DATA ISGCOL /0/
C  LIMP SHOULD BE LESS 10 ( LOSS OF ACCURACY] )
      DATA LIMP/10/
      DATA ALFP/1.05/
      DATA S0N/1.0/,S0PI/1.0/,S0K/1.0/
      DATA R2PN/2.4/,R2PI/2.28/,R2PK/2.56/
      DATA GAMPN/3.2 /,GAMPPI/1.07/,GAMKP/1.27/
      DATA CN/3.0/,CPI/3.2/,CK/3.4/,ALFPP/0.2 /
      DATA GAMR/1.0/,ALFR/0.5/,R2R/1.695/,ALFRP/0.843/
      DATA COEF/0.65,1.95,3.5,2*1.,0.35,1.,3.5,1.,.78,1./
      DATA COEF1/0.375,49.0,12.26,2*1.,0.325,1.,3.787,1.,12.00,.330/
       END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      BLOCK DATA BDAM6
      IMPLICIT REAL*8 (A-H,O-Z)
C     PARTICLE INTERACTION CROSS-SECTIONS
      COMMON/TABLE/ SIGMA(30,21),ENERGY(30,4)
      DIMENSION ENERG1(30,2),ENERG2(30,2)
      EQUIVALENCE(ENERGY(1,1),ENERG1(1,1))
      EQUIVALENCE(ENERGY(1,3),ENERG2(1,1))
      DIMENSION SIGMA1(30,3),SIGMA2(30,2),SIGMA3(30,2),SIGMA4(30,3),
     *     SIGMA5(30,2),SIGMA6(30,2),SIGMA7(30,3),SIGMA8(30,2),
     *     SIGMA9(30,2)
      EQUIVALENCE(SIGMA(1,1),SIGMA1(1,1))
      EQUIVALENCE(SIGMA(1,4),SIGMA2(1,1))
      EQUIVALENCE(SIGMA(1,6),SIGMA3(1,1))
      EQUIVALENCE(SIGMA(1,8),SIGMA4(1,1))
      EQUIVALENCE(SIGMA(1,11),SIGMA5(1,1))
      EQUIVALENCE(SIGMA(1,13),SIGMA6(1,1))
      EQUIVALENCE(SIGMA(1,15),SIGMA7(1,1))
      EQUIVALENCE(SIGMA(1,18),SIGMA8(1,1))
      EQUIVALENCE(SIGMA(1,20),SIGMA9(1,1))
      COMMON/DATA10/ PAROM,PARKSI,PARSIG,PARF0
      DATA PAROM/0.53/,PARKSI/0.66/,PARSIG/0.81/,PARF0/0.62/
      DATA SIGMA1 /
     *   1.900 ,    2.300 ,    3.500 ,    5.500 ,    9.000 ,
     *  14.000 ,   28.000 ,   60.000 ,  163.000 ,  195.000 ,
     * 185.000 ,  145.000 ,  113.000 ,   45.000 ,   25.200 ,
     *  21.600 ,   15.600 ,   15.200 ,   19.500 ,   22.800 ,
     *  24.500 ,   27.600 ,   36.700 ,   41.000 ,   39.000 ,
     *  32.300 ,   28.900 ,   27.700 ,   24.900 ,   23.500 ,
     *   1.900 ,    2.300 ,    3.500 ,    5.500 ,    9.000 ,
     *  14.000 ,   28.000 ,   60.000 ,  163.000 ,  195.000 ,
     * 184.900 ,  144.800 ,  112.800 ,   44.400 ,   23.200 ,
     *  18.600 ,   10.800 ,    7.700 ,    9.000 ,   10.200 ,
     *  11.300 ,   13.500 ,   16.900 ,   19.000 ,   16.900 ,
     *  12.600 ,    5.700 ,    5.600 ,    4.900 ,    4.000 ,
     *   6.000 ,    6.000 ,    6.500 ,    7.000 ,    8.500 ,
     *  10.500 ,   16.500 ,   25.300 ,   57.500 ,   68.500 ,
     *  64.500 ,   52.000 ,   40.500 ,   25.700 ,   29.000 ,
     *  32.100 ,   45.600 ,   38.000 ,   44.300 ,   54.000 ,
     *  58.000 ,   45.700 ,   35.300 ,   35.000 ,   34.300 ,
     * 34.000 ,   32.400 ,   30.400 ,   26.500 ,   25.000   /
      DATA SIGMA2 /
     *   2.000 ,    2.000 ,    2.250 ,    2.300 ,    2.400 ,
     *   2.500 ,    4.500 ,    7.700 ,   20.000 ,   25.200 ,
     *  23.900 ,   21.000 ,   16.000 ,   10.500 ,   11.200 ,
     *  14.000 ,   20.300 ,   16.000 ,   19.300 ,   26.000 ,
     *  26.500 ,   18.700 ,   11.700 ,   10.500 ,    9.600 ,
     *   9.500 ,    6.800 ,    5.900 ,    5.000 ,    4.000 ,
     *   4.000 ,    4.000 ,    4.250 ,    4.700 ,    6.100 ,
     *   8.000 ,   12.000 ,   17.600 ,   37.500 ,   43.300 ,
     *  40.600 ,   31.000 ,   24.500 ,   13.100 ,   11.000 ,
     *   9.500 ,    8.300 ,    5.000 ,    5.500 ,    6.800 ,
     *   7.000 ,    3.500 ,    2.000 ,    1.900 ,    1.800 ,
     *   1.600 ,     .220 ,     .150 ,     .048 ,     .009   /
      DATA SIGMA3 /
     *  10.000 ,   12.000 ,   14.000 ,   16.000 ,   17.000 ,
     *  20.000 ,   30.000 ,   38.000 ,   42.000 ,   38.500 ,
     *  32.000 ,   24.000 ,   18.000 ,    1.000 ,       .0 ,
     *      .0 ,       .0 ,       .0 ,       .0 ,       .0 ,
     *      .0 ,       .0 ,       .0 ,       .0 ,       .0 ,
     *      .0 ,       .0 ,       .0,        .0,        .0 ,
     *  17613.0 ,  330.000 ,  154.000 ,   96.000 ,   70.000 ,
     *  51.000 ,   38.400 ,   30.000 ,   23.600 ,   22.400 ,
     *  22.500 ,   22.700 ,   23.450 ,   24.300 ,   29.000 ,
     *  41.500 ,   47.800 ,   47.600 ,   47.500 ,   46.700 ,
     *  46.000 ,   45.000 ,   42.500 ,   41.200 ,   40.800 ,
     *  40.000 ,   39.800 ,   39.000 ,   39.000 ,   38.500 /
      DATA SIGMA4 /
     *  17613.0 ,  330.000 ,  154.000 ,   96.000 ,   70.000 ,
     *  51.000 ,   38.400 ,   30.000 ,   23.600 ,   22.400 ,
     *  22.500 ,   22.700 ,   23.000 ,   23.500 ,   24.500 ,
     *  25.600 ,   26.000 ,   25.000 ,   23.000 ,   21.500 ,
     *  19.500 ,   17.500 ,   15.000 ,   13.700 ,   12.000 ,
     *  11.000 ,    9.800 ,    8.800 ,    8.500 ,    6.500 ,
     * 20357.0 ,  950.000 ,  480.000 ,  300.000 ,  200.000 ,
     * 156.000 ,  109.000 ,   73.700 ,   49.500 ,   41.000 ,
     *  36.500 ,  34.01067,   32.6228,   31.5561,   30.032 ,
     *  30.761 ,   33.500 ,   35.000 ,   36.500 ,   38.500 ,
     *  39.500 ,   40.500 ,   41.300 ,   42.000 ,   42.500 ,
     *  42.000 ,   41.300 ,   39.750 ,   39.500 ,   39.000 ,
     * 20357.0 ,  950.000 ,  480.000 ,  300.000 ,  200.000 ,
     * 156.000 ,  109.000 ,   73.700 ,   49.500 ,   41.000 ,
     *  36.500 ,   34.000,    32.5000,   31.200 ,   28.500 ,
     *  24.000 ,   20.000 ,   19.000 ,   17.300 ,   16.000 ,
     *  15.200 ,   14.000 ,   12.000 ,   11.000 ,   10.900 ,
     *  10.600 ,   10.000 ,    8.000 ,    6.500 ,    6.200 /
      DATA SIGMA5 /
     *      .0 ,       .0 ,       .0 ,     .050 ,     .100 ,
     *    .300 ,     .600 ,    1.200 ,    2.200 ,    3.200 ,
     *   3.400 ,    3.600 ,    3.700 ,    3.800 ,    3.900 ,
     *   3.900 ,    3.900 ,    4.000 ,    4.000 ,    4.000 ,
     *   4.000 ,    4.000 ,    3.900 ,    3.800 ,    3.500 ,
     *   3.200 ,    2.900 ,    2.700 ,    2.400 ,    2.100 ,
     *      .0 ,       .0 ,       .0 ,     .400 ,     .800 ,
     *   1.400 ,    2.300 ,    4.400 ,    8.000 ,   10.800 ,
     *  15.000 ,   16.100 ,   16.600 ,   17.000 ,   17.300 ,
     *  17.500 ,   17.600 ,   17.700 ,   17.800 ,   17.900 ,
     *  17.800 ,   17.500 ,   16.800 ,   16.000 ,   13.700 ,
     * 12.600 ,   11.600 ,   10.800 ,    9.200 ,    8.000   /
      DATA SIGMA6 /
     *   0.    ,    0.    ,  0.009632,   0.09939,   0.2666 ,
     *   0.5429,    0.9929,    1.704 ,    2.707 ,    3.851 ,
     *   4.887 ,    5.689 ,    6.261 ,    6.653 ,    6.915 ,
     *   7.086 ,    7.197 ,    7.324 ,    7.412 ,    7.494 ,
     *   7.576 ,    7.755 ,    7.900 ,    7.700 ,    7.000 ,
     *   6.600 ,    6.200 ,    5.800 ,    4.600 ,    3.500 ,
     *   0.    ,    0.    ,   0.00052,   0.01166,   0.04471,
     *   0.1182,    0.2695,    0.5573,    0.9979,    1.455 ,
     *    1.782 ,    1.982 ,    2.107 ,    2.191 ,    2.255 ,
     *    2.310 ,    2.366 ,    2.504 ,    2.698 ,    2.937 ,
     *    3.199 ,    3.759 ,    4.000 ,    3.900 ,    3.500 ,
     *    3.200 ,    2.700 ,    2.100 ,    1.000 ,    0.600 /
      DATA SIGMA7 /
     *      .0 ,       .0 ,       .0 ,       .0 ,     .200 ,
     *    .600 ,    1.200 ,    2.100 ,    3.300 ,    4.100 ,
     *   4.900 ,    6.000 ,    7.700 ,    8.900 ,   10.000 ,
     *  10.200 ,   10.100 ,    9.800 ,    9.800 ,   10.100 ,
     *  10.300 ,    8.500 ,    6.900 ,    5.500 ,    3.900 ,
     *   3.300 ,    3.000 ,    2.700 ,    2.200 ,    2.100 ,
     *      .0 ,       .0 ,       .0 ,       .0 ,     .100 ,
     *    .300 ,     .500 ,     .700 ,     .900 ,    1.100 ,
     *   1.300 ,    1.500 ,    1.800 ,    2.200 ,    2.600 ,
     *   2.600 ,    2.400 ,    2.400 ,    3.000 ,    3.400 ,
     *   3.500 ,    3.100 ,    2.700 ,    2.400 ,    2.100 ,
     *   1.900 ,    1.800 ,    1.700 ,    1.500 ,    1.300 ,
     *      .0 ,       .0 ,       .0 ,     .200 ,     .500 ,
     *    .800 ,    1.300 ,    2.100 ,    3.400 ,    4.700 ,
     *   6.400 ,    6.800 ,    6.200 ,    5.200 ,    7.100 ,
     *   8.000 ,    8.900 ,    9.800 ,   10.300 ,   10.500 ,
     *  10.600 ,    9.900 ,    7.700 ,    5.500 ,    3.400 ,
     *   2.800 ,    2.700 ,    2.600 ,    2.200 ,    1.900   /
      DATA SIGMA8 /
     *      .0 ,     .100 ,     .500 ,    1.200 ,    3.000 ,
     *   4.200 ,    5.000 ,    5.200 ,    6.100 ,    7.900 ,
     *   8.900 ,    9.600 ,    9.900 ,   10.000 ,   10.100 ,
     *  10.100 ,   10.000 ,    9.800 ,    9.500 ,    8.800 ,
     *   8.000 ,    6.800 ,    5.900 ,    5.200 ,    4.200 ,
     *    2.900  ,  2.700  ,  2.500  ,  2.200  ,  1.900  ,
     *      .0 ,       .0 ,       .0 ,     .200 ,     .700 ,
     *   1.400 ,    3.000 ,    4.900 ,    5.100 ,    5.000 ,
     *   4.400 ,    3.600 ,    3.100 ,    2.700 ,    2.400 ,
     *   2.200 ,    2.000 ,    1.700 ,    1.600 ,    1.400 ,
     *   1.300 ,    1.100 ,     .900 ,     .800 ,     .600 ,
     *    .600 ,     .600 ,     .500 ,     .400 ,     .300   /
      DATA SIGMA9 /
     *     246.,     244.8,  243.7,   242.1,   239.5,
     *     238.5,    199.7,  188.0,   156.5,   137.4,
     *     128.0,    123.0,  115.6,   108.3,   100.0,
     *      94.5,     91.4,   87.4,    83.2,   79.5,
     *     76.7,     73.7,   68.8,    58.3,    51.7,
     *     46.1,     45.6,   45.0,    43.9,    43.0,
     *     79.0,    77.2,    76.8,    76.3,    75.5,
     *     74.7,    71.1,    68.7,    51.7,    53.4,
     *     49.3,    47.2,    44.5,    42.1,    37.5,
     *     35.7,    35.4,    30.8,    28.6,    25.6,
     *     23.7,    20.6,    16.0,    13.8,    11.6,
     *      8.67,    8.50,    7.17,    8.20,    7.30/
      DATA  ENERG1/
     *      0.0 ,    0.010 ,    0.020 ,    0.030 ,    0.040 ,
     *    .050 ,     .070 ,     .100 ,     .150 ,     .200 ,
     *    .250 ,     .300 ,     .350 ,     .400 ,     .500 ,
     *    .650 ,     .850 ,     .950 ,    1.100 ,    1.300 ,
     *   1.500 ,    2.000 ,    3.000 ,    4.000 ,    5.000 ,
     *   7.000 ,   10.000 ,   16.000 ,   22.000 ,   30.000 ,
     *      .0 ,     .010 ,     .020 ,     .030 ,     .040 ,
     *    .050 ,     .075 ,     .100 ,     .150 ,     .175 ,
     *    .200 ,     .225 ,     .250 ,     .350 ,     .450 ,
     *    .500 ,     .600 ,     .700 ,     .800 ,     .850 ,
     *    .900 ,    1.000 ,    1.200 ,    1.300 ,    1.400 ,
     *   1.600 ,    3.000 ,    4.000 ,   10.000 ,   20.000 /
      DATA ENERG2 /
     *    .200 ,     .250 ,     .300 ,     .350 ,     .400 ,
     *    .450 ,     .500 ,     .550 ,     .600 ,     .650 ,
     *    .700 ,     .750 ,     .800 ,     .850 ,     .900 ,
     *    .950 ,    1.000 ,    1.100 ,    1.200 ,    1.300 ,
     *   1.400 ,    1.600 ,    1.800 ,    2.000 ,    2.400 ,
     *   2.600 ,    2.800 ,    3.000 ,    3.500 ,    4.000 ,
     *    0.0,      0.01,     0.02,      0.03,      0.04,
     *    0.049,    0.084,    0.10,      0.150,     0.226,
     *    0.288,    0.335,    0.426,     0.609,     0.831,
     *    1.100,    1.226,    1.408,     1.667,     1.967,
     *    2.205,    2.830,    3.756,     6.420,    11.098,
     *   24.079,   31.075,   39.07,     49.071,    69.0/
      END
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      BLOCK DATA BDAM7
      IMPLICIT REAL*8 (A-H,O-Z)
C  COEFICIENTS FOR CALCULATION OF ANGULAR DISTRIBUTION
      COMMON/COEF3/ ANKJ(4,4,13)
      COMMON/TABLE1/ B12(30,3),ENER(30)
      DIMENSION ANKJ1(4,4,3),ANKJ2(4,4,3),ANKJ3(4,4,3),ANKJ4(4,4,4)
      EQUIVALENCE(ANKJ(1,1,1),ANKJ1(1,1,1))
      EQUIVALENCE(ANKJ(1,1,4),ANKJ2(1,1,1))
      EQUIVALENCE(ANKJ(1,1,7),ANKJ3(1,1,1))
      EQUIVALENCE(ANKJ(1,1,10),ANKJ4(1,1,1))
      DATA  ANKJ1 /
     * 2.7404E 00 , -9.6998E 00 ,  1.0400E 01 ,  2.3882E 00 ,
     *-7.5137E 00 ,  4.4096E 01 , -7.4379E 01 ,  4.6038E 01 ,
     * 7.5479E 00 , -3.9274E 01 ,  6.4835E 01 , -4.1609E 01 ,
     *-1.8369E 00 ,  8.6911E 00 , -1.3060E 01 ,  7.1880E 00 ,
     *-3.0853E 01 ,  1.0624E 02 , -1.2939E 02 ,  5.4339E 01 ,
     * 1.9465E 01 , -6.8102E 01 ,  9.6358E 01 , -5.6827E 01 ,
     *-3.4831E 00 ,  1.2341E 01 , -1.8592E 01 ,  1.2024E 01 ,
     * 1.8941E-01 , -6.7880E-01 ,  1.0665E 00 , -7.2910E-01 ,
     * 1.0258E-01 , -1.0542E 00 ,  1.1389E 01 , -1.6638E 01 ,
     *-4.9607E-01 ,  1.1800E 01 , -9.0857E 01 ,  1.6476E 02 ,
     * 1.5437E 00 , -3.3769E 01 ,  2.5192E 02 , -4.5071E 02 ,
     *-1.2021E 00 ,  2.5336E 01 , -1.8658E 02 ,  3.3254E 02  /
      DATA  ANKJ2 /
     * 1.5789E-01 ,  2.9671E 00 , -5.5251E 00 ,  6.8925E 00 ,
     *-7.0218E 00 , -2.0534E 02 ,  5.6951E 02 , -8.9858E 02 ,
     * 1.3496E 02 ,  4.8722E 03 , -1.4674E 04 ,  2.3924E 04 ,
     *-8.2116E 02 , -3.2586E 04 ,  1.0098E 05 , -1.6553E 05 ,
     * 3.1531E-01 , -7.4981E 00 ,  4.3295E 01 , -7.6360E 01 ,
     *-6.5373E 00 ,  1.9307E 02 , -1.0181E 03 ,  1.7426E 03 ,
     * 4.6864E 01 , -1.3030E 03 ,  6.7291E 03 , -1.1075E 04 ,
     *-9.5192E 01 ,  2.6373E 03 , -1.2857E 04 ,  2.0294E 04 ,
     *-1.7953E 01 ,  1.0972E 02 , -2.3954E 02 ,  2.2826E 02 ,
     * 9.1968E 01 , -5.1963E 02 ,  1.1266E 03 , -1.0740E 03 ,
     *-1.3270E 02 ,  7.4112E 02 , -1.6000E 03 ,  1.5249E 03 ,
     * 5.8598E 01 , -3.1874E 02 ,  6.7751E 02 , -6.4011E 02  /
      DATA  ANKJ3 /
     * 4.2169E-01 ,  1.4705E 02 , -6.5335E 02 ,  9.1507E 02 ,
     *-3.5198E 00 , -2.6019E 02 ,  1.2250E 03 , -1.7481E 03 ,
     * 3.6373E 00 ,  1.5592E 02 , -7.5201E 02 ,  1.0796E 03 ,
     *-7.8041E-01 , -3.0563E 01 ,  1.4795E 02 , -2.1250E 02 ,
     *-3.8288E-01 ,  3.7587E 00 , -6.5144E 00 ,  6.7740E 00 ,
     * 1.0381E 02 , -2.7282E 02 ,  4.7759E 02 , -5.1222E 02 ,
     *-1.7882E 03 ,  4.3052E 03 , -7.9314E 03 ,  9.3471E 03 ,
     * 7.1475E 03 , -3.3395E 03 , -4.1392E 03 , -4.4364E 03 ,
     * 2.4991E-01 ,  3.2028E 01 , -1.1882E 02 ,  1.5099E 02 ,
     *-2.6994E 00 , -4.6045E 02 ,  1.8959E 03 , -2.5190E 03 ,
     * 1.6268E 01 ,  2.1384E 03 , -9.1262E 03 ,  1.2431E 04 ,
     *-2.9654E 01 , -3.1823E 03 ,  1.3944E 04 , -1.9342E 04  /
      DATA  ANKJ4 /
     *3.9025E 00 , -9.1126E 01 ,  3.2373E 02 , -4.0048E 02 ,
     *-2.0619E 01 ,  4.9170E 02 , -1.7155E 03 ,  2.1143E 03 ,
     * 3.3004E 01 , -7.6684E 02 ,  2.7003E 03 , -3.3525E 03 ,
     *-1.6367E 01 ,  3.7394E 02 , -1.3202E 03 ,  1.6423E 03 ,
     * 1.9402E 01 , -2.2446E 02 ,  7.4733E 02 , -9.3570E 02 ,
     *-4.4180E 01 ,  4.7194E 02 , -1.4856E 03 ,  1.8055E 03 ,
     * 3.1567E 01 , -3.0176E 02 ,  9.0763E 02 , -1.0773E 03 ,
     *-6.8648E 00 ,  6.0476E 01 , -1.7520E 02 ,  2.0381E 02 ,
     * 1.4988E-01 ,  2.8753E 00 , -5.3078E 00 ,  6.2233E 00 ,
     *-5.9558E 00 , -1.6203E 02 ,  4.3079E 02 , -6.2548E 02 ,
     * 1.2875E 02 ,  3.1402E 03 , -7.9189E 03 ,  1.0983E 04 ,
     *-8.5161E 02 , -1.8780E 04 ,  4.4607E 04 , -5.8790E 04 ,
     * 5.3689E-01 , -1.3216E 01 ,  8.1011E 01 , -1.4285E 02 ,
     *-1.0550E 01 ,  2.9629E 02 , -1.6957E 03 ,  2.8935E 03 ,
     * 6.9621E 01 , -1.9245E 03 ,  1.0620E 04 , -1.7468E 04 ,
     *-1.3865E 02 ,  3.9281E 03 , -2.0293E 04 ,  3.2058E 04 /
      DATA B12 /
     *1.13,    1.52,    1.67,    1.94,    2.45,
     *2.70,    3.00,    3.54,    4.14,    4.46,
     *4.71,    5.99,    7.04,    7.58,    8.57,
     *9.37,   10.06,   10.87,   11.12,   12.27,
     *13.55,   14.73,   15.32,   15.09,   15.33,
     *17.50,   18.84,   29.66,   53.06,   84.33,
     *0.0,     0.0,     0.0,      0.0,     0.0,
     *0.0,     0.0,     0.0,      0.18,    0.21,
     *0.23,    0.30,    0.35,     0.38,    0.429,
     *0.469,   0.503,   0.544,    0.56,    0.61,
     *0.677,   0.736,   0.77,     0.80,    0.851,
     *0.875,   1.050,   3.707,    5.97,    7.028,
     *0.0,     0.0,     0.0,     -0.90,   -0.80,
     *-0.68,   -0.53,   -0.37,   -0.19,   -0.02,
     *0.11,    0.21,    0.27,     0.30,    0.34,
     *0.40,    0.44,    0.48,     0.50,    0.54,
     *0.57,    0.60,    0.61,     0.63,    0.65,
     *0.67,    0.75,    0.75,    0.906,    0.913/
      DATA ENER /
     *0.020,    0.040,    0.050,    0.063,    0.083,
     *0.110,    0.137,    0.175,    0.226,    0.288,
     *0.334,    0.425,    0.499,    0.538,    0.608,
     *0.664,    0.713,    0.771,    0.830,    0.916,
     *1.011,   1.100,    1.144,    1.189,    1.279,
     *1.379,   1.572,    2.203,    4.840,    7.115/
      END

c
c      ANKJ(N,K,3)
c     * 1.0258E-01 , -1.0542E 00 ,  1.1389E 01 , -1.6638E 01 ,
c     *-4.9607E-01 ,  1.1800E 01 , -9.0857E 01 ,  1.6476E 02 ,
c     * 1.5437E 00 , -3.3769E 01 ,  2.5192E 02 , -4.5071E 02 ,
c     *-1.2021E 00 ,  2.5336E 01 , -1.8658E 02 ,  3.3254E 02  /
c      C(1,1)= 1.0258E-01
c      C(2,1)=-1.0542E 00
c      C(3,1)= 1.1389E 01
c      C(4,1)=-1.6638E 01
c
c      C(1,2)=-4.9607E-01
c      C(2,2)= 1.1800E 01
c      C(3,2)=-9.0857E 01
c      C(4,2)= 1.6476E 02
c
c      C(1,3)= 1.5437E 00
c      C(2,3)=-3.3769E 01
c      C(3,3)= 2.5192E 02
c      C(4,3)=-4.5071E 02
c
c      C(1,4)=-1.2021E 00
c      C(2,4)= 2.5336E 01
c      C(3,4)=-1.8658E 02
c      C(4,4)= 3.3254E 02
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      BLOCK DATA BDAM8
      IMPLICIT REAL*8 (A-H,O-Z)
C        INITIALIZE ALL COMMON BLOCKS
C
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      COMMON/FORCE/ NFORCE,IFORCE(20),MFORCE(5,20)
C     LOOK MUST BE DIMENSIONED TO THE MAXIMUM VALUE OF INDEX
      COMMON/DKYTAB/LOOK(400),CBR(600),MODE(5,600)
      COMMON/IDRUN/IDVER,IDG(2),IEVT
      LOGICAL NODCAY,NOETA,NOPI0,NONUNU,NOEVOL,NOHADR,NOKA0
      COMMON/NODCAY/NODCAY,NOETA,NOPI0,NONUNU,NOEVOL,NOHADR,NOKA0
      COMMON/PRIMAR/SCM,HALFE,ECM,NJET,IDIN(2),NEVENT,NTRIES
      COMMON/PRTOUT/NEVPRT,NJUMP
      LOGICAL KEYS,KEYON
      COMMON/KEYS/IKEYS,KEYON,KEYS(20)
      COMMON/MASQUA/ AMQ21,AMQ22
      COMMON/QLMASS/ AMLEP(52),NQLEP,NMES,NBARY
      LOGICAL GH1H2
      COMMON/H1H2/ GH1H2(11)
      COMMON/COMNC/ NOCOL
      LOGICAL NOCOL
      COMMON/COMMUL/ MULTP
      LOGICAL MULTP
      COMMON/NOTRE/ NOTRE
      LOGICAL NOTRE
      COMMON /PRINTS/ IPRINT
      LOGICAL IPRINT
      COMMON/PRIMP0/ P0
      COMMON/PRIMPL/ PL
      COMMON/COMRII/ R12,R1,R2
      COMMON/NUMCO/ NELA,NINEL
      COMMON/NEEDR/ NRET
      COMMON/COMASS/ AM1,AM2
      COMMON/COMXM/ XMIN,XMAX
      COMMON/BEES/ B,BFOR
      COMMON/CSIGA/ SIGAN
      COMMON/CSIGSU/ SIGSUM
      COMMON/WCON/SIN2W,WMASS(4),WGAM(4),AQ(12,4),BQ(12,4),COUT(4),
     1MATCH(25,4),WCBR(25,4),CUTOFF,CUTPOW,TBRWW(4,2),RBRWW(12,4,2),EZ,
     2AQDP(12,4),BQDP(12,4),EZDP
      COMMON/COMIND/ PUD,SIGMA,ALFA,BETA
      COMMON/COMTRE/ XMI,PTI,PTF,PTMAX,NSTART
      COMMON/COMANN/ DIQAN
      COMMON/TIMES/ TIME1,TIME2
      LOGICAL DIQAN
      COMMON/LSTPRT/ LSTPRT
      COMMON/COMWTI/ WTIME
      COMMON/COMECB/ ECMB
      COMMON/COMENB/ ENBOU
      COMMON/COMFR/ ICMS
      COMMON/HARDSC/ HARDSC
      LOGICAL HARDSC
      COMMON/KEYHH/ KEYHH
      LOGICAL KEYHH
      LOGICAL WTIME
C
C  DATA FOR COMECB
      DATA ECMB/30./
C
C  DATA FOR COMENB
      DATA ENBOU/4.4/
C
C
C  DATA FOR COMWTI
      DATA WTIME/.FALSE./
C
C  DATA FOR TIMES
      DATA TIME1,TIME2/2*0./
C
C
C  DATA FOR FORCE
      DATA NFORCE/0/,IFORCE/20*0/,MFORCE/100*0/
C
C       DATA FOR MASQUA
      DATA AMQ21/0./,AMQ22/0./
C
C       DATA FOR H1H2
      DATA GH1H2/11*.TRUE./
C
C          DATA FOR IDRUN
C          IDVER=100*VERSION+CYCLE
      DATA IDVER/102/,IDG/2*0/,IEVT/0/
C
C          DATA FOR ITAPES
      DATA ITDKY,ITEVT,ITCOM,ITLIS/18,17,15,16/
C          DATA FOR LSTPRT
      DATA LSTPRT/0/
C          DATA FOR NODCAY
      DATA NODCAY/.FALSE./,NOETA/.FALSE./,NOKA0/.TRUE./
      DATA NOPI0,NONUNU,NOEVOL,NOHADR/4*.TRUE./
C
C          DATA FOR NOTRE
      DATA NOTRE/.TRUE./
C
C          DATA FOR PRINTS
      DATA IPRINT/.FALSE./
C
C          DATA FOR PRIMAR
      DATA IDIN/1120,1120/,HALFE/10./,SCM/400./,ECM/20./
      DATA NTRIES/ 20/,NJET/2/,NEVENT/13/
C
C          DATA FOR DKYTAB
      DATA LOOK/400*0/,CBR/600*0./,MODE/3000*0/
C
C
C            DATA FOR PRTOUT
      DATA NEVPRT,NJUMP/1000000,1/
C
C          DATA FOR KEYS
      DATA KEYS/20*.FALSE./,KEYON/.FALSE./,IKEYS/0/
C
C         DATA FOR QLMASS
C         AMLEP LABELED BY INDEX...SEE FLAVOR
C         SETW RESETS W+- AND Z0 MASSES
C     DATA AMLEP/.3,.3,.5,1.6,4.9,30.,-1.,-1.,0.,0.,
      DATA AMLEP/.14,.14,.45,1.6,4.9,30.,-1.,-1.,0.,0.,
     *0.,.511003E-3,0.,.105661,0.,1.807,3*-1.,.49767,.49767,
     *100.3,100.3,100.5,101.6,104.9,130.,2*-1.,100.,0.,
     *100.,100.005,100.,100.1,100.,101.8,2*-1.,100.,100.,
     *11*0./
      DATA NQLEP,NMES,NBARY/41,2,2/
C
C  DATA FOR COMTRE
      DATA NSTART/0/,XMI/-1./,PTI/0./,PTF/5./,PTMAX/5.0/
C
C  DATA FOR COMIND
      DATA PUD/0.492/,SIGMA/0.60/,ALFA/0.5/,BETA/2.5/
C BELOW THE DUBNA PARAMETER SET IS GIVEN
C     DATA PUD/0.425/,SIGMA/0.51/,ALFA/0.5/,BETA/2.5/
C
C  DATA FOR COMNC
      DATA NOCOL/.TRUE./
C
C  DATA FOR COMMUL
      DATA MULTP/.TRUE./
C
C
C  DATA FOR COMANN
      DATA DIQAN/.FALSE./
C
C  DATA FOR PRIMP0
      DATA P0/7.0/
C
C  DATA FOR PRIMPL
      DATA PL/32.0/
C
C  DATA FOR COMRII
      DATA R12/0./
C
C  DATA FOR NUMCO
      DATA NELA,NINEL/2*0/
C
C  DATA FOR NEEDR
      DATA NRET/0/
C
C  DATA FOR COMASS
      DATA AM1,AM2/2*0.93828/
C
C  DATA FOR COMXM
      DATA XMIN/0./,XMAX/1.0/
C
C
C  DATA FOR BEES
      DATA B/11.0/,BFOR/11.0/
C
C  DATA FOR CSIGSU
      DATA SIGSUM/0./
C
C  DATA FOR WCON
      DATA SIN2W/0./,WMASS/4*0./,WGAM/4*0./,AQ/48*0./,BQ/48*0./
      DATA COUT/4*0./,WCBR/100*0./
      DATA TBRWW/8*0./,AQDP/48*0./,BQDP/48*0./,EZDP/0./
C     DATA MATCH/100*0/,CUTOFF/0./,CUTPOW/0./
C     NEW     DATA FOR WCON
      DATA MATCH/
     $0,3,2,5,4,7,6,9,8,11,10,13,12,0,0,17,16,0,0,21,20,0,0,25,24,
     $0,5,0,0,2,0,8,7,0,0,12,11,0,17,0,0,14,21,0,0,18,25,0,0,22,
     $0,0,4,3,0,9,0,0,6,13,0,0,10,0,16,15,0,0,20,19,0,0,24,23,0,
     $0,3,2,5,4,7,6,9,8,11,10,13,12,15,14,17,16,19,18,21,20,23,22,25,24/
      DATA CUTOFF,CUTPOW/.200,1.0/
C
C  DATA FOR HARDSC
      DATA HARDSC/.FALSE./
C     DATA HARDSC/.TRUE./
C
C  DATA FOR KEYHH
      DATA KEYHH/.FALSE./
C
C  DATA FOR COMFR
      DATA ICMS/1/
      END


