//************************
// Magnet (Yoke and Cryostat)
// version 24-Mar-2009 (Lx=Ly=5510,Lz1=5400,Lz2=6300 mm) litvin
//************************

//************************************************
//   Yoke (barrel and endcaps):
//************************************************
ms01yokebarrel
cave
PGON
iron
2
15 360 12
-2700. 2305. 2755.
 2700. 2305. 2755.

0. 0. 0.
1. 0. 0. 0. 1. 0. 0. 0. 1.
//************************
ms01yokeendin#1
cave
PGON
iron
2
15. 360. 12
2600. 800. 1800. 
2700. 800. 1800. 
0. 0. 0.
1. 0. 0. 0. 1. 0. 0. 0. 1.
//************************
ms01yokeendout#1
cave
PGON
iron
2
15 360 12
2700. 800.  2755. 
3150. 800.  2755. 
0. 0. 0.
1. 0. 0. 0. 1. 0. 0. 0. 1.
//************************
ms01yokeendin#2
cave
0. 0. 0.
1. 0. 0. 0. 1. 0. 0. 0. -1.
//************************
ms01yokeendout#2
cave
0. 0. 0.
1. 0. 0. 0. 1. 0. 0. 0. -1.
//************************

//************************************************
//   Cryostat (steel width 5mm and some copper solenoid inside):
//************************************************
ms01cryostat
cave
PGON
vacuum
2
15 360 12
-2700. 2000. 2200. 
 2700. 2000. 2200. 

0. 0. 0.
1. 0. 0. 0. 1. 0. 0. 0. 1.
//************************
ms01solenoid 
ms01cryostat
PGON
copper
2
15 360 12
-2690. 2010. 2190. 
 2690. 2010. 2190. 

0. 0. 0.
1. 0. 0. 0. 1. 0. 0. 0. 1.
//************************
ms01cryostatbarrelin 
ms01cryostat
PGON
steel
2
15 360 12
-2695. 2000. 2005. 
 2695. 2000. 2005. 

0. 0. 0.
1. 0. 0. 0. 1. 0. 0. 0. 1.
//************************
ms01cryostatbarrelout 
ms01cryostat
PGON
steel
2
15 360 12
-2695. 2195. 2200. 
 2695. 2195. 2200. 

0. 0. 0.
1. 0. 0. 0. 1. 0. 0. 0. 1.
//************************
ms01cryostatendcap#1
ms01cryostat
PGON
steel
2
15 360 12
2695. 2000. 2200. 
2700. 2000. 2200. 

0. 0. 0.
1. 0. 0. 0. 1. 0. 0. 0. 1.
//************************
ms01cryostatendcap#2
ms01cryostat
0. 0. 0.
1. 0. 0. 0. 1. 0. 0. 0. -1.
//************************

//************************
