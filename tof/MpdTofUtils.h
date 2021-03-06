//--------------------------------------------------------------------------------------------------------------
#ifndef __HH_MPDTOFUTILS
#define __HH_MPDTOFUTILS 1
 
#include <Rtypes.h> // bit manipulation 
//--------------------------------------------------------------------------------------------------------------
class MpdTofUtils
{
public:
	enum k_side 		{ Up=0, Right, Down, Left, Absent= -1 };
	enum k_HitType 		{ IsSingle = BIT(0), IsDouble = BIT(1), IsTriple = BIT(2), HaveTail = BIT(3), InCluster = BIT(4), McAbsent = BIT(5), IsSelected = BIT(12) };
	enum k_LinkType 	{ mcPointIndex = 1, mcTrackIndex = 2, volumeUID = 3, expDigitIndex = 4 }; 
	enum k_DetType 		{ IsBarrel = 1, IsEndcapLeft = 2, IsEndcapRight = 3 }; 
};
#endif
//--------------------------------------------------------------------------------------------------------------
