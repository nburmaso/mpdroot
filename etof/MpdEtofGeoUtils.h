//------------------------------------------------------------------------------------------------------------------------
#ifndef __MPD_ETOF_GEO_UTILS_H
#define __MPD_ETOF_GEO_UTILS_H 1

#include <TVector3.h>
#include <TList.h>

#include "IntervalTree.h"
#include "MpdTofGeoUtils.h"

//------------------------------------------------------------------------------------------------------------------------
class TH1D;
class TH2D;
class MpdEtofGeoUtils 
{	
	MStripType			mStrips; 		//! indexing strips by detectorUID			
	intervalTreeType		mDetectorsR[2]; 	//! detectors R[cm] location interval tree, two sides
	intervalTreeType		mDetectorsPhi[2];	//! detectors Phi[rads] location interval tree, two sides	
	
	MpdEtofGeoUtils();
	static	MpdEtofGeoUtils *instance;
	
public:
	static MpdEtofGeoUtils*  Instance(){ if(instance == nullptr) instance = new MpdEtofGeoUtils; return instance;}
	
	void			FindNeighborStrips(Double_t thresh, TH1D* h1 = nullptr, TH2D* h2 = nullptr, bool forced = false); // thresh, [cm] <--- thresh. distance between neighbor strips, (see h1TestDistance histo)
	void			ParseTGeoManager(bool useMCinput, TH2D* h1 = nullptr, bool forced = false);
	const LStrip*		FindStrip(Int_t UID);
	
	const intervalTreeType*	GetDetR() { return mDetectorsR; };
	const intervalTreeType*	GetDetPhi() { return mDetectorsPhi; };	
};
//------------------------------------------------------------------------------------------------------------------------
#endif

