/*
 * MpdSharedPadsCut.cxx
 *
 *  Created on: 30 paź 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdPairSharedPadsCut.h"

#include "NicaMpdTrackTpcPads.h"
#include "NicaTpcTrack.h"
MpdPairSharedPadsCut::MpdPairSharedPadsCut() : NicaTwoTrackCut(2){
	SetUnitName("Overlap Pads [%]",OverlappedPads());
	SetUnitName("SharedPads [%]",SharedPads());
	SetMinMax(0,100,OverlappedPads());
	SetMinMax(0, 100,SharedPads());
}

Bool_t MpdPairSharedPadsCut::Pass(NicaTwoTrack* pair) {
	NicaMpdTrackTpcPads *track1 = (NicaMpdTrackTpcPads*)pair->GetTrack1();
	NicaMpdTrackTpcPads *track2 = (NicaMpdTrackTpcPads*)pair->GetTrack2();
	track1->CalculatePads();
	track2->CalculatePads();
	const Double_t pads1 = track1->GetPadsNo();
	const Double_t pads2 = track2->GetPadsNo();
	const Int_t start1_pad = track1->GetFirstGoodPad();
	const Int_t start2_pad = track2->GetFirstGoodPad();
	const Int_t	end1_pad = track1->GetFirstBadPad();
	const Int_t end2_pad = track2->GetFirstBadPad();
	Double_t maxPads  = TMath::Max(end1_pad,end2_pad)-TMath::Min(start1_pad,start2_pad);
	const Int_t first_common_pad = TMath::Max(start1_pad,start2_pad);
	const Int_t last_common_pad = TMath::Min(end1_pad,end2_pad);
	Double_t overlapPads = 0;
	Double_t sharedPads = 0;
	ULong64_t map1 = track1->GetHitMap();
	ULong64_t map2 = track2->GetHitMap();
	for(int iLay=first_common_pad;iLay<last_common_pad;iLay++){
		Int_t pad1_id = track1->GetPadID(iLay);
		Int_t pad2_id = track2->GetPadID(iLay);
		if(pad1_id==pad2_id){
			overlapPads++;
			if(TESTBIT(map1,iLay)&&TESTBIT(map2,iLay)){// have also hit in this rows
				sharedPads++;
			}
		}
	}
	SetValue(100.0*overlapPads/maxPads,OverlappedPads());
	SetValue(100.0*sharedPads/maxPads,SharedPads());
	return Validate();
}

MpdPairSharedPadsCut::~MpdPairSharedPadsCut() {
	// TODO Auto-generated destructor stub
}

