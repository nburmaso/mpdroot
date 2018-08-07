/*
 * MpdRejectSPlittedPairsCut.cxx
 *
 *  Created on: 28 lip 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdSplittedPairsCut.h"

MpdSplittedPairsCut::MpdSplittedPairsCut() :NicaTwoTrackCut(1){
	fReject = kTRUE;
}

Bool_t MpdSplittedPairsCut::Pass(NicaTwoTrack* pair) {
	NicaComplexTrack *track1 = (NicaComplexTrack*)pair->GetTrack1();
	NicaComplexTrack *track2 = (NicaComplexTrack*)pair->GetTrack2();
	if(track1->GetMatchID()==track2->GetMatchID()){
		std::cout<<!fReject<<"\""<<track1->GetMatchID()<<std::endl;
		return ForcedUpdate(!fReject);
	}else{
		return ForcedUpdate(fReject);
	}
}

MpdSplittedPairsCut::~MpdSplittedPairsCut() {
	// TODO Auto-generated destructor stub
}

