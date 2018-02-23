/*
 * MpdDstWriteTask.cxx
 *
 *  Created on: 23 lut 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdDstCompressTask.h"

#include "FairRunAna.h"
MpdDstCompressTask::MpdDstCompressTask():MpdDstCompressTask("dstwirite",1) {
}

InitStatus MpdDstCompressTask::CheckBranches() {
	FairRootManager *mngr = FairRootManager::Instance();
	fMpdEvent =(MpdEvent*)mngr->GetObject("MPDEvent.");
	if(fMpdEvent==NULL) return kFATAL;
	mngr->Register("MPDEvent.", "MPD", fMpdEvent, kTRUE);
	if(fUseMC){
		fMCTracks = (TClonesArray*)mngr->GetObject("MCTrack");
		if(fMCTracks==NULL){
			LOG(WARNING)<<"MC tracks requested but not found!"<<FairLogger::endl;
			fUseMC=NULL;
		}else{
			mngr->Register("MCTrack", "MC",fMCTracks, kTRUE);
		}
	}
	if(fUseFreezouts){
		fFreezouts = (TClonesArray*)mngr->GetObject("Freezouts.");
		if(fFreezouts==NULL){
			LOG(WARNING)<<"Freeouts tracks requested but not found!"<<FairLogger::endl;
			fUseFreezouts=NULL;
		}else{
			mngr->Register("Freezouts.",  "Freezouts", fFreezouts,kTRUE);
		}
	}
	if(fUseTpcKalmans){
		fTpcKalmans = (TClonesArray*)mngr->GetObject("TpcKalmanTrack");
		if(fTpcKalmans==NULL){
			LOG(WARNING)<<"Kalman TPC tracks requested but not found!"<<FairLogger::endl;
			fUseTpcKalmans=NULL;
		}else{
			mngr->Register("TpcKalmanTrack",  "TPC", fTpcKalmans,kTRUE);
		}
	}
	if(fUseTpcHits){
		fTpcHits = (TClonesArray*)mngr->GetObject("TpcHit");
		if(fTpcHits==NULL){
			LOG(WARNING)<<"TPC hits requested but not found!"<<FairLogger::endl;
			fUseTpcHits=NULL;
		}else{
			mngr->Register("TpcHi.",  "TPC", fTpcHits,kTRUE);
		}
	}
	if(!fUseHeader){
	/*	 TODO when FairROOT will be ugpraded
		FairRunAna::Instance()->SetEventHeaderPersistence(kFALSE);
		*/
	}
	return kSUCCESS;
}

InitStatus MpdDstCompressTask::Init() {
	if(CheckBranches()==kFATAL){
		LOG(FATAL)<<"End of macro MPDEvent not found"<<FairLogger::endl;
		return kFATAL;
	}
	return kSUCCESS;
}

MpdDstCompressTask::MpdDstCompressTask(const char* name, Int_t Verbose):FairTask(name,Verbose) ,
				fUseMC(kFALSE),fUseFreezouts(kFALSE),
				fUseTpcKalmans(kFALSE),fUseTpcHits(kFALSE),fUseHeader(kFALSE),
				fMpdEvent(NULL),fFreezouts(NULL),fMCTracks(NULL),
				fTpcKalmans(NULL),fTpcHits(NULL),fEventHeader(NULL){
}

MpdDstCompressTask::~MpdDstCompressTask() {
	// TODO Auto-generated destructor stub
}

