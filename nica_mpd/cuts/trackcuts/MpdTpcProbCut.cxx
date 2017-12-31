/*
 * MpdTpcProbCut.cxx
 *
 *  Created on: 12 kwi 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdTpcProbCut.h"
#include "NicaMpdTrack.h"

MpdTpcProbCut::MpdTpcProbCut() :NicaTrackCut(4){
	SetUnitName("P(#pi)",PidPion());
	SetUnitName("P(p)",PidProton());
	SetUnitName("P(K)",PidKaon());
	SetUnitName("P(e)",PidElectron());
}

Bool_t MpdTpcProbCut::Pass(NicaTrack* track) {
	NicaMpdTrack *tr = (NicaMpdTrack*)track;
	SetValue(tr->GetPidProbElectron(),PidElectron());
	SetValue(tr->GetPidProbPion(),PidPion());
	SetValue(tr->GetPidProbKaon(),PidKaon());
	SetValue(tr->GetPidProbProton(),PidProton());
	return Validate();
}

Bool_t MpdTpcProbCut::Init(Int_t task_id) {
	return kTRUE;
}

MpdTpcProbCut::~MpdTpcProbCut() {
	// TODO Auto-generated destructor stub
}

