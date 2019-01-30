/*
 * MpdPairCutModularTpc.cxx
 *
 *  Created on: 22 lis 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdModularTpcPairCut.h"

MpdModularTpcPairCut::MpdModularTpcPairCut(Int_t size) :NicaTwoTrackCut(size){
}

Bool_t MpdModularTpcPairCut::Init(Int_t task_id) {
	return NicaTwoTrackCut::Init(task_id);
}

MpdModularTpcPairCut::~MpdModularTpcPairCut() {
}

