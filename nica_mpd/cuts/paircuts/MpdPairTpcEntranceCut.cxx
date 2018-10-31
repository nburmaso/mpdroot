/*
 * MpdTpcEntranceCut.cxx
 *
 *  Created on: 30 paź 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdPairTpcEntranceCut.h"

MpdPairTpcEntranceCut::MpdPairTpcEntranceCut():MpdNominalTpcPairDistanceCut(1) {
	SetUnitName("TpcNominalEntrance [cm]");
	SetMinMax(0,1E+5);
}

Bool_t MpdPairTpcEntranceCut::Pass(NicaTwoTrack* pair) {
	InitPass(pair);
	if(fFirstCommonPad==0){
		SetValue(GetDistance(0));
	}else{
		SetValue(TMath::TwoPi()*40);
	}
	return Validate();
}

MpdPairTpcEntranceCut::~MpdPairTpcEntranceCut() {
}

