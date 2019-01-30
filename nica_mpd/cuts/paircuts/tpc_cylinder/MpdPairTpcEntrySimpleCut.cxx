/*
 * MpdPairTpcEntrySimpleCut.cxx
 *
 *  Created on: 22 lis 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdPairTpcEntrySimpleCut.h"

#include "NicaTpcSectorGeo.h"

MpdPairTpcEntrySimpleCut::MpdPairTpcEntrySimpleCut(): MpdPairTpcSimpleCut(3),
fRmin(40.3){
	SetUnitName("Tpc Entry Sep R [cm]",XYZ());
	SetUnitName("Tpc Entry Sep XY [cm]",XY());
	SetUnitName("Tpc Entry Sep Z [cm]",Z());
	SetMinMax(0,1E+5,0);
	SetMinMax(1,1E+5,0);
	SetMinMax(2,1E+5,0);
}

MpdPairTpcEntrySimpleCut::~MpdPairTpcEntrySimpleCut() {
}

Bool_t MpdPairTpcEntrySimpleCut::Pass(NicaTwoTrack* pair) {
	NicaExpTrack *track1 = (NicaExpTrack*)pair->GetTrack1();
	NicaExpTrack *track2 = (NicaExpTrack*)pair->GetTrack2();
	TVector3 pos1 =PosNominal(track1, fRmin);
	TVector3 pos2 =PosNominal(track2, fRmin);
	pos1-=pos2;
	SetValue(pos1.Pt(),XY());
	SetValue(TMath::Abs(pos1.Z()),Z());
	SetValue(pos1.Mag(),XYZ());
	return Validate();
}

Bool_t MpdPairTpcEntrySimpleCut::Init(Int_t format_id) {
	NicaTpcSectorGeo *geo = NicaTpcSectorGeo::Instance();
	fRmin = geo->GetMinY();
	return NicaTwoTrackCut::Init(format_id);
}
