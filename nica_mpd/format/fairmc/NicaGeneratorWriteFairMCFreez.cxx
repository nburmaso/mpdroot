/*
 * NicaGeneratorWriteFairMCFreez.cxx
 *
 *  Created on: 13 wrz 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaGeneratorWriteFairMCFreez.h"
#include <TLorentzVector.h>
#include "NicaRunSim.h"

NicaGeneratorWriteFairMCFreez::NicaGeneratorWriteFairMCFreez() :NicaGeneratorWriteFairMC(3,8,0,1,3,11,0,0),fFreez(NULL){
	Register(11,"px","py","pz","vx","vy","vz","vt","frx","fry","frz","frt");
}

InitStatus NicaGeneratorWriteFairMCFreez::Init() {
	fFreez = new TClonesArray("TLorentzVector",1000);
	NicaRunSim *mngr = NicaRunSim::Instance();
	mngr->GetTree()->Branch("Freezouts.",&fFreez);
	return NicaGeneratorWriteFairMC::Init();
}

void NicaGeneratorWriteFairMCFreez::ClearEvent() {
	NicaGeneratorWriteFairMC::ClearEvent();
	fFreez->Delete();
}

void NicaGeneratorWriteFairMCFreez::AddParticle() {
	TLorentzVector *freez = (TLorentzVector*)fFreez->ConstructedAt(fTrackCounter);
	freez->SetXYZT(fTrackParameters->GetDouble(8),fTrackParameters->GetDouble(9),fTrackParameters->GetDouble(10),fTrackParameters->GetDouble(11));
	NicaGeneratorWriteFairMC::AddParticle();
}

NicaGeneratorWriteFairMCFreez::~NicaGeneratorWriteFairMCFreez() {
	// TODO Auto-generated destructor stub
}

