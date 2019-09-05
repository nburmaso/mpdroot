/*
 * NicaGeneratorWriteFairMC.cxx
 *
 *  Created on: 14 sie 2015
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "NicaGeneratorWriteFairMC.h"
#include "NicaRunSim.h"

NicaGeneratorWriteFairMC::NicaGeneratorWriteFairMC() :NicaGeneratorWrite(3,8,0,1,3,7,0,0),fTrackCounter(0){
	fTracks = new TClonesArray("FairMCTrack",1000);
	fEventHeader = new FairMCEventHeader();
	//register int
	Register(3,"PrimaryTrackNo","Flag","Nr");
	//register long int
	Register(1,"RunId");
	//register double
	Register(8,"Vx","Vy","Vz","Vt","B","RotX","RotY","RotZ");
	//register track parameters
	Register(3,"pdg","parent","pointN");
	Register(7,"px","py","pz","vx","vy","vz","vt");
}

InitStatus NicaGeneratorWriteFairMC::Init() {
	InitStatus stat = NicaGeneratorWrite::Init();
	NicaRunSim  *manager = NicaRunSim::Instance();
	manager->GetTree()->Branch("MCEventHeader.",&fEventHeader);
	manager->GetTree()->Branch("MCTrack",&fTracks);
	return stat;
}

void NicaGeneratorWriteFairMC::ClearEvent() {
	fTracks->Delete();
	fTrackCounter= 0;
}

void NicaGeneratorWriteFairMC::AddEvent() {
	fEventHeader->SetRunID(fEventParameters->GetLong(1));
	fEventHeader->SetEventID(fEventParameters->GetInt(3));
	fEventHeader->MarkSet(fEventParameters->GetInt(2));
	fEventHeader->SetNPrim(fEventParameters->GetInt(1));
	fEventHeader->SetVertex(fEventParameters->GetDouble(1),fTrackParameters->GetDouble(2),fTrackParameters->GetDouble(3));
	fEventHeader->SetTime(fEventParameters->GetDouble(4));
	fEventHeader->SetB(fEventParameters->GetDouble(5));
	fEventHeader->SetRotX(fEventParameters->GetDouble(6));
	fEventHeader->SetRotY(fEventParameters->GetDouble(7));
	fEventHeader->SetRotZ(fEventParameters->GetDouble(8));
}

void NicaGeneratorWriteFairMC::AddParticle() {
	new ((*fTracks)[fTrackCounter++]) FairMCTrack(
			fTrackParameters->GetInt(1),
			fTrackParameters->GetInt(2),
			fTrackParameters->GetDouble(1),
			fTrackParameters->GetDouble(2),
			fTrackParameters->GetDouble(3),
			fTrackParameters->GetDouble(4),
			fTrackParameters->GetDouble(5),
			fTrackParameters->GetDouble(6),
			fTrackParameters->GetDouble(7),
			fTrackParameters->GetInt(3)
			);
}

NicaGeneratorWriteFairMC::~NicaGeneratorWriteFairMC() {
	// TODO Auto-generated destructor stub
}

NicaGeneratorWriteFairMC::NicaGeneratorWriteFairMC(Int_t event_par_i,
		Int_t event_par_d, Int_t event_par_s, Int_t event_par_u,
		Int_t track_par_i, Int_t track_par_d, Int_t track_par_s,
		Int_t track_par_u) :NicaGeneratorWrite(event_par_i,event_par_d,event_par_s,event_par_u,track_par_i,track_par_d,track_par_s,track_par_u){
	//NicaGeneratorWrite(3,8,0,1,3,7,0,0)
	if(event_par_i<3||event_par_d<8||event_par_u<1||track_par_i<3||track_par_d<8){
		NicaCout::PrintInfo("Not enought paramterers called in NicaGeneratorWriteFairMC based class",kImportantWarning);
	}
	fTracks = new TClonesArray("FairMCTrack",1000);
	fEventHeader = new FairMCEventHeader();
	fTrackCounter=0;
	//register int
	Register(3,"PrimaryTrackNo","Flag","Nr");
	//register long int
	Register(1,"RunId");
	//register double
	Register(8,"Vx","Vy","Vz","Vt","B","RotX","RotY","RotZ");
	//register track parameters
	Register(3,"pdg","parent","pointN");
	Register(7,"px","py","pz","vx","vy","vz","vt");
}
