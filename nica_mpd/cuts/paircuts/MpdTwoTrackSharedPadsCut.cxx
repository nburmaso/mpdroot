/*
 * MptTwoTrackSharedPadsCut.cxx
 *
 *  Created on: 28 gru 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdTwoTrackSharedPadsCut.h"
#include "NicaExpTrack.h"
#include "NicaMpdTrack.h"

MpdTwoTrackSharedPadsCut::MpdTwoTrackSharedPadsCut() {
	fSec = MpdTpcSectorGeo::Instance();
	fHelix1 = new NicaHelix();
	fHelix1->SetMagFiled(0.5);
	fHelix2 = new NicaHelix();
	fHelix2->SetMagFiled(0.5);
}

Bool_t MpdTwoTrackSharedPadsCut::Pass(NicaTwoTrack* pair) {
	NicaMpdTrack *track1 = (NicaMpdTrack*)pair->GetTrack1();
	NicaMpdTrack *track2 = (NicaMpdTrack*)pair->GetTrack2();
	TVector3 *start1 = track1->GetDCA();
	TVector3 *start2 = track2->GetDCA();
	start1->SetXYZ(0,0,0);
	start2->SetXYZ(0,0,0);
	TVector3 mom1 = track1->GetMomentum()->Vect();
	TVector3 mom2 = track2->GetMomentum()->Vect();

	fHelix1->SetParams(*start1,mom1,track1->GetCharge());
	fHelix2->SetParams(*start2,mom2,track2->GetCharge());
	Double_t R_min = fSec->GetMinY();
	Double_t padH1 = fSec->PadHeight(0);
	Double_t padH2 = fSec->PadHeight(1);
	Double_t R = 0;
	Double_t x,y;
	Int_t Sec1,Sec2;
	TVector3 glob,loc;
	Double_t shared = 0;
	Double_t total = 0;
	for(double i=0;i<fSec->NofRowsReg(0);i++){
		R = R_min + i*padH1+0.5*padH1;
		Overlap(fHelix1,R,x,y);
		if(x==0&&y==0) break;
		glob.SetXYZ(x,y,0);//z is not important
		Sec1 = fSec->Global2Local(glob,loc,-1);
		Overlap(fHelix2,R,x,y);
		if(x==0&&y==0) break;
		glob.SetXYZ(x,y,0);
		Sec2 = fSec->Global2Local(glob,loc,-1);
		if(Sec1==Sec2) shared++;
		Int_t row1 = fSec->PadRow(Sec1);
		Int_t pad1 = fSec->Sector(Sec1);
		Int_t row2 = fSec->PadRow(Sec2);
		Int_t pad2 = fSec->Sector(Sec2);
		/*
		if(Sec1!=-1){
			std::cout<<"TR1\t"<<row1<<"\t"<<pad1<<"\t\t"<<Sec1<<std::endl;
		}
		if(Sec2!=-1){
			std::cout<<"TR2\t"<<row2<<"\t"<<pad2<<"\t\t"<<Sec2<<std::endl;
			std::cout<<Form("%4.1f \t%4.1f",x,y)<<std::endl;\
		}*/
		total++;
	}
	R_min = fSec->GetRocY(1);
	for(double i =0;i<fSec->NofRowsReg(1);i++){
		R = R_min + i*padH2+0.5*padH2;
		Overlap(fHelix1,R,x,y);
		if(x==0||y==0) break;
		glob.SetXYZ(x,y,0);//z is not important
		Sec1 = fSec->Global2Local(glob,loc,-1);
		Overlap(fHelix2,R,x,y);
		if(x==0||y==0) break;
		glob.SetXYZ(x,y,0);
		Sec2 = fSec->Global2Local(glob,loc,-1);
		if(Sec1==Sec2) shared++;
		total++;
	}
	SetValue(shared/total);
	return Validate();

}

void MpdTwoTrackSharedPadsCut::Overlap(NicaHelix* helix, Double_t R,
		Double_t& x, Double_t& y) {
	Double_t s1,s2;
	helix->PathLength(R, s1, s2); //to m
	Double_t s = TMath::Min(s1,s2);
	TVector3 pos = helix->Evaluate(s);
	x = pos.X();
	y = pos.Y();
}

MpdTwoTrackSharedPadsCut::~MpdTwoTrackSharedPadsCut() {
	delete fHelix1;
	delete fHelix2;
}

