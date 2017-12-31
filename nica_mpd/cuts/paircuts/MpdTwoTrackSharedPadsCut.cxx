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
	fHelix1 = new MpdNicaHelix();
	fHelix1->SetMagFiled(0.5);
	fHelix2 = new MpdNicaHelix();
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
		if(Sec1!=-1){
			std::cout<<"TR1\t"<<row1<<"\t"<<pad1<<"\t\t"<<Sec1<<std::endl;
		}
		if(Sec2!=-1){
			std::cout<<"TR2\t"<<row2<<"\t"<<pad2<<"\t\t"<<Sec2<<std::endl;
			std::cout<<Form("%4.1f \t%4.1f",x,y)<<std::endl;
		//	std::cout<<"R\t"<<R_min<<" "<<R<<"\t"<<TMath::Sqrt(x*x+y*y)<<std::endl;
		}
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

void MpdTwoTrackSharedPadsCut::Overlap(MpdNicaHelix* helix, Double_t R,
		Double_t& x, Double_t& y) {
	Double_t R2 = TMath::Abs(100.0/helix->GetHelix(4)); //go to cm
	Double_t xc = helix->GetHelix(6)*100;
	Double_t yc = helix->GetHelix(7)*100;
	Double_t r = TMath::Sqrt(xc*xc+yc*yc);
// R1 = R    rc = r  R2 = r
	Double_t cosA = (R*R+r*r-R2*R2)/(2.0*R*r);
	Double_t A = TMath::ACos(cosA);
	Double_t x1,x2,y1,y2;
	Double_t cosa = TMath::Cos(A);
	Double_t sina = TMath::Sin(A);
	if(cosA==1){//one solution
		x = R*(xc*cosa-yc*sina)/r;
		y = R*(xc*sina+yc*cosa)/r;
	}else if(cosA<1){//two solutions
		x1 = R*(xc*cosa-yc*sina)/r;
		y1 = R*(xc*sina+yc*cosa)/r;
		x2 = R*(xc*cosa+yc*sina)/r;
		y2 = R*(yc*cosa-xc*sina)/r;
		Double_t ph1 = helix->GetPhase(x1*0.01,y1*0.01);
		Double_t ph2 = helix->GetPhase(x2*0.01,y2*0.01);
		std::cout<<"p1\t"<<x1<<" "<<y1<<" "<<ph1<<std::endl;
		std::cout<<"p2\t"<<x2<<" "<<y2<<" "<<ph2<<std::endl;
		if(ph1>ph2){
			x = x1;
			y = y1;
		}else{
			x = x2;
			y = y2;
		}
	}else{// no intersection
		x = y = 0;
	}
	/*
	Double_t xc2 = xc*xc;
	Double_t yc2 = yc*yc;
	Double_t rc = TMath::Sqrt(xc2+yc2);
	Double_t a = (r*r-R*R-rc)/(-2.0);
	Double_t delta = TMath::Sqrt(R*R*yc2*yc2-a*a*yc2+R*R*xc2*yc2);
	Double_t x1 = (a*xc +delta)/(rc*rc);
	Double_t x2 = (a*xc-delta)/(rc*rc);
	Double_t y1, y2;
	y1 = TMath::Sqrt(R*R-x1*x1);
	y2 = TMath::Sqrt(R*R-x2*x2);



	y = TMath::Sqrt(R*R-x*x);*/
}

MpdTwoTrackSharedPadsCut::~MpdTwoTrackSharedPadsCut() {
	delete fHelix1;
	delete fHelix2;
}

