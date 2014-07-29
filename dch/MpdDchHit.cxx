//------------------------------------------------------------------------------------------------------------------------

#include <iostream>
using namespace std;

#include "MpdDchHit.h"
//------------------------------------------------------------------------------------------------------------------------
MpdDchHit::MpdDchHit()
  : FairHit(),
    fFlag(0),
    fNofDim(1),
    fPhi(0.)
{
  	fMeas[1] = fError[1] = 0.;
}
//------------------------------------------------------------------------------------------------------------------------
MpdDchHit::MpdDchHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index, Int_t flag)
  : FairHit(detID, pos, dpos, index),
    fFlag(flag),
    fNofDim(1),
    fPhi(0.)
{
	fMeas[1] = fError[1] = 0.;
}
//------------------------------------------------------------------------------------------------------------------------
MpdDchHit::MpdDchHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index)
  : FairHit(detID, pos, dpos, index),
    fFlag(0),
    fNofDim(1),
    fPhi(0.)
{
  	fMeas[1] = fError[1] = 0.;
}
//------------------------------------------------------------------------------------------------------------------------
MpdDchHit::~MpdDchHit() { }
//------------------------------------------------------------------------------------------------------------------------
void MpdDchHit::Print(const Option_t* opt) const
{
  	cout<<"-I- MpdDchHit"<<endl;
  	cout<<"    DetectorID: " << fDetectorID << endl;
  	cout<<"    Position: (" <<fX<<", "<<fY<<", "<<fZ<< ") cm"<< endl;
  	cout<<"    Position error: ("<<fDx<< ", "<<fDy<< ", "<<fDz<< ") cm"<< endl;
  	cout<<"    Flag: "<<fFlag<< endl;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdDchHit::SetIndex(Int_t indx)
{
  /// Add point index

  	Int_t size = fIndex.GetSize();
  	fIndex.Set (size + 1);
  	fIndex[size] = indx;
}
//------------------------------------------------------------------------------------------------------------------------
Int_t MpdDchHit::Compare(const TObject* hit) const
{
  /// "Compare" function to sort in ascending order in abs(Z)

  	MpdDchHit *kHit = (MpdDchHit*) hit;
  	if 	(GetLayer() < kHit->GetLayer()) return -1;
  	else if (GetLayer() > kHit->GetLayer()) return 1;
  	else 
	{
    		if 	(TMath::Abs(fZ) < TMath::Abs(kHit->GetZ())) return -1;
    		else if (TMath::Abs(fZ) > TMath::Abs(kHit->GetZ())) return 1;
    		else return 0;
  	}
}
//------------------------------------------------------------------------------------------------------------------------

ClassImp(MpdDchHit)
