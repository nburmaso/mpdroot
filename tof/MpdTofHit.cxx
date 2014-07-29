//------------------------------------------------------------------------------------------------------------------------
#include <iostream>

#include "MpdTofUtils.h"
#include "MpdTofHit.h"

using namespace std;
//------------------------------------------------------------------------------------------------------------------------
MpdTofHit::MpdTofHit()
{
	fTime = 0.;
	fFlag = 0;
}
//------------------------------------------------------------------------------------------------------------------------
MpdTofHit::MpdTofHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index, Double_t time, Int_t flag)
 : FairHit(detID, pos, dpos, index)
{
	fTime = time;
	fFlag = flag;
}
//------------------------------------------------------------------------------------------------------------------------
MpdTofHit::~MpdTofHit() { }
//------------------------------------------------------------------------------------------------------------------------
bool			MpdTofHit::CheckVolumeUID(Int_t uid)
{
 	Int_t  nLinks = GetNLinks();
	FairLink link;
  	
  	if(nLinks > 0)
		for(Int_t i = 0; i < nLinks; i++) 							
		{
			link = GetLink(i);
			if(link.GetType() == MpdTofUtils::IsVolumeUID && link.GetIndex() == uid) return true;	
		}
return false;
}
//------------------------------------------------------------------------------------------------------------------------
bool			MpdTofHit::CheckTrackID(Int_t uid)
{
 	Int_t  nLinks = GetNLinks();
	FairLink link;
  	
  	if(nLinks > 0)
		for(Int_t i = 0; i < nLinks; i++) 							
		{
			link = GetLink(i);
			if(link.GetType() == MpdTofUtils::IsMCTrackIndex && link.GetIndex() == uid) return true;	
		}
return false;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTofHit::Print(const Option_t* opt) const
{
	cout<<"\n-I- [MpdTofHit]	DetectorID: "<< fDetectorID
		<<"\n	Position: ("<< fX <<", "<< fY <<", "<< fZ <<") cm"
       		<<"\n	Position error: ("<< fDx <<", "<< fDy <<", "<< fDz << ") cm"
		<<"\n	Time: "<<fTime<<" ns"<< ",    Flag: "<<fFlag;
}
//------------------------------------------------------------------------------------------------------------------------
ClassImp(MpdTofHit)
