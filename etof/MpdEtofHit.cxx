
#include <iostream>

#include "MpdTofUtils.h"
#include "MpdEtofHit.h"

using namespace std;
//------------------------------------------------------------------------------------------------------------------------
MpdEtofHit::MpdEtofHit()
{
  fTime = 0.;
  fFlag = 1;
}
//------------------------------------------------------------------------------------------------------------------------
MpdEtofHit::MpdEtofHit(Int_t detID, TVector3 pos, TVector3 dpos, Int_t index, Double_t time, Int_t flag)
: FairHit(detID, pos, dpos, index), fTime(time), fFlag(flag)
{
}
//------------------------------------------------------------------------------------------------------------------------
MpdEtofHit::~MpdEtofHit() 
{ 
}
//------------------------------------------------------------------------------------------------------------------------
bool			MpdEtofHit::CheckTrackID(Int_t uid)
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
void MpdEtofHit::Print(const char* comment) const
{
	cout<<"\n -I- MpdEtofHit: "; 
	if(comment != nullptr) cout<<comment;	
	cout<<"\tdetectorID= "<<fDetectorID<<" position("<<fX<<", "<<fY<<", "<<fZ
		<<") cm, position error("<<fDx<<", "<<fDy<<", "<<fDz<<") cm, time= "<<fTime<<" ns, flag: "<<fFlag;
}
//------------------------------------------------------------------------------------------------------------------------
ClassImp(MpdEtofHit)
