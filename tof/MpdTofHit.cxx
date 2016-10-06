//------------------------------------------------------------------------------------------------------------------------
#include <iostream>
#include <limits>

#include "MpdTofUtils.h"
#include "MpdTofHit.h"

using namespace std;

const double MpdTofHit::nan = numeric_limits<double>::quiet_NaN();
//------------------------------------------------------------------------------------------------------------------------
MpdTofHit::MpdTofHit()
{
	fTime = fX = fY = fZ = fDx = fDy = fDz = MpdTofHit::nan;
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
bool			MpdTofHit::CheckVolumeUID(Int_t uid)
{
 	Int_t  nLinks = GetNLinks();
	FairLink link;
  	
  	if(nLinks > 0)
		for(Int_t i = 0; i < nLinks; i++) 							
		{
			link = GetLink(i);
			if(link.GetType() == MpdTofUtils::volumeUID && link.GetIndex() == uid) return true;	
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
			if(link.GetType() == MpdTofUtils::mcTrackIndex && link.GetIndex() == uid) return true;	
		}
return false;
}
//------------------------------------------------------------------------------------------------------------------------
void MpdTofHit::Print(const char* comment) const
{
	cout<<endl;
	if(comment != nullptr) 	cout<<comment;		
	
	cout<<" MpdTofHit UID: "<< fDetectorID<<" Position: ("<< fX <<", "<< fY <<", "<< fZ <<") cm"
       		<<" Position error: ("<< fDx <<", "<< fDy <<", "<< fDz << ") cm"<<" Time: "<<fTime<<" ns"<< ",    Flag: "<<fFlag;
}
//------------------------------------------------------------------------------------------------------------------------
ClassImp(MpdTofHit)
