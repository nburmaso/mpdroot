// -------------------------------------------------------------------------
// -----                       CbmTof source file                      -----
// -----                  Created 28/07/04  by V. Friese               -----
// -------------------------------------------------------------------------

#include <iostream>

#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "TParticle.h"
#include "TVirtualMC.h"
//#include "TGeant3.h"

#include "FairGeoInterface.h"
#include "FairGeoLoader.h"
#include "FairGeoNode.h"
#include "FairGeoRootBuilder.h"
#include "CbmGeoStt.h"
#include "FairRootManager.h"
#include "CbmStt.h"
#include "CbmSttPoint.h"
#include "FairRuntimeDb.h"
#include "CbmGeoSttPar.h"
#include "TObjArray.h"
#include "FairRun.h"
#include "FairVolume.h"
#include "FairStack.h"

class FairVolume;


// TODO: read this from geant initialization
#define innerStrawDiameter 1.
//#define redefineLambdaChargedDecay 0

// -----   Default constructor   -------------------------------------------
CbmStt::CbmStt()
{
    fSttCollection = new TClonesArray("CbmSttPoint");
    fPosIndex = 0;
    fVerboseLevel = 1;
    fIsInitialized = kFALSE;
    fSensNodes = NULL;
    fPassNodes = NULL;
    fParameters = NULL;
    //    fRun = NULL;
}
// -------------------------------------------------------------------------



// -----   Standard constructor   ------------------------------------------
CbmStt::CbmStt(const char* name, Bool_t active)
  : FairDetector(name, active)
{
    fSttCollection = new TClonesArray("CbmSttPoint");
    fPosIndex = 0;
    fVerboseLevel = 1;
    fIsInitialized = kFALSE;
    fSensNodes = NULL;
    fPassNodes = NULL;
    fParameters = NULL;
    //    fRun = NULL;
}
// -------------------------------------------------------------------------




// -----   Destructor   ----------------------------------------------------
CbmStt::~CbmStt()
{
    if (fSttCollection) 
    {
	fSttCollection->Delete(); 
	delete fSttCollection;
    }
}
// -------------------------------------------------------------------------



// -----   Private method GetSquaredDistanceFromWire -----------------------
float CbmStt::GetSquaredDistanceFromWire()
{
    TLorentzVector
	entryPosition;  
    
    float
	positionInMother[3],
	positionInStraw[3];
    
    gMC->TrackPosition(entryPosition);
    positionInMother[0] = entryPosition.X();
    positionInMother[1] = entryPosition.Y();
    positionInMother[2] = entryPosition.Z();
    gMC->Gmtod(positionInMother, positionInStraw, 1);
    
    return positionInStraw[0] * positionInStraw[0] + 
	positionInStraw[1] * positionInStraw[1];
}
// -------------------------------------------------------------------------

// -----   Public method ProcessHits  --------------------------------------
Bool_t  CbmStt::InitProcessHits()
{
 
  //cout << "in process hits" << endl;
  if (!fIsInitialized)
    {
      fIsInitialized = kTRUE;  
  
      //FairRun *fRun = FairRun::Instance();
      
      FairRuntimeDb
        *rtdb= FairRun::Instance()->GetRuntimeDb();
      
      if (!rtdb)
	{
          cout << "-I- CbmStt: No runtime database found."  << endl;
	  return kFALSE;
	}
      
      fParameters = (CbmGeoSttPar*)(rtdb->getContainer("CbmGeoSttPar"));
      
      if (!fParameters)
	{
          cout << "-I- CbmStt: No geometry container CbmGeoSttPar found in runtime database."  << endl;
	  return kFALSE;
	}
      
	fSensNodes = fParameters->GetGeoSensitiveNodes();
	
	if (!fSensNodes)
	{
            cout << "-I- CbmStt: No sensitive nodes found in geometry container."  << endl;
	    return kFALSE;
	}
	
	fPassNodes = fParameters->GetGeoPassiveNodes();

	if (!fPassNodes)
	{
            cout << "-I- CbmStt: No passive nodes found in geometry container."  << endl;
	    return kFALSE;
	}
    }
    return kTRUE;
}

bool CbmStt::Split(string &aDest, string &aSrc, char aDelim)
{
   if(aSrc.empty())
      return false;

   string::size_type 
       pos = aSrc.find(aDelim);

   aDest = aSrc.substr(0, pos);

   if(pos != string::npos)
       aSrc = aSrc.substr(pos + 1);
   else
       aSrc = "";

   return true;
}

string CbmStt::GetStringPart(string &aSrc, Int_t part, char aDelim)
{	
    string 
	retval = "",
	sub;

    int
	counter = 0;

    while(Split(sub, aSrc, aDelim))
    {
	if (counter == part)
	{
	    retval = sub;
	    break;
	}
	counter++;
    }

    return retval;
}

// -----   Public method ProcessHits  --------------------------------------
Bool_t  CbmStt::ProcessHits(FairVolume* vol)
{
    if (!InitProcessHits())
    {
	return kFALSE;
    }

    if (gMC->TrackCharge() != 0.)
      {

	if ( gMC->IsTrackEntering() ) 
	  {
	     if (sqrt(GetSquaredDistanceFromWire()) > (innerStrawDiameter / 4.))

	      {
		// Set parameters at entrance of volume. Reset ELoss.
		fELoss  = 0.;
		fTime   = gMC->TrackTime() * 1.0e09;
		fLength = gMC->TrackLength();
		gMC->TrackPosition(fPos);
		gMC->TrackMomentum(fMomIn);
		gMC->TrackPosition(fpostotin);// da cancellare
		Double_t globalPos[3] = {0., 0., 0.}; // stt1 modified
		Double_t localPos[3] = {0., 0., 0.}; // stt1 modified
		
	 	globalPos[0] = fPos.X();
		globalPos[1] = fPos.Y();
		globalPos[2] = fPos.Z();
		
	
		gMC->Gmtod(globalPos, localPos, 1);
		fPosInLocal.SetXYZM(localPos[0], localPos[1], localPos[2], 0.0);
 

	
	      }
	  }
	
	// Sum energy loss for all steps in the active volume
	fELoss += gMC->Edep();
	
        // Create CbmSttPoint at exit of active volume -- but not into the wire
	if (gMC->IsTrackExiting() && (sqrt(GetSquaredDistanceFromWire()) > (innerStrawDiameter / 4.))) 
	  {
	    fTrackID  = gMC->GetStack()->GetCurrentTrackNumber();
	    fVolumeID = vol->getMCid();
	    fMass = gMC->TrackMass();   // mass (GeV)
	    gMC->TrackPosition(fPosOut);
	    gMC->TrackMomentum(fMomOut);
	    gMC->TrackPosition(fpostotout);// da cancellare
	    Double_t globalPos[3] = {0., 0., 0.}; // stt1 modified
	    Double_t localPos[3] = {0., 0., 0.}; // stt1 modified
	    
	    gMC->Gdtom(localPos, globalPos, 1);

	    fPos.SetXYZM(globalPos[0], globalPos[1], globalPos[2], 0.0);
	    
	    globalPos[0] = fPosOut.X();
	    globalPos[1] = fPosOut.Y();
	    globalPos[2] = fPosOut.Z();
	    
	    gMC->Gmtod(globalPos, localPos, 1);
	    fPosOutLocal.SetXYZM(localPos[0], localPos[1], localPos[2], 0.0);

	    
	    // string basename("stt1tube");
	    string basename;
	    TString volumename;
	    string
	      hashmark("#"),
	      volName,
	      fullName,
	      number,
	      specialname,
	      volPath(gMC->CurrentVolPath()),
	      volPath2(gMC->CurrentVolPath());

	    volumename = volPath;
	    volName = GetStringPart(volPath, 2, '/');
	    number = GetStringPart(volName, 1, '_');	
	    
	    Int_t start =volPath2.find("stt",0);
            specialname = volPath2.substr(start,volPath2.find("_",start)-start);
	    
	    if(volumename.Contains("stt01")) basename = "stt01tube";
	    if(volumename.Contains("stt02")) basename = "stt02tube";
	    if(volumename.Contains("stt03")) basename = "stt03tube";
	    if(volumename.Contains("stt04")) basename = "stt04tube";
	    if(volumename.Contains("stt05")) basename = "stt05tube";
	    if(volumename.Contains("stt06")) basename = "stt06tube";
	    if(volumename.Contains("stt07")) basename = "stt07tube";
	    if(volumename.Contains("stt08")) basename = "stt08tube";
	    if(volumename.Contains("stt09")) basename = "stt09tube";
	    if(volumename.Contains("stt10")) basename = "stt10tube";
            if(volumename.Contains("stt11")) basename = "stt11tube";
	    if(volumename.Contains("stt12")) basename = "stt12tube";
	    if(volumename.Contains("stt13")) basename = "stt13tube";
	    if(volumename.Contains("stt14")) basename = "stt14tube";
	    if(volumename.Contains("stt15")) basename = "stt15tube";
	    
	    fullName = basename + hashmark + number;
	    
	    //  cout << "volname: " << volName <<  " " << volumename << endl;
	    // 	    cout << "number: " << number << endl;
	    //      cout << gMC->CurrentVolPath() << endl;
	    // 	    cout << "fullname: " << fullName << endl;
 	  

            FairGeoNode *vol_obj = dynamic_cast<FairGeoNode*> (fPassNodes->FindObject(fullName.c_str()));
	    
	    if(number=="0") {
              vol_obj = dynamic_cast<FairGeoNode*> (fPassNodes->FindObject(specialname.c_str()));
              //cout<<">>>>"<<endl;
              //cout<<"special "<<specialname.c_str()<<endl;
	    }
	    
            if (!vol_obj)
	      {
                cout << "-I- CbmStt: No volume " << fullName.c_str() << " found in geometry container."  << endl;
		return kFALSE;
	      }
	    
            FairGeoRotation rotation = vol_obj->getLabTransform()->getRotMatrix();
	    
            FairGeoVector
	      originalVector(0., 0., 1.),
	      rotatedVector = rotation * originalVector;
	    
	    //   cout << "positionc: " << fPos.X() << " " << fPos.Y() << " " << fPos.Z() << endl;	// da cancellare
	    //    cout << "position: " << fpostot.X() << " " << fpostot.Y() << " " << fpostot.Z() << endl;
	  
	    // if(sqrt(fPosInLocal.X()*fPosInLocal.X() + fPosInLocal.Y()*fPosInLocal.Y()) < 0.45) {
// 	      cout << "position in : " << sqrt(fPosInLocal.X()*fPosInLocal.X() + fPosInLocal.Y()*fPosInLocal.Y()) << endl;
// 	    }
// 	    if(sqrt(fPosOutLocal.X()*fPosOutLocal.X() + fPosOutLocal.Y()*fPosOutLocal.Y()) < 0.45)  {
// 	      cout << "position out: " << sqrt(fPosOutLocal.X()*fPosOutLocal.X() + fPosOutLocal.Y()*fPosOutLocal.Y()) << endl;

// 	    }
 
	    fpostot.SetXYZM((fpostotin.X() +  fpostotout.X())/2., (fpostotin.Y() + fpostotout.Y())/2., (fpostotin.Z() + fpostotout.Z())/2.,0.0); // da cancellare


	   //  cout << "in : " << fpostotin.X() << " " << fpostotin.Y() << " " << fpostotin.Z() << endl;
// 	    cout << "out: " << fpostotout.X() << " " << fpostotout.Y() << " " << fpostotout.Z() << endl;
// 	    cout << "tot: " << fpostot.X() << " " << fpostot.Y() << " " << fpostot.Z() << endl;
// 	    cout << (fpostotin.X() +  fpostotout.X())/2. << " " << (fpostotin.Y() + fpostotout.Y())/2. << " " << (fpostotin.Z() + fpostotout.Z())/2. << endl;



	    AddHit(fTrackID, fVolumeID,
		   TVector3(fPos.X(),   fPos.Y(),   fPos.Z()),
		   TVector3(fPosInLocal.X(),   fPosInLocal.Y(),   fPosInLocal.Z()),
		   TVector3(fPosOutLocal.X(),  fPosOutLocal.Y(),  fPosOutLocal.Z()),
		   TVector3(fMomIn.Px(),  fMomIn.Py(),  fMomIn.Pz()),
		   TVector3(fMomOut.Px(), fMomOut.Py(), fMomOut.Pz()),
		   TVector3(rotatedVector.getX(), rotatedVector.getY(), rotatedVector.getZ()),
		   fTime, fLength, fELoss, fMass, TVector3(fpostot.X(), fpostot.Y(), fpostot.Z())); // da cancellare fpostot
	    
	    // Increment number of stt points for TParticle
	    Int_t 
	      points = gMC->GetStack()->GetCurrentTrack()->GetMother(1);
	    
	    Int_t 
	      nSttPoints = (points & (15<<20)) >> 20;
	    
	    nSttPoints ++;
	    
	    if (nSttPoints > 15) 
	      nSttPoints = 15;
	    
	    points = ( points & ( ~ (15<<20) ) ) | (nSttPoints << 20);
	
	    gMC->GetStack()->GetCurrentTrack()->SetMother(1,points);

	    ((FairStack*)gMC->GetStack())->AddPoint(kECT);

	    ResetParameters();
	  }
      }
 
    return kTRUE;
}
// -------------------------------------------------------------------------



// -----   Public method EndOfEvent   --------------------------------------
void CbmStt::EndOfEvent()
{
    if (fVerboseLevel) 
	Print();
    fSttCollection->Clear();
    fPosIndex = 0;
}
// -------------------------------------------------------------------------



// -----   Public method Register   ----------------------------------------
void CbmStt::Register()
{
    FairRootManager::Instance()->Register("STTPoint", "Stt", fSttCollection, kTRUE);
}
// -------------------------------------------------------------------------



// -----   Public method GetCollection   -----------------------------------
TClonesArray* CbmStt::GetCollection(Int_t iColl) const
{
    if (iColl == 0) 
	return fSttCollection;
    else 
	return NULL;
}
// -------------------------------------------------------------------------



// -----   Public method Print   -------------------------------------------
void CbmStt::Print() const
{
    Int_t 
	nHits = fSttCollection->GetEntriesFast();
    
    cout << "-I- CbmStt: " << nHits << " points registered in this event."  << endl;
    
    if (fVerboseLevel>1)
	for (Int_t i=0; i<nHits; i++) 
	    (*fSttCollection)[i]->Print();
}
// -------------------------------------------------------------------------



// -----   Public method Reset   -------------------------------------------
void CbmStt::Reset()
{
    fSttCollection->Clear();
    ResetParameters();
}
// -------------------------------------------------------------------------



// -----   Public method CopyClones   --------------------------------------
void CbmStt::CopyClones(TClonesArray* cl1, TClonesArray* cl2, Int_t offset)
{
    Int_t 
	nEntries = cl1->GetEntriesFast();
    
    cout << "-I- CbmStt: " << nEntries << " entries to add." << endl;
    
    TClonesArray& clref = *cl2;
    
    CbmSttPoint
	*oldpoint = NULL;
    for (Int_t i=0; i<nEntries; i++) 
    {
        oldpoint = (CbmSttPoint*) cl1->At(i);
	
	Int_t 
	    index = oldpoint->GetTrackID() + offset;
	
	oldpoint->SetTrackID(index);
        new (clref[fPosIndex]) CbmSttPoint(*oldpoint);
	fPosIndex++;
    }
    cout << "-I- CbmStt: " << cl2->GetEntriesFast() << " merged entries."
	 << endl;
}
// -------------------------------------------------------------------------



// -----   Public method ConstructGeometry   -------------------------------
void CbmStt::ConstructGeometry()
{
  if (!InitProcessHits())
    {
      return;
    }
  // get pointer to the instantons which interface 
  // to monte carlo
  
  FairGeoLoader
    *geoLoad = FairGeoLoader::Instance();
  
  FairGeoInterface
    *geoFace = geoLoad->getGeoInterface();
  
  CbmGeoStt
    *sttGeo  = new CbmGeoStt();
  
  sttGeo->setGeomFile(GetGeometryFileName());
  geoFace->addGeoModule(sttGeo);
  
  Bool_t 
    rc = geoFace->readSet(sttGeo);
  
  if (rc) 
    sttGeo->create(geoLoad->getGeoBuilder());
  
  TList* 
    volList = sttGeo->getListOfVolumes();

  FairRun *fRun = FairRun::Instance();
//    fRun = FairRun::Instance();
  
  TListIter 
    iter(volList);
  
  FairGeoNode
    *node = NULL;
  
  FairGeoVolume
    *aVol = NULL;
  
  while( (node = (FairGeoNode*)iter.Next()) )
    {
      aVol = dynamic_cast<FairGeoVolume*> ( node );
      
      if ( node->isSensitive()  ) 
	{
	  if (fSensNodes)
	    {
	      fSensNodes->AddLast( aVol );
	    }
	}
      else
	{
	  if (fPassNodes)
	    {
	      fPassNodes->AddLast( aVol );
	    }
	}
    }
  
  fParameters->setChanged();
  fParameters->setInputVersion(fRun->GetRunId(),1);
  
  ProcessNodes ( volList );
}
// -------------------------------------------------------------------------




// -----   Private method AddHit   -----------------------------------------
CbmSttPoint* CbmStt::AddHit(Int_t trackID, Int_t detID, TVector3 pos,
			    TVector3 posInLocal, TVector3 posOutLocal, 
			    TVector3 momIn, TVector3 momOut, TVector3 wireDir,
			    Double_t time, Double_t length, Double_t eLoss,  Double_t mass, TVector3 postot)   // da cancellare postot
{
  TClonesArray& 
      clref = *fSttCollection;

  Int_t 
      size = clref.GetEntriesFast();

  return new(clref[size]) CbmSttPoint(trackID, detID, pos, posInLocal, posOutLocal,
				      momIn, momOut, wireDir, time, length, eLoss, mass, postot);
}
// -------------------------------------------------------------------------

 

ClassImp(CbmStt)
    
