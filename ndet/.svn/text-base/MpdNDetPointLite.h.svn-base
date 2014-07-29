// MpdNDetPointLite --- stripped MCPoint class for ECAL 

#ifndef MPDNDETPOINTLITE_H
#define MPDNDETPOINTLITE_H

#include "TObject.h"

class MpdNDetPointLite : public TObject
{
public:
	//Default constuctor
	MpdNDetPointLite() :
	  fTrackID(-10), fDetectorID(-10), fTime(-10), fELoss(-10) {};

	MpdNDetPointLite(Int_t trackID, Int_t detID, Double32_t tof, Double32_t eloss)
	  : fTrackID(trackID), fDetectorID(detID), fTime(tof), fELoss(eloss) {};
	//Getters
	Int_t GetTrackID() const	{return fTrackID;}
	Int_t GetDetectorID() const	{return fDetectorID;}
	Double32_t GetTime() const	{return fTime;}
	Double32_t GetEnergyLoss() const{return fELoss;}

	//Setters
	void SetTrackID(Int_t trackID)		{fTrackID=trackID;}
	void SetDetectorID(Int_t detID)		{fDetectorID=detID;}
	void SetTime(Double32_t time)		{fTime=time;}
	void SetEnergyLoss(Double32_t eloss)	{fELoss=eloss;}

	//Need implementation
	void Print() {};

	virtual ~MpdNDetPointLite() {};
private:
	Int_t fTrackID;		//Index of track
	Int_t fDetectorID;	//Number of volume in neutron detector system
	Double32_t fTime;	//Time since interaction moment 
	Double32_t fELoss;	//Energy deposition in cell
	ClassDef(MpdNDetPointLite,1)
};

#endif

