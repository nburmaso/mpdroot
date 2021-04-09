//------------------------------------------------------------------------------------------------------------------------
/// \class MpdFfdHitProducer
/// 
/// \brief 
/// \author Sergei Lobastov (LHE, JINR, Dubna)
//------------------------------------------------------------------------------------------------------------------------

#ifndef __HH_MPDFFDHITPRODUCER_H
#define __HH_MPDFFDHITPRODUCER_H 1

#include <map>

#include <TList.h>
#include <TString.h>
#include <TVector3.h>
#include <TH2D.h>
#include <TH1D.h>
#include <TEfficiency.h>

#include "FairTask.h"
//------------------------------------------------------------------------------------------------------------------------
class TClonesArray;
class TGraph;
class MpdFfdPoint;
class MpdFfdHit;

//------------------------------------------------------------------------------------------------------------------------
class MpdFfdHitProducer : public FairTask
{
        TClonesArray 			*aMcPoints  = nullptr;	//! <--- MC input
        TClonesArray 			*aMcTracks  = nullptr;	//! <--- MC input
        TClonesArray 			*aExpDigits = nullptr;	//! <--- Exp input
        TClonesArray 			*aFfdHits   = nullptr;	//! ---> output

	Bool_t				fUseMCData;
	Bool_t				fDoTest = false;	
	Double_t			fNpeThresh = 80.;// default value = 80 photo electrons
	Double_t			fErrXY = 4./sqrt(12.), fErrZ = 1.5/sqrt(12.); // 4x4x1.5 [cm]
      	TList				fList;	
	TString				fFlnm;
	std::multimap<size_t, const MpdFfdHit*>  mmCounter; //!

	TH1D				*hSuids = nullptr;
	TH2D				*hOpYield = nullptr, *hOpYieldPion = nullptr,  *hOpYieldProton = nullptr, *hOpYieldElectron = nullptr;
	TH2D				*hPeYield = nullptr, *hPeYieldPion = nullptr,  *hPeYieldProton = nullptr, *hPeYieldElectron = nullptr;
	TH2D				*hOpTrash = nullptr, *hXY = nullptr, *hXmap = nullptr, *hYmap = nullptr, *hZmap = nullptr;
	TH2D				*hXcenter = nullptr, *hYcenter = nullptr, *hCenter = nullptr, *hOccup = nullptr;
	TEfficiency			*hPMTeff = nullptr;
	static TGraph			*gPMTeff_pdf; // PDF for PMT yield

  	virtual void 		SetParContainers();

	void			Add(TEfficiency *hist){ hist->SetDirectory(nullptr); fList.Add(hist);}
	void			Add(TH1 *hist){ hist->SetDirectory(nullptr); fList.Add(hist);}
	MpdFfdHit* 		AddHit(const MpdFfdPoint *point, const TVector3& pos, const TVector3& dpos, Int_t refIndex, size_t npe, Int_t flag);
	TVector3		GetPadCenter(Int_t suid);  // [1,161]

public:
  	MpdFfdHitProducer(const char* name = "FFD hit Producer", Bool_t useMCdata = true, Int_t verbose = 1, const char* flnm = nullptr);
  	virtual ~MpdFfdHitProducer();

  	virtual InitStatus 	Init();
  	virtual void 		Exec(Option_t* opt);
  	virtual void  		Finish();

	void			SetErrors(double XYerr, double Zerr){ fErrXY = XYerr; fErrZ = Zerr;};
	void  			SetPeThresh(Double_t n) {fNpeThresh = n;};
	static bool 		IsPeCreated(double energy); // [eV] 

ClassDef(MpdFfdHitProducer,2);  
};
//------------------------------------------------------------------------------------------------------------------------
#endif

