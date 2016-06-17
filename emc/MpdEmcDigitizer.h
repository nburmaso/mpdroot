//--------------------------------------------------------------------
//
// Description:
//      MPD EMC Digitizer - takes EmcPoints and makes digits
//
//
// Author List:
//      Alexander Zinchenko LHEP, JINR, Dubna - 8-May-2016
//
//--------------------------------------------------------------------

#ifndef MPDEMCDIGITIZER_H
#define MPDEMCDIGITIZER_H 1

#include "MpdEmcDigit.h"
#include "MpdEmcGeoPar.h"

#include "FairTask.h"
#include <iostream>
#include <map>

class TClonesArray;

class MpdEmcDigitizer : public FairTask {
public:

    /** Default constructor **/
    MpdEmcDigitizer();


    /** Destructor **/
    ~MpdEmcDigitizer();


    /** Virtual method Init **/
    virtual InitStatus Init();


    /** Virtual method Exec **/
    virtual void Exec(Option_t* opt);
    void virtual Finish();

private:

    /** Input array of MpdEmcPoints **/
    TClonesArray* fPointArray;
    /** Input array of MCTracks **/
    TClonesArray* fMcTrackArray;

    /** Output array of MpdEmcHit **/
    TClonesArray* fDigiArray;

    void RedoId(TClonesArray *digis, TClonesArray *mctrs);
    UInt_t GetSecId(Float_t x, Float_t y, Float_t z);
    UInt_t GetRowId(Float_t z);
    UInt_t GetSupModId(Float_t x, Float_t y, Float_t z, UInt_t sec);
    UInt_t GetModId(Float_t x, Float_t y, UInt_t supMod, UInt_t sec);

    void FindChanPhiZ(Double_t &phi, Double_t &z);
    Float_t CalcZCenter(UInt_t sec, UInt_t row, UInt_t mod);
    Float_t CalcPhiCenter(UInt_t sec, UInt_t supMod, UInt_t mod);

    MpdEmcDigit* SearchHit(TString tower);

    MpdEmcGeoPar* fGeoPar;
    std::map<TString,MpdEmcDigit*> fHitMap; //! 

    ClassDef(MpdEmcDigitizer, 1);

};

#endif
