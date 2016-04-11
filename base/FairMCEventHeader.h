 /********************************************************************************
 *    Copyright (C) 2014 GSI Helmholtzzentrum fuer Schwerionenforschung GmbH    *
 *                                                                              *
 *              This software is distributed under the terms of the             * 
 *         GNU Lesser General Public Licence version 3 (LGPL) version 3,        *  
 *                  copied verbatim in the file "LICENSE"                       *
 ********************************************************************************/
#ifndef FAIRMCEVENTHEADER_H
#define FAIRMCEVENTHEADER_H 1

#include "TNamed.h"                     // for TNamed

#include "Rtypes.h"                     // for Double_t, UInt_t, etc
#include "TVector3.h"                   // for TVector3

class FairMCEventHeader : public TNamed
{

  public:

    FairMCEventHeader();


    FairMCEventHeader(Int_t iEvent, Double_t x, Double_t y, Double_t z,
                      Double_t t, Double_t b, Int_t nPrim);


    FairMCEventHeader(UInt_t runId);


    virtual ~FairMCEventHeader();


    UInt_t GetRunID()   const { return fRunId; }     
    UInt_t GetEventID() const { return fEventId; }   
    Double_t GetX()     const { return fX; }         
    Double_t GetY()     const { return fY; }         
    Double_t GetZ()     const { return fZ; }         
    Double_t GetT()     const { return fT; }         
    Double_t GetB()     const { return fB; }         
    Int_t GetNPrim()    const { return fNPrim; }     
    Bool_t IsSet()      const { return fIsSet; }     
    Double_t GetRotX()     const { return fRotX; } 
    Double_t GetRotY()     const { return fRotY; } 
    Double_t GetRotZ()     const { return fRotZ; } 

    void GetVertex(TVector3& vertex) { vertex.SetXYZ(fX, fY, fZ); }


    void SetEventID(UInt_t eventId) { fEventId = eventId; }
    void SetRunID(UInt_t runId) { fRunId = runId; }
    void SetTime(Double_t t)       { fT = t; }
    void SetB(Double_t b)          { fB = b; }
    void SetNPrim(Int_t nPrim)     { fNPrim = nPrim; }
    void MarkSet(Bool_t isSet)     { fIsSet = isSet; }
    void SetVertex(Double_t x, Double_t y, Double_t z);
    void SetVertex(const TVector3& vertex);
    void SetRotX(Double_t rotx) { fRotX = rotx; }
    void SetRotY(Double_t roty) { fRotY = roty; }
    void SetRotZ(Double_t rotz) { fRotZ = rotz; }

    void Reset();

    virtual void Register();

  protected:


    UInt_t     fRunId;       
    UInt_t     fEventId;     
    Double32_t fX;           
    Double32_t fY;           
    Double32_t fZ;           
    Double32_t fT;           
    Double32_t fB;           
    Int_t      fNPrim;       
    Bool_t     fIsSet;       
    Double32_t fRotX;        
    Double32_t fRotY;        
    Double32_t fRotZ;        


    ClassDef(FairMCEventHeader,2);

};


inline void FairMCEventHeader::SetVertex(Double_t x, Double_t y,
    Double_t z)
{
  fX = x;
  fY = y;
  fZ = z;
}


inline void FairMCEventHeader::SetVertex(const TVector3& vertex)
{
  fX = vertex.X();
  fY = vertex.Y();
  fZ = vertex.Z();
}


#endif
