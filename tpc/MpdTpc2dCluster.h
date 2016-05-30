//---------------------------------------------------//
//                                                                 
//  Class that will hold 2D cluster information                    
//                                                                 
//  original author:                                               
//  Jonathan Paley   08/18/04   jpaley@indiana.edu                 
//                                                                 
//  edited:                                                        
//  Artem Basalaev 12/25/12                                        
//----------------------------------------------------//

#ifndef MPDTPC2DCLUSTER_H
#define MPDTPC2DCLUSTER_H

#include "TObject.h"
#include "TRef.h"
#include "TRefArray.h"
#include <iosfwd>
#include <vector>

class MpdTpc2dCluster : public TObject
{
 public:
  MpdTpc2dCluster() {;}
  MpdTpc2dCluster(Int_t row, Int_t sec);
  MpdTpc2dCluster(const MpdTpc2dCluster& cl);
  ~MpdTpc2dCluster();
  bool Insert(Int_t row, Int_t col, Int_t bkt,  Float_t adc) ;
  Bool_t Insert(Int_t sec, Int_t row, Int_t col, Int_t bkt,  Float_t adc);
  
 public:
  Int_t ID        () const {return fId;}
  Int_t NDigits   () const {return fAdcList.size();}
  Int_t Row       () const {return Row(0);}
  Int_t MinCol    () const {return fMinCol;}
  Int_t MaxCol    () const {return fMaxCol;}
  Int_t MinBkt    () const {return fMinBkt;}
  Int_t MaxBkt    () const {return fMaxBkt;}
  Int_t NTracks   () const;
//  Int_t ADCSum    () const {return fADCSum;}

//  Float_t AvgCol () const {return fAvgCol;}
//  Float_t SigCol () const {return fSigCol;}
//  Float_t AvgBkt () const {return fAvgBkt;}
//  Float_t SigBkt () const {return fSigBkt;}

  Float_t Adc(Int_t i) const; 
  Int_t Row(Int_t i) const; 
  Int_t Col(Int_t i) const; 
  Int_t Bkt(Int_t i) const; 
  Int_t Sec(Int_t i) const;
 

  void SetID       (Int_t i) {fId = i;}
  void SetMinCol   (Int_t i) {fMinCol = i;}
  void SetMaxCol   (Int_t i) {fMaxCol = i;}
  void SetMinBkt   (Int_t i) {fMinBkt = i;}
  void SetMaxBkt   (Int_t i) {fMaxBkt = i;}
//  void SetAvgCol   (Float_t val) {fAvgCol = val;}
//  void SetSigCol   (Float_t val) {fSigCol = val;}
//  void SetAvgBkt   (Float_t val) {fAvgBkt = val;}
//  void SetSigBkt   (Float_t val) {fSigBkt = val;}
  
  void SetX (Float_t val) {fX = val;}
  void SetY (Float_t val) {fY = val;}
  void SetZ (Float_t val) {fZ = val;}
  void SetGlobX (Float_t val) {fX_global = val;}
  void SetGlobY (Float_t val) {fY_global = val;}
  void SetGlobZ (Float_t val) {fZ_global = val;}
  void SetSect (Int_t val) {fSector = val;}
  void SetADC (Float_t val) {fADCSum = val;}
  
  void SetErrX (Float_t val) {fErrX = val;}
  void SetErrY (Float_t val) {fErrY = val;}
  void SetErrZ (Float_t val) {fErrZ = val;}
  
  UInt_t GetNumDigits () const {return (UInt_t)fAdcList.size();}
  UInt_t GetNumPads () const {return (UInt_t)(fMaxCol - fMinCol + 1);}
  UInt_t GetNumTimeBins () const {return (UInt_t)(fMaxBkt - fMinBkt + 1);}
  Float_t GetX () const {return fX;}
  Float_t GetY () const {return fY;}
  Float_t GetZ () const {return fZ;}
  Float_t GetGlobX () const {return fX_global;}
  Float_t GetGlobY () const {return fY_global;}
  Float_t GetGlobZ () const {return fZ_global;}  
  Float_t GetErrX () const {return fErrX;}
  Float_t GetErrY () const {return fErrY;}
  Float_t GetErrZ () const {return fErrZ;}
  Int_t GetSect () const {return fSector;}
  Float_t GetADC () const {return fADCSum;}  
  Int_t GetId () const {return fId;}
  
 private:

  Int_t fId;
  Int_t fMinBkt; 
  Int_t fMaxBkt; 
  Int_t fMinCol;
  Int_t fMaxCol;
  
  Float_t fX, fY, fZ;
  Float_t fX_global, fY_global, fZ_global;
  Float_t fErrX, fErrY, fErrZ;
  
  std::vector<Int_t> fSecList;
  std::vector<Int_t> fRowList;
  std::vector<Int_t> fColList;
  std::vector<Int_t> fBktList;
  std::vector<Float_t> fAdcList;
  Int_t fSector;
  Float_t fADCSum;
  Float_t fAvgCol, fSigCol, fAvgBkt, fSigBkt;

  ClassDef(MpdTpc2dCluster, 1)
};

#endif 
