//-----------------------------------------------------------
// File and Version Information:
// $Id$
//
// Description:
//      Implementation of class MpdTpcClusterFinderMlem
//      see MpdTpcClusterFinderMlem.h for details
//
// Environment:
//      Software developed for the MPD Detector at NICA.
//
// Author List:
//      Alexandr Zinchenko LHEP, JINR, Dubna - 11-January-2016
//
//-----------------------------------------------------------

// This Class' Header ------------------
#include "MpdTpcClusterFinderMlem.h"

// Collaborating Class Headers --------
#include "MpdTpc2dCluster.h"
#include "MpdTpcDigit.h"
#include "MpdTpcHit.h"
#include "MpdTpcSectorGeo.h"
#include "TpcPoint.h"

#include "FairRootManager.h"

#include <TClonesArray.h>
#include <TFile.h>
#include <TH2.h>
#include <TMath.h>
#include <TMatrixD.h>
#include <TROOT.h>
#include <TSystem.h>

// C/C++ Headers ----------------------
#include <iostream>
#include <math.h>
#include <set>
#include <vector>

using namespace std;

static Bool_t SortPix(const MpdTpcClusterFinderMlem::pixel i, 
		      const MpdTpcClusterFinderMlem::pixel j); // sorting function

//__________________________________________________________________________

MpdTpcClusterFinderMlem::MpdTpcClusterFinderMlem()
  : FairTask("TPC Cluster finder Mlem"), fPersistence(kFALSE)
{
  /*
  std::string tpcGasFile = gSystem->Getenv("VMCWORKDIR");
  tpcGasFile += "/geometry/Ar-90_CH4-10.asc";
  fGas= new TpcGas(tpcGasFile, 130);
  std::cout<<*fGas<<std::endl;
  */
}

//__________________________________________________________________________

MpdTpcClusterFinderMlem::~MpdTpcClusterFinderMlem()
{
  //delete fGas;
}

//__________________________________________________________________________

void MpdTpcClusterFinderMlem::FinishTask()
{
  for (Int_t i = 0; i < fgkNsec2; ++i) {
    //fPrimArray[i]->Delete();
    delete [] fDigiSet[i];
  }
}

//__________________________________________________________________________

InitStatus MpdTpcClusterFinderMlem::Init()
{

  // Create containers for digits
  fSecGeo = MpdTpcSectorGeo::Instance();
  //Int_t nRows = MpdTpcSectorGeo::Instance()->NofRows();
  Int_t nRows = fSecGeo->NofRows();
  for (Int_t i = 0; i < fgkNsec2; ++i) fDigiSet[i] = new set<Int_t> [nRows];

  //Get ROOT Manager
  FairRootManager* ioman = FairRootManager::Instance();

  if (ioman == 0) {
    Error("MpdTpcClusterFinderMlem::Init","RootManager not instantiated!");
    return kERROR;
  }
  
  // Get input collection
  fDigiArray = (TClonesArray*) ioman->GetObject("MpdTpcDigit");
  
  if (fDigiArray == 0) {
    Error("TpcClusterFinderMlem::Init","Array of digits not found!");
    return kERROR;
  }
  
  // Create and register output array
  //SetPersistence();
  fClusArray = new TClonesArray("MpdTpc2dCluster"); 
  ioman->Register("TpcCluster", "Tpc", fClusArray, fPersistence);
  fHitArray = new TClonesArray("MpdTpcHit"); 
  ioman->Register("TpcRecPoint", "Tpc", fHitArray, fPersistence);

  return kSUCCESS;
}

//__________________________________________________________________________

void MpdTpcClusterFinderMlem::Exec(Option_t* opt)
{

  fClusArray->Delete();
  fHitArray->Delete();
  const Int_t nSec = fgkNsec2 / 2; // number of TPC readout sectors
  // Clear digi containers
  Int_t nRows = fSecGeo->NofRows();
  for (Int_t i = 0; i < fgkNsec2; ++i) {
    for (Int_t j = 0; j < nRows; ++j) fDigiSet[i][j].clear();
  }
  
  // Fill digi containers
  Int_t nDigis = fDigiArray->GetEntriesFast();
  cout << " Total number of digits: " << nDigis << endl;
  for (Int_t i = 0; i < nDigis; ++i) {
    MpdTpcDigit *digi = (MpdTpcDigit*) fDigiArray->UncheckedAt(i);
    Int_t isec = digi->GetSector();
    Int_t irow = digi->GetRow();
    fDigiSet[isec][irow].insert(i);
  }
  
  Int_t nSum = 0;
  // Loop over sectors
  for (Int_t isec = 0; isec < fgkNsec2; ++isec) {

    // Loop over padrows
    for (Int_t irow = 0; irow < nRows; ++irow) {
      if (fDigiSet[isec][irow].size() == 0) continue;
      //cout << " Sector, row, digits: " << isec << " " << irow << " " << fDigiSet[isec][irow].size() << endl; 
      ProcessPadrow(isec, irow);
      nSum += fDigiSet[isec][irow].size();
    }
  }
  cout << " Control sum: " << nSum << endl;

  // Find hits
  FindHits();
}

//__________________________________________________________________________

void MpdTpcClusterFinderMlem::ProcessPadrow(Int_t isec, Int_t irow)
{
  // Process one padrow of a sector

  memset(fFlags, 0, sizeof(fFlags[0][0]) * fgkNpads * fgkNtimes);

  set<Int_t>::iterator it = fDigiSet[isec][irow].begin();

  for ( ; it != fDigiSet[isec][irow].end(); ++it) {
    MpdTpcDigit *digi = (MpdTpcDigit*) fDigiArray->UncheckedAt(*it);
    Int_t ipad = digi->GetPad(), itime = digi->GetTimeBin();
    if (ipad >= fgkNpads) Fatal("ProcessPadrow", "Too few pads!!! %i %i", ipad, fgkNpads);
    if (itime >= fgkNtimes) Fatal("ProcessPadrow", "Too few time bins!!! %i %i", itime, fgkNtimes);
    fCharges[ipad][itime] = digi->GetAdc();
    //fDigis[ipad][itime] = *it;
    fDigis[ipad][itime] = digi->GetOrigin(); // store trackID with max charge contribution
    fFlags[ipad][itime] = 1;
  }
  
  // Find (pre)clusters
  Int_t nclus0 = fClusArray->GetEntriesFast(), nclus = nclus0;
  for (Int_t ipad = 0; ipad < fgkNpads; ++ipad) {
    for (Int_t itime = 0; itime < fgkNtimes; ++itime) {
      if (fFlags[ipad][itime] <= 0) continue;
      // New cluster
      MpdTpc2dCluster* clus = new ((*fClusArray)[nclus++]) MpdTpc2dCluster(irow, isec); 
      //clus->Insert(fDigis[ipad][itime], irow, ipad, itime, fCharges[ipad][itime]);
      clus->Insert(fDigis[ipad][itime], irow, ipad+2, itime, fCharges[ipad][itime]); // extra shift
      clus->SetID(fDigis[ipad][itime]); // trackID
      clus->SetErrY(fCharges[ipad][itime]); // ADC counts
      fFlags[ipad][itime] = -1;
      for (Int_t ip = 0; ip < 2; ++ip) {
	for (Int_t it = 0; it < 2; ++it) {
	  if (it == ip) continue;
	  Int_t ip1 = ipad + ip, it1 = itime + it;
	  if (ip1 >= fgkNpads) continue;
	  if (it1 >= fgkNtimes) continue;
	  if (fFlags[ip1][it1] <= 0) continue;
	  NextPixel(clus, ip1, it1);
	}
      }
    }
  }
  //cout << " Found preclusters: " << nclus - nclus0 << endl;
}

//__________________________________________________________________________

void MpdTpcClusterFinderMlem::NextPixel(MpdTpc2dCluster* clus, Int_t ipad, Int_t itime)
{
  // Add next pixel to the cluster

  //clus->Insert(fDigis[ipad][itime], clus->Row(), ipad, itime, fCharges[ipad][itime]);
  clus->Insert(fDigis[ipad][itime], clus->Row(), ipad+2, itime, fCharges[ipad][itime]); // extra shift
  clus->SetID (TMath::Min(clus->ID(),fDigis[ipad][itime])); // min trackID
  clus->SetErrY (TMath::Max(Double_t(clus->GetErrY()),fCharges[ipad][itime])); // max ADC counts
  fFlags[ipad][itime] = -1;
  for (Int_t ip = -1; ip < 2; ++ip) {
    for (Int_t it = -1; it < 2; ++it) {
      if (TMath::Abs(it) == TMath::Abs(ip)) continue;
      Int_t ip1 = ipad + ip, it1 = itime + it;
      if (ip1 < 0 || ip1 >= fgkNpads) continue;
      if (it1 < 0 || it1 >= fgkNtimes) continue;
      if (fFlags[ip1][it1] <= 0) continue;
      NextPixel(clus, ip1, it1);
    }
  }
}

//__________________________________________________________________________

void MpdTpcClusterFinderMlem::FindHits()
{
  // Reconstruct hits (find local maxima)

  /*
    fCharges[ipad][itime] = digi->GetAdc();
    //fDigis[ipad][itime] = *it;
    fDigis[ipad][itime] = digi->GetOrigin(); // store trackID with max charge contribution
    fFlags[ipad][itime] = 1;
    */
  TVector3 p3loc, p3glob, p3err(0.05,0.0,0.1);
  Int_t nclus = fClusArray->GetEntriesFast(), ihit = 0;

  for (Int_t iclus = 0; iclus < nclus; ++iclus) {
    MpdTpc2dCluster* clus = (MpdTpc2dCluster*) fClusArray->UncheckedAt(iclus);
    Int_t nDigis = clus->NDigits();
    //if (nDigis == 1) continue; // skip too small clusters

    memset(fFlags, 0, sizeof(fFlags[0][0]) * fgkNpads * fgkNtimes);
    for (Int_t idig = 0; idig < nDigis; ++idig) {
      Int_t ipad = clus->Col(idig);
      Int_t itime = clus->Bkt(idig);
      fCharges[ipad][itime] = clus->Adc(idig);
      fFlags[ipad][itime] = 1;
      fDigis[ipad][itime] = idig;
    }

    // Exclude pads which are not local maxima
    Int_t novfw = 0;
    for (Int_t idig = 0; idig < nDigis; ++idig) {
      Int_t ipad = clus->Col(idig);
      Int_t itime = clus->Bkt(idig);
      if (clus->Adc(idig) > fgkOvfw - 1) ++novfw;
      for (Int_t ip = -1; ip < 2; ++ip) {
	for (Int_t it = -1; it < 2; ++it) {
	  if (TMath::Abs(it) == TMath::Abs(ip)) continue; // exclude diagonals
	  if (it == 0 && ip == 0) continue;
	  Int_t ip1 = ipad + ip, it1 = itime + it;
	  if (ip1 < 0 || ip1 >= fgkNpads) continue;
	  if (it1 < 0 || it1 >= fgkNtimes) continue;
	  if (fFlags[ip1][it1] == 0) continue;
	  if (clus->Adc(idig) < fCharges[ip1][it1]) { fFlags[ipad][itime] = -1; break; }
	  else if (clus->Adc(idig) == fCharges[ip1][it1] && fFlags[ip1][it1] > 0) { fFlags[ipad][itime] = -1; break; }
	}
      }
    }
    if (novfw) clus->SetOverflows(novfw);

    multimap<Double_t,Int_t> localMax;
    for (Int_t idig = 0; idig < nDigis; ++idig) {
      Int_t ipad = clus->Col(idig);
      Int_t itime = clus->Bkt(idig);
      if (fFlags[ipad][itime] <= 0) continue;
      localMax.insert(pair<Double_t,Int_t>(clus->Adc(idig),idig));
      //cout << clus->Col(idig) << " " << clus->Bkt(idig) << " " << clus->Adc(idig) << endl;
    }
    //cout << " Local max: " << clus->GetSect() << " " << clus->Row() << " " << localMax.size() << endl;

    Int_t nLocMax0 = localMax.size();
    clus->SetUniqueID(nLocMax0); // for testing

    // Check for edge effect - maximum digit at the sector edge
    Int_t edge = 0;
    Int_t idig = localMax.rbegin()->second;
    Int_t ipad = clus->Col(idig);
    if (ipad-2 == 0 || ipad-2 == 2*fSecGeo->NPadsInRows()[clus->Row()] - 1) edge = 1; // remember extra shift !!!
    //if (clus->GetNumPads() > 1) edge = -edge;
    if (edge > 0) {
      // Add "virtual" digit
      Int_t itime = clus->Bkt(idig), ipadv = 0, ipadn = -9;
      if (clus->GetNumPads() == 1) ipadv = (ipad-2 == 0) ? ipad + 1 : ipad - 1;
      else ipadv = (ipad-2 == 0) ? ipad - 1 : ipad + 1;
      fFlags[ipadv][itime] = -1;
      //if (clus->GetNumPads() == 1) fCharges[ipadv][itime] = 20; // below threshold
      if (clus->GetNumPads() == 1) fCharges[ipadv][itime] = TMath::Min(29.0,fCharges[ipad][itime]*0.1); // below threshold
      //else fCharges[ipadv][itime] = fCharges[ipad][itime] * 0.9;
      else {
	ipadn = (ipad-2 == 0) ? ipad + 1 : ipad - 1;
	fCharges[ipadv][itime] = fCharges[ipadn][itime];
	//fCharges[ipadv][itime] = fCharges[ipad][itime] * 1.5;
	//fCharges[ipadv][itime] = TMath::Min (fCharges[ipadv][itime], 1.0*fgkOvfw);
      }
      fDigis[ipadv][itime] = fDigis[ipad][itime];
      clus->Insert(fDigis[ipadv][itime], clus->Row(), ipadv, itime, fCharges[ipadv][itime]);
      //cout << " Edge: " << clus->Row() << " " << clus->GetId() << " " << ipad << endl; 
      clus->SetVirtual(); // flag "virtual" pad addition
    }
    if (edge) clus->SetEdge(); // flag edge effect

    if (edge || localMax.size() > 1) {
      // Run MLEM procedure
      //PeakAndValley(clus, localMax);
      Mlem(iclus, localMax);
      clus->SetFlag(clus->Flag()|1); // flag MLEM procedure usage
      continue;
    }

    map<Int_t,Double_t> mapIdQ;
    Double_t padMean = 0, timeMean = 0, adcTot = 0, sum2t = 0, sum2p = 0;

    // Process simple cluster (only 1 local max)
    for (Int_t idig = 0; idig < nDigis; ++idig) {
      Int_t ip = clus->Col(idig);
      Int_t it = clus->Bkt(idig);
      padMean += ip * fCharges[ip][it];
      timeMean += it * fCharges[ip][it];
      adcTot += fCharges[ip][it];
      Int_t id = clus->Sec(fDigis[ip][it]);
      //if (mapIdQ.find(id) == mapIdQ.end()) mapIdQ[id] = fCharges[ip][it];
      //else mapIdQ[id] = mapIdQ[id] + fCharges[ip][it];
      //else mapIdQ[id] = TMath::Max (mapIdQ[id], fCharges[ip][it]);
      if (mapIdQ.find(id) == mapIdQ.end()) mapIdQ[id] = 1;
      else mapIdQ[id] = mapIdQ[id] + 1;
      
      sum2t += it * it * fCharges[ip][it];
      sum2p += ip * ip * fCharges[ip][it];
    }

    //padMean /= adcTot;
    padMean = padMean / adcTot - 2; // correct for shift
    timeMean /= adcTot;

    Double_t xloc = fSecGeo->Pad2Xloc(padMean,clus->Row());
    Int_t padID = fSecGeo->PadID(clus->GetSect() % fSecGeo->NofSectors(), clus->Row());
    Double_t yloc = fSecGeo->LocalPadPosition(padID).Y();
    Double_t zloc = fSecGeo->TimeBin2Z(timeMean);
    p3loc.SetXYZ(xloc, yloc, zloc);
    Double_t rmsZ = TMath::Sqrt (sum2t/adcTot - timeMean*timeMean);
    Double_t rmsX = TMath::Sqrt (sum2p/adcTot - padMean*padMean);
    //p3err[1] = fSecGeo->TimeBin2Z(rms); // to pass the value
    //p3err[1] = rms;
    //cout << " Result: " << nLocMax0 << " " << pixels.size() << " " << xloc << " " << zloc << endl;

    // Apply corrections
    TVector3 p3errCor(p3err);
    CorrectReco(p3loc, p3errCor, clus->GetNumPads(), adcTot);
    
    if (clus->GetSect() >= fSecGeo->NofSectors()) p3loc[2] = -p3loc[2];
    fSecGeo->Local2Global(clus->GetSect(), p3loc, p3glob);
    
    // Dip angle correction (for interaction point with Z = 0)
    Double_t dip = TMath::ATan2 (TMath::Abs(p3glob[2]),p3glob.Pt()) * TMath::RadToDeg();
    p3errCor[2] = TMath::Max (p3errCor[2], 0.116 - 0.0046 * dip + 0.00015 * dip * dip);
    p3errCor[2] = TMath::Min (p3errCor[2], 0.5);
    // Correct Z-coordinate for rows = 0 and 27
    //if ((clus->Row() == 0 || clus->Row() == 27) && rmsZ > 1.0) {
    if (clus->Row() == 0 && rmsZ > 1.0) {
      Double_t zcor = 0;
      if (clus->Row() == 0) zcor = -0.011 + 0.002 * dip;
      else zcor = -0.029 + 0.005 * dip + 3.886e-5 * dip * dip;
      zcor = TMath::Max (zcor, 0.);
      zcor = TMath::Min (zcor, 0.6);
      p3loc[2] -= TMath::Sign (zcor, p3loc[2]);
      p3glob[2] -= TMath::Sign (zcor, p3glob[2]);
    }
    
    ihit = fHitArray->GetEntriesFast();
    MpdTpcHit* hit = new ((*fHitArray)[ihit]) MpdTpcHit(padID, p3glob, p3errCor, iclus); 
    hit->SetLayer(clus->Row());
    hit->SetLocalPosition(p3loc); // point position
    hit->SetEnergyLoss(adcTot);
    Int_t ireg = (clus->Row() < fSecGeo->NofRowsReg(0)) ? 0 : 1;
    Double_t step = fSecGeo->PadHeight(ireg);
    hit->SetStep(step);
    hit->SetModular(1); // modular geometry flag
    hit->SetPad(Int_t(padMean));
    hit->SetBin(Int_t(timeMean));
    hit->SetRMS(rmsX, 0);
    hit->SetRMS(rmsZ, 1);
    hit->SetNdigits(nDigis);
    
    // !!! Warning: FairLinks are not persistent !!! 
    //hit->AddLink(FairLink(MpdTpcHit::PointIndex, pointIndx));
    vector<Int_t> vecDig;
    // Store maximum 3 track IDs with the highest charge contribution
    multimap<Double_t,Int_t> mapDig;
    for (map<Int_t,Double_t>::iterator mit = mapIdQ.begin(); mit != mapIdQ.end(); ++mit) 
      mapDig.insert(pair<Double_t,Int_t>(-mit->second,mit->first));
    multimap<Double_t,Int_t>::iterator mit = mapDig.begin();
    while (mit != mapDig.end() && vecDig.size() < 9) {
      //while (mit != mapDig.end() && vecDig.size() < 1) {
      vecDig.push_back(mit->second);
      ++mit;
    }
    Int_t ndig = vecDig.size();
    /*
      for (Int_t idig = 0; idig < ndig; ++idig)
      hit->AddLink(FairLink(MpdTpcHit::MCTrackIndex, clus->Sec(vecDig[idig]))); // trackID stored in Sec
    */
    for (Int_t idig = 0; idig < ndig; ++idig) {
      hit->AddLink(FairLink(MpdTpcHit::MCTrackIndex, vecDig[idig], idig)); // weight = idig
      hit->AddID(vecDig[idig]);
    }
    //cout << hit->GetNLinks() << " " << *(vecDig.begin()) << " " << hit->GetLinksWithType(MpdTpcHit::MCTrackIndex).GetLink(0).GetIndex() << " " << hit->GetLinksWithType(MpdTpcHit::MCTrackIndex).GetLink(hit->GetNLinks()-1).GetIndex() << endl;

  } // for (Int_t iclus = 0; iclus < nclus;
}

//__________________________________________________________________________

void MpdTpcClusterFinderMlem::PeakAndValley(const MpdTpc2dCluster* clus, multimap<Double_t,Int_t> &localMax)
{
  // Apply peak-and-valley cuts to remove some local maxima

  const Double_t ratio = 1.3;
  multimap<Double_t,Int_t>::reverse_iterator rit = localMax.rbegin(), rit1;
  ++rit;

  for ( ; rit != localMax.rend(); ++rit) {
    Int_t idig = rit->second;
    Int_t ipad = clus->Col(idig);
    Int_t itime = clus->Bkt(idig);
    if (fFlags[ipad][itime] <= 0) continue; // merged local max

    // Loop over higher peaks
    rit1 = localMax.rbegin();
    for ( ; rit1 != rit; ++rit1) {
      Int_t idig0 = rit1->second;
      Int_t ipad0 = clus->Col(idig0);
      Int_t itime0 = clus->Bkt(idig0);
      if (fFlags[ipad0][itime0] <= 0) continue; // merged local max

      Int_t dpad = ipad - ipad0, dt = itime - itime0;
      Int_t i0 = itime0, i1 = itime, j0 = ipad0, j1 = ipad, intime = 1;
      if (TMath::Abs(dpad) > TMath::Abs(dt)) i0 = ipad0, i1 = ipad, j0 = itime0, j1 = itime, intime = 0;
      Int_t stepi = TMath::Sign(1,i1-i0), stepj = TMath::Sign(1,j1-j0);
      
      //Int_t valOk = 0;
      Int_t merge = 0;
      //if (TMath::Abs(dpad) <= 1 && TMath::Abs(dt) <= 1) merge = 1;
      //else {
      if (TMath::Abs(dpad) <= 1 && TMath::Abs(dt) <= 1) {
	if (fCharges[ipad-dpad][itime] > fCharges[ipad][itime] / ratio ||
	    fCharges[ipad][itime-dt] > fCharges[ipad][itime] / ratio) merge = 1;
      } else {
	for (Int_t ii = i0 + stepi; ii != i1; ii += stepi) {
	  merge = 0;
	  for (Int_t jj = j0; jj != j1 + stepj; jj += stepj) {
	    if (TMath::Abs(jj-j0) > TMath::Abs(ii-i0)) continue;
	    if (TMath::Abs(jj-j1) > TMath::Abs(ii-i1)) continue;
	    Int_t ip = ii, it = jj;
	    if (intime) ip = jj, it = ii;
	    //if (fCharges[ip][it] < fCharges[ipad][itime] / ratio) { valOk = 1; break; }
	    if (fCharges[ip][it] > fCharges[ipad][itime] / ratio) { merge = 1; break; }
	  }
	  //if (valOk) break;
	  if (!merge) break;
	}
      }
      //if (!valOk) fFlags[ipad][itime] = -fFlags[ipad][itime]; // not deep enough valley
      if (merge) fFlags[ipad][itime] = -fFlags[ipad][itime]; // not deep enough valley
    }
  }

  // Remove failed peaks
  multimap<Double_t,Int_t>::iterator it = localMax.begin();
  //for ( ; it != localMax.end(); ++it) cout << it->second << " ";
  //cout << endl;

  it = localMax.begin();
  for ( ; it != localMax.end(); ++it) {
    Int_t idig = it->second;
    Int_t ipad = clus->Col(idig);
    Int_t itime = clus->Bkt(idig);
    if (fFlags[ipad][itime] > 0) continue;
    //cout << " Before: " << idig << " " << itime << " " << ipad << " " << it->first << ", ";
    localMax.erase(it);
    //cout << " After: " << it->first << endl;
  }
}

//__________________________________________________________________________

void MpdTpcClusterFinderMlem::CorrectReco(TVector3 &p3loc, TVector3 &p3errCor, Int_t nPads, Double_t adc)
{
  // Correct reconstructed coordinates

  Double_t xsec = (p3loc.Y() + fSecGeo->GetMinY()) * TMath::Tan(fSecGeo->Dphi()/2);
  Double_t xloc = p3loc.X(), xloc0 = xloc, edge = 0.0; // distance to sector boundary
  if (xloc < 0) edge = xloc + xsec;
  else edge = xloc - xsec;
  if (TMath::Abs(edge) > 1.5 || nPads > 3) return; // no corrections
  
  if (nPads == 1) {
    /*
    xloc -= TMath::Sign(0.520,edge);
    Double_t corr = -0.15;
    if (adc < 50000) corr = 0.206 - 2.532e-5 * adc + 8.715e-10 * adc * adc
      - 1.610e-14 * adc * adc * adc + 1.193e-19 * adc * adc * adc * adc;
    p3errCor[0] = 0.116;
    */
    Double_t corr = 0.092;
    if (edge < 0) corr = -corr;
    xloc -= corr;
    p3errCor[0] = 0.386;
  } else if (nPads == 2) {
    /*
    xloc -= TMath::Sign(0.200,edge);
    Double_t corr = -0.07;
    if (adc < 200000) corr = 0.374 - 1.046e-5 * adc + 9.595e-11 * adc * adc
	- 4.098e-16 * adc * adc * adc + 6.811e-22 * adc * adc * adc * adc;
    p3errCor[0] = 0.110;
    */
    Double_t corr = 0.002;
    if (edge < 0) corr = -corr;
    xloc -= corr;
    p3errCor[0] = 0.03;
  } else if (nPads > 2) {
    /*
    xloc -= TMath::Sign(0.033,edge);
    Double_t corr = 0.01;
    if (adc < 200000) corr = 0.188 - 4.007e-6 * adc + 3.364e-11 * adc * adc
	- 1.265e-16 * adc * adc * adc + 1.771e-22 * adc * adc * adc * adc;
    p3errCor[0] = 0.073;
    */
    Double_t corr = 0.004;
    if (edge < 0) corr = -corr;
    xloc -= corr;
    p3errCor[0] = 0.04;
  } else return;  
  if (TMath::Abs(xloc) > xsec) xloc = TMath::Sign(xsec,xloc0);
  p3loc.SetX(xloc);
}

//__________________________________________________________________________
void MpdTpcClusterFinderMlem::Mlem(Int_t iclus, multimap<Double_t,Int_t> &localMax)
{
  // Run MLEM procedure

  const Int_t nDigMax = 200; // max number of digits to apply pixel reduction

  if (gROOT->FindObject("hMlem0")) gROOT->FindObject("hMlem0")->Delete();
  if (gROOT->FindObject("hMlem1")) gROOT->FindObject("hMlem1")->Delete();
  if (gROOT->FindObject("hTimePad")) gROOT->FindObject("hTimePad")->Delete();
  //TCanvas *c = (TCanvas*) gROOT->FindObject("c");

  vector<pixel> bins, pixels[2];
  vector<pixel>::iterator it;

  MpdTpc2dCluster* clus = (MpdTpc2dCluster*) fClusArray->UncheckedAt(iclus);
  Int_t nDigis = clus->NDigits();
  Int_t nx = clus->GetNumTimeBins();
  Int_t ny = clus->GetNumPads() + 2;

  TH2D *hXY = new TH2D("hTimePad","Pad No. vs Time bin",nx,clus->MinBkt(),clus->MaxBkt()+1,
		       ny,clus->MinCol()-1,clus->MaxCol()+2);
  TH2D *hOvfw = NULL;
  if (clus->Overflows() > 1 && nDigis < 50) {
    // Special treatment for (relatively) small clusters with many overflows
    if (gROOT->FindObject("hOvfw")) gROOT->FindObject("hOvfw")->Delete();
    hOvfw = (TH2D*) hXY->Clone("hOvfw");
  }

  // Check for edge effect - maximum digit at the edge
  Int_t edge = 0;
  Int_t idig = localMax.rbegin()->second;
  Int_t ipad = clus->Col(idig);
  if (ipad-2 == 0 || ipad-2 == 2*fSecGeo->NPadsInRows()[clus->Row()] - 1) edge = 1; // extra shift

  for (Int_t idig = 0; idig < nDigis; ++idig) {
    Int_t ipad = clus->Col(idig);
    Int_t itime = clus->Bkt(idig);
    //fCharges[ipad][itime] = clus->Adc(idig);
    //fFlags[ipad][itime] = 1;
    //fDigis[ipad][itime] = idig;
    if (fCharges[ipad][itime] < 0.1) continue;
    pixel pix;
    pix.ix = hXY->GetXaxis()->FindBin(itime+0.1);
    pix.iy = hXY->GetYaxis()->FindBin(ipad+0.1);
    pix.qq = fCharges[ipad][itime];
    pix.vis = 0;
    //if (pix.qq < fgkOvfw) bins.push_back(pix);
    bins.push_back(pix);
    pixels[1].push_back(pix);
    pixels[0].push_back(pix);
    if (hOvfw && fCharges[ipad][itime] > fgkOvfw - 1) hOvfw->Fill(itime+0.1,ipad+0.1,fCharges[ipad][itime]);
    // Consider edge effect
    //*
    if (edge && (ipad-2 == 0 || ipad-2 == 2*fSecGeo->NPadsInRows()[clus->Row()] - 1)) { // extra shift
      if (ipad-2 == 0) {
	--pix.iy;
	fDigis[ipad-1][itime] = fDigis[ipad][itime];
      } else { 
	++pix.iy;
	fDigis[ipad+1][itime] = fDigis[ipad][itime];
      }
      if (hXY->GetBinContent(pix.ix,pix.iy) > 0.1) continue;
      pixels[1].push_back(pix);
      pixels[0].push_back(pix);
      //cout << " Edge: " << clus->Row() << " " << clus->GetId() << " " << ipad << endl; 
    }
    //*/
  }
  // Get parameters of the response function
  Double_t sigt, sigp, correl;
  GetResponse(clus, hXY, hOvfw, sigt, sigp, correl);

  Int_t nbins = bins.size();
  //sort (pixels[1].begin(), pixels[1].end(), SortPix);
  //if (pixels[1].size() > nbins) pixels[1].erase(pixels[1].begin()+nbins,pixels[1].end());
  //for (Int_t ibin = 0; ibin < nbins; ++ibin) cout << ibin << " " << pixels[1][ibin].qq << " " 
  //						  << pixels[1][ibin].ix << " " << pixels[1][ibin].iy << endl;

  //TH2D *hMlem[2] = {NULL, (TH2D*)hXY->Clone("hMlem1")};
  TH2D *hMlem[2] = {(TH2D*)hXY->Clone("hMlem0"), (TH2D*)hXY->Clone("hMlem1")};
  //TH2D *hExp = (TH2D*) hXY->Clone("hExp"); // for testing

  // Decrease pixel size
  //const Int_t ndiv = 5, ndiv2 = ndiv * ndiv, nloops = 2; // 
  const Int_t ndiv = 3, ndiv2 = ndiv * ndiv, nloops = 2; // 
  Int_t icur = 0;
  TMatrixD cij(2,2);

  for (Int_t iloop = 0; iloop < nloops; ++iloop) {
    if (iloop) {
      if (nDigis > nDigMax) continue;
      nx *= ndiv; 
      ny *= ndiv; 
    } else if (nDigis <= nDigMax) continue; // skip first iteration
    Int_t iprev = (iloop + 1) % 2;
    icur = iloop % 2;
    TString hname = "hMlem";
    hname += icur;
    if (hMlem[icur]) hMlem[icur]->Delete();
    hMlem[icur] = new TH2D (hname,"",nx,hXY->GetXaxis()->GetXmin(),hXY->GetXaxis()->GetXmax(),
			    ny,hXY->GetYaxis()->GetXmin(),hXY->GetYaxis()->GetXmax());
    pixels[icur].clear();
    Int_t npix = 0;
    if (iloop) cij.ResizeTo(pixels[iprev].size()*ndiv2,nbins);
    else cij.ResizeTo(pixels[iprev].size(),nbins);

    Double_t visMax = 0.0;
    //sort (pixels[iprev].begin(), pixels[iprev].end(), SortPix);

    for (it = pixels[iprev].begin(); it != pixels[iprev].end(); ++it) {
      Double_t xc = hMlem[iprev]->GetXaxis()->GetBinCenter(it->ix);
      Double_t yc = hMlem[iprev]->GetYaxis()->GetBinCenter(it->iy);
      Double_t xmin = hMlem[iprev]->GetXaxis()->GetBinLowEdge(it->ix);
      Double_t ymin = hMlem[iprev]->GetYaxis()->GetBinLowEdge(it->iy);

      for (Int_t ij = 0; ij < ndiv2; ++ij) {
	if (iloop == 0 && ij) break;
	Double_t x0 = xc;
	Double_t y0 = yc;
	pixel pix;
	if (iloop == 0) {
	  pix.ix = it->ix;
	  pix.iy = it->iy;
	} else {
	  x0 = xmin + hMlem[icur]->GetXaxis()->GetBinWidth(1) * (ij % ndiv + 0.5);
	  y0 = ymin + hMlem[icur]->GetYaxis()->GetBinWidth(1) * (ij / ndiv + 0.5);
	  pix.ix = hMlem[icur]->GetXaxis()->FindBin(x0);
	  pix.iy = hMlem[icur]->GetYaxis()->FindBin(y0);
	  //cout << xc << " " << yc << " " << x0 << " " << y0 << endl;
	}
	pix.qq = 1.0;
	//if (npix == 0) pix.qq = 9746;
	pix.vis = 0.0;
	pixels[icur].push_back(pix);

	// Compute couplings for this pixel
	for (Int_t ibin = 0; ibin < nbins; ++ibin) {
	  Double_t x1 = hXY->GetXaxis()->GetBinCenter(bins[ibin].ix);
	  Double_t y1 = hXY->GetYaxis()->GetBinCenter(bins[ibin].iy);
	  cij(npix,ibin) = GetCij(x0, y0, x1, y1, sigt, sigp, correl);
	  //?? pixels[icur][npix].vis += cij(npix,ibin); // visibility
	  if (bins[ibin].qq < fgkOvfw - 1.5) pixels[icur][npix].vis += cij(npix,ibin); // visibility
	}
	visMax = TMath::Max(visMax,pixels[icur][npix].vis);

	//cout << npix << " " << pixels[icur][npix].vis << endl;
	++npix;
	//if (npix >= nbins) break;
      } // for (Int_t ij = 0;
      //if (npix >= nbins) break;
    } // for (it = pixels[iprev].begin();

    //for (Int_t ipix = 0; ipix < npix; ++ipix) cout << ipix << " " << pixels[icur][ipix].vis << " " << pixels[icur][ipix].qq << " " << pixels[icur][ipix].ix << " " <<  pixels[icur][ipix].iy << endl;

    // MLEM procedure
    Int_t niter = 10;
    for (Int_t iter = 0; iter < niter; ++iter) {

      // Calculate expectations for all pads
      for (Int_t ibin = 0; ibin < nbins; ++ibin) {
	bins[ibin].vis = 0.0;
	for (Int_t i = 0; i < npix; ++i) bins[ibin].vis += (pixels[icur][i].qq * cij(i,ibin)); // bins[ibin].vis - just storage
	//?? bins[ibin].vis = TMath::Min (bins[ibin].vis, fglOvfw);
      }

      // Maximization
      for (Int_t ipix = 0; ipix < npix; ++ipix) {
	Double_t sum = 0.0;

	for (Int_t ibin = 0; ibin < nbins; ++ibin) {
	  if (bins[ibin].vis > 1.e-6) sum += (bins[ibin].qq / bins[ibin].vis * cij(ipix,ibin));
	}

	if (pixels[icur][ipix].vis > 1.e-6) pixels[icur][ipix].qq *= (sum / pixels[icur][ipix].vis);
	//if (pixels[icur][ipix].vis > 1.e-6) pixels[icur][ipix].qq *= (sum / visMax);
      }
    } // for (Int_t iter = 0; 

    // Visual control
    /*
    if (iloop == nloops - 1) {
      for (Int_t ipix = 0; ipix < npix; ++ipix) 
	hMlem[icur]->SetBinContent(pixels[icur][ipix].ix,pixels[icur][ipix].iy,pixels[icur][ipix].qq);
      //c->cd(3);
      hMlem[icur]->Draw("col");
    }
    */
    /*
    hExp->Reset();
    for (Int_t ipix = 0; ipix < nbins; ++ipix) 
      hExp->SetBinContent(bins[ipix].ix,bins[ipix].iy,bins[ipix].vis);
    */
  } // for (Int_t iloop = 0; 

  //
  // Find local maxima
  //

  vector<Double_t> tmp;
  vector<vector<Double_t> > charges;
  tmp.assign(nx,0);
  charges.assign(ny,tmp);
  vector<Int_t> itmp;
  vector<vector<Int_t> > flags;
  itmp.assign(nx,0);
  flags.assign(ny,itmp);
  localMax.clear();
  Int_t npix = pixels[icur].size();

  for (Int_t ipix = 0; ipix < npix; ++ipix) {
    Int_t ipad = pixels[icur][ipix].iy - 1;
    Int_t itime = pixels[icur][ipix].ix - 1;
    charges[ipad][itime] = pixels[icur][ipix].qq;
    flags[ipad][itime] = ipix + 1;
  }

  // Exclude pads which are not local maxima
  for (Int_t ipix = 0; ipix < npix; ++ipix) {
    Int_t ipad = pixels[icur][ipix].iy - 1;
    Int_t itime = pixels[icur][ipix].ix - 1;
    Int_t iok = 1;

    for (Int_t ip = -1; ip < 2; ++ip) {
      for (Int_t it = -1; it < 2; ++it) {
	if (TMath::Abs(it) == TMath::Abs(ip)) continue; // exclude diagonals
	if (it == 0 && ip == 0) continue;
	Int_t ip1 = ipad + ip, it1 = itime + it;
	if (ip1 < 0 || ip1 >= ny) continue;
	if (it1 < 0 || it1 >= nx) continue;
	if (flags[ip1][it1] == 0) continue;
	//if (pixels[icur][ipix].qq < charges[ip1][it1]) { flags[ipad][itime] = -1; break; }
	//else if (pixels[icur][ipix].qq == charges[ip1][it1] && flags[ip1][it1] > 0) { flags[ipad][itime] = -1; break; }
	//if (flags[ip1][it1] <= 0) continue;
	if (pixels[icur][ipix].qq < charges[ip1][it1]) { flags[ipad][itime] *= -1; iok = 0; break; }
	else if (pixels[icur][ipix].qq == charges[ip1][it1] && flags[ip1][it1] > 0) { flags[ipad][itime] *= -1; iok = 0; break; }
      }
      if (!iok) break;
    }
  }

  for (Int_t ipix = 0; ipix < npix; ++ipix) {
    Int_t ipad = pixels[icur][ipix].iy - 1;
    Int_t itime = pixels[icur][ipix].ix - 1;
    if (flags[ipad][itime] <= 0) continue;
    localMax.insert(pair<Double_t,Int_t>(pixels[icur][ipix].qq,ipix));
  }

  // Apply Peak-and-valley procedure in pixel domain
  if (localMax.size() > 1) PeakAndValley(pixels[icur], localMax, charges, flags);

  // Create hits
  Int_t nHits0 = fHitArray->GetEntriesFast();
  vector<multimap<Double_t,Int_t> > pixInMax;
  multimap<Double_t,Int_t> mtmp;
  pixInMax.assign(localMax.size(),mtmp);
  CreateHits(pixels[icur], localMax, charges, flags, iclus, pixInMax);

  // Get correct charge 
  ChargeMlem(nHits0, pixels[icur], bins, pixInMax, cij);
}

//__________________________________________________________________________

void MpdTpcClusterFinderMlem::PeakAndValley(const vector<pixel> &pixels, multimap<Double_t,Int_t> &localMax, 
					    vector<vector<Double_t> > &charges, vector<vector<Int_t> > &flags)
{
  // Apply peak-and-valley cuts to remove some local maxima in pixel domain

  const Double_t ratio = 1.2;
  multimap<Double_t,Int_t>::reverse_iterator rit = localMax.rbegin(), rit1;
  ++rit;

  for ( ; rit != localMax.rend(); ++rit) {
    Int_t ipix = rit->second;
    Int_t ipad = pixels[ipix].iy - 1;
    Int_t itime = pixels[ipix].ix - 1;
    if (flags[ipad][itime] <= 0) continue; // merged local max

    // Loop over higher peaks
    rit1 = localMax.rbegin();
    for ( ; rit1 != rit; ++rit1) {
      Int_t ipix0 = rit1->second;
      Int_t ipad0 = pixels[ipix0].iy - 1;
      Int_t itime0 = pixels[ipix0].ix - 1;
      if (flags[ipad0][itime0] <= 0) continue; // merged local max

      Int_t dpad = ipad - ipad0, dt = itime - itime0;
      Int_t i0 = itime0, i1 = itime, j0 = ipad0, j1 = ipad, intime = 1;
      if (TMath::Abs(dpad) > TMath::Abs(dt)) i0 = ipad0, i1 = ipad, j0 = itime0, j1 = itime, intime = 0;
      Int_t stepi = TMath::Sign(1,i1-i0), stepj = TMath::Sign(1,j1-j0);
      
      //Int_t valOk = 0;
      Int_t merge = 0;
      //if (TMath::Abs(dpad) <= 1 && TMath::Abs(dt) <= 1) merge = 1;
      //else {
      if (TMath::Abs(dpad) <= 1 && TMath::Abs(dt) <= 1) {
	if (charges[ipad-dpad][itime] > charges[ipad][itime] / ratio ||
	    charges[ipad][itime-dt] > charges[ipad][itime] / ratio) merge = 1;
      } else {
	for (Int_t ii = i0 + stepi; ii != i1; ii += stepi) {
	  merge = 0;
	  for (Int_t jj = j0; jj != j1 + stepj; jj += stepj) {
	    if (TMath::Abs(jj-j0) > TMath::Abs(ii-i0)) continue;
	    if (TMath::Abs(jj-j1) > TMath::Abs(ii-i1)) continue;
	    Int_t ip = ii, it = jj;
	    if (intime) ip = jj, it = ii;
	    //if (fCharges[ip][it] < fCharges[ipad][itime] / ratio) { valOk = 1; break; }
	    if (charges[ip][it] > charges[ipad][itime] / ratio) { merge = 1; break; }
	  }
	  //if (valOk) break;
	  if (!merge) break;
	}
      }
      //if (!valOk) fFlags[ipad][itime] = -fFlags[ipad][itime]; // not deep enough valley
      if (merge) flags[ipad][itime] = -flags[ipad][itime]; // not deep enough valley
    }
  }

  // Remove failed peaks
  multimap<Double_t,Int_t>::iterator it = localMax.begin();
  //for ( ; it != localMax.end(); ++it) cout << it->second << " ";
  //cout << endl;

  it = localMax.begin();
  for ( ; it != localMax.end(); ++it) {
    Int_t ipix = it->second;
    Int_t ipad = pixels[ipix].iy - 1;
    Int_t itime = pixels[ipix].ix - 1;
    //cout << " Before: " << ipix << " " << itime << " " << ipad << " " << it->first << endl;
    if (flags[ipad][itime] > 0) continue;
    //cout << " Before: " << idig << " " << itime << " " << ipad << " " << it->first << ", ";
    localMax.erase(it);
    //cout << " After: " << it->first << endl;
  }
}

//__________________________________________________________________________

//static Bool_t MpdTpcClusterFinderMlem::SortPix(const pixel i, const pixel j)
static Bool_t SortPix(const MpdTpcClusterFinderMlem::pixel i, const MpdTpcClusterFinderMlem::pixel j)
{
  // Sorting function

  return i.qq > j.qq;
}

//__________________________________________________________________________

void MpdTpcClusterFinderMlem::GetResponse(const MpdTpc2dCluster* clus, TH2D *hXY, TH2D *hOvfw, 
					  Double_t &sigt, Double_t &sigp, Double_t &correl)
{
  // Get response parameters

  const Double_t sigx[2] = {0.584, 0.597};
  const Double_t coef[2][5] = {{1.10951, -1.12393e-3, 1.62066e-4, -4.40694e-6, 6.26894e-8},
			       {1.11737, 1.16204e-3, -1.01821e-4, 3.83333e-6, 0.0}};
  
  // Dip angle estimate
  Double_t padMean = (hXY->GetYaxis()->GetXmin() + hXY->GetYaxis()->GetXmax()) / 2;
  Double_t timeMean = (hXY->GetXaxis()->GetXmin() + hXY->GetXaxis()->GetXmax()) / 2;
  Double_t xloc = fSecGeo->Pad2Xloc(padMean,clus->Row());
  Int_t padID = fSecGeo->PadID(clus->GetSect() % fSecGeo->NofSectors(), clus->Row());
  Double_t yloc = fSecGeo->LocalPadPosition(padID).Y();
  Double_t zloc = fSecGeo->TimeBin2Z(timeMean);
  TVector3 p3loc(xloc, yloc, zloc), p3glob;
  if (clus->GetSect() >= fSecGeo->NofSectors()) p3loc[2] = -p3loc[2];
  fSecGeo->Local2Global(clus->GetSect(), p3loc, p3glob);
  Double_t dip = TMath::ATan2 (TMath::Abs(p3glob[2]),p3glob.Pt()) * TMath::RadToDeg();
  dip = TMath::Min (80.0,TMath::Abs(dip));

  Int_t ireg = (clus->Row() < 27) ? 0 : 1; // pad height region
  Double_t dip2 = dip * dip;
  Double_t dip3 = dip2 * dip;
  Double_t dip4 = dip3 * dip;
  sigt = coef[ireg][0] + coef[ireg][1] * dip + coef[ireg][2] * dip2 + coef[ireg][3] * dip3 
    + coef[ireg][4] * dip4;
  sigp = sigx[ireg];
  correl = 0.0;
  if (hOvfw) {
    correl = hOvfw->GetCorrelationFactor();
    //sigt = hOvfw->GetRMS(1);
    //sigp = hOvfw->GetRMS(2);
  }
}

//__________________________________________________________________________

Double_t MpdTpcClusterFinderMlem::GetCij(Double_t x0, Double_t y0, Double_t x1, Double_t y1, 
					 Double_t sigt, Double_t sigp, Double_t correl)
{
  // Compute couplings - Gauss parameter Sigma_t as a function of the dip angle

  Double_t dx = x1 - x0;
  Double_t dy = y1 - y0;
  dx /= sigt;
  dy /= sigp; 

  if (TMath::Abs(correl) < 0.001) return TMath::Exp(-dx*dx/2 - dy*dy/2) / 2 / sigt / sigp / TMath::Pi();
  else {
    Double_t corr2 = 1 - correl * correl;
    return TMath::Exp((-dx*dx/2 + correl*dx*dy - dy*dy/2)/corr2) / 2 / sigt / sigp / TMath::Pi() / TMath::Sqrt(corr2);
  }
}

//__________________________________________________________________________

void MpdTpcClusterFinderMlem::CreateHits(const vector<pixel> &pixels, multimap<Double_t,Int_t> &localMax, 
					 vector<vector<Double_t> > &charges, vector<vector<Int_t> > &flags,
					 Int_t iclus, vector<multimap<Double_t,Int_t> > &pixInMax)
{
  // Create hits from pixels 

  Int_t nLocMax0 = localMax.size();
  MpdTpc2dCluster* clus = (MpdTpc2dCluster*) fClusArray->UncheckedAt(iclus);
  TH2D *hXY = (TH2D*) gROOT->FindObject("hTimePad");
  TH2D *hMlem = (TH2D*) gROOT->FindObject("hMlem1");
  //if (hMlem == NULL) hMlem = (TH2D*) gROOT->FindObject("hMlem1");
  Double_t scale = hXY->GetBinWidth(1) / hMlem->GetBinWidth(1);

  multimap<Double_t,Int_t>::reverse_iterator rit = localMax.rbegin();
  //vector<Int_t> vecDig;
  map<Int_t,Double_t> mapIdQ;
  TVector3 p3loc, p3glob, p3err(0.05,0.0,0.1);
  Int_t ihit = fHitArray->GetEntriesFast(), nHitsAdd = 0;

  for ( ; rit != localMax.rend(); ++rit) {
    Int_t ipix = rit->second;
    Int_t ipad = pixels[ipix].iy - 1;
    Int_t itime = pixels[ipix].ix - 1;
    if (flags[ipad][itime] <= 0) continue; // merged local max
    //vecDig.clear();
    mapIdQ.clear();

    set<pair<Int_t,Int_t> > selPix;
    Double_t padMean = 0, timeMean = 0, adcTot = 0, sum2t = 0, sum2p = 0;

    // Process complex cluster - start from maximum and go outward (up to 5 steps), 
    // adding pixels with respectively lower charges
    for (Int_t idirp = -1; idirp < 2; idirp += 2) {
      for (Int_t ip = 0; ip < 5; ++ip) {
      //for (Int_t ip = 0; ip < 8; ++ip) {
	if (idirp > 0 && ip == 0) continue;
	Int_t ipsign = ip * idirp;
	Int_t ip1 = ipad + ipsign;
	if (ip1 < 0 || ip1 >= flags.size()) break;
	
	for (Int_t idirt = -1; idirt < 2; idirt += 2) {
	  for (Int_t it = 0; it < 5; ++it) {
	    if (idirt > 0 && it == 0) continue;
	    Int_t itsign = it * idirt;
	    
	    Int_t it1 = itime + itsign;
	    if (it1 < 0 || it1 >= (flags[0]).size()) break;
	    if (flags[ip1][it1] == 0) continue;
	    
	    Int_t add = 1;
	    if (ip || it) {
	      if (flags[ip1][it1] > 0) continue; // case when 2 local max next to each other on 1 diagonal 
	      // Check gradient
	      add = 0;
	      Int_t ipprev = ipsign, itprev = itsign;
	      if (it) {
		itprev -= idirt;
		Int_t it10 = itime + itprev;
		if (it10 >= 0 && it10 < (flags[0]).size() && flags[ip1][it10] != 0) {
		  if (selPix.find(pair<Int_t,Int_t>(ip1,it10)) != selPix.end() 
		      && charges[ip1][it1] <= charges[ip1][it10]) add = 1;
		}
	      }
	      if (add == 0 && ip) {
		ipprev -= idirp;
		Int_t ip10 = ipad + ipprev;
		if (ip10 >= 0 && ip10 < flags.size() && flags[ip10][it1] != 0) {
		  if (selPix.find(pair<Int_t,Int_t>(ip10,it1)) != selPix.end() 
		      && charges[ip1][it1] <= charges[ip10][it1]) add = 1;
		}
	      }
	    }
		
	    if (!add) break;
	    padMean += ip1 * charges[ip1][it1];
	    timeMean += it1 * charges[ip1][it1];
	    adcTot += charges[ip1][it1];
	    //vecDig.push_back(fDigis[ip1][it1]);
	    Int_t ipBin = hXY->GetYaxis()->FindBin(hMlem->GetYaxis()->GetBinCenter(ip1+1)); // original bin number
	    Int_t itBin = hXY->GetXaxis()->FindBin(hMlem->GetXaxis()->GetBinCenter(it1+1)); // original bin number
	    ipBin = TMath::Max (TMath::Nint (hXY->GetYaxis()->GetBinLowEdge(ipBin)), 0);
	    itBin = TMath::Nint (hXY->GetXaxis()->GetBinLowEdge(itBin));
	    Int_t id = clus->Sec(fDigis[ipBin][itBin]);
	    //if (mapIdQ.find(id) == mapIdQ.end()) mapIdQ[id] = fCharges[ip1][it1];
	    //else mapIdQ[id] = mapIdQ[id] + fCharges[ip1][it1];
	    //else mapIdQ[id] = TMath::Max (mapIdQ[id], fCharges[ip1][it1]);
	    if (mapIdQ.find(id) == mapIdQ.end()) mapIdQ[id] = 1;
	    else mapIdQ[id] = mapIdQ[id] + 1; // number of digits with the same ID
	    selPix.insert(pair<Int_t,Int_t>(ip1,it1));

	    sum2t += it1 * it1 * charges[ip1][it1];
	    sum2p += ip1 * ip1 * charges[ip1][it1];

	    pixInMax[nHitsAdd].insert(pair<Double_t,Int_t>(-charges[ip1][it1],flags[ip1][it1]));
	  }
	}
      }
    }

    //padMean = (padMean / adcTot + 0.5) / scale;
    padMean = (padMean / adcTot + 0.5) / scale - 2; // coorrect for shift
    timeMean = (timeMean / adcTot + 0.5) / scale;

    Double_t xloc = fSecGeo->Pad2Xloc(padMean-0.5+hMlem->GetYaxis()->GetXmin(),clus->Row());
    Int_t padID = fSecGeo->PadID(clus->GetSect() % fSecGeo->NofSectors(), clus->Row());
    Double_t yloc = fSecGeo->LocalPadPosition(padID).Y();
    Double_t zloc = fSecGeo->TimeBin2Z(timeMean-0.5+hMlem->GetXaxis()->GetXmin());
    p3loc.SetXYZ(xloc, yloc, zloc);
    Double_t rmsZ = TMath::Sqrt (sum2t/adcTot/scale/scale - timeMean*timeMean);
    Double_t rmsX = TMath::Sqrt (sum2p/adcTot/scale/scale - padMean*padMean);
    //p3err[1] = fSecGeo->TimeBin2Z(rms); // to pass the value
    //p3err[1] = rms;
    //cout << " Result: " << nLocMax0 << " " << pixels.size() << " " << xloc << " " << zloc << endl;
    
    // Apply corrections
    TVector3 p3errCor(p3err);
    CorrectRecoMlem(p3loc, p3errCor, clus, adcTot);
    
    if (clus->GetSect() >= fSecGeo->NofSectors()) p3loc[2] = -p3loc[2];
    fSecGeo->Local2Global(clus->GetSect(), p3loc, p3glob);
    
    // Dip angle correction (for interaction point with Z = 0)
    Double_t dip = TMath::ATan2 (TMath::Abs(p3glob[2]),p3glob.Pt()) * TMath::RadToDeg();
    p3errCor[2] = TMath::Max (p3errCor[2], 0.116 - 0.0046 * dip + 0.00015 * dip * dip);
    p3errCor[2] = TMath::Min (p3errCor[2], 0.5);
    // Correct Z-coordinate for rows = 0 and 27
    //if ((clus->Row() == 0 || clus->Row() == 27) && rmsZ > 1.0) {
    if (clus->Row() == 0 && rmsZ > 1.0) {
      Double_t zcor = 0;
      if (clus->Row() == 0) zcor = -0.011 + 0.002 * dip;
      else zcor = -0.029 + 0.005 * dip + 3.886e-5 * dip * dip;
      zcor = TMath::Max (zcor, 0.);
      zcor = TMath::Min (zcor, 0.6);
      p3loc[2] -= TMath::Sign (zcor, p3loc[2]);
      p3glob[2] -= TMath::Sign (zcor, p3glob[2]);
    }
    
    MpdTpcHit* hit = new ((*fHitArray)[ihit++]) MpdTpcHit(padID, p3glob, p3errCor, iclus); 
    hit->SetLayer(clus->Row());
    hit->SetLocalPosition(p3loc); // point position
    hit->SetEnergyLoss(adcTot);
    Int_t ireg = (clus->Row() < fSecGeo->NofRowsReg(0)) ? 0 : 1;
    Double_t step = fSecGeo->PadHeight(ireg);
    hit->SetStep(step);
    hit->SetModular(1); // modular geometry flag
    hit->SetPad(Int_t(padMean-0.5+hMlem->GetYaxis()->GetXmin()));
    hit->SetBin(Int_t(timeMean-0.5+hMlem->GetXaxis()->GetXmin()));
    hit->SetRMS(rmsX, 0);
    hit->SetRMS(rmsZ, 1);
    hit->SetNdigits(-selPix.size()); // negative value
    ++nHitsAdd;
    
    // !!! Warning: FairLinks are not persistent !!! 
    //hit->AddLink(FairLink(MpdTpcHit::PointIndex, pointIndx));
    vector<Int_t> vecDig;
    // Store maximum 3 track IDs with the highest charge contribution
    multimap<Double_t,Int_t> mapDig;
    for (map<Int_t,Double_t>::iterator mit = mapIdQ.begin(); mit != mapIdQ.end(); ++mit) 
      mapDig.insert(pair<Double_t,Int_t>(-mit->second,mit->first));
    multimap<Double_t,Int_t>::iterator mit = mapDig.begin();
    while (mit != mapDig.end() && vecDig.size() < 9) {
      //while (mit != mapDig.end() && vecDig.size() < 1) {
      vecDig.push_back(mit->second);
      ++mit;
    }
    Int_t ndig = vecDig.size();
    /*
      for (Int_t idig = 0; idig < ndig; ++idig)
      hit->AddLink(FairLink(MpdTpcHit::MCTrackIndex, clus->Sec(vecDig[idig]))); // trackID stored in Sec
    */
    for (Int_t idig = 0; idig < ndig; ++idig) {
      hit->AddLink(FairLink(MpdTpcHit::MCTrackIndex, vecDig[idig], idig)); // weight = idig
      hit->AddID(vecDig[idig]);
    }
    //cout << hit->GetNLinks() << " " << *(vecDig.begin()) << " " << hit->GetLinksWithType(MpdTpcHit::MCTrackIndex).GetLink(0).GetIndex() << " " << hit->GetLinksWithType(MpdTpcHit::MCTrackIndex).GetLink(hit->GetNLinks()-1).GetIndex() << endl;
  } // for ( ; rit != localMax.rend();
}

//__________________________________________________________________________

void MpdTpcClusterFinderMlem::CorrectRecoMlem(TVector3 &p3loc, TVector3 &p3errCor, MpdTpc2dCluster *clus, Double_t adc)
{
  // Correct reconstructed coordinates

  Double_t xsec = (p3loc.Y() + fSecGeo->GetMinY()) * TMath::Tan(fSecGeo->Dphi()/2);
  Double_t xloc = p3loc.X(), xloc0 = xloc, edge = 0.0; // distance to sector boundary
  if (xloc < 0) edge = xloc + xsec;
  else edge = xloc - xsec;
  if (TMath::Abs(edge) > 1.1 || adc > 2000) return; // no corrections
  
  Double_t adc2 = adc * adc;
  Double_t adc3 = adc2 * adc;
  if (clus->Edge()) {
    // Max pad at the edge
    Double_t corr = 3.886 - 0.007 * adc + 3.371e-6 * adc2 - 8.175e-11 * adc3 - 1.812e-13 * adc3 * adc;
    corr /= 10; // cm
    if (edge < 0) corr = -corr;
    xloc -= corr;
    p3errCor[0] = 0.15;
  } else {
    if (adc > 1000) return;
    Double_t corr = -0.22 / 10; // cm
    if (adc < 700) corr = 7.521 - 0.012 * adc - 9.085e-6 * adc2 + 1.365e-8 * adc3; 
    corr /= 10; // cm
    if (edge < 0) corr = -corr;
    xloc -= corr;
    p3errCor[0] = 0.2;
  }
  if (TMath::Abs(xloc) > xsec) xloc = TMath::Sign(xsec,xloc0);
  p3loc.SetX(xloc);
}

//__________________________________________________________________________

void MpdTpcClusterFinderMlem::ChargeMlem(Int_t nHits0, vector<pixel> &pixels, vector<pixel> &bins, 
					 vector<multimap<Double_t,Int_t> > &pixInMax, const TMatrixD &cij)
{
  // Get correct charge of hits after MLEM 
  // (reduce number of pixels not to exceed number of time-pad bins)

  Int_t nH = fHitArray->GetEntriesFast();
  Double_t qTot = 0.0;

  for (Int_t i = nHits0; i < nH; ++i) {
    MpdTpcHit* hit = (MpdTpcHit*) fHitArray->UncheckedAt(i);
    qTot += hit->GetQ();
  }

  Int_t nBinsTot = bins.size(), nBins = 0, nHits = pixInMax.size();
  Double_t coef = nBinsTot / qTot;
  vector<pixel> pixs;
  multimap<Double_t,Int_t>::iterator it;

  // Divide bins between hits proportionally to their charges
  for (Int_t i = nH-1; i >= nHits0; --i) {
    MpdTpcHit* hit = (MpdTpcHit*) fHitArray->UncheckedAt(i);
    Int_t nb = TMath::Nint (hit->GetQ() * coef);
    if (nb == 0) ++nb;
    nBins += nb;
    if (nBins > nBinsTot) nb -= (nBins - nBinsTot);
    else if (i == nHits0 && nBins < nBinsTot) nb -= (nBins - nBinsTot);
    Int_t jmax = i - nHits0, nsel = 0;

    for (it = pixInMax[jmax].begin(); it != pixInMax[jmax].end(); ++it) {
      ++nsel;
      if (nsel > nb) break;
      Int_t ipix = TMath::Abs(it->second) - 1;
      pixs.push_back(pixels[ipix]);
      pixs.back().qq = 1;
      pixs.back().ix = ipix; // save pixel index
      pixs.back().iy = i; // save hit index
    }
  }

  // MLEM procedure
  Int_t niter = 2, npix = pixs.size();
  for (Int_t iter = 0; iter < niter; ++iter) {

    // Calculate expectations for all pads
    for (Int_t ibin = 0; ibin < nBinsTot; ++ibin) {
      bins[ibin].vis = 0.0;
      for (Int_t i = 0; i < npix; ++i) bins[ibin].vis += (pixs[i].qq * cij(pixs[i].ix,ibin)); // bins[ibin].vis - just storage
      //?? bins[ibin].vis = TMath::Min (bins[ibin].vis, fglOvfw);
    }

    // Maximization
    for (Int_t ipix = 0; ipix < npix; ++ipix) {
      Double_t sum = 0.0;

      for (Int_t ibin = 0; ibin < nBinsTot; ++ibin) {
	if (bins[ibin].vis > 1.e-6) sum += (bins[ibin].qq / bins[ibin].vis * cij(pixs[ipix].ix,ibin));
      }

      if (pixs[ipix].vis > 1.e-6) pixs[ipix].qq *= (sum / pixs[ipix].vis);
      //if (pixels[icur][ipix].vis > 1.e-6) pixels[icur][ipix].qq *= (sum / visMax);
    }
  } // for (Int_t iter = 0; 

  // Compute hit charges
  map<Int_t,Double_t> charges;
  for (Int_t ipix = 0; ipix < npix; ++ipix) {
    if (charges.find(pixs[ipix].iy) == charges.end()) charges[pixs[ipix].iy] = pixs[ipix].qq;
    else charges[pixs[ipix].iy] += pixs[ipix].qq;
  }

  
  for (Int_t ih = nHits0; ih < nH; ++ih) {
    MpdTpcHit* hit = (MpdTpcHit*) fHitArray->UncheckedAt(ih);
    //cout << hit->GetQ() << " " << charges[ih] << " " << hit->GetNdigits() << " " << hit->GetLayer() << " " << nHits << endl;
    hit->SetEnergyLoss(charges[ih]/1.089); // coefficient due to different peak with and w/out MLEM
  }
}

//__________________________________________________________________________
ClassImp(MpdTpcClusterFinderMlem)
