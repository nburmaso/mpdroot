//
#include <string>
#include <sstream>
 
#include "CbmSttHelixTrackFitter.h"
#include "CbmSttTrackMatch.h"
#include "FairRootManager.h"
#include "FairTask.h"
 
#include "TArc.h"
#include "TH2.h"
#include "TClonesArray.h"
// #include "TH3.h"
// #include "TLine.h"
#include "TMatrixD.h" // stt1
#include "TMarker.h"
#include "TLine.h"
#include "TPolyLine.h"
#include "TMinuit.h"
//#include "CbmSttGeomPoint.h"

#include <iostream>
#include "TMath.h"

using std::cout;
using std::cerr;
using std::endl;

// #define rootoutput kFALSE

TH2F *h3;
TCanvas *eventCanvas3;

TArrayD marray(100);

CbmSttHelixTrackFitter::CbmSttHelixTrackFitter()
{
  fEventCounter = 0;
  //  fHotArray = new TClonesArray("CbmSttHOT");
}

CbmSttHelixTrackFitter::CbmSttHelixTrackFitter(Int_t verbose)
{
  fVerbose = verbose;
  if (verbose < 3)
    rootoutput = kFALSE;
  else
    rootoutput = kTRUE;
  fEventCounter = 0; 
  // fHotArray = new TClonesArray("CbmSttHOT");
}

CbmSttHelixTrackFitter::~CbmSttHelixTrackFitter()
{ 
  //  if (fHotArray) 
  //     {
  // 	fHotArray->Delete(); 
  // 	delete fHotArray;
  //     }
}

void CbmSttHelixTrackFitter::Init()
{
  fEventCounter = 0;

  // Get and check FairRootManager
  FairRootManager
    *ioman = FairRootManager::Instance();

  if (! ioman) 
    {
      cout << "-E- CbmSttHelixTrackFitter::Init: "
	   << "RootManager not instantised!" << endl;
      //      return kFATAL;
    }
  
  // Get hit Array
  fHitArray = (TClonesArray*) ioman->GetObject("STTHit");
  if (!fHitArray)
    {
      cout << "-W- CbmSttHelixTrackFitter::Init: No Hit array!"
	   << endl;
    }

  // Get point Array
  fPointArray = (TClonesArray*) ioman->GetObject("STTPoint");
  if ( ! fPointArray) 
    {
      cout << "-W- CbmSttHelixTrackFitter::Init: No Point array!"
	   << endl;
    }

  //  // Create fHotArray
  //   fHotArray = new TClonesArray("CbmSttHOT",1000);


  if(rootoutput) {
    h1 = new TH2F("h1","event display - transverse plane",100,-50,50,100,-50,50);
    h2 = new TH2F("h2","z fit",100,-5,20,100,-50,50);  
    h3 = new TH2F("h3","conformal plane",100,-1.5, 1.5, 100, -1.5, 1.5);
    eventCanvas = new TCanvas("eventcanvas", "eventcanvas", 600, 600); 
    eventCanvas2 = new TCanvas("eventcanvas2", "eventcanvas2", 650, 0 ,600, 600); 
    eventCanvas3 = new TCanvas("eventcanvas3", "eventcanvas3", 650, 0 ,600, 600); 
  }
 
}



Int_t CbmSttHelixTrackFitter::DoFit(CbmSttTrack* pTrack, Int_t pidHypo)
{

  fEventCounter++;
 
  if(!pTrack) return 0;
  fTrack = pTrack;
  
  //  fHotArray->Clear();
  if(rootoutput) {
    char goOnChar;
    cout << "press any key to continue: " << endl;
    cin >> goOnChar;
    eventCanvas->cd();
    h1->Draw();
    eventCanvas2->cd();
    h2->Draw();
    cout << "EVENT: " << fEventCounter << endl;
    eventCanvas3->cd();
    h3->Draw();
  }
  
  Int_t fit = 0;
 
  fit = Fit4b(pTrack, 1);
  
  if(fit == 0 || pTrack->GetParamLast()->GetTx() == 0 || !(pTrack->GetParamLast()->GetTx()) || pTrack->GetParamLast()->GetTx() > 3000) {
    cout << "PREPREFIT FAILED " << fit << " " << pTrack->GetParamLast()->GetTx() << endl;
    pTrack->GetParamLast()->SetTx(-999);
    return 0;
  }
  
 
  cout << "prefit-------------------------------" << endl;
  fit = 0;
  fit = MinuitFit(pTrack, 1);
 
  // if the fit fails
  if(fit == 0 || pTrack->GetParamLast()->GetTx() == 0 || !(pTrack->GetParamLast()->GetTx()) || pTrack->GetParamLast()->GetTx() > 3000) {
    pTrack->GetParamLast()->SetTx(-999); 
    cout << "PREFIT FAILED" << endl;
    return 0;
  }
  else {
    pTrack->SetFlag(1); // prefit done 
    Bool_t Rint = IntersectionFinder(pTrack, pTrack->GetParamLast());
    cout << "refit" << endl;                                       
    // se il refit e' andato bene
    if(Rint == kTRUE) fit = Fit4b(pTrack, 2);  
    if(fit == 1 && pTrack->GetParamLast()->GetTx() != 0 && (pTrack->GetParamLast()->GetTx()) != 0 && pTrack->GetParamLast()->GetTx() < 3000) {
     
      pTrack->SetFlag(2); // refit done 
      Bool_t zint = ZFinder(pTrack, 1);
      if(zint == kTRUE) {
	Int_t zfit = Zfit(pTrack, 1);
	if(zfit == 1) {
	  pTrack->SetFlag(3); // z fit done 
	} 
      }
    }
  }
  
  cout << "param last x: "  << pTrack->GetParamLast()->GetX() << endl;
  cout << "param last y: "  << pTrack->GetParamLast()->GetY() << endl;
  cout << "param last tx: " << pTrack->GetParamLast()->GetTx() << endl;
  cout << "param last ty: " << pTrack->GetParamLast()->GetTy() << endl;
  cout << "param last qp: " << pTrack->GetParamLast()->GetQp() << endl;

  

  if(rootoutput) {
    eventCanvas->Update();
    eventCanvas->Modified();
    eventCanvas2->Update();
    eventCanvas2->Modified();
   }
  
  return 0;
}

// Int_t CbmSttHelixTrackFitter::AddHitOnTrack(CbmSttTrack *pTrack) { 
//   if(!pTrack) return 0;
//   Int_t hitcounter = pTrack->GetNofHits();
//   for (Int_t k = 0; k < hitcounter; k++) {
//     Int_t iHit = pTrack->GetHitIndex(k);
//     CbmSttHit *currenthit = (CbmSttHit*) fHitArray->At(iHit);
//     if(currenthit->GetXint() == -999 || currenthit->GetXint() == -999) continue;
//     Int_t refindex = currenthit->GetRefIndex(); 
//     // get point
//     CbmSttPoint *iPoint = (CbmSttPoint*) fPointArray->At(refindex);
//     TVector3 wiredirection(iPoint->GetXWireDirection(), iPoint->GetYWireDirection(), iPoint->GetZWireDirection());

//  //     cout << "x: " << currenthit->GetXint() << endl;
//  //     cout << "y: " << currenthit->GetYint() << endl;
//  //     cout << "z: " << currenthit->GetZint() << endl;

//     // axial tubes
//     if(wiredirection != TVector3(0.,0.,1.)) {
//       AddHOT(currenthit->GetXint(), currenthit->GetYint(), -999, iHit, refindex, 0);
//     }
//     // skewd tubes
//     else { 
//       AddHOT(currenthit->GetXint(), currenthit->GetYint(), currenthit->GetZint(), iHit, refindex, 0);
//     }
//   }

//   // pTrack->SetHOT(fHotArray);
  
//   //  fHotArray->Clear();
//   return 0;
// }



Int_t CbmSttHelixTrackFitter::Fit4(CbmSttTrack* pTrack, Int_t pidHypo) {
 
  if(!pTrack) return 0;

  Int_t hitcounter = pTrack->GetNofHits();
  Bool_t first = kFALSE;
  if(hitcounter == 0) return 0;
  if(hitcounter > 50 || hitcounter < 5) {
    cout << "Bad No of hits in STT " << hitcounter << endl;
    return 0;
  }
  cout << "FIT 4 ********************" << endl;
 
  // traslation and rotation -----------
  // traslation near the first point
  Double_t s = 0.001;
  Double_t trasl[2];
  // rotation
  Double_t alpha;
  cout << "hitcounter: " << hitcounter << endl;
  for(Int_t k = 0; k <hitcounter; k++) {
    Int_t iHit = pTrack->GetHitIndex(k);
    CbmSttHit *currenthit = (CbmSttHit*) fHitArray->At(iHit);
    if(!currenthit) continue;

    if(currenthit->GetXint() == -999 || currenthit->GetYint() == -999) continue;
    Int_t refindex = currenthit->GetRefIndex(); 
    // get point
    CbmSttPoint *iPoint = (CbmSttPoint*) fPointArray->At(refindex);
    CbmSttHit *hitfirst, *hitlast;

    TVector3 wiredirection(iPoint->GetXWireDirection(), iPoint->GetYWireDirection(), iPoint->GetZWireDirection());
    if(wiredirection != TVector3(0.,0.,1.)) continue;
    else if(first == kFALSE){
            hitfirst = (CbmSttHit*) fHitArray->At(iHit);
            first = kTRUE;
            trasl[0] = hitfirst->GetXint();
            trasl[1] = hitfirst->GetYint();
         }
         else{
            hitlast = (CbmSttHit*) fHitArray->At(iHit);
            alpha = TMath::ATan2(hitlast->GetYint() -  hitfirst->GetYint(), hitlast->GetXint() - hitfirst->GetXint());
         }
  }

  // error <--> resolution
  Double_t sigr, sigxy, sigx, sigy; // = 0.14;
    
  // FITTING IN X-Y PLANE:
  // v = a + bu + cu^2
    
 
  Double_t Suu, Su, Sv, Suv, S1, Suuu, Suuv, Suuuu;
  
  Su = 0.;
  Sv = 0.;
  Suu = 0.;
  Suv = 0.;
  Suuu = 0.;
  S1 = 0.;
  Suuv = 0.;
  Suuuu = 0.;
  
  Int_t digicounter = 0;
  TArrayD uarray(hitcounter);
  TArrayD varray(hitcounter);
  TArrayD sigv2array(hitcounter);
  TArrayD sigu2array(hitcounter);

  for(Int_t i=0; i < hitcounter; i++){
    uarray.AddAt(-999, i);
    varray.AddAt(-999, i);
    sigv2array.AddAt(-999, i);
    sigu2array.AddAt(-999, i);
  }
  for(Int_t i=0; i < hitcounter; i++){
      
    Int_t iHit = pTrack->GetHitIndex(i);
    CbmSttHit *currenthit = (CbmSttHit*) fHitArray->At(iHit);
    if(!currenthit) continue;
    if(currenthit->GetXint() == -999 || currenthit->GetYint() == -999) continue;
    Int_t refindex = currenthit->GetRefIndex(); 
    // get point
    CbmSttPoint *iPoint = (CbmSttPoint*) fPointArray->At(refindex);
    
    TVector3 wiredirection(iPoint->GetXWireDirection(), iPoint->GetYWireDirection(), iPoint->GetZWireDirection());
    if(wiredirection != TVector3(0.,0.,1.)) continue;
    

    if(pidHypo == 2) {
      Double_t resx = iPoint->GetXtot() - currenthit->GetXint();
      Double_t resy = iPoint->GetYtot() - currenthit->GetYint();
      Double_t resdist = TMath::Sqrt((iPoint->GetYtot() - currenthit->GetYint())*(iPoint->GetYtot() - currenthit->GetYint()) + (iPoint->GetXtot() - currenthit->GetXint())*(iPoint->GetXtot() - currenthit->GetXint()));
      
    }
 
    if(rootoutput) { 
      //     if(pidHypo == 2) {
      eventCanvas->cd();
      TMarker *cir1 = new TMarker(currenthit->GetXint(), currenthit->GetYint(), 6);
      cir1->SetMarkerColor(2);
      if(pidHypo == 2) cir1->Draw("SAME");
      TMarker *cir2 = new TMarker(currenthit->GetX(), currenthit->GetY(), 6);
      if(pidHypo == 1) cir2->Draw("SAME");
      //     }
    }
 
    Double_t xtrasl, ytrasl;
    // traslation
    xtrasl = currenthit->GetXint() - trasl[0];
    ytrasl = currenthit->GetYint() - trasl[1];

    Double_t xrot, yrot;
    // rotation 
    xrot = TMath::Cos(alpha)*xtrasl + TMath::Sin(alpha)*ytrasl;
    yrot = -TMath::Sin(alpha)*xtrasl + TMath::Cos(alpha)*ytrasl;

    // re-traslation
    xtrasl = xrot + s;
    ytrasl = yrot;
     
    if(rootoutput) { 
      eventCanvas->cd();
      TMarker *pt = new TMarker(xrot, yrot, 6);
      pt->SetMarkerColor(3);
      //  if(pidHypo == 2) 
      // pt->Draw("SAME");
    }

    // change coordinate
    Double_t u, v, sigv2, sigu2;
    u = xtrasl / (xtrasl*xtrasl + ytrasl*ytrasl);
    v = ytrasl / (xtrasl*xtrasl + ytrasl*ytrasl);

    if(rootoutput){// && pidHypo == 2) { 
      eventCanvas3->cd();
      TMarker *uv = new TMarker(u, v, 6);
      uv->SetMarkerColor(3);
      if(pidHypo == 2) uv->Draw("SAME");
      eventCanvas3->Update();
      eventCanvas3->Modified();
    }
    
    if(pidHypo == 1) {
      sigr = sqrt(2.) * currenthit->GetIsochrone()/sqrt(12.);
      sigx = sigr;
      sigy = sigr;
    }
    else {
      sigr = 0.0150;
      sigx = fabs(sigr * TMath::Cos(TMath::ATan(marray.At(i))));
      sigy = fabs(sigr * TMath::Sin(TMath::ATan(marray.At(i))));
      sigxy = sigr * TMath::Sqrt(fabs(TMath::Cos(TMath::ATan(marray.At(i))) * TMath::Sin(TMath::ATan(marray.At(i)))));;
    }

    Double_t dvdx = (-2 * xtrasl * ytrasl)/pow((xtrasl*xtrasl + ytrasl*ytrasl),2);
    Double_t dvdy = (xtrasl*xtrasl - ytrasl*ytrasl) / pow((xtrasl*xtrasl + ytrasl*ytrasl),2);
    Double_t dudx = (ytrasl*ytrasl - xtrasl*xtrasl) / pow((xtrasl*xtrasl + ytrasl*ytrasl),2);
    Double_t dudy = (-2 * xtrasl * ytrasl)/pow((xtrasl*xtrasl + ytrasl*ytrasl),2);

    sigu2 = dudx * dudx * sigx * sigx + dudy * dudy * sigy * sigy + 2 * dudx * dudy * sigx * sigy; 
    sigv2 = dvdx * dvdx * sigx * sigx + dvdy * dvdy * sigy * sigy + 2 * dvdx * dvdy * sigx * sigy; 

    uarray.AddAt(u, digicounter);
    varray.AddAt(v, digicounter);
    sigu2array.AddAt(sigu2, digicounter);
    sigv2array.AddAt(sigv2, digicounter);
    
    Su = Su + (u/sigv2);
    Sv = Sv + (v/sigv2);
    
    Suv = Suv + ((u*v)/sigv2);
    Suu = Suu + ((u*u)/sigv2);
    
    Suuu = Suuu + ((u*u*u)/sigv2);
    Suuv = Suuv + ((u*u*v)/sigv2);  
    
    Suuuu = Suuuu + ((u*u*u*u)/sigv2);  
      
    S1 = S1 + 1/sigv2;

    digicounter++;
  }

  TMatrixD matrix(3,3);
  matrix[0][0] = S1;
  matrix[0][1] = Su;
  matrix[0][2] = Suu;
  
  matrix[1][0] = Su;
  matrix[1][1] = Suu;
  matrix[1][2] = Suuu;
  
  matrix[2][0] = Suu;
  matrix[2][1] = Suuu;
  matrix[2][2] = Suuuu;
  
  Double_t determ;
  
  determ = matrix.Determinant();
  
  if (determ != 0) {
    matrix.Invert();
  }
  else {
    return 0;
    cout << "DET 0" << endl;
  }
  
  TMatrixD column(3,1);
  column[0][0] = Sv;
  column[1][0] = Suv;
  column[2][0] = Suuv;
  
  TMatrixD column2(3,1);
  column2.Mult(matrix, column);
  
  Double_t a, b, c;
  a = column2[0][0];
  b = column2[1][0];
  c = column2[2][0];
  
  //     std::cout << "1) parabolic parameters:\n";
  //     std::cout << "a = " << column2[0][0] <<  "\n";
  //     std::cout << "b = " << column2[1][0] <<  "\n";
  //     std::cout << "c = " << column2[2][0] <<  "\n";
  
  Double_t chi2;
  chi2 = 0.;
  for(Int_t i=0; i < digicounter; i++){
    if(uarray.At(i) == -999 || varray.At(i) == -999 || sigv2array.At(i) == -999) continue;
    chi2 = chi2 + pow(((varray.At(i) -  (a + b*uarray.At(i) + c*uarray.At(i)*uarray.At(i))) /sqrt(sigv2array.At(i))),2) ;
  }

  //------------------ with right errors
  Su = 0.;
  Sv = 0.;
  Suu = 0.;
  Suv = 0.;
  Suuu = 0.;
  S1 = 0.;
  Suuv = 0.;
  Suuuu = 0.;
 
  TArrayD sigE2array(digicounter);
  Double_t sigE2;
  for(Int_t i=0; i< digicounter; i++){
    if(uarray.At(i) == -999 || varray.At(i) == -999 || sigv2array.At(i) == -999) continue;
    sigE2 = sigv2array.At(i) + sigu2array.At(i) * pow((b + 2*c*uarray.At(i)),2);
    sigE2array.AddAt(sigE2, i);
   
    Su = Su + (uarray.At(i)/sigE2);
    Sv = Sv + (varray.At(i)/sigE2);
    
    Suv = Suv + ((uarray.At(i)*varray.At(i))/sigE2);
    Suu = Suu + ((uarray.At(i)*uarray.At(i))/sigE2);
    
    Suuu = Suuu + ((uarray.At(i)*uarray.At(i)*uarray.At(i))/sigE2);
    Suuv = Suuv + ((uarray.At(i)*uarray.At(i)*varray.At(i))/sigE2);  
    
    Suuuu = Suuuu + ((uarray.At(i)*uarray.At(i)*uarray.At(i)*uarray.At(i))/sigE2);  
    
    S1 = S1 + 1/sigE2;
  }

  TMatrixD matrixb(3,3);
  matrixb[0][0] = S1;
  matrixb[0][1] = Su;
  matrixb[0][2] = Suu;
  
  matrixb[1][0] = Su;
  matrixb[1][1] = Suu;
  matrixb[1][2] = Suuu;
  
  matrixb[2][0] = Suu;
  matrixb[2][1] = Suuu;
  matrixb[2][2] = Suuuu;
  
  determ = matrixb.Determinant();
  
  if (determ != 0) {
    matrixb.Invert();
  }
  else {
    return 0;
    cout << "DET 0" << endl;
  }

  TMatrixD columnb(3,1);
  columnb[0][0] = Sv;
  columnb[1][0] = Suv;
  columnb[2][0] = Suuv;
  
  TMatrixD column2b(3,1);
  column2b.Mult(matrixb, columnb);
  
  a = column2b[0][0];
  b = column2b[1][0];
  c = column2b[2][0];
   
  //  std::cout << "*************\n";
  //     std::cout << "2) parabolic parameters:\n";
  //     std::cout << "a = " << column2b[0][0] <<  "\n";
  //     std::cout << "b = " << column2b[1][0] <<  "\n";
  //     std::cout << "c = " << column2b[2][0] <<  "\n";
  
  //v = a + bu + cu^2
  chi2 = 0.;
  for(Int_t i=0; i<digicounter; i++){ 
    if(uarray.At(i) == -999 || varray.At(i) == -999 || sigv2array.At(i) == -999) continue;
    chi2 = chi2 + pow(((varray.At(i) - (a + b*uarray.At(i) + c*uarray.At(i)*uarray.At(i)))/sqrt(sigE2array.At(i))), 2);
  }
  
  if(rootoutput){// && pidHypo == 2) { 
    eventCanvas3->cd();
    Double_t uu[100];
    Double_t vv[100];
    for(Int_t p = 0; p<100; p++){
      uu[p] = -1.5 + p*3*0.01;
      vv[p] = a + b*uu[p] + c*uu[p]*uu[p];
    }
    TPolyLine *uvline = new TPolyLine(100, uu, vv);
    if(pidHypo == 2) uvline->Draw("SAME");
    eventCanvas3->Update();
    eventCanvas3->Modified();
  }

  // center and radius
  Double_t xcrot, ycrot, xc, yc, epsilon, R;
  ycrot = 1/(2*a);
  xcrot = -b/(2*a);
  epsilon = -c*pow((1+(b*b)), -3/2);
  R = epsilon + sqrt((xcrot*xcrot)+(ycrot*ycrot));

  // // errors on parameters -------------------
  //     // a, b, c
  //     Double_t Da[MAXNUMSTTHITS], Db[MAXNUMSTTHITS], Dc[MAXNUMSTTHITS];
  //     Double_t p1[MAXNUMSTTHITS], pu[MAXNUMSTTHITS], puu[MAXNUMSTTHITS];
  //     Double_t erra2, errb2, errc2;
  //     erra2 = 0.;
  //     errb2 = 0.;
  //     errc2 = 0.; 
    
  //     for(Int_t i=0; i<fTrackNumSttHits[fitTrack]; i++){
  //       p1[i] = 1/sigE2[i]; 
  //       pu[i] = u[i]*p1[i];
  //       puu[i] = u[i]*pu[i];
      
  //       Da[i] = matrixb[0][0] * p1[i] + matrixb[0][1] * pu[i] + matrixb[0][2] * puu[i];
  //       Db[i] = matrixb[1][0] * p1[i] + matrixb[1][1] * pu[i] + matrixb[1][2] * puu[i];
  //       Dc[i] = matrixb[2][0] * p1[i] + matrixb[2][1] * pu[i] + matrixb[2][2] * puu[i];
      
  //       erra2 = erra2 + (Da[i]*Da[i]*sigE2[i]);
  //       errb2 = errb2 + (Db[i]*Db[i]*sigE2[i]);
  //       errc2 = errc2 + (Dc[i]*Dc[i]*sigE2[i]);
  //     }
    
  //     //  std::cout << "**************\n";
  //     //   std::cout << "erra = " <<  sqrt(erra2) << "\n";
  //     //   std::cout << "errb = " <<  sqrt(errb2)<< "\n";
  //     //   std::cout << "errc = " <<  sqrt(errc2)<< "\n";
    
  //     // errors on xc, yc, R
  //     Double_t errxcrot2, errycrot2, errR2;
  //     errxcrot2 = (1./(4*pow(a,4))) * (b*b*erra2 + a*a*errb2 - 2*a*b*sqrt(erra2*errb2)) ;
  //     errycrot2 = (1./4)*(erra2/pow(a,4)) ;
  //     errR2 = (1./(R*R)) * (ycrot*ycrot*errxcrot2 + xcrot*xcrot*errycrot2 + 2*xcrot*ycrot*sqrt(errxcrot2*errycrot2)) ;
 
  // re-rotation and re-traslation of xc and yc
  // translation
  xc = xc - s;
  // rotation    
  xc = TMath::Cos(alpha)*xcrot - TMath::Sin(alpha)*ycrot;
  yc = TMath::Sin(alpha)*xcrot + TMath::Cos(alpha)*ycrot;
  // traslation
  xc = xc + trasl[0];
  yc = yc + trasl[1];
  //     cout << "FIT2: " << xc << " " << yc << endl;
  Double_t phi = TMath::ATan2(yc, xc); // CHECK
  Double_t d;
  d = ((xc + yc) - R*(TMath::Cos(phi) + TMath::Sin(phi)))/(TMath::Cos(phi) + TMath::Sin(phi)); // CHECK
  
  pTrack->GetParamLast()->SetX(d);                
  pTrack->GetParamLast()->SetY(phi);
  //    Double_t newZ = -999.; // CHECK da cambiare
  //    pTrack->GetParamLast()->SetZ(newZ);                
  pTrack->GetParamLast()->SetTx(R);
  //    Double_t newTheta = -999.; // CHECK da cambiare
  //    pTrack->GetParamLast()->SetTy(newTheta);                
  pTrack->GetParamLast()->SetQp(0.); 

  //   cout << "FIT: " << xc << " " << yc << endl;
  //   cout << "RAGGIO: " << R << endl;

  if(rootoutput) {
    eventCanvas->cd();
    TArc *fitarc = new TArc(((pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * cos(pTrack->GetParamLast()->GetY())), ((pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * sin(pTrack->GetParamLast()->GetY())), pTrack->GetParamLast()->GetTx());
    if(pidHypo == 2)  fitarc->SetLineColor(2);
    fitarc->Draw("SAME");
    eventCanvas->Update();
    eventCanvas->Modified();
  }
  return 1;
}
Int_t CbmSttHelixTrackFitter::Fit4b(CbmSttTrack* pTrack, Int_t pidHypo) {
 
  if(!pTrack) return 0;

  Int_t hitcounter = pTrack->GetNofHits();
  Bool_t first = kFALSE;
  if(hitcounter == 0) return 0;
  if(hitcounter > 50 || hitcounter < 5) {
    cout << "Bad No of hits in STT " << hitcounter << endl;
    return 0;
  }
  cout << "FIT 4b ********************" << endl;
 
  // traslation and rotation -----------
  // traslation near the first point
  Double_t s = 0.001;
  Double_t trasl[2];
  // rotation
  Double_t alpha;
  cout << "hitcounter: " << hitcounter << endl;
  for(Int_t k = 0; k <hitcounter; k++) {
    Int_t iHit = pTrack->GetHitIndex(k);
    CbmSttHit *currenthit = (CbmSttHit*) fHitArray->At(iHit);
    if(!currenthit) continue;

    if(currenthit->GetXint() == -999 || currenthit->GetYint() == -999) continue;
    Int_t refindex = currenthit->GetRefIndex(); 
    // get point
    CbmSttPoint *iPoint = (CbmSttPoint*) fPointArray->At(refindex);
    CbmSttHit *hitfirst, *hitlast;

    TVector3 wiredirection(iPoint->GetXWireDirection(), iPoint->GetYWireDirection(), iPoint->GetZWireDirection());
    if(wiredirection != TVector3(0.,0.,1.)) continue;
    else if(first == kFALSE)
      {
	hitfirst = (CbmSttHit*) fHitArray->At(iHit); 
	first = kTRUE;
	trasl[0] = hitfirst->GetXint();
	trasl[1] = hitfirst->GetYint();
      }
    else{
      hitlast = (CbmSttHit*) fHitArray->At(iHit);  
      alpha = TMath::ATan2(hitlast->GetYint() -  hitfirst->GetYint(), hitlast->GetXint() - hitfirst->GetXint());
    }
  }  

  // error <--> resolution
  Double_t sigr, sigxy, sigx, sigy; // = 0.14;
    
  // FITTING IN X-Y PLANE:
  // v = a + bu + cu^2
    
 
  Double_t Suu, Su, Sv, Suv, S1, Suuu, Suuv, Suuuu;
  
  Su = 0.;
  Sv = 0.;
  Suu = 0.;
  Suv = 0.;
  Suuu = 0.;
  S1 = 0.;
  Suuv = 0.;
  Suuuu = 0.;
  
  Int_t digicounter = 0;
  TArrayD uarray(hitcounter);
  TArrayD varray(hitcounter);
  TArrayD sigv2array(hitcounter);
  TArrayD sigu2array(hitcounter);

  for(Int_t i=0; i < hitcounter; i++){
    uarray.AddAt(-999, i);
    varray.AddAt(-999, i);
    sigv2array.AddAt(-999, i);
    sigu2array.AddAt(-999, i);
  }
  for(Int_t i=0; i < hitcounter; i++){
   
    Int_t iHit = pTrack->GetHitIndex(i);
    CbmSttHit *currenthit = (CbmSttHit*) fHitArray->At(iHit);
    if(!currenthit) continue;
    if(currenthit->GetXint() == -999 || currenthit->GetYint() == -999) continue;
    Int_t refindex = currenthit->GetRefIndex(); 
    // get point
    CbmSttPoint *iPoint = (CbmSttPoint*) fPointArray->At(refindex);
    
    TVector3 wiredirection(iPoint->GetXWireDirection(), iPoint->GetYWireDirection(), iPoint->GetZWireDirection());
    if(wiredirection != TVector3(0.,0.,1.)) continue;
    

    if(pidHypo == 2) {
      Double_t resx = iPoint->GetXtot() - currenthit->GetXint();
      Double_t resy = iPoint->GetYtot() - currenthit->GetYint();
      Double_t resdist = TMath::Sqrt((iPoint->GetYtot() - currenthit->GetYint())*(iPoint->GetYtot() - currenthit->GetYint()) + (iPoint->GetXtot() - currenthit->GetXint())*(iPoint->GetXtot() - currenthit->GetXint()));
      
    }
 
    if(rootoutput) { 
      //     if(pidHypo == 2) {
      eventCanvas->cd(); 
      if(pidHypo == 1) {
	// draw MC hits
	TMarker *cir0 = new TMarker(iPoint->GetXtot(), iPoint->GetYtot(), 6);
	cir0->SetMarkerStyle(6);
	cir0->SetMarkerColor(4);
	//	cir0->Draw("SAME");
	//	cout << "MC: " << iPoint->GetXtot() << " " << iPoint->GetYtot() << endl;
      }
      TMarker *cir1 = new TMarker(currenthit->GetXint(), currenthit->GetYint(), 6);
      cir1->SetMarkerColor(2);
      if(pidHypo == 2) cir1->Draw("SAME");
      TMarker *cir2 = new TMarker(currenthit->GetX(), currenthit->GetY(), 6);
      if(pidHypo == 1) cir2->Draw("SAME");
      //     }
    }
 
    Double_t xtrasl, ytrasl;
    // traslation
    xtrasl = currenthit->GetXint() - trasl[0];
    ytrasl = currenthit->GetYint() - trasl[1];

    Double_t xrot, yrot;
    // rotation 
    xrot = TMath::Cos(alpha)*xtrasl + TMath::Sin(alpha)*ytrasl;
    yrot = -TMath::Sin(alpha)*xtrasl + TMath::Cos(alpha)*ytrasl;
   
    // re-traslation
    xtrasl = xrot + s;
    ytrasl = yrot;

    if(rootoutput) { 
      eventCanvas->cd();
      TMarker *pt = new TMarker(xrot, yrot, 6);
      pt->SetMarkerColor(3);
      //  if(pidHypo == 2) 
      // pt->Draw("SAME");
    }

    // change coordinate
    Double_t u, v, sigv2, sigu2;
    u = xtrasl / (xtrasl*xtrasl + ytrasl*ytrasl);
    v = ytrasl / (xtrasl*xtrasl + ytrasl*ytrasl);

    if(rootoutput){// && pidHypo == 2) { 
      eventCanvas3->cd();
      TMarker *uv = new TMarker(u, v, 6);
      uv->SetMarkerColor(3);
      if(pidHypo == 2) uv->Draw("SAME");
      eventCanvas3->Update();
      eventCanvas3->Modified();
    }

    if(pidHypo == 1) {
      if(currenthit->GetIsochrone() == 0) sigr = sqrt(2.)/sqrt(12.);
      else sigr = sqrt(2.) * currenthit->GetIsochrone()/sqrt(12.);
      sigx = sigr;
      sigy = sigr;
    }
    else {
      if(currenthit->GetIsochrone() == 0) {
	sigr = sqrt(2.)/sqrt(12.);
	sigx = sigr;
	sigy = sigr;
      }
      else {
	sigr = 0.0150;
	sigx = fabs(sigr * TMath::Cos(TMath::ATan(marray.At(i))));
	sigy = fabs(sigr * TMath::Sin(TMath::ATan(marray.At(i))));
	sigxy = sigr * TMath::Sqrt(fabs(TMath::Cos(TMath::ATan(marray.At(i))) * TMath::Sin(TMath::ATan(marray.At(i)))));;
      }
    }

    Double_t dvdx = (-2 * xtrasl * ytrasl)/pow((xtrasl*xtrasl + ytrasl*ytrasl),2);
    Double_t dvdy = (xtrasl*xtrasl - ytrasl*ytrasl) / pow((xtrasl*xtrasl + ytrasl*ytrasl),2);
    Double_t dudx = (ytrasl*ytrasl - xtrasl*xtrasl) / pow((xtrasl*xtrasl + ytrasl*ytrasl),2);
    Double_t dudy = (-2 * xtrasl * ytrasl)/pow((xtrasl*xtrasl + ytrasl*ytrasl),2);

    sigu2 = dudx * dudx * sigx * sigx + dudy * dudy * sigy * sigy + 2 * dudx * dudy * sigx * sigy; 
    sigv2 = dvdx * dvdx * sigx * sigx + dvdy * dvdy * sigy * sigy + 2 * dvdx * dvdy * sigx * sigy; 

    uarray.AddAt(u, digicounter);
    varray.AddAt(v, digicounter);
    sigu2array.AddAt(sigu2, digicounter);
    sigv2array.AddAt(sigv2, digicounter); 

    Su = Su + (u/sigv2);
    Sv = Sv + (v/sigv2);
    
    Suv = Suv + ((u*v)/sigv2);
    Suu = Suu + ((u*u)/sigv2);
    
    Suuu = Suuu + ((u*u*u)/sigv2);
    Suuv = Suuv + ((u*u*v)/sigv2);  
    
    Suuuu = Suuuu + ((u*u*u*u)/sigv2);  
      
    S1 = S1 + 1/sigv2;

    digicounter++;
  }

  TMatrixD matrix(3,3);
  matrix[0][0] = S1;
  matrix[0][1] = Su;
  matrix[0][2] = Suu;
  
  matrix[1][0] = Su;
  matrix[1][1] = Suu;
  matrix[1][2] = Suuu;
  
  matrix[2][0] = Suu;
  matrix[2][1] = Suuu;
  matrix[2][2] = Suuuu;
  
  Double_t determ;
  
  determ = matrix.Determinant();
  
  if (determ != 0) {
    matrix.Invert();
  }
  else {
    return 0;
    cout << "DET 0" << endl;
  }
  
  TMatrixD column(3,1);
  column[0][0] = Sv;
  column[1][0] = Suv;
  column[2][0] = Suuv;
  
  TMatrixD column2(3,1);
  column2.Mult(matrix, column);
  
  Double_t a, b, c;
  a = column2[0][0];
  b = column2[1][0];
  c = column2[2][0];
  
  //     std::cout << "1) parabolic parameters:\n";
  //     std::cout << "a = " << column2[0][0] <<  "\n";
  //     std::cout << "b = " << column2[1][0] <<  "\n";
  //     std::cout << "c = " << column2[2][0] <<  "\n";
  
  Double_t chi2;
  chi2 = 0.;
  for(Int_t i=0; i < digicounter; i++){
    if(uarray.At(i) == -999 || varray.At(i) == -999 || sigv2array.At(i) == -999) continue;
    chi2 = chi2 + pow(((varray.At(i) -  (a + b*uarray.At(i) + c*uarray.At(i)*uarray.At(i))) /sqrt(sigv2array.At(i))),2) ;
  }

  cout << "digicounter: " << digicounter << endl;

  //------------------ with right errors
  Su = 0.;
  Sv = 0.;
  Suu = 0.;
  Suv = 0.;
  Suuu = 0.;
  S1 = 0.;
  Suuv = 0.;
  Suuuu = 0.;
 
  TArrayD sigE2array(digicounter);
  Double_t sigE2;
  for(Int_t i=0; i< digicounter; i++){
    if(uarray.At(i) == -999 || varray.At(i) == -999 || sigv2array.At(i) == -999) continue;
    sigE2 = sigv2array.At(i) + sigu2array.At(i) * pow((b + 2*c*uarray.At(i)),2);
    sigE2array.AddAt(sigE2, i);
   
    Su = Su + (uarray.At(i)/sigE2);
    Sv = Sv + (varray.At(i)/sigE2);
    
    Suv = Suv + ((uarray.At(i)*varray.At(i))/sigE2);
    Suu = Suu + ((uarray.At(i)*uarray.At(i))/sigE2);
    
    Suuu = Suuu + ((uarray.At(i)*uarray.At(i)*uarray.At(i))/sigE2);
    Suuv = Suuv + ((uarray.At(i)*uarray.At(i)*varray.At(i))/sigE2);  
    
    Suuuu = Suuuu + ((uarray.At(i)*uarray.At(i)*uarray.At(i)*uarray.At(i))/sigE2);  
    
    S1 = S1 + 1/sigE2;
  }

  TMatrixD matrixb(3,3);
  matrixb[0][0] = S1;
  matrixb[0][1] = Su;
  matrixb[0][2] = Suu;
  
  matrixb[1][0] = Su;
  matrixb[1][1] = Suu;
  matrixb[1][2] = Suuu;
  
  matrixb[2][0] = Suu;
  matrixb[2][1] = Suuu;
  matrixb[2][2] = Suuuu;
  
  determ = matrixb.Determinant();
  
  if (determ != 0) {
    matrixb.Invert();
  }
  else {
    return 0;
    cout << "DET 0" << endl;
  }

  TMatrixD columnb(3,1);
  columnb[0][0] = Sv;
  columnb[1][0] = Suv;
  columnb[2][0] = Suuv;
  
  TMatrixD column2b(3,1);
  column2b.Mult(matrixb, columnb);
  
  a = column2b[0][0];
  b = column2b[1][0];
  c = column2b[2][0];
   
  //  std::cout << "*************\n";
  //     std::cout << "2) parabolic parameters:\n";
  //     std::cout << "a = " << column2b[0][0] <<  "\n";
  //     std::cout << "b = " << column2b[1][0] <<  "\n";
  //     std::cout << "c = " << column2b[2][0] <<  "\n";

  //v = a + bu + cu^2
  chi2 = 0.;
  for(Int_t i=0; i<digicounter; i++){ 
    if(uarray.At(i) == -999 || varray.At(i) == -999 || sigv2array.At(i) == -999) continue;
    chi2 = chi2 + pow(((varray.At(i) - (a + b*uarray.At(i) + c*uarray.At(i)*uarray.At(i)))/sqrt(sigE2array.At(i))), 2);
  }
  
  if(rootoutput){// && pidHypo == 2) { 
    eventCanvas3->cd();
    Double_t uu[100];
    Double_t vv[100];
    for(Int_t p = 0; p<100; p++){
      uu[p] = -1.5 + p*3*0.01;
      vv[p] = a + b*uu[p] + c*uu[p]*uu[p];
    }
    TPolyLine *uvline = new TPolyLine(100, uu, vv);
    if(pidHypo == 2) uvline->Draw("SAME");
    eventCanvas3->Update();
    eventCanvas3->Modified();
  }

  // center and radius
  Double_t xcrot, ycrot, xc, yc, epsilon, R;
  ycrot = 1/(2*a);
  xcrot = -b/(2*a);
  epsilon = -c*pow((1+(b*b)), -3/2);
  R = epsilon + sqrt((xcrot*xcrot)+(ycrot*ycrot));

  // // errors on parameters -------------------
  //     // a, b, c
  //     Double_t Da[MAXNUMSTTHITS], Db[MAXNUMSTTHITS], Dc[MAXNUMSTTHITS];
  //     Double_t p1[MAXNUMSTTHITS], pu[MAXNUMSTTHITS], puu[MAXNUMSTTHITS];
  //     Double_t erra2, errb2, errc2;
  //     erra2 = 0.;
  //     errb2 = 0.;
  //     errc2 = 0.; 
    
  //     for(Int_t i=0; i<fTrackNumSttHits[fitTrack]; i++){
  //       p1[i] = 1/sigE2[i]; 
  //       pu[i] = u[i]*p1[i];
  //       puu[i] = u[i]*pu[i];
      
  //       Da[i] = matrixb[0][0] * p1[i] + matrixb[0][1] * pu[i] + matrixb[0][2] * puu[i];
  //       Db[i] = matrixb[1][0] * p1[i] + matrixb[1][1] * pu[i] + matrixb[1][2] * puu[i];
  //       Dc[i] = matrixb[2][0] * p1[i] + matrixb[2][1] * pu[i] + matrixb[2][2] * puu[i];
      
  //       erra2 = erra2 + (Da[i]*Da[i]*sigE2[i]);
  //       errb2 = errb2 + (Db[i]*Db[i]*sigE2[i]);
  //       errc2 = errc2 + (Dc[i]*Dc[i]*sigE2[i]);
  //     }
    
  //     //  std::cout << "**************\n";
  //     //   std::cout << "erra = " <<  sqrt(erra2) << "\n";
  //     //   std::cout << "errb = " <<  sqrt(errb2)<< "\n";
  //     //   std::cout << "errc = " <<  sqrt(errc2)<< "\n";
    
  //     // errors on xc, yc, R
  //     Double_t errxcrot2, errycrot2, errR2;
  //     errxcrot2 = (1./(4*pow(a,4))) * (b*b*erra2 + a*a*errb2 - 2*a*b*sqrt(erra2*errb2)) ;
  //     errycrot2 = (1./4)*(erra2/pow(a,4)) ;
  //     errR2 = (1./(R*R)) * (ycrot*ycrot*errxcrot2 + xcrot*xcrot*errycrot2 + 2*xcrot*ycrot*sqrt(errxcrot2*errycrot2)) ;
 
  // re-rotation and re-traslation of xc and yc
  // translation
  xc = xc - s;
  // rotation    
  xc = TMath::Cos(alpha)*xcrot - TMath::Sin(alpha)*ycrot;
  yc = TMath::Sin(alpha)*xcrot + TMath::Cos(alpha)*ycrot;
  // traslation
  xc = xc + trasl[0];
  yc = yc + trasl[1];
  Double_t phi = TMath::ATan2(yc, xc); // CHECK
  Double_t d;
  d = ((xc + yc) - R*(TMath::Cos(phi) + TMath::Sin(phi)))/(TMath::Cos(phi) + TMath::Sin(phi)); // CHECK
  
  pTrack->GetParamLast()->SetX(d);                
  pTrack->GetParamLast()->SetY(phi);
  //    Double_t newZ = -999.; // CHECK da cambiare
  //    pTrack->GetParamLast()->SetZ(newZ);                
  pTrack->GetParamLast()->SetTx(R);
  //    Double_t newTheta = -999.; // CHECK da cambiare
  //    pTrack->GetParamLast()->SetTy(newTheta);                
  pTrack->GetParamLast()->SetQp(0.); 

  //   cout << "FIT: " << xc << " " << yc << endl;
  //   cout << "RAGGIO: " << R << endl;

  if(rootoutput) {
    eventCanvas->cd();
    TArc *fitarc = new TArc(((pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * cos(pTrack->GetParamLast()->GetY())), ((pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * sin(pTrack->GetParamLast()->GetY())), pTrack->GetParamLast()->GetTx());
    if(pidHypo == 2)  fitarc->SetLineColor(2);
    fitarc->Draw("SAME");
    eventCanvas->Update();
    eventCanvas->Modified();
  }
  return 1;
}


// -------------- IntersectionFinder  --------------------------------------
Bool_t CbmSttHelixTrackFitter::IntersectionFinder(CbmSttTrack *pTrack, FairTrackParam *par)
{

  ResetMArray();

  // calculation of the curvature from the helix prefit
  if(pTrack->GetParamLast()->GetTx() == 0) return kFALSE;

  TVector2 vec((pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * cos(pTrack->GetParamLast()->GetY()), (pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * sin(pTrack->GetParamLast()->GetY()));

  //==========
  // POINT ----------------------------------------------------
  // 1. find the cooordinates of the point fired wire of the track
  TVector2 point; // point
  Double_t radius;

  Int_t counter = 0;
  // loop over input points
  Int_t hitcounter = pTrack->GetNofHits();
  for(Int_t k = 0; k < hitcounter; k++){
    // get hit
    Int_t iHit = pTrack->GetHitIndex(k);
    CbmSttHit *pMhit = (CbmSttHit*) fHitArray->At(iHit);
    if (!pMhit ) continue;

    Int_t refindex = pMhit->GetRefIndex(); 
    // get point
    CbmSttPoint *iPoint = (CbmSttPoint*) fPointArray->At(refindex);
    TVector3 wiredirection(iPoint->GetXWireDirection(), iPoint->GetYWireDirection(), iPoint->GetZWireDirection());
    
    if(wiredirection != TVector3(0.,0.,1.)) continue;

    // [xp, yp] point = coordinates xy of the centre of the firing tube
    point.Set(pMhit->GetX(), pMhit->GetY());
    radius = pMhit->GetIsochrone();

    // the coordinates of the point are taken from the intersection
    // between the circumference from the drift time and the R radius of
    // curvature. -------------------------------------------------------
    // 2. find the intersection between the little circle and the line // R
    TVector2 first;
    TVector2 second;
    // 2.a
    // find the line passing throught [xc, yc] (centre of curvature) and [xp, yp] (first wire)
    // y = mx + q
    Double_t m = (point.Y() - vec.Y())/(point.X() - vec.X());
    Double_t q = point.Y() - m*point.X();

    if(rootoutput) { 
      eventCanvas->cd();
      TArc *archetto = new TArc(point.X(), point.Y(), radius);
      archetto->Draw("SAME");
      TLine *line = new TLine(-50, -50*m + q, 50, 50*m + q);
      line->SetLineColor(4);
      // line->Draw("SAME"); 
      eventCanvas->Update();
      eventCanvas->Modified();
    }
   
    // cut on radius CHECK
    // if the simulated radius is too small, the pMhit
    // is not used for the fit 
    if(radius < 0.1) {
      marray.AddAt(-999, k);
      pMhit->SetXint(-999);
      pMhit->SetYint(-999);
      continue;  // CHECK cosi' butto l' hit
    }
    // 2.b
    // intersection little circle and line --> [x1, y1]
    // + and - refer to the 2 possible intersections
    // +
    Double_t x1 = (-(m*(q - point.Y()) - point.X()) + sqrt((m*(q - point.Y()) - point.X())*(m*(q - point.Y()) - point.X()) - (m*m + 1)*((q - point.Y())*(q - point.Y()) + point.X()*point.X() - radius*radius))) / (m*m + 1);
    Double_t y1 = m*x1 + q;
    first.Set(x1, y1);
   
    // - 
    Double_t x2 = (-(m*(q - point.Y()) - point.X()) - sqrt((m*(q - point.Y()) - point.X())*(m*(q - point.Y()) - point.X()) - (m*m + 1)*((q - point.Y())*(q - point.Y()) + point.X()*point.X() - radius*radius))) / (m*m + 1);
    Double_t y2 = m*x2 + q;
    second.Set(x2, y2);
    
    // 2.c intersection between line and circle
    // +
    Double_t xb1 = (-(m*(q - vec.Y()) - vec.X()) + sqrt((m*(q - vec.Y()) - vec.X())*(m*(q - vec.Y()) - vec.X()) - (m*m + 1)*((q - vec.Y())*(q - vec.Y()) + vec.X()*vec.X() - (pTrack->GetParamLast()->GetTx()) *(pTrack->GetParamLast()->GetTx()) ))) / (m*m + 1);
    Double_t yb1 = m*xb1 + q;
    // -
    Double_t xb2 = (-(m*(q - vec.Y()) - vec.X()) - sqrt((m*(q - vec.Y()) - vec.X())*(m*(q - vec.Y()) - vec.X()) - (m*m + 1)*((q - vec.Y())*(q - vec.Y()) + vec.X()*vec.X() - (pTrack->GetParamLast()->GetTx()) *(pTrack->GetParamLast()->GetTx())))) / (m*m + 1);
    Double_t yb2 = m*xb2 + q;
    
    // calculation of the distance between [xb, yb] and [xp, yp]
    Double_t distb1 = sqrt((yb1 - point.Y())*(yb1 - point.Y()) + (xb1 - point.X())*(xb1 - point.X()));
    Double_t distb2 = sqrt((yb2 - point.Y())*(yb2 - point.Y()) + (xb2 - point.X())*(xb2 - point.X()));
    
    // choice of [xb, yb]
    TVector2 xyb;
    if(distb1 > distb2) xyb.Set(xb2, yb2); 
    else xyb.Set(xb1, yb1); 

    // calculation of the distance between [x, y] and [xb. yb]
    Double_t dist1 = sqrt((xyb.Y() - y1)*(xyb.Y() - y1) + (xyb.X() - x1)*(xyb.X() - x1));
    Double_t dist2 = sqrt((xyb.Y() - y2)*(xyb.Y() - y2) + (xyb.X() - x2)*(xyb.X() - x2));

    // choice of [x, y]
    TVector2 *xy;
    if(dist1 > dist2) xy = new TVector2(x2, y2);
    else xy = new TVector2(x1, y1);   // <========= THIS IS THE NEW POINT to be used for the fit

    // SET AS DEBUG
    //    if (TMath::Sqrt((xy->X() - point.X())*(xy->X() - point.X()) + (xy->Y() - point.Y())*(xy->Y() - point.Y())) - radius > 0.000001) { 
    //      cout << "ATTENZIONE: " << "differenza = " << TMath::Sqrt((xy->X() - point.X())*(xy->X() - point.X()) + (xy->Y() - point.Y())*(xy->Y() - point.Y())) - radius << endl;
    //    }

    //    cout << "x hit prima: " << pMhit->GetX() << " " << pMhit->GetXint() << endl;
    //    cout << prima: " << pMhit->GetXint() << " " << pMhit->GetYint() << " " << pMhit->GetZint() << endl;

    marray.AddAt(m, k);
    pMhit->SetXint(xy->X());
    pMhit->SetYint(xy->Y());
    
    if(rootoutput) { 
      eventCanvas->cd();
      TMarker *np = new TMarker(pMhit->GetXint(), pMhit->GetYint(), 6);
      np->SetMarkerColor(4);
      //      np->Draw("SAME");
      eventCanvas->Update();
      eventCanvas->Modified();
    }

    counter++;
  }

  if(counter==0) return kFALSE;
  else return kTRUE;
}

// -------------- IntersectionFinder  --------------------------------------
Bool_t CbmSttHelixTrackFitter::IntersectionFinder4b(CbmSttTrack *pTrack, FairTrackParam *par)
// if the drift radius is too small the center of the tube is used
{

  ResetMArray();

  // calculation of the curvature from the helix prefit
  if(pTrack->GetParamLast()->GetTx() == 0) return kFALSE;

  TVector2 vec((pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * cos(pTrack->GetParamLast()->GetY()), (pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * sin(pTrack->GetParamLast()->GetY()));

  //==========
  // POINT ----------------------------------------------------
  // 1. find the cooordinates of the point fired wire of the track
  TVector2 point; // point
  Double_t radius;

  Int_t counter = 0;
  // loop over input points
  Int_t hitcounter = pTrack->GetNofHits();
  for(Int_t k = 0; k < hitcounter; k++){
    // get hit
    Int_t iHit = pTrack->GetHitIndex(k);
    CbmSttHit *pMhit = (CbmSttHit*) fHitArray->At(iHit);
    if (!pMhit ) continue;

    Int_t refindex = pMhit->GetRefIndex(); 
    // get point
    CbmSttPoint *iPoint = (CbmSttPoint*) fPointArray->At(refindex);
    TVector3 wiredirection(iPoint->GetXWireDirection(), iPoint->GetYWireDirection(), iPoint->GetZWireDirection());
    
    if(wiredirection != TVector3(0.,0.,1.)) continue;


    // [xp, yp] point = coordinates xy of the centre of the firing tube
    point.Set(pMhit->GetX(), pMhit->GetY());
    radius = pMhit->GetIsochrone();

    // the coordinates of the point are taken from the intersection
    // between the circumference from the drift time and the R radius of
    // curvature. -------------------------------------------------------
    // 2. find the intersection between the little circle and the line // R
    TVector2 first;
    TVector2 second;
    // 2.a
    // find the line passing throught [xc, yc] (centre of curvature) and [xp, yp] (first wire)
    // y = mx + q
    Double_t m = (point.Y() - vec.Y())/(point.X() - vec.X());
    Double_t q = point.Y() - m*point.X();

    if(rootoutput) { 
      eventCanvas->cd();
      TArc *archetto = new TArc(point.X(), point.Y(), radius);
      archetto->Draw("SAME");
      TLine *line = new TLine(-50, -50*m + q, 50, 50*m + q);
      line->SetLineColor(4);
      // line->Draw("SAME"); 
      eventCanvas->Update();
      eventCanvas->Modified();
    }
   
    // cut on radius CHECK
    // if the simulated radius is too small, the pMhit
    // is not used for the fit 
    if(radius < 0.1) {
      marray.AddAt(-999, k);
      pMhit->SetXint(pMhit->GetX());
      pMhit->SetYint(pMhit->GetY());
      continue;  // CHECK cosi' butto l' hit
    }
    // 2.b
    // intersection little circle and line --> [x1, y1]
    // + and - refer to the 2 possible intersections
    // +
    Double_t x1 = (-(m*(q - point.Y()) - point.X()) + sqrt((m*(q - point.Y()) - point.X())*(m*(q - point.Y()) - point.X()) - (m*m + 1)*((q - point.Y())*(q - point.Y()) + point.X()*point.X() - radius*radius))) / (m*m + 1);
    Double_t y1 = m*x1 + q;
    first.Set(x1, y1);
   
    // - 
    Double_t x2 = (-(m*(q - point.Y()) - point.X()) - sqrt((m*(q - point.Y()) - point.X())*(m*(q - point.Y()) - point.X()) - (m*m + 1)*((q - point.Y())*(q - point.Y()) + point.X()*point.X() - radius*radius))) / (m*m + 1);
    Double_t y2 = m*x2 + q;
    second.Set(x2, y2);
    
    // 2.c intersection between line and circle
    // +
    Double_t xb1 = (-(m*(q - vec.Y()) - vec.X()) + sqrt((m*(q - vec.Y()) - vec.X())*(m*(q - vec.Y()) - vec.X()) - (m*m + 1)*((q - vec.Y())*(q - vec.Y()) + vec.X()*vec.X() - (pTrack->GetParamLast()->GetTx()) *(pTrack->GetParamLast()->GetTx()) ))) / (m*m + 1);
    Double_t yb1 = m*xb1 + q;
    // -
    Double_t xb2 = (-(m*(q - vec.Y()) - vec.X()) - sqrt((m*(q - vec.Y()) - vec.X())*(m*(q - vec.Y()) - vec.X()) - (m*m + 1)*((q - vec.Y())*(q - vec.Y()) + vec.X()*vec.X() - (pTrack->GetParamLast()->GetTx()) *(pTrack->GetParamLast()->GetTx())))) / (m*m + 1);
    Double_t yb2 = m*xb2 + q;
    
    // calculation of the distance between [xb, yb] and [xp, yp]
    Double_t distb1 = sqrt((yb1 - point.Y())*(yb1 - point.Y()) + (xb1 - point.X())*(xb1 - point.X()));
    Double_t distb2 = sqrt((yb2 - point.Y())*(yb2 - point.Y()) + (xb2 - point.X())*(xb2 - point.X()));
    
    // choice of [xb, yb]
    TVector2 xyb;
    if(distb1 > distb2) xyb.Set(xb2, yb2); 
    else xyb.Set(xb1, yb1); 

    // calculation of the distance between [x, y] and [xb. yb]
    Double_t dist1 = sqrt((xyb.Y() - y1)*(xyb.Y() - y1) + (xyb.X() - x1)*(xyb.X() - x1));
    Double_t dist2 = sqrt((xyb.Y() - y2)*(xyb.Y() - y2) + (xyb.X() - x2)*(xyb.X() - x2));

    // choice of [x, y]
    TVector2 *xy;
    if(dist1 > dist2) xy = new TVector2(x2, y2);
    else xy = new TVector2(x1, y1);   // <========= THIS IS THE NEW POINT to be used for the fit

    // SET AS DEBUG
    //    if (TMath::Sqrt((xy->X() - point.X())*(xy->X() - point.X()) + (xy->Y() - point.Y())*(xy->Y() - point.Y())) - radius > 0.000001) { 
    //      cout << "ATTENZIONE: " << "differenza = " << TMath::Sqrt((xy->X() - point.X())*(xy->X() - point.X()) + (xy->Y() - point.Y())*(xy->Y() - point.Y())) - radius << endl;
    //    }

    //    cout << "x hit prima: " << pMhit->GetX() << " " << pMhit->GetXint() << endl;
    //    cout << prima: " << pMhit->GetXint() << " " << pMhit->GetYint() << " " << pMhit->GetZint() << endl;

    marray.AddAt(m, k);
    pMhit->SetXint(xy->X());
    pMhit->SetYint(xy->Y());

    if(rootoutput) { 
      eventCanvas->cd();
      TMarker *np = new TMarker(pMhit->GetXint(), pMhit->GetYint(), 6);
      np->SetMarkerColor(4);
      //      np->Draw("SAME");
      eventCanvas->Update();
      eventCanvas->Modified();
    }
   
    counter++;
  }

  if(counter==0) return kFALSE;
  else return kTRUE;
}



// -------- ZFinder --------------------------------------------
Bool_t CbmSttHelixTrackFitter::ZFinder(CbmSttTrack* pTrack, Int_t pidHypo) {

  if(!pTrack) return 0;

  Int_t hitcounter = pTrack->GetNofHits();
 
  // cut on number of hits
  if(hitcounter > 30) {
    //    cout << "more than 30 hits" << endl;
    //  pTrack->GetParamLast()->SetX(-999);                
    //     pTrack->GetParamLast()->SetY(-999);
    //     //    Double_t newZ = -999.; // CHECK da cambiare
    //     //    pTrack->GetParamLast()->SetZ(newZ);                
    //     pTrack->GetParamLast()->SetTx(-999);
    //     //    Double_t newTheta = -999.; // CHECK da cambiare
    //     //    pTrack->GetParamLast()->SetTy(newTheta);                
    //     pTrack->GetParamLast()->SetQp(0.);
    return 0;
  }
  if(hitcounter < 5)  {
    //    cout << "less than 5 hits" << endl;
    //     pTrack->GetParamLast()->SetX(-999);                
    //     pTrack->GetParamLast()->SetY(-999);
    //     //    Double_t newZ = -999.; // CHECK da cambiare
    //     //    pTrack->GetParamLast()->SetZ(newZ);                
    //     pTrack->GetParamLast()->SetTx(-999);
    //     //    Double_t newTheta = -999.; // CHECK da cambiare
    //     //    pTrack->GetParamLast()->SetTy(newTheta);                
    //     pTrack->GetParamLast()->SetQp(0.);
    return 0;
  }
  
  ZPointsArray = new TObjArray(2*hitcounter);

  // SCOSL ======
  // phi0
  TVector3 *v0;
  Double_t Phi0;
  // ============

  // ZFIT =======
  if(hitcounter == 0) return kFALSE;
  
  Double_t Sxx, Sx, Sz, Sxz, S1z;
  Double_t Detz = 0.;
  Double_t fitm, fitp;
  Double_t sigz = 1.;  // CHECK
  
  Sx = 0.;
  Sz = 0.;
  Sxx = 0.;
  Sxz = 0.;
  S1z = 0.;
  // ============

  TVector3 *tofit, *tofit2;
 
  // centre of curvature
  Double_t x_0 = (pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * cos(pTrack->GetParamLast()->GetY());
  Double_t y_0 = (pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * sin(pTrack->GetParamLast()->GetY());
  // radius of curvature
  Double_t R  = pTrack->GetParamLast()->GetTx();
  Int_t wireOk = 0;
  for (Int_t i = 0; i < hitcounter; i++) {
   
    // get index of hit
    Int_t iHit = pTrack->GetHitIndex(i);

    // get hit
    CbmSttHit *pMhit = (CbmSttHit*) fHitArray->At(iHit);

    if(pMhit == NULL) continue;

    Int_t refindex = pMhit->GetRefIndex(); 
    // get point
    CbmSttPoint *iPoint = (CbmSttPoint*) fPointArray->At(refindex);
    TVector3 wiredirection(iPoint->GetXWireDirection(), iPoint->GetYWireDirection(), iPoint->GetZWireDirection());
    TVector3 wiredirection2;

    wiredirection2 = 75. * wiredirection;
    TVector3 cenposition(pMhit->GetX(), pMhit->GetY(), 0.);  // CHECK! z = 35!!


    TVector3 min, max;
    min = cenposition - wiredirection2;
    max = cenposition + wiredirection2;
    
    // first extremity
    Double_t x_1= min.X(); 
    Double_t y_1= min.Y(); 
    Double_t z_1= min.Z(); 
    
    // second extremity
    Double_t x_2= max.X();
    Double_t y_2= max.Y();
    Double_t z_2= max.Z();
    
    Double_t rcur = pMhit->GetIsochrone();

    if(wiredirection != TVector3(0.,0.,1.)) 
      {
	wireOk++;
	Double_t a = -999;
	Double_t b = -999;

        Double_t x1 = -9999.;
        Double_t y1 = -9999.;
        Double_t x2 = -9999.;
        Double_t y2 = -9999.;
      
	// intersection point between the reconstructed 
	// circumference and the line joining the centres
	// of the reconstructed circle and the i_th drift circle
	if(fabs(x_2-x_1)>0.0001) {
	  a =(y_2-y_1)/(x_2-x_1);
	  b =(y_1-a*x_1);
	  Double_t A = a*a+1;
	  Double_t B = x_0+a*y_0-a*b;
	  Double_t C = x_0*x_0+y_0*y_0+b*b-R*R-2*y_0*b;
	  if((B*B-A*C)>0) {
	    x1= (B+TMath::Sqrt(B*B-A*C))/A;
	    x2= (B-TMath::Sqrt(B*B-A*C))/A;
	    y1=a*x1+b;
	    y2=a*x2+b;
	  }
	}
	else if(fabs(y_2-y_1)>0.0001) {
	  Double_t A = 1;
	  Double_t B = y_0;
	  Double_t C = y_0*y_0 +(x_1-x_0)*(x_1-x_0) -R*R;

	  if((B*B-A*C)>0) {
	    y1= (B+TMath::Sqrt(B*B-A*C))/A;
	    y2= (B-TMath::Sqrt(B*B-A*C))/A;
	    x1=x2=x_1;
	  }
	  
	}
	
	//x1 and x2 are the 2 intersection points
	Double_t d1=TMath::Sqrt((x1-cenposition.X())*(x1-cenposition.X())+
				(y1-cenposition.Y())*(y1-cenposition.Y()));
	Double_t d2=TMath::Sqrt((x2-cenposition.X())*(x2-cenposition.X())+
				(y2-cenposition.Y())*(y2-cenposition.Y()));
	
	Double_t x_ = x1;
	Double_t y_ = y1;
	
	// the intersection point nearest to the drift circle's centre is taken
	if(d2<d1) {x_=x2;y_=y2;}    
	
	// now we need to find the actual centre of the drift circle,
	// by translating the drift circle until it becomes tangent
	// to the reconstructed circle. 
	// Two solutions are possible (left rigth abiguity), 
	// they are both kept, only the following zed fit will discard the wrong ones.
	// Using the parametric equation of the 3d-straigth line and taking the
	// x points just obtained, the zed coordinate of the skewed tube centre is calculated.
	
	//solving the equation to find out the centre of the tangent circle
	Double_t A = a*a+1;
	Double_t B = -(a*b-a*y_-x_);
	Double_t C = x_*x_+ y_*y_+b*b-2*b*y_-rcur*rcur;
	if((B*B-A*C)>0) {
	  x1= (B+TMath::Sqrt(B*B-A*C))/A;
	  x2= (B-TMath::Sqrt(B*B-A*C))/A;
	  y1=a*x1+b;
	  y2=a*x2+b;
	}
	
	d1=TMath::Sqrt((x1-cenposition.X())*(x1-cenposition.X())+(y1-cenposition.Y())*(y1-cenposition.Y()));
	d2=TMath::Sqrt((x2-cenposition.X())*(x2-cenposition.X())+(y2-cenposition.Y())*(y2-cenposition.Y()));
	
	Double_t xcen0=x1;
	Double_t xcen1=x2;
	Double_t ycen0=y1;
	Double_t ycen1=y2;
	
	if(d2<d1) { // z2 contains the points nearest (in x-y) to the initial centre of the skewed tube
	  xcen0=x2;
	  xcen1=x1;
	  ycen0=y2;
	  ycen1=y1;
	  
	}
	
	// zed association
	if(fabs(x_2-x_1)<0.001) return kFALSE;
	Double_t t_      =(xcen0-x_1)/(x_2-x_1); // x= a_x*t + x_1 [t=1 x=x_2]
	Double_t z_      =(z_2-z_1)*t_ +z_1;     // z= a_z*t + z_1 [t=1 z=z_2]

	Double_t t_bis   =(xcen1-x_1)/(x_2-x_1); // from x_'s (the 2 solutions of the 2nd order equation)
	Double_t z_bis   =(z_2-z_1)*t_bis +z_1;  // and the 2 parametric equations the z coord. are obtained 

	tofit = new TVector3(xcen0,ycen0,z_);
	ZPointsArray->Add(tofit);
	tofit2 = new TVector3(xcen1,ycen1,z_bis);
	ZPointsArray->Add(tofit2);
   
	// FIRST CHOICE
	//====================  
	// scosl
	Double_t x0;
	Double_t y0;
	if(wireOk == 1) {
	  if(tofit != NULL) {
	    // phi0
	    v0 = new TVector3(tofit->X(), tofit->Y(), tofit->Z());
	  }
	  else if(tofit2 != NULL) {
	    v0 = new TVector3(tofit2->X(), tofit2->Y(), tofit2->Z());
	  }
	   
	  Phi0 = TMath::ATan2((v0->Y() - y_0),(v0->X() - x_0));
	  
	  // CHECK
	  // we are using the first digi for the arc length calculation
	  // but this may be wrong
	  x0 = v0->X();
	  y0 = v0->Y();
	}
	
	TVector3 vi(tofit->X(), tofit->Y(), tofit->Z());
	Double_t scos = R*TMath::ATan2((vi.Y() - y0)*TMath::Cos(Phi0) - (vi.X() - x0)*TMath::Sin(Phi0) , R + (vi.X() - x0) * TMath::Cos(Phi0) + (vi.Y()-y0) * TMath::Sin(Phi0));
	//====================  
	if(rootoutput) {
	  eventCanvas2->cd();
	  TMarker *mrk = new TMarker(scos, tofit->Z(), 6);
	  mrk->Draw("SAME");
	}
	//====================  
	// zfit
	//
	Sx = Sx + (scos/(sigz * sigz));
	Sz = Sz + (tofit->Z()/(sigz * sigz));
	Sxz = Sxz + ((scos * tofit->Z())/(sigz * sigz));
	Sxx = Sxx + ((scos * scos)/(sigz * sigz));
	S1z = S1z + 1/(sigz * sigz);
	//====================

	// SECOND CHOICE
	//====================  
	// scosl
	vi.SetXYZ(tofit2->X(), tofit2->Y(), tofit2->Z());
	scos = R*TMath::ATan2((vi.Y() - y0)*TMath::Cos(Phi0) - (vi.X() - x0)*TMath::Sin(Phi0) , R + (vi.X() - x0) * TMath::Cos(Phi0) + (vi.Y()-y0) * TMath::Sin(Phi0));
	//====================  
	
	//====================  
	// zfit
	//
	Sx = Sx + (scos/(sigz * sigz));
	Sz = Sz + (tofit2->Z()/(sigz * sigz));
	Sxz = Sxz + ((scos * tofit2->Z())/(sigz * sigz));
	Sxx = Sxx + ((scos * scos)/(sigz * sigz));
	S1z = S1z + 1/(sigz * sigz);
	//====================
	if(rootoutput) {     
	  eventCanvas2->cd();
	  TMarker *mrk2 = new TMarker(scos, tofit2->Z(), 6);
	  mrk2->Draw("SAME");
	}
      }
    
  }
  cout << "skewed: " << wireOk << endl;
  if(wireOk < 2) return kFALSE;
 
  Detz = S1z*Sxx - Sx*Sx;
  if(Detz == 0) {
    return kFALSE;
  }
 
  fitp = (1/Detz)*(Sxx*Sz - Sx*Sxz);
  fitm = (1/Detz)*(S1z*Sxz - Sx*Sz);
  //   fiterrp2 = (1/Detz)*Sxx;
  //   fiterrm2 = (1/Detz)*S1z;

  if(rootoutput) {  
    eventCanvas2->cd();
    TLine *line = new TLine(-20, -20*fitm + fitp, 20, (20*fitm + fitp));
    line->Draw("SAME");
  }
 
 
  //  Double_t chi2z;
  //  chi2z = 0.;
  //  for(Int_t i=0; i < hitcounter; i++) {
  //    TVector3 * vi= (TVector3 *) ZArray->At(i);
  //    chi2z = chi2z + pow(((vi->Z()-(fitp+fitm*scosl->At(i)))/sigz), 2);
  //       // RESIDUALS
  //       //    if(pow(((vi->Z()-(fitp+fitm*scosl[i]))/sigz), 2)>10) continue;
  //       //   bestz[ctbest]=vi->Z();
  //       //   bests[ctbest]=scosl[i];
  //       //   ctbest++;
  //     }
 
  // y = m*x + p
  TVector2 outz(fitm, fitp);
  Int_t counter = 0;

  if(fitm == 0. && fitp == 0.) return kFALSE;
 
  if( wireOk < 2) return kFALSE;

  wireOk = 0;
  Int_t okcounter = 0;
  for(Int_t i = 0; i < (2*hitcounter); i+=2) {
    // get index of hit
    //   cout << i << " counter: " << counter << endl;
    Int_t iHit = pTrack->GetHitIndex(counter);
    counter++;
   
    // get hit
    CbmSttHit *pMhit = (CbmSttHit*) fHitArray->At(iHit);
  
    Int_t refindex = pMhit->GetRefIndex(); 
    // get point
    CbmSttPoint *iPoint = (CbmSttPoint*) fPointArray->At(refindex);
    TVector3 wiredirection(iPoint->GetXWireDirection(), iPoint->GetYWireDirection(), iPoint->GetZWireDirection());

    if(wiredirection == TVector3(0.,0.,1.)) continue;
    wireOk++;
   
    sigz = 1.; // CHECK
   
    Double_t x0;
    Double_t y0;
    if(wireOk==1) {
      TVector3 *vi = (TVector3*) ZPointsArray->At(okcounter);
      // phi0
      v0->SetXYZ(vi->X(), vi->Y(), vi->Z());
      Phi0 = TMath::ATan2((v0->Y() - y_0),(v0->X() - x_0));
     
      // CHECK
      // we are using the first digi for the arc length calculation
      // but this may be wrong
      x0 = v0->X();
      y0 = v0->Y();
    }
   
    // FIRST CHOICE
    TVector3 *vi = (TVector3*) ZPointsArray->At(okcounter);
 
    if(vi == NULL) continue;
    Double_t scos = R*TMath::ATan2((vi->Y() - y0)*TMath::Cos(Phi0) - (vi->X() - x0)*TMath::Sin(Phi0) , R + (vi->X() - x0) * TMath::Cos(Phi0) + (vi->Y()-y0) * TMath::Sin(Phi0));
 
    Bool_t fitdone = kFALSE;
    Double_t distfirst = pow(((vi->Z() - (outz.Y() + outz.X() * scos))/sigz), 2);
    if(distfirst < 10) {

      pMhit->SetXint(vi->X());
      pMhit->SetYint(vi->Y());
      pMhit->SetZint(vi->Z());
      fitdone = kTRUE;
    }
    // SECOND CHOICE
    vi = (TVector3*) ZPointsArray->At(okcounter+1); 
    if(vi == NULL) continue;
    scos = R*TMath::ATan2((vi->Y() - y0)*TMath::Cos(Phi0) - (vi->X() - x0)*TMath::Sin(Phi0) , R + (vi->X() - x0) * TMath::Cos(Phi0) + (vi->Y()-y0) * TMath::Sin(Phi0));
    Double_t distsecond = pow(((vi->Z() - (outz.Y() + outz.X() * scos))/sigz), 2);

    if(fitdone == kTRUE){
      if(distsecond < 10 && distsecond < distfirst) {
	pMhit->SetXint(vi->X());
	pMhit->SetYint(vi->Y());
	pMhit->SetZint(vi->Z());
      }
    }
    else {
      if (distsecond < 10) {
	pMhit->SetXint(vi->X());
	pMhit->SetYint(vi->Y());
	pMhit->SetZint(vi->Z());
	fitdone = kTRUE;
      }
      else {
	pMhit->SetXint(-999);
	pMhit->SetYint(-999);
	pMhit->SetZint(-999);
      }
    }

    if(rootoutput) {
      if(pMhit->GetZint()!= -999) {
	eventCanvas2->cd();
	TMarker *mrk3 = new TMarker(scos, pMhit->GetZint(), 6);
	mrk3->SetMarkerColor(2);    
	mrk3->Draw("SAME");
      }
    }
   
    okcounter = okcounter + 2;
  }
 
  return kTRUE;
  //   fiterrp = sqrt(fiterrp2);
  //   fiterrm = sqrt(fiterrm2);
 
} 
 
// -------- ZFinder2 --------------------------------------------
Bool_t CbmSttHelixTrackFitter::ZFinder2(CbmSttTrack* pTrack, Int_t pidHypo) {
  // CHANGED Scosl

  if(!pTrack) return 0;

  Int_t hitcounter = pTrack->GetNofHits();
 
  // cut on number of hits
  if(hitcounter > 30) {
    //    cout << "more than 30 hits" << endl;
    //  pTrack->GetParamLast()->SetX(-999);                
    //     pTrack->GetParamLast()->SetY(-999);
    //     //    Double_t newZ = -999.; // CHECK da cambiare
    //     //    pTrack->GetParamLast()->SetZ(newZ);                
    //     pTrack->GetParamLast()->SetTx(-999);
    //     //    Double_t newTheta = -999.; // CHECK da cambiare
    //     //    pTrack->GetParamLast()->SetTy(newTheta);                
    //     pTrack->GetParamLast()->SetQp(0.);
    return 0;
  }
  if(hitcounter < 5)  {
    //    cout << "less than 5 hits" << endl;
    //     pTrack->GetParamLast()->SetX(-999);                
    //     pTrack->GetParamLast()->SetY(-999);
    //     //    Double_t newZ = -999.; // CHECK da cambiare
    //     //    pTrack->GetParamLast()->SetZ(newZ);                
    //     pTrack->GetParamLast()->SetTx(-999);
    //     //    Double_t newTheta = -999.; // CHECK da cambiare
    //     //    pTrack->GetParamLast()->SetTy(newTheta);                
    //     pTrack->GetParamLast()->SetQp(0.);
    return 0;
  }
  
  ZPointsArray = new TObjArray(2*hitcounter);

  // SCOSL ======
  // phi0
  TVector3 *v0;
  Double_t Phi0;
  // ============

  // ZFIT =======
  if(hitcounter == 0) return kFALSE;
  
  Double_t Sxx, Sx, Sz, Sxz, S1z;
  Double_t Detz = 0.;
  Double_t fitm, fitp;
  Double_t sigz = 1.;  // CHECK
  
  Sx = 0.;
  Sz = 0.;
  Sxx = 0.;
  Sxz = 0.;
  S1z = 0.;
  // ============

  TVector3 *tofit, *tofit2;
 
  // centre of curvature
  Double_t x_0 = (pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * cos(pTrack->GetParamLast()->GetY());
  Double_t y_0 = (pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * sin(pTrack->GetParamLast()->GetY());
  // radius of curvature
  Double_t R  = pTrack->GetParamLast()->GetTx();
  Int_t wireOk = 0;
  for (Int_t i = 0; i < hitcounter; i++) {
   
    // get index of hit
    Int_t iHit = pTrack->GetHitIndex(i);

    // get hit
    CbmSttHit *pMhit = (CbmSttHit*) fHitArray->At(iHit);

    if(pMhit == NULL) continue;

    Int_t refindex = pMhit->GetRefIndex(); 
    // get point
    CbmSttPoint *iPoint = (CbmSttPoint*) fPointArray->At(refindex);
    TVector3 wiredirection(iPoint->GetXWireDirection(), iPoint->GetYWireDirection(), iPoint->GetZWireDirection());
    TVector3 wiredirection2;

    wiredirection2 = 75. * wiredirection;
    TVector3 cenposition(pMhit->GetX(), pMhit->GetY(), 0.);  // CHECK! z = 35!!


    TVector3 min, max;
    min = cenposition - wiredirection2;
    max = cenposition + wiredirection2;
    
    // first extremity
    Double_t x_1= min.X(); 
    Double_t y_1= min.Y(); 
    Double_t z_1= min.Z(); 
    
    // second extremity
    Double_t x_2= max.X();
    Double_t y_2= max.Y();
    Double_t z_2= max.Z();
    
    Double_t rcur = pMhit->GetIsochrone();

    if(wiredirection != TVector3(0.,0.,1.)) 
      {
	wireOk++;
	Double_t a = -999;
	Double_t b = -999;

        Double_t x1 = -9999.;
        Double_t y1 = -9999.;
        Double_t x2 = -9999.;
        Double_t y2 = -9999.;
      
	// intersection point between the reconstructed 
	// circumference and the line joining the centres
	// of the reconstructed circle and the i_th drift circle
	if(fabs(x_2-x_1)>0.0001) {
	  a =(y_2-y_1)/(x_2-x_1);
	  b =(y_1-a*x_1);
	  Double_t A = a*a+1;
	  Double_t B = x_0+a*y_0-a*b;
	  Double_t C = x_0*x_0+y_0*y_0+b*b-R*R-2*y_0*b;
	  if((B*B-A*C)>0) {
	    x1= (B+TMath::Sqrt(B*B-A*C))/A;
	    x2= (B-TMath::Sqrt(B*B-A*C))/A;
	    y1=a*x1+b;
	    y2=a*x2+b;
	  }
	}
	else if(fabs(y_2-y_1)>0.0001) {
	  Double_t A = 1;
	  Double_t B = y_0;
	  Double_t C = y_0*y_0 +(x_1-x_0)*(x_1-x_0) -R*R;

	  if((B*B-A*C)>0) {
	    y1= (B+TMath::Sqrt(B*B-A*C))/A;
	    y2= (B-TMath::Sqrt(B*B-A*C))/A;
	    x1=x2=x_1;
	  }
	  
	}
	
	//x1 and x2 are the 2 intersection points
	Double_t d1=TMath::Sqrt((x1-cenposition.X())*(x1-cenposition.X())+
				(y1-cenposition.Y())*(y1-cenposition.Y()));
	Double_t d2=TMath::Sqrt((x2-cenposition.X())*(x2-cenposition.X())+
				(y2-cenposition.Y())*(y2-cenposition.Y()));
	
	Double_t x_ = x1;
	Double_t y_ = y1;
	
	// the intersection point nearest to the drift circle's centre is taken
	if(d2<d1) {x_=x2;y_=y2;}    
	
	// now we need to find the actual centre of the drift circle,
	// by translating the drift circle until it becomes tangent
	// to the reconstructed circle. 
	// Two solutions are possible (left rigth abiguity), 
	// they are both kept, only the following zed fit will discard the wrong ones.
	// Using the parametric equation of the 3d-straigth line and taking the
	// x points just obtained, the zed coordinate of the skewed tube centre is calculated.
	
	//solving the equation to find out the centre of the tangent circle
	Double_t A = a*a+1;
	Double_t B = -(a*b-a*y_-x_);
	Double_t C = x_*x_+ y_*y_+b*b-2*b*y_-rcur*rcur;
	if((B*B-A*C)>0) {
	  x1= (B+TMath::Sqrt(B*B-A*C))/A;
	  x2= (B-TMath::Sqrt(B*B-A*C))/A;
	  y1=a*x1+b;
	  y2=a*x2+b;
	}
	
	d1=TMath::Sqrt((x1-cenposition.X())*(x1-cenposition.X())+(y1-cenposition.Y())*(y1-cenposition.Y()));
	d2=TMath::Sqrt((x2-cenposition.X())*(x2-cenposition.X())+(y2-cenposition.Y())*(y2-cenposition.Y()));
	
	Double_t xcen0=x1;
	Double_t xcen1=x2;
	Double_t ycen0=y1;
	Double_t ycen1=y2;
	
	if(d2<d1) { // z2 contains the points nearest (in x-y) to the initial centre of the skewed tube
	  xcen0=x2;
	  xcen1=x1;
	  ycen0=y2;
	  ycen1=y1;
	  
	}
	
	// zed association
	if(fabs(x_2-x_1)<0.001) return kFALSE;
	Double_t t_      =(xcen0-x_1)/(x_2-x_1); // x= a_x*t + x_1 [t=1 x=x_2]
	Double_t z_      =(z_2-z_1)*t_ +z_1;     // z= a_z*t + z_1 [t=1 z=z_2]

	Double_t t_bis   =(xcen1-x_1)/(x_2-x_1); // from x_'s (the 2 solutions of the 2nd order equation)
	Double_t z_bis   =(z_2-z_1)*t_bis +z_1;  // and the 2 parametric equations the z coord. are obtained 

	tofit = new TVector3(xcen0,ycen0,z_);
	ZPointsArray->Add(tofit);
	tofit2 = new TVector3(xcen1,ycen1,z_bis);
	ZPointsArray->Add(tofit2);
   
	// FIRST CHOICE
	//====================  
	// scosl
	Double_t x0;
	Double_t y0;
	if(wireOk == 1) {
	  if(tofit != NULL) {
	    // phi0
	    v0 = new TVector3(tofit->X(), tofit->Y(), tofit->Z());
	  }
	  else if(tofit2 != NULL) {
	    v0 = new TVector3(tofit2->X(), tofit2->Y(), tofit2->Z());
	  }
	   
	  Phi0 = TMath::ATan2((v0->Y() - y_0),(v0->X() - x_0));
	  
	  // CHECK
	  // we are using the first digi for the arc length calculation
	  // but this may be wrong
	  x0 = v0->X();
	  y0 = v0->Y();


	}
	
	TVector3 vi(tofit->X(), tofit->Y(), tofit->Z());
	//	Double_t scos = R*TMath::ATan2((vi.Y() - y0)*TMath::Cos(Phi0) - (vi.X() - x0)*TMath::Sin(Phi0) , R + (vi.X() - x0) * TMath::Cos(Phi0) + (vi.Y()-y0) * TMath::Sin(Phi0));
	Double_t scos = R*(TMath::ATan2(R*TMath::Sin(Phi0) - (vi.Y() - y0), R * TMath::Cos(Phi0) - (vi.X() - x0)) - Phi0);
	//====================  
	if(rootoutput) {
	  eventCanvas2->cd();
	  TMarker *mrk = new TMarker(scos, tofit->Z(), 6);
	  mrk->Draw("SAME");
	}
	//====================  
	// zfit
	//
	Sx = Sx + (scos/(sigz * sigz));
	Sz = Sz + (tofit->Z()/(sigz * sigz));
	Sxz = Sxz + ((scos * tofit->Z())/(sigz * sigz));
	Sxx = Sxx + ((scos * scos)/(sigz * sigz));
	S1z = S1z + 1/(sigz * sigz);
	//====================

	// SECOND CHOICE
	//====================  
	// scosl
	vi.SetXYZ(tofit2->X(), tofit2->Y(), tofit2->Z());
	//	scos = R*TMath::ATan2((vi.Y() - y0)*TMath::Cos(Phi0) - (vi.X() - x0)*TMath::Sin(Phi0) , R + (vi.X() - x0) * TMath::Cos(Phi0) + (vi.Y()-y0) * TMath::Sin(Phi0));	
	scos = R*(TMath::ATan2(R*TMath::Sin(Phi0) - (vi.Y() - y0), R * TMath::Cos(Phi0) - (vi.X() - x0)) - Phi0);

	//====================  
	
	//====================  
	// zfit
	//
	Sx = Sx + (scos/(sigz * sigz));
	Sz = Sz + (tofit2->Z()/(sigz * sigz));
	Sxz = Sxz + ((scos * tofit2->Z())/(sigz * sigz));
	Sxx = Sxx + ((scos * scos)/(sigz * sigz));
	S1z = S1z + 1/(sigz * sigz);
	//====================
	if(rootoutput) {     
	  eventCanvas2->cd();
	  TMarker *mrk2 = new TMarker(scos, tofit2->Z(), 6);
	  mrk2->Draw("SAME");
	}
      }
    
  }
  cout << "skewed: " << wireOk << endl;
  if(wireOk < 2) return kFALSE;
 
  Detz = S1z*Sxx - Sx*Sx;
  if(Detz == 0) {
    return kFALSE;
  }
 
  fitp = (1/Detz)*(Sxx*Sz - Sx*Sxz);
  fitm = (1/Detz)*(S1z*Sxz - Sx*Sz);
  //   fiterrp2 = (1/Detz)*Sxx;
  //   fiterrm2 = (1/Detz)*S1z;

  if(rootoutput) {  
    eventCanvas2->cd();
    TLine *line = new TLine(-20, -20*fitm + fitp, 20, (20*fitm + fitp));
    line->Draw("SAME");
  }
 
 
  //  Double_t chi2z;
  //  chi2z = 0.;
  //  for(Int_t i=0; i < hitcounter; i++) {
  //    TVector3 * vi= (TVector3 *) ZArray->At(i);
  //    chi2z = chi2z + pow(((vi->Z()-(fitp+fitm*scosl->At(i)))/sigz), 2);
  //       // RESIDUALS
  //       //    if(pow(((vi->Z()-(fitp+fitm*scosl[i]))/sigz), 2)>10) continue;
  //       //   bestz[ctbest]=vi->Z();
  //       //   bests[ctbest]=scosl[i];
  //       //   ctbest++;
  //     }
 
  // y = m*x + p
  TVector2 outz(fitm, fitp);
  Int_t counter = 0;

  if(fitm == 0. && fitp == 0.) return kFALSE;
 
  if( wireOk < 2) return kFALSE;

  wireOk = 0;
  Int_t okcounter = 0;
  for(Int_t i = 0; i < (2*hitcounter); i+=2) {
    // get index of hit
    //   cout << i << " counter: " << counter << endl;
    Int_t iHit = pTrack->GetHitIndex(counter);
    counter++;
   
    // get hit
    CbmSttHit *pMhit = (CbmSttHit*) fHitArray->At(iHit);
  
    Int_t refindex = pMhit->GetRefIndex(); 
    // get point
    CbmSttPoint *iPoint = (CbmSttPoint*) fPointArray->At(refindex);
    TVector3 wiredirection(iPoint->GetXWireDirection(), iPoint->GetYWireDirection(), iPoint->GetZWireDirection());
  
    if(wiredirection == TVector3(0.,0.,1.)) continue;
  
    wireOk++;
   
    sigz = 1.; // CHECK
   
    Double_t x0;
    Double_t y0;
    if(wireOk==1) {
      TVector3 *vi = (TVector3*) ZPointsArray->At(okcounter);
      // phi0
      v0->SetXYZ(vi->X(), vi->Y(), vi->Z());
      Phi0 = TMath::ATan2((v0->Y() - y_0),(v0->X() - x_0));
     
      // CHECK
      // we are using the first digi for the arc length calculation
      // but this may be wrong
      x0 = v0->X();
      y0 = v0->Y();
    
    }
   
    // FIRST CHOICE
    TVector3 *vi = (TVector3*) ZPointsArray->At(okcounter);
 

    if(vi == NULL) continue;
    Double_t scos = R*(TMath::ATan2(R*TMath::Sin(Phi0) - (vi->Y() - y0), R * TMath::Cos(Phi0) - (vi->X() - x0)) - Phi0);

    Bool_t fitdone = kFALSE;
    Double_t distfirst = pow(((vi->Z() - (outz.Y() + outz.X() * scos))/sigz), 2);
    if(distfirst < 10) {

      pMhit->SetXint(vi->X());
      pMhit->SetYint(vi->Y());
      pMhit->SetZint(vi->Z());
      fitdone = kTRUE;
    }
    // SECOND CHOICE
    vi = (TVector3*) ZPointsArray->At(okcounter+1); 
    if(vi == NULL) continue;
    scos = R*(TMath::ATan2(R*TMath::Sin(Phi0) - (vi->Y() - y0), R * TMath::Cos(Phi0) - (vi->X() - x0)) - Phi0);
     
    Double_t distsecond = pow(((vi->Z() - (outz.Y() + outz.X() * scos))/sigz), 2);
   
    if(fitdone == kTRUE){
      if(distsecond < 10 && distsecond < distfirst) {
	pMhit->SetXint(vi->X());
	pMhit->SetYint(vi->Y());
	pMhit->SetZint(vi->Z());
      }
    }
    else {
      if (distsecond < 10) {
	pMhit->SetXint(vi->X());
	pMhit->SetYint(vi->Y());
	pMhit->SetZint(vi->Z());
	fitdone = kTRUE;
      }
      else {
	pMhit->SetXint(-999);
	pMhit->SetYint(-999);
	pMhit->SetZint(-999);
      }
    }

  
    if(rootoutput) {
      if(pMhit->GetZint()!= -999) {
	eventCanvas2->cd();
	TMarker *mrk3 = new TMarker(scos, pMhit->GetZint(), 6);
	mrk3->SetMarkerColor(2);    
	mrk3->Draw("SAME");
      }
    }
 
    okcounter = okcounter + 2;
  }
 
 
  return kTRUE;
  //   fiterrp = sqrt(fiterrp2);
  //   fiterrm = sqrt(fiterrm2);
 
} 

// ----- Zfit  ----------------------------------------
Int_t CbmSttHelixTrackFitter::Zfit(CbmSttTrack* pTrack, Int_t pidHypo) {

  // fEventCounter++;

  if(!pTrack) return 0;
  
  //  TVector2 *position = NULL;
  Int_t hitcounter = pTrack->GetNofHits();
 
  // cut on number of hits
  //  if(hitcounter > 30) return 0;
  if(hitcounter < 5) return 0;
  
  // CHECK: ATTENTION
  // refit for error correction missing
  
  
  // SCOSL ======
  // phi0
  TVector3 * v0;
  Double_t Phi0;
  // ============


  Double_t Sxx, Sx, Sz, Sxz, S1z;
  Double_t Detz = 0.;
  Double_t fitm, fitp;
  Double_t sigz = 1.;  // CHECK
  
  Sx = 0.;
  Sz = 0.;
  Sxx = 0.;
  Sxz = 0.;
  S1z = 0.;
  
  // centre of curvature
  Double_t x_0 = (pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * cos(pTrack->GetParamLast()->GetY());
  Double_t y_0 = (pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * sin(pTrack->GetParamLast()->GetY());
  // radius of curvature
  Double_t R  = pTrack->GetParamLast()->GetTx();
  Int_t counter = 0;
  Int_t wireOk = 0;
  for (Int_t i = 0; i < hitcounter; i++) {
   
    // get index of hit
    Int_t iHit = pTrack->GetHitIndex(i);

    // get hit
    CbmSttHit *pMhit = (CbmSttHit*) fHitArray->At(iHit);
    if ( ! pMhit ) continue;

    if(pMhit->GetXint() == -999 || pMhit->GetYint() == -999 || pMhit->GetZint() == -999) continue; // CHECK

    Int_t refindex = pMhit->GetRefIndex(); 
    // get point
    CbmSttPoint *iPoint = (CbmSttPoint*) fPointArray->At(refindex);
    TVector3 wiredirection(iPoint->GetXWireDirection(), iPoint->GetYWireDirection(), iPoint->GetZWireDirection());
    
    if(wiredirection == TVector3(0.,0.,1.)) continue;
    counter++;
    // SCOSL
    Double_t x0;
    Double_t y0;
    wireOk++;
    if(wireOk == 1) {
      TVector3 *vi = new TVector3(pMhit->GetXint(), pMhit->GetYint(), pMhit->GetZint());
      
      // phi0
      v0 = new TVector3(vi->X(), vi->Y(), vi->Z());
      Phi0 = TMath::ATan2((v0->Y() - y_0),(v0->X() - x_0));
      
      // CHECK
      // we are using the first digi for the arc length calculation
      // but this may be wrong
      x0 = v0->X();
      y0 = v0->Y();
    }
    
    TVector3 *vi = new TVector3(pMhit->GetXint(), pMhit->GetYint(), pMhit->GetZint());
    Double_t scos = R*TMath::ATan2((pMhit->GetYint() - y0)*TMath::Cos(Phi0) - (pMhit->GetXint() - x0)*TMath::Sin(Phi0) , R + (pMhit->GetXint() - x0) * TMath::Cos(Phi0) + (pMhit->GetYint()-y0) * TMath::Sin(Phi0));
   
    Sx = Sx + (scos /(sigz * sigz));
    Sz = Sz + (vi->Z()/(sigz * sigz));
    Sxz = Sxz + ((scos *vi->Z())/(sigz * sigz));
    Sxx = Sxx + ((scos * scos)/(sigz * sigz));
    S1z = S1z + 1/(sigz * sigz);
  }

  if(counter == 0) return 0;
    
  Detz = S1z*Sxx - Sx*Sx;
  if(Detz == 0) {
    return 0;
  }

  fitp = (1/Detz)*(Sxx*Sz - Sx*Sxz);
  fitm = (1/Detz)*(S1z*Sxz - Sx*Sz);

  //    cout <<  "fit: " << fitp << " " << fitm << " " << Detz << endl;


  //   fiterrp2 = (1/Detz)*Sxx;
  //   fiterrm2 = (1/Detz)*S1z;
    
  // Double_t chi2z;
  //     chi2z = 0.;
  //     for(Int_t i=0; i < zcounter; i++) {
  //       TVector3 * vi= (TVector3 *) ZArray->At(i);
  //       chi2z = chi2z + pow(((vi->Z()-(fitp+fitm*scosl->At(i)))/sigz), 2);
  //       // RESIDUALS
  //       //    if(pow(((vi->Z()-(fitp+fitm*scosl[i]))/sigz), 2)>10) continue;
  //       //   bestz[ctbest]=vi->Z();
  //       //   bests[ctbest]=scosl[i];
  //       //   ctbest++;
  //     }
    
  // y = m*x + p
  TVector2 outz(fitm, fitp);
  
  pTrack->GetParamLast()->SetZ(fitp); // z (adesso fitp) CHECK
  // pTrack->GetParamLast()->SetTy((TMath::Pi()/2.) - TMath::ATan(fitm)); // theta
  pTrack->GetParamLast()->SetTy(fitm); // CHECK da cambiare con theta

  if(rootoutput) {
    eventCanvas2->cd();
    TLine *line2 = new TLine(-20, -20*fitm + fitp, 20, (20*fitm + fitp));
    line2->SetLineColor(3);
    line2->Draw("SAME");
  } 
  return 1;
  //   fiterrp = sqrt(fiterrp2);
  //   fiterrm = sqrt(fiterrm2);
}
// -------------------------------------------------------------------------------


// ----- Zfit2  ----------------------------------------
Int_t CbmSttHelixTrackFitter::Zfit2(CbmSttTrack* pTrack, Int_t pidHypo) {

  // fEventCounter++;
  if(!pTrack) return 0;
  
  //  TVector2 *position = NULL;
  Int_t hitcounter = pTrack->GetNofHits();
 
  // cut on number of hits
  //  if(hitcounter > 30) return 0;
  if(hitcounter < 5) return 0;
  
  // CHECK: ATTENTION
  // refit for error correction missing
  
  //  if(ZArray==NULL || (xy.X() == 0 && xy.Y() == 0 && xy.Z() == 0)) return TVector2(0.,0.);
  //  if(ZArray->GetEntries() <= 1) return TVector2(0.,0.);
  
  // SCOSL ======
  // phi0
  TVector3 * v0;
  Double_t Phi0;
  // ============


  Double_t Sxx, Sx, Sz, Sxz, S1z;
  Double_t Detz = 0.;
  Double_t fitm, fitp;
  Double_t sigz = 1.;  // CHECK
  
  Sx = 0.;
  Sz = 0.;
  Sxx = 0.;
  Sxz = 0.;
  S1z = 0.;
  
  // centre of curvature
  Double_t x_0 = (pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * cos(pTrack->GetParamLast()->GetY());
  Double_t y_0 = (pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * sin(pTrack->GetParamLast()->GetY());
  // radius of curvature
  Double_t R  = pTrack->GetParamLast()->GetTx();
  Int_t counter = 0;
  Int_t wireOk = 0;
  for (Int_t i = 0; i < hitcounter; i++) {
   
    // get index of hit
    Int_t iHit = pTrack->GetHitIndex(i);

    // get hit
    CbmSttHit *pMhit = (CbmSttHit*) fHitArray->At(iHit);
    if ( ! pMhit ) continue;

    if(pMhit->GetXint() == -999 || pMhit->GetYint() == -999 || pMhit->GetZint() == -999) continue; // CHECK

    Int_t refindex = pMhit->GetRefIndex(); 
    // get point
    CbmSttPoint *iPoint = (CbmSttPoint*) fPointArray->At(refindex);
    TVector3 wiredirection(iPoint->GetXWireDirection(), iPoint->GetYWireDirection(), iPoint->GetZWireDirection());
    
    if(wiredirection == TVector3(0.,0.,1.)) continue;
    counter++;
    // SCOSL
    Double_t x0;
    Double_t y0;
    wireOk++;
    if(wireOk == 1) {
      TVector3 *vi = new TVector3(pMhit->GetXint(), pMhit->GetYint(), pMhit->GetZint());
      
      // phi0
      v0 = new TVector3(vi->X(), vi->Y(), vi->Z());
      Phi0 = TMath::ATan2((v0->Y() - y_0),(v0->X() - x_0));
      
      // CHECK
      // we are using the first digi for the arc length calculation
      // but this may be wrong
      x0 = v0->X();
      y0 = v0->Y();
    }
    
    TVector3 *vi = new TVector3(pMhit->GetXint(), pMhit->GetYint(), pMhit->GetZint());
    //    Double_t scos = R*TMath::ATan2((pMhit->GetYint() - y0)*TMath::Cos(Phi0) - (pMhit->GetXint() - x0)*TMath::Sin(Phi0) , R + (pMhit->GetXint() - x0) * TMath::Cos(Phi0) + (pMhit->GetYint()-y0) * TMath::Sin(Phi0));
    Double_t scos = R*(TMath::ATan2(R*TMath::Sin(Phi0) - (pMhit->GetYint() - y0), R * TMath::Cos(Phi0) - (pMhit->GetXint() - x0)) - Phi0);

    Sx = Sx + (scos /(sigz * sigz));
    Sz = Sz + (vi->Z()/(sigz * sigz));
    Sxz = Sxz + ((scos *vi->Z())/(sigz * sigz));
    Sxx = Sxx + ((scos * scos)/(sigz * sigz));
    S1z = S1z + 1/(sigz * sigz);
  }

  if(counter == 0) return 0;
    
  Detz = S1z*Sxx - Sx*Sx;
  if(Detz == 0) {
    return 0;
  }

  fitp = (1/Detz)*(Sxx*Sz - Sx*Sxz);
  fitm = (1/Detz)*(S1z*Sxz - Sx*Sz);

  //    cout <<  "fit: " << fitp << " " << fitm << " " << Detz << endl;


  //   fiterrp2 = (1/Detz)*Sxx;
  //   fiterrm2 = (1/Detz)*S1z;
    
  // Double_t chi2z;
  //     chi2z = 0.;
  //     for(Int_t i=0; i < zcounter; i++) {
  //       TVector3 * vi= (TVector3 *) ZArray->At(i);
  //       chi2z = chi2z + pow(((vi->Z()-(fitp+fitm*scosl->At(i)))/sigz), 2);
  //       // RESIDUALS
  //       //    if(pow(((vi->Z()-(fitp+fitm*scosl[i]))/sigz), 2)>10) continue;
  //       //   bestz[ctbest]=vi->Z();
  //       //   bests[ctbest]=scosl[i];
  //       //   ctbest++;
  //     }
    
  // y = m*x + p
  TVector2 outz(fitm, fitp);
  
  pTrack->GetParamLast()->SetZ(fitp); // z (adesso fitp) CHECK
  // pTrack->GetParamLast()->SetTy((TMath::Pi()/2.) - TMath::ATan(fitm)); // theta
  pTrack->GetParamLast()->SetTy(fitm); // CHECK da cambiare con theta

  if(rootoutput) {
    eventCanvas2->cd();
    TLine *line2 = new TLine(-20, -20*fitm + fitp, 20, (20*fitm + fitp));
    line2->SetLineColor(3);
    line2->Draw("SAME");
  } 
  return 1;
  //   fiterrp = sqrt(fiterrp2);
  //   fiterrm = sqrt(fiterrm2);
}
// ----
 
// ----------------------------------------------------

void CbmSttHelixTrackFitter::Extrapolate(CbmSttTrack* track, Double_t r, FairTrackParam *param )
{
  cout << "-W- CbmSttMinuitTrackFitter::Extrapolate: Not yet implemented, sorry!"
       << endl;
}

// // -----   Private method AddHOT   -----------------------------------------
// CbmSttHOT* CbmSttHelixTrackFitter::AddHOT(Double_t x, Double_t y, Double_t z, Int_t hitindex, Int_t pointindex, Int_t trackindex)
// {
//   TClonesArray& 
//       clref = *fHotArray;

//   Int_t 
//       size = clref.GetEntriesFast();

//   //  cout << "filling HOT" << endl;


//   return new(clref[size]) CbmSttHOT(x, y, z, hitindex, pointindex, trackindex);

// }
// // -------------------------------------------------------------------------

void CbmSttHelixTrackFitter::ResetMArray() {
  for(Int_t i = 0; i < 100; i++) marray.AddAt(-999, i);
}


Int_t CbmSttHelixTrackFitter::MinuitFit(CbmSttTrack* pTrack, Int_t pidHypo)
{    
  cout << "MINUIT FIT " << pTrack->GetNofHits() << endl;
 
  fEventCounter++;
  Double_t hitcounter = pTrack->GetNofHits();
     
  Double_t rstart = pTrack->GetParamLast()->GetTx();
  Double_t xcstart = (pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * cos(pTrack->GetParamLast()->GetY());
  Double_t ycstart = (pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * sin(pTrack->GetParamLast()->GetY());
  
  
  TMinuit minimizer(3);
  
  cout << "*******MINUIT********" << endl;
  cout << "D   SEED: " << pTrack->GetParamLast()->GetX()  << endl;
  cout << "PHI SEED: " << pTrack->GetParamLast()->GetY()  << endl;
  cout << "R   SEED: " << pTrack->GetParamLast()->GetTx() << endl;
  cout << "********************" << endl;



  minimizer.SetFCN(fcnHelix);
  //  minimizer.SetErrorDef(1);  // ???
  
  minimizer.DefineParameter(0, "xc", xcstart, 0.1, -3000., 3000.); // ???
  minimizer.DefineParameter(1, "yc", ycstart, 0.1, -3000., 3000.); // ??? LIMITS ???
  minimizer.DefineParameter(2, "r", rstart, 0.1, 0., 3000.);   // ???
  cout << "xcstart: " << xcstart << " ycxtart: " << ycstart << " rstart: " << rstart << endl;
  minimizer.SetObjectFit(this);
 
  minimizer.SetPrintLevel(-1); 

  minimizer.SetMaxIterations(500);

  minimizer.Migrad();

  Double_t chisquare, resultsRadial[3], errorsRadial[3]; 

  minimizer.GetParameter(0, resultsRadial[0], errorsRadial[0]);
  minimizer.GetParameter(1, resultsRadial[1], errorsRadial[1]);
  minimizer.GetParameter(2, resultsRadial[2], errorsRadial[2]);
  
  //  minimizer.Eval(3, NULL, chisquare, resultsRadial, 0); // ???
  
  cout << "xc: " << resultsRadial[0] << endl; 
  cout << "yc: " << resultsRadial[1] << endl; 
  cout << "R:  " << resultsRadial[2] << endl;
  
  Double_t phi = TMath::ATan2(resultsRadial[1], resultsRadial[0]); // CHECK
  Double_t d;
  d = ((resultsRadial[0] + resultsRadial[1]) - resultsRadial[2] *(TMath::Cos(phi) + TMath::Sin(phi)))/(TMath::Cos(phi) + TMath::Sin(phi)); // CHECK
  
  pTrack->SetChi2Rad(chisquare);
  pTrack->SetNDF(fTrack->GetNofHits());
  pTrack->GetParamLast()->SetX(d);                
  pTrack->GetParamLast()->SetY(phi);
  //   pTrack->GetParamLast()->SetZ(z0);                
  pTrack->GetParamLast()->SetTx(resultsRadial[2]);
  //   pTrack->GetParamLast()->SetTy(alpha);                
  pTrack->GetParamLast()->SetQp(0.);
  
  if(rootoutput) {
    eventCanvas->cd();
    TArc *fitarc = new TArc(((pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * cos(pTrack->GetParamLast()->GetY())), ((pTrack->GetParamLast()->GetX() + pTrack->GetParamLast()->GetTx()) * sin(pTrack->GetParamLast()->GetY())), pTrack->GetParamLast()->GetTx());
    fitarc->SetLineColor(3);
    fitarc->Draw("SAME");
    eventCanvas->Update();
    eventCanvas->Modified();
  }

  return 1;
}

void fcnHelix(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag)
{

  const CbmSttHelixTrackFitter *mama = (CbmSttHelixTrackFitter *)gMinuit->GetObjectFit();

  CbmSttTrack *fTrack = mama->GetTrack(); 

  CbmSttHit *currenthit  = NULL;

  Double_t chisq = 0;
  Double_t delta = 0;
  Int_t hitcounter = fTrack->GetNofHits();

  Double_t xcur=0;
  Double_t ycur=0;

  for (Int_t i = 0; i < hitcounter; i++)
    { 

      // get index of hit
      Int_t iHit = fTrack->GetHitIndex(i);

      // get hit
      currenthit = mama->GetHitFromCollections(iHit);

      if(!currenthit) continue;

      if(currenthit->GetXint() == -999 || currenthit->GetYint() == -999) continue;
      TVector3 wiredirection = currenthit->GetWireDirection();

      if(wiredirection != TVector3(0.,0.,1.)) continue;

      xcur = currenthit->GetXint();
      ycur = currenthit->GetYint();
      // cout <<"MINUIT: " << currenthit->GetXint() << " " << currenthit->GetYint() << endl;
    

      delta =sqrt((xcur-par[0])*(xcur-par[0])+(ycur-par[1])*(ycur-par[1])) -par[2] ;
      if(currenthit->GetIsochrone() == 0) chisq += (delta * delta * 12.);
      else chisq += (delta*delta)/(currenthit->GetIsochrone() * currenthit->GetIsochrone() / 12.);
      
    }
 
  f = chisq;
}



CbmSttHit* CbmSttHelixTrackFitter::GetHitFromCollections(Int_t hitCounter) const
{
  CbmSttHit
    *retval = NULL;
 
  Int_t
    relativeCounter = hitCounter;

  for (Int_t collectionCounter = 0; collectionCounter < fHitCollectionList.GetEntries(); collectionCounter++)
    {
      Int_t
	size = ((TClonesArray *)fHitCollectionList.At(collectionCounter))->GetEntriesFast();

      if (relativeCounter < size)
	{
	  retval = (CbmSttHit*) ((TClonesArray *)fHitCollectionList.At(collectionCounter))->At(relativeCounter);
	  break;
	}
      else
	{
	  relativeCounter -= size;
	}
    }

  return retval;
}





ClassImp(CbmSttTrackFitter)
