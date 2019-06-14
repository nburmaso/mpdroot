//
// The class describes collision parameters
//

// C++ headers
#include <iostream>

// ROOT headers
#include "TMath.h"

// McDst headers
#include "MpdMcRun.h"

#ifdef __ROOT__
ClassImp(MpdMcRun);
#endif

Double_t MpdMcRun::mProtMass = 0.938272029;
Double_t MpdMcRun::mNeutMass = 0.939565360;
Double_t MpdMcRun::mPionMass = 0.13957018;

//_________________
MpdMcRun::MpdMcRun() : TNamed("run", "Run Header" ),
		 fGenerator( "" ), fComment( "" ), fDecayer( "" ),
		 fAProj( 0 ), fZProj( 0 ), fPProj( 0 ),
		 fATarg( 0 ), fZTarg( 0 ), fPTarg( 0 ),
		 fBMin( 0 ), fBMax( 0 ), fBWeight( 0 ),
		 fPhiMin( 0 ), fPhiMax( 0 ), fXSection( 0 ), fNEvents( 0 ) {
  // Default constructor. Should not be used
  std::cout << "Warning: Default constructor of MpdMcRun should not be used!"
            << std::endl;
}

//_________________
MpdMcRun::MpdMcRun(const char* generator, const char* comment, const Int_t& aProj,
	     const Int_t& zProj, const Double_t& pProj, const Int_t& aTarg,
	     const Int_t& zTarg, const Double_t& pTarg, const Double_t& bMin,
	     const Double_t& bMax, const Int_t& bWeight, const Double_t& phiMin,
	     const Double_t& phiMax, const Double_t& sigma, const Int_t& nEvents)
  : TNamed("run", "Run Header") {
  // Standard constructor
  fGenerator = generator;
  fComment = comment;
  fAProj = ( ( TMath::Abs( aProj ) >= std::numeric_limits<short>::max() ) ?
	     std::numeric_limits<short>::max() : (Short_t)aProj ) ;
  fZProj = ( ( TMath::Abs( zProj ) >= std::numeric_limits<short>::max() ) ?
	     std::numeric_limits<short>::max() : (Short_t)zProj );
  fPProj = (Float_t)pProj;
  fATarg = ( ( TMath::Abs( aTarg ) >= std::numeric_limits<short>::max() ) ?
	     std::numeric_limits<short>::max() : (Short_t)aTarg );
  fZTarg = ( ( TMath::Abs( zTarg ) >= std::numeric_limits<short>::max() ) ?
	     std::numeric_limits<short>::max() : (Short_t)zTarg );
  fPTarg = (Float_t)pTarg;
  fBMin = (Float_t)bMin;
  fBMax = (Float_t)bMax;
  fBWeight = bWeight;
  fPhiMin = (Float_t)phiMin;
  fPhiMax = (Float_t)phiMax;
  fXSection = (Float_t)sigma;
  fNEvents   = nEvents;
}

//_________________
MpdMcRun::~MpdMcRun() { // Destructor
  /* empty */
}

//----------------
void MpdMcRun::print() const {
  // Print all data members to the standard output
  std::cout << "--------------------------------------------------" << std::endl
            << "-I-                 Run Header                 -I-" << std::endl
            << "Generator                     : " << fGenerator << std::endl
            << "Comment                       : " << fComment << std::endl
            << "Decayer                       : " << fDecayer << std::endl
            << "Projectile mass               : " << fAProj << std::endl
            << "Projectile charge             : " << fZProj << std::endl
            << "Projectile momentum (AGeV/c)  : " << fPProj << std::endl
            << "Target mass                   : " << fATarg << std::endl
            << "Target charge                 : " << fZTarg << std::endl
            << "Target momentum (AGeV/c)      : " << fPTarg << std::endl
            << "Minimal impact parameter (fm) : " << fBMin << std::endl
            << "Maximal impact parameter (fm) : " << fBMax << std::endl
            << "Impact parameter weighting    : " << fBWeight << std::endl
            << "Minimal azimuthal angle (rad) : " << fPhiMin << std::endl
            << "Maximal azimuthal angle (rad) : " << fPhiMax << std::endl
            << "Cross-section (mb)            : " << fXSection << std::endl
            << "Requested number of events    : " << fNEvents << std::endl
            << "--------------------------------------------------" << std::endl;
}

//_________________
void MpdMcRun::Print( Option_t* option __attribute__((unused)) ) const {
  // Print all data members to the standard output
  std::cout << "--------------------------------------------------" << std::endl
            << "-I-                 Run Header                 -I-" << std::endl
            << "Generator                     : " << fGenerator << std::endl
            << "Comment                       : " << fComment << std::endl
            << "Decayer                       : " << fDecayer << std::endl
            << "Projectile mass               : " << fAProj << std::endl
            << "Projectile charge             : " << fZProj << std::endl
            << "Projectile momentum (AGeV/c)  : " << fPProj << std::endl
            << "Target mass                   : " << fATarg << std::endl
            << "Target charge                 : " << fZTarg << std::endl
            << "Target momentum (AGeV/c)      : " << fPTarg << std::endl
            << "Minimal impact parameter (fm) : " << fBMin << std::endl
            << "Maximal impact parameter (fm) : " << fBMax << std::endl
            << "Impact parameter weighting    : " << fBWeight << std::endl
            << "Minimal azimuthal angle (rad) : " << fPhiMin << std::endl
            << "Maximal azimuthal angle (rad) : " << fPhiMax << std::endl
            << "Cross-section (mb)            : " << fXSection << std::endl
            << "Requested number of events    : " << fNEvents << std::endl
            << "--------------------------------------------------" << std::endl;
}

//_________________
Double_t MpdMcRun::projectileEnergy() const {
  // Get the projectile energy
  Double_t eProj = 0.;
  if ( fAProj > 0 ) {         // nucleus
    eProj = fZProj  * TMath::Sqrt( fPProj*fPProj + mProtMass*mProtMass ) +
      (fAProj - fZProj) * TMath::Sqrt( fPProj*fPProj + mNeutMass*mNeutMass );
  }
  else if ( fAProj ==  0 ) {  // photon
    eProj = fPProj;
  }
  else if ( fAProj == -1 ) { // pion
    eProj = TMath::Sqrt( fPProj*fPProj + mPionMass*mPionMass );
  }
  else {
    std::cout << "Warning:: MpdMcRun: Projectile mass " << fAProj
	      << " not valid! " << std::endl;
  }
  return eProj;
}

//_________________
Double_t MpdMcRun::targetEnergy() const {
  // Get the target energy
  Double_t eTarg = 0.;
  if ( fATarg > 0 ) {          // nucleus
    eTarg = fZTarg  * TMath::Sqrt( fPTarg*fPTarg + mProtMass*mProtMass ) +
      (fATarg - fZTarg) * TMath::Sqrt( fPTarg*fPTarg + mNeutMass*mNeutMass );
  }
  else if ( fAProj ==  0 ) {   // photon
    eTarg = fPTarg;
  }
  else if ( fAProj == -1 ) {   // pion
    eTarg = TMath::Sqrt( fPTarg*fPTarg + mPionMass*mPionMass );
  }
  else {
    std::cout << "Warning:: MpdMcRun: Target mass " << fATarg
	      << " not valid! " << std::endl;
  }
  return eTarg;
}

//_________________
Double_t MpdMcRun::nnSqrtS() const {
  // Get the cm energy
  Double_t eSum = ( TMath::Sqrt( fPTarg*fPTarg + mProtMass*mProtMass ) +
		                TMath::Sqrt( fPProj*fPProj + mNeutMass*mNeutMass ) );
  Double_t pSum = Double_t(fPProj + fPTarg);
  Double_t ecm = TMath::Sqrt( eSum*eSum - pSum*pSum );
  return ecm;
}

//_________________
Double_t MpdMcRun::sqrtS() const {
  // Get the cm energy
  Double_t eSum = projectileEnergy() + targetEnergy();
  Double_t pSum = Double_t(fAProj) * fPProj + Double_t(fATarg) * fPTarg;
  Double_t ecm = TMath::Sqrt( eSum*eSum - pSum*pSum );
  return ecm;
}

//_________________
Double_t MpdMcRun::betaCM() const {
  // Get cm velocity
  Double_t eSum = projectileEnergy() + targetEnergy();
  Double_t pSum = Double_t(fAProj) * fPProj + Double_t(fATarg) * fPTarg;
  return pSum / eSum;
}

//_________________
Double_t MpdMcRun::gammaCM() const {
  // Get cm gamma factor
  return 1. / TMath::Sqrt( 1. - betaCM()*betaCM() );
}
