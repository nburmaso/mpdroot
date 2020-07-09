// MpdRoot headers
#include "MpdEvent.h"
#include "MpdVertex.h"
#include "MpdTrack.h"
#include "MpdTpcKalmanTrack.h"
#include "MpdKalmanFilter.h"
#include "MpdKalmanGeoScheme.h"
#include "MpdTofHit.h"
#include "MpdEmcDigitKI.h"
#include "MpdEmcClusterKI.h"
#include "MpdTofMatchingData.h"
#include "MpdZdcDigi.h"
#include "MpdMCTrack.h"
#include "MpdGenTrack.h"

// MiniDst headers
#include "MpdMiniDstFillTask.h"
#include "MpdMiniDst.h"
#include "MpdMiniEvent.h"
#include "MpdMiniTrack.h"
#include "MpdMiniBTofHit.h"
#include "MpdMiniBTofPidTraits.h"
#include "MpdMiniBECalCluster.h"
#include "MpdMiniTrackCovMatrix.h"
#include "MpdMiniFHCalHit.h"
#include "MpdMiniMcEvent.h"
#include "MpdMiniMcTrack.h"
#include "MpdMiniMessMgr.h"
#include "MpdMiniArrays.cxx"

//_________________
MpdMiniDstFillTask::MpdMiniDstFillTask() {
  // Default constructor
  /* empty */
}

//_________________
MpdMiniDstFillTask::MpdMiniDstFillTask(TString name) :
fEvents(nullptr),
fMiniDst(new MpdMiniDst()),
fBField( 0.5 ), // mag. field in T (MUST be changed to the real magnetic field
fOutputFile(nullptr),
fTTree(nullptr),
fSplit(99),
fCompression(9),
fBufferSize(65536 * 4),
fMiniArrays(nullptr),
fIsUseCovMatrix(kTRUE),
fIsUseECal(kTRUE),
//fEmcDigits(nullptr),
fEmcClusters(nullptr) {

  // Standard constructor
  TString truncatedInFileName = name;
  Ssiz_t lastOccurence = name.Last('/');
  if ( lastOccurence != kNPOS ) {
    truncatedInFileName = name( (lastOccurence+1), name.Length() );
  }
  fOutputFileName = truncatedInFileName.ReplaceAll(".root", ".MiniDst.root");

  if ( !fMcTrk2MiniMcTrk.empty() ) fMcTrk2MiniMcTrk.clear();
  if ( !fMcTrk2EcalCluster.empty() ) fMcTrk2EcalCluster.clear();

  streamerOff();
  createArrays();
}

//_________________
MpdMiniDstFillTask::~MpdMiniDstFillTask() {
  // Destructor
  if ( !fMcTrk2MiniMcTrk.empty() ) fMcTrk2MiniMcTrk.clear();
  if ( !fMcTrk2EcalCluster.empty() ) fMcTrk2EcalCluster.clear();
  delete fMiniDst;
}

//_________________
InitStatus MpdMiniDstFillTask::Init() {
  // Output name must exist
  if ( fOutputFileName.IsNull() ) {
    return kERROR;
  }

  // Creating output tree with miniDST
  fOutputFile = new TFile(fOutputFileName.Data(), "RECREATE");
  // Inform about the creation
  LOG_INFO << " Output file: " << fOutputFileName.Data() << " created." << endm;
  // Set compression level
  fOutputFile->SetCompressionLevel(fCompression);
  int bufsize = fBufferSize;
  if (fSplit) {
    bufsize /= 4;
  }
  // Create TTree
  fTTree = new TTree("MiniDst", "MpdMiniDst", fSplit);
  fTTree->SetAutoSave(1000000);

  // Create arrays
  for (Int_t i = 0; i < MpdMiniArrays::NAllMiniArrays; ++i) {
    fTTree->Branch(MpdMiniArrays::miniArrayNames[i], &fMiniArrays[i], bufsize, fSplit);
  }

  // Initialize FairRoot manager
  FairRootManager* ioman = FairRootManager::Instance();
  if (!ioman) {
    std::cout << "[ERROR] MpdMiniDstFillTask::Init - Not FairRootManager instance has been found"
	      << std::endl;
    exit(0);
  }

  // Reading all possible input branches ...

  // Get MC event header
  fEventHeaders = (FairMCEventHeader*) ioman->GetObject("MCEventHeader.");
  if (!fEventHeaders) {
    std::cout << "[WARNING] MpdMiniDstFillTask::Init - No MCEventHeader has been found"
	      << std::endl;
  }

  // Get information on event
  fEvents = (MpdEvent*) ioman->GetObject("MPDEvent.");
  if (!fEvents) {
    std::cout << "[WARNING] MpdMiniDstFillTask::Init - No MpdEvent has been found"
	      << std::endl;
  }

  // Get information on reconstructed vertex
  fVertices = (TClonesArray*) ioman->GetObject("Vertex");
  if (!fVertices) {
    std::cout << "[WARNING] MpdMiniDstFillTask::Init - No Vertex has been found"
	      << std::endl;
  }

  // Retrieve information about tracks in TPC
  fTpcTracks = (TClonesArray*) ioman->GetObject("TpcKalmanTrack");
  if (!fTpcTracks) {
    std::cout << "[WARNING] MpdMiniDstFillTask::Init - No TpcKalmanTrack has been found"
	      << std::endl;
  }

  // Retrieve information about TOF hits
  fTofHits = (TClonesArray*) ioman->GetObject("TOFHit");
  if (!fTofHits) {
    std::cout << "[WARNING] MpdMiniDstFillTask::Init - No TOFHit has been found"
	      << std::endl;
  }

  // Get information about TPC tracks that match TOF
  fTofMatching = (TClonesArray*) ioman->GetObject("TOFMatching");
  if (!fTofMatching) {
    std::cout << "[WARNING] MpdMiniDstFillTask::Init - No TOFMatching has been found"
	      << std::endl;
  }

  // Retrieve MC Tracks
  fMCTracks = (TClonesArray*) ioman->GetObject("MCTrack");
  if (!fMCTracks) {
    std::cout << "[WARNING] MpdMiniDstFillTask::Init - No MCTracks have been found"
	      << std::endl;
  }

  // Retrieve generator (primary) tracks
  fGenTracks = (TClonesArray*) ioman->GetObject("GenTracks");
  if (!fGenTracks) {
    std::cout << "[WARNING] MpdMiniDstFillTask::Init - No GenTracks have been found"
	      << std::endl;
  }

  // Retrieve information about barrel ECal digits
  // fEmcDigits = (TClonesArray*) ioman->GetObject("EmcDigit");
  // if (!fEmcDigits) {
  //   std::cout << "[WARNING] MpdMiniDstFillTask::Init - No EmcDigits have been found"
  // 	      << std::endl;
  // }

  if ( fIsUseECal ) {
    // Retrieve information about barrel ECal clusters
    fEmcClusters = (TClonesArray*) ioman->GetObject("EmcCluster");
    if (!fEmcClusters) {
      std::cout << "[WARNING] MpdMiniDstFillTask::Init - No EmcClusters have been found"
		<< std::endl;
    }
  } // if ( fIsUseECal )

  // Retrieve information about FHCal
  fZdcDigits = (TClonesArray*) ioman->GetObject("ZdcDigi");
  if (!fZdcDigits) {
    std::cout << "[WARNING] MpdMiniDstFillTask::Init - No ZdcDigits have been found"
	      << std::endl;
  }

  return kSUCCESS;
}

//_________________
void MpdMiniDstFillTask::Exec(Option_t* option) {

  // Clear container at the beginning of each event
  if ( !fMcTrk2MiniMcTrk.empty() ) fMcTrk2MiniMcTrk.clear();
  if ( !fMcTrk2EcalCluster.empty() ) fMcTrk2EcalCluster.clear();
  
  Bool_t isGood = isGoodEvent();

  // Convert only events that are normally reconstructed
  if (isGood) {
    
    fillEvent();
    fillFHCalHits();
    fillBTofHits();
    fillMcTracks();
    if ( fIsUseECal ) {
      fillECalClusters();
    }
    fillTracks();
  
    if (fVerbose != 0) {
      fMiniDst->printTracks();
    }
  
    // Fill TTree
    fTTree->Fill();
  } // if (isGood)
  else {
    std::cout << "[WARNING] MpdMiniDstFillTask::Exec - Bad event reconstuction. Skipping event"
	      << std::endl;
  }
}

//_________________
Bool_t MpdMiniDstFillTask::isGoodEvent() {

  Bool_t isGoodAmount = kFALSE;
  Bool_t isGoodOrder = kTRUE;
  
  TClonesArray* glTracks = (TClonesArray*)fEvents->GetGlobalTracks();
  Int_t nGlobalTracks = glTracks->GetEntriesFast();
  Int_t nKalmanTracks = fTpcTracks->GetEntriesFast();

  if (nGlobalTracks == nKalmanTracks) {
    isGoodAmount = kTRUE;
  }
  
  // Loop over kalman (aka TPC) tracks
  for (Int_t iKTrack=0; iKTrack<nKalmanTracks; iKTrack++) {
    // Retrieve kalman track
    MpdTpcKalmanTrack* kTrack = (MpdTpcKalmanTrack*)fTpcTracks->UncheckedAt(iKTrack);
    if (!kTrack) continue;
    // Retrieve global track
    MpdTrack* gTrack = (MpdTrack*)glTracks->UncheckedAt(iKTrack);
    if ( kTrack->GetTrackID() != gTrack->GetID() || !gTrack ) {
      isGoodOrder = kFALSE;
    }
  } // for (Int_t iKTracks=0; iKTracks<nKalmanTracks; iKTracks++)

  return (isGoodAmount && isGoodOrder);
}

//_________________
void MpdMiniDstFillTask::Finish() {
  // Write data to the output file and close it
  if (fOutputFile) {
    fOutputFile->Write();
    fOutputFile->Close();
  }
}

//_________________
void MpdMiniDstFillTask::streamerOff() {
  MpdMiniEvent::Class()->IgnoreTObjectStreamer();
  MpdMiniTrack::Class()->IgnoreTObjectStreamer();
  MpdMiniBTofHit::Class()->IgnoreTObjectStreamer();
  MpdMiniBECalCluster::Class()->IgnoreTObjectStreamer();
  MpdMiniBTofPidTraits::Class()->IgnoreTObjectStreamer();
  MpdMiniTrackCovMatrix::Class()->IgnoreTObjectStreamer();
  MpdMiniFHCalHit::Class()->IgnoreTObjectStreamer();
  MpdMiniMcEvent::Class()->IgnoreTObjectStreamer();
  MpdMiniMcTrack::Class()->IgnoreTObjectStreamer();
}

//_________________
void MpdMiniDstFillTask::createArrays() {
  // Create MiniDst arrays
  fMiniArrays = new TClonesArray*[MpdMiniArrays::NAllMiniArrays];

  for (Int_t iArr = 0; iArr < MpdMiniArrays::NAllMiniArrays; iArr++) {
    fMiniArrays[iArr] = new TClonesArray( MpdMiniArrays::miniArrayTypes[iArr],
					  MpdMiniArrays::miniArraySizes[iArr] );
  }

  // Set pointers to the arrays
  fMiniDst->set(fMiniArrays);
}

//_________________
void MpdMiniDstFillTask::fillEvent() {

  // Fill event information
  TClonesArray* miniEventHeaders = fMiniArrays[MpdMiniArrays::Event];
  miniEventHeaders->Delete();

  // Create new event information
  MpdMiniEvent* miniEvent = new ((*miniEventHeaders)[miniEventHeaders->GetEntriesFast()]) MpdMiniEvent();
  if (!miniEvent) {
    std::cout << "[ERROR] MpdMiniDstFillTask::fillEvent - No MiniEvent has been created"
	      << std::endl;
    // In this case we need to quit (probably completely)
    return;
  }

  // Retrieve information about reconstructed primary vertex
  MpdVertex* vertex = (MpdVertex*) fVertices->UncheckedAt(0);

  // Primary vertex position
  if (vertex) {
    miniEvent->setPrimaryVertexPosition( vertex->GetX(),
					 vertex->GetY(),
					 vertex->GetZ() );
  } else {
    // In case that vertex has not been reconstructed
    miniEvent->setPrimaryVertexPosition( -999., -999., -999. );
  }

  // Number of global tracks in the current event
  if (!fEvents) {
    std::cout << "[ERROR] MpdMiniDstFillTask::fillEvent - No event information has been found"
	      << std::endl;
    return;
  }

  // Retrieve global tracks
  TClonesArray* glTracks = (TClonesArray*) fEvents->GetGlobalTracks();
  miniEvent->setNumberOfGlobalTracks( glTracks->GetEntriesFast() );

  // Fill McEvent info
  TClonesArray* miniMcEventHeaders = fMiniArrays[MpdMiniArrays::McEvent];
  miniMcEventHeaders->Delete();

  // Retrieve number of primary tracks
  Int_t nPrim = fEventHeaders->GetNPrim();
  Int_t nColl = -1;
  Double_t rp = -1;
  // Impact parameter
  Double_t b = fEventHeaders->GetB();
  // Run number
  UInt_t runId = fEventHeaders->GetRunID();
  // Event number
  UInt_t eventId = fEventHeaders->GetEventID();
  // Time
  Double_t time = fEventHeaders->GetT();
  // Fill MC vertex
  TVector3 vtx;
  fEventHeaders->GetVertex( vtx );

  // Create and fill MiniMcEvent
  new ((*miniMcEventHeaders)[miniMcEventHeaders->GetEntriesFast()])
    MpdMiniMcEvent( runId, eventId, rp, b, nPrim, nColl, vtx, time );
}

//_________________
void MpdMiniDstFillTask::fillFHCalHits() {

  TClonesArray* miniFHCal = fMiniArrays[MpdMiniArrays::FHCalHit];
  miniFHCal->Delete();
  
  // Retrieve number of hits (aka channels = detId(1,2)*modId(1-45)*ch(1-42) )
  Int_t nFHCalHits = fZdcDigits->GetEntries();

  // Total number of FHCal modules (aka towers)
  Int_t nFHCalModules = 45;
  // Energy deposition in the modules (towers)
  Float_t eLoss[90] = {};

  Int_t det = 0;
  Int_t mod = 0;

  // Loop over hits
  for (Int_t iHit=0; iHit<nFHCalHits; iHit++) {
    
    // Retrieve i-th tower information
    MpdZdcDigi *zdcHit = (MpdZdcDigi*)fZdcDigits->At(iHit);
    
    if (!zdcHit) continue;

    // FHCal miniHit
    MpdMiniFHCalHit* hit = new ((*miniFHCal)[miniFHCal->GetEntriesFast()]) MpdMiniFHCalHit();
    
    det = zdcHit->GetDetectorID();
    mod = zdcHit->GetModuleID();

    // Set hit information
    hit->setId( det, mod, zdcHit->GetChannelID() );
    hit->setEDep( zdcHit->GetELoss() );
    //hit->setAdc( zdcHit->ADC( zdcHit->GetELoss() ) );

    // Measure energy deposition for each module
    eLoss[ (det-1) * nFHCalModules + mod - 1 ] += zdcHit->GetELoss();
  } // for (Int_t iTower=0; iTower<nFHCalTower; iTower++)
  
  MpdMiniEvent* miniEvent = (MpdMiniEvent*)fMiniArrays[MpdMiniArrays::Event]->At(0);

  // Fill energy deposition in each tower
  for (Int_t iTow=0; iTow<(2 * nFHCalModules); iTow++) {
    miniEvent->setFHCalEnergyDepositInModule( iTow, eLoss[iTow] );
  }
}

//_________________
void MpdMiniDstFillTask::fillBTofHits() {
  
  // Instantiate MpdMiniBTofHit array
  TClonesArray* miniToF = fMiniArrays[MpdMiniArrays::BTofHit];
  miniToF->Delete();

  // Loop over TOF hits
  for (Int_t iHit = 0; iHit < fTofHits->GetEntriesFast(); iHit++) {

    // Retrieve TOF hit
    MpdTofHit* tofHit = (MpdTofHit*) fTofHits->UncheckedAt( iHit );

    if (!tofHit) continue;

    MpdMiniBTofHit* miniTofHit =
      new ((*miniToF)[miniToF->GetEntriesFast()]) MpdMiniBTofHit();
    
    miniTofHit->setId( tofHit->GetDetectorID() );
    miniTofHit->setHitPositionXYZ( tofHit->GetX(), tofHit->GetY(), tofHit->GetZ() );
    miniTofHit->setTime( tofHit->GetTime() );
  } // for (Int_t iHit = 0; iHit < fTofHits->GetEntriesFast(); iHit++)
}

//_________________
void MpdMiniDstFillTask::fillMcTracks() {

  //
  // Prepare mapping of MC tracks to reconstructed ones
  // and those used in barrel ECal clusters
  //

  TClonesArray* miniMcTracks = fMiniArrays[MpdMiniArrays::McTrack];
  miniMcTracks->Delete();

  // Fill McTracks if exist
  Bool_t isEmcTrack = kFALSE;
  Bool_t isGenLevelTrack = kFALSE;

  // Loop over MC tracks
  for (Int_t iMcTrk = 0; iMcTrk < fMCTracks->GetEntriesFast(); iMcTrk++) {

    // Retrieve MCTrack
    MpdMCTrack* mcTrack = (MpdMCTrack*)fMCTracks->UncheckedAt(iMcTrk);

    // MC track must exist
    if ( !mcTrack ) continue;

    // Clean variables
    isEmcTrack = kFALSE;
    isGenLevelTrack = kFALSE;

    // Check if MC track is a generator level track
    if ( mcTrack->GetMotherId() == -1 ) isGenLevelTrack = kTRUE;
    // {
    //   std::cout << "McTrk: " << iMcTrk << " pdgCode: " << mcTrack->GetPdgCode()
    // 		<< " px/py/pz: " << Form("%4.2f/%4.2f/%4.2f",
    // 					 mcTrack->GetPx(),
    // 					 mcTrack->GetPy(),
    // 					 mcTrack->GetPz())
    // 		<< " MothId: " << mcTrack->GetMotherId()
    // 		<< std::endl;
    // }

    if ( fIsUseECal ) {
      // Check if MC track that was used in ECal clusters
      for (Int_t iCluster = 0; iCluster < fEmcClusters->GetEntriesFast(); iCluster++) {
	// Retrieve barrel ECal cluster
	MpdEmcClusterKI* cluster =
	  (MpdEmcClusterKI*) fEmcClusters->UncheckedAt( iCluster );
      
	if (!cluster) continue;

	// Loop over tracks in the cluster
	for (Int_t iTrk=0; iTrk < cluster->GetNumberOfTracks(); iTrk++) {
	  Int_t id = -1;
	  Float_t eDep = -1.;
	  cluster->GetMCTrack( iTrk, id, eDep );
	  if ( id == iMcTrk ) {
	    isEmcTrack = kTRUE;
	    fMcTrk2EcalCluster[iMcTrk] = iCluster;
	    break;
	  }
	} // for (Int_t iTrk=0; iTrk < cluster->GetNumberOfTracks(); iTrk++)
      } // for (Int_t iCluster = 0; iCluster < fEmcClusters->GetEntriesFast(); iCluster++)
    } // if ( fIsUseECal )

    // Check if generator level or ECal track
    if ( isGenLevelTrack || isEmcTrack ) {
    
      // Create new MiniMcTrack
      MpdMiniMcTrack* miniMcTrack =
	new ((*miniMcTracks)[miniMcTracks->GetEntriesFast()]) MpdMiniMcTrack();
    
      if ( !miniMcTrack ) {
	std::cout << "[WARNING] MpdMiniDstFillTask::fillTracks - No miniMcTrack has been found"
		  << std::endl;
	continue;
      }

      // Set McTrack information
      miniMcTrack->setId( iMcTrk );
      miniMcTrack->setPdgId( mcTrack->GetPdgCode() );
      miniMcTrack->setPx( mcTrack->GetPx() );
      miniMcTrack->setPy( mcTrack->GetPy() );
      miniMcTrack->setPz( mcTrack->GetPz() );
      miniMcTrack->setEnergy( mcTrack->GetEnergy() );

      // Assume that there is no branch with GenTracks ...
      miniMcTrack->setX( -1. );
      miniMcTrack->setY( -1. );
      miniMcTrack->setZ( -1. );
      miniMcTrack->setT( -1. );

      if (fGenTracks && isGenLevelTrack) {
	for (Int_t iGenTrack = 0; iGenTrack < fGenTracks->GetEntriesFast(); iGenTrack++) {
	  MpdGenTrack* genTrack = (MpdGenTrack*) fGenTracks->UncheckedAt(iGenTrack);
	  if ( genTrack->GetIsUsed() ) continue;

	  Double_t absMomDiff = TMath::Abs(genTrack->GetMomentum().Mag() - mcTrack->GetP());
	  if (absMomDiff < DBL_EPSILON) {
	    genTrack->SetIsUsed( kTRUE );

	    TLorentzVector spaceTime = genTrack->GetCoordinates();
	    miniMcTrack->setX( spaceTime.X() );
	    miniMcTrack->setY( spaceTime.Y() );
	    miniMcTrack->setZ( spaceTime.Z() );
	    miniMcTrack->setT( spaceTime.T() );
	  } // if (absMomDiff < DBL_EPSILON)
	} // for (Int_t iGenTrack = 0; iGenTrack < fGenTracks->GetEntriesFast(); iGenTrack++)
      } // if (fGenTracks)

      // Store indices
      fMcTrk2MiniMcTrk.push_back( std::make_pair( iMcTrk,
						  (UShort_t)miniMcTracks->GetEntriesFast()-1) );
    } // if ( isGenLevelTrack || isEmcTrack )
  } // for (Int_t iMcTrk = 0; iMcTrk < fMCTracks->GetEntriesFast(); iMcTrk++)
}

//_________________
void MpdMiniDstFillTask::fillECalClusters() {

  //
  // Fill barrel ECal cluster information. Need to do it here
  // because of the mapping
  //
  
  // Instantiate MpdMiniBECalCluster array
  TClonesArray* miniEmcClusters = fMiniArrays[MpdMiniArrays::BECalCluster];
  miniEmcClusters->Delete();

  // Check if barrel ECal clusters exist
  if (fEmcClusters) {

    // Variables for smaller and larger dispertion axes
    Float_t lambda1 = 0;
    Float_t lambda2 = 0;

    // Loop over barrel ECal clusters
    for (Int_t iCluster = 0; iCluster < fEmcClusters->GetEntriesFast(); iCluster++) {
      
      // Retrieve barrel ECal cluster
      MpdEmcClusterKI* cluster =
	(MpdEmcClusterKI*) fEmcClusters->UncheckedAt( iCluster );

      // Cluster must exist
      if (!cluster) continue;

      // Clear
      lambda1 = 0;
      lambda2 = 0;

      // Create miniCluster
      MpdMiniBECalCluster* miniCluster =
	new ((*miniEmcClusters)[miniEmcClusters->GetEntriesFast()]) MpdMiniBECalCluster();

      // Fill miniCluster info
      miniCluster->setEnergy( cluster->GetE() );
      miniCluster->setECore( cluster->GetEcore() );
      miniCluster->setECore1p( cluster-> GetEcore_1p() );
      miniCluster->setECore2p( cluster-> GetEcore_2p() );
      miniCluster->setTime( cluster->GetTime() );
      miniCluster->setXYZ( cluster->GetX(), cluster->GetY(), cluster->GetZ() );
      miniCluster->setDPhi( cluster->GetDPhi() );
      miniCluster->setDz( cluster->GetDZ() );
      miniCluster->setTrackId( miniMcIdxFromMcIdx( cluster->GetTrackIndex() ) );
      
      cluster->GetLambdas(lambda1, lambda2);
      miniCluster->setLambdas( lambda1, lambda2 );
      miniCluster->setChi2( cluster->GetChi2() );
      miniCluster->setNLM( cluster->GetNLM() );
      
      // Fill digit info
      Int_t id = -1;
      Float_t eDep = -1.;
      for (Int_t iDigi = 0; iDigi < cluster->GetMultiplicity(); iDigi++) {
	id = -1;
	eDep = -1.;	
	cluster->GetDigitParams( iDigi, id, eDep );
	miniCluster->addDigit( id, eDep );
      } // for (Int_t iDigi = 0; iDigi < cluster->GetMultiplicity(); iDigi++)

      // Fill MC track info
      for (Int_t iTrk=0; iTrk < cluster->GetNumberOfTracks(); iTrk++) {
	id = -1;
	eDep = -1.;
	cluster->GetMCTrack( iTrk, id, eDep );
	miniCluster->addMcTrack( miniMcIdxFromMcIdx(id), eDep );
      } // for (Int_t iTrk=0; iTrk < cluster->GetNumberOfTracks(); iTrk++)
    } // for (Int_t iCluster = 0; iCluster < fEmcClusters->GetEntriesFast(); iCluster++)
  } // if (fEmcClusters)
}

//_________________
void MpdMiniDstFillTask::fillTracks() {

  //
  // Fill miniTrack, corresponding covariance matrix
  // and BTof matching information
  //

  // Reconstructed tracks
  TClonesArray* miniTracks = fMiniArrays[MpdMiniArrays::Track];
  miniTracks->Delete();
  // Convariance matrices of reconstructed tracks
  TClonesArray* miniTrackCovMatrices = fMiniArrays[MpdMiniArrays::TrackCovMatrix];
  miniTrackCovMatrices->Delete();
  // Create TOF-matching information
  TClonesArray* miniBTofTraits = fMiniArrays[MpdMiniArrays::BTofPidTraits];
  miniBTofTraits->Delete();

  // Retrieve global tracks from event
  TClonesArray* glTracks = (TClonesArray*) fEvents->GetGlobalTracks();

  // Reconstructed primary vertex in event
  MpdVertex* vtx = (MpdVertex*) fVertices->UncheckedAt(0);

  // Retrieve primary track indices in the fTpcTracks array that were used
  // for the primary vertex reconstruction
  TArrayI* ind = vtx->GetIndices();

  std::vector< Int_t > indices;
  for (Int_t iEle = 0; iEle < ind->GetSize(); iEle++) {
    indices.push_back( ind->At(iEle) );
  }

  // Reference multiplicities
  Int_t grefMult = 0;
  Int_t refMultPos = 0;
  Int_t refMultNeg = 0;
  Int_t refMultHalfPosEast = 0;
  Int_t refMultHalfNegEast = 0;
  Int_t refMultHalfPosWest = 0;
  Int_t refMultHalfNegWest = 0;
  Int_t refMult2PosEast = 0;
  Int_t refMult2NegEast = 0;
  Int_t refMult2PosWest = 0;
  Int_t refMult2NegWest = 0;
  Int_t nTofMatched = 0;
  Int_t nBECalMatched = 0;
  
  // Fill global and primary track information. All tracks originate
  // from kalman track, so loop over them and copy information
  for (Int_t iKalmanTrk = 0; iKalmanTrk < fTpcTracks->GetEntriesFast(); iKalmanTrk++) {
    
    // Retrieve i-th TpcKalmanTrack
    MpdTpcKalmanTrack* kalTrack = (MpdTpcKalmanTrack*) fTpcTracks->UncheckedAt(iKalmanTrk);
    
    // Skip non-existing tracks
    if ( !kalTrack ) continue;

    // Create miniTrack
    MpdMiniTrack* miniTrack =
      new ((*miniTracks)[miniTracks->GetEntriesFast()]) MpdMiniTrack();

    // Set track parameters
    miniTrack->setId( kalTrack->GetTrackID() );
    miniTrack->setChi2( kalTrack->GetChi2() );
    miniTrack->setNHits( kalTrack->GetNofHits() * kalTrack->Charge() );

    // Fill track covariance matrix if needed
    if (fIsUseCovMatrix) {
      // Create miniTrack covariance matrix
      MpdMiniTrackCovMatrix* miniTrackCovMatrix =
	new ((*miniTrackCovMatrices)[miniTrackCovMatrices->GetEntriesFast()]) MpdMiniTrackCovMatrix();
      fillCovMatrix( kalTrack, miniTrackCovMatrix );
    }

    // Let's find primary tracks from the whole set of reconstructed
    // tracks from the current event
    Bool_t isPrimary = kFALSE;

    for (auto it : indices) {
      if (TMath::Abs(it - iKalmanTrk) == 0) {
	isPrimary = kTRUE;
	break;
      }
    }

    // If track is primary then fill primary momentum
    if ( isPrimary ) {

      // Refit track to primary vertex
      refit2Vp( miniTrack, iKalmanTrk, vtx );

      //
      // TODO: Here we should check TOF-matching of the primary track
      //
  
      // Estimate reference multiplicities
      if ( miniTrack->nHits() > 15 ) {

	TVector3 pMom = miniTrack->pMom();
	Int_t charge = ( miniTrack->charge() > 0 ) ? 1 : -1;
	
	// Central region
	if ( TMath::Abs( pMom.Eta() ) < 0.5 ) {
	  ( charge > 0 ) ? refMultPos++ : refMultNeg++;
	}

	// Halfs of the TPC
	if ( pMom.Eta() > 0 ) {
	  ( charge > 0 ) ? refMultHalfPosWest++ : refMultHalfNegWest++;
	}
	if ( pMom.Eta() < 0 ) {
	  ( charge > 0 ) ? refMultHalfPosEast++ : refMultHalfNegEast++;
	}
	
	if ( 0.5 < pMom.Eta() && pMom.Eta() < 1. ) {
	  ( charge > 0 ) ? refMult2PosWest++ : refMult2NegWest++;
	}
	if ( -1. < pMom.Eta() && pMom.Eta() < -0.5 ) {
	  ( charge > 0 ) ? refMult2PosEast++ : refMult2NegEast++;
	}
	
      } // if ( miniTrack->nHits() > 15 )
    } // if (isPrimary)
    else {
      miniTrack->setPrimaryMomentum( TVector3(0., 0., 0.) );
    }

    // Retrieve global track to get more parameters that are not available
    // directly from kalman track
    MpdTrack* glTrack = (MpdTrack*) glTracks->UncheckedAt(iKalmanTrk);
    
    // Track must exist
    if ( !glTrack || ( kalTrack->GetTrackID() != glTrack->GetID() ) ) {
      std::cout << "[WARNING] MpdMiniDstFillTask::fillTracks -"
		<< " No gtrk that corresponds to ktrk"
		<< std::endl;
      continue;
    }

    // Set nSigma and dE/dx info
    miniTrack->setNSigmaElectron( glTrack->GetNSigmaElectron() );
    miniTrack->setNSigmaPion( glTrack->GetNSigmaPion() );
    miniTrack->setNSigmaKaon( glTrack->GetNSigmaKaon() );
    miniTrack->setNSigmaProton( glTrack->GetNSigmaProton() );
    miniTrack->setDedx( glTrack->GetdEdXTPC() );
    miniTrack->setHitMap( glTrack->GetLayerHitMap() );

    // Setting global track momentum at DCA to primary vertex
    TVector3 globMom( glTrack->GetPx(), glTrack->GetPy(), glTrack->GetPz() );
    miniTrack->setGlobalMomentum( globMom.X(), globMom.Y(), globMom.Z() );

    // Getting primary vertex the first primary vertex
    MpdVertex* vertex = (MpdVertex*) fVertices->UncheckedAt(0);

    // Get primary vertex position
    TVector3 primVertex( vertex->GetX(), vertex->GetY(), vertex->GetZ() );
    TVector3 firstPoint( glTrack->GetFirstPointX(),
			 glTrack->GetFirstPointY(),
			 glTrack->GetFirstPointZ() );

    // Physical helix instantiation
    MpdMiniPhysicalHelix helix( globMom, firstPoint,
				fBField * kilogauss,
				glTrack->GetCharge() );
    double pathLength = helix.pathLength( primVertex );
    TVector3 dcaPosition = helix.at( pathLength );
    miniTrack->setOrigin( dcaPosition.X(),
			  dcaPosition.Y(),
			  dcaPosition.Z() );

    // Estimate refMult of global tracks
    if ( TMath::Abs( miniTrack->gMom().Eta() ) < 0.5 &&
	 miniTrack->charge() != 0 && miniTrack->nHits() > 15 ) {
      grefMult++;
    }

    // Store index to miniMcTrack
    miniTrack->setMcTrackIndex( miniMcIdxFromMcIdx( kalTrack->GetTrackID() ) );

    // Update miniMcTrack global track ID information
    // in case of match
    if ( miniTrack->mcTrackIndex() >= 0 ) {
      
      MpdMiniMcTrack* miniMcTrack =
	(MpdMiniMcTrack*)fMiniArrays[MpdMiniArrays::McTrack]->At( miniMcIdxFromMcIdx( kalTrack->GetTrackID() ) );
      miniMcTrack->addGlobalTrackId( (UShort_t)miniTracks->GetEntriesFast()-1 );
    }

    //
    //
    // ECal-matching information
    //
    //

    auto itr2 = fMcTrk2EcalCluster.find( kalTrack->GetTrackID() );
    if ( itr2 != fMcTrk2EcalCluster.end() ) {
      miniTrack->setBECalClusterIndex( itr2->second );
    }
    else {
      miniTrack->setBECalClusterIndex( -1 );
    }

    // Check number of BECal-matched tracks
    if ( miniTrack->isBECalTrack() ) nBECalMatched++;


    //
    //
    // TOF-matching information
    //
    // TODO: must recalculate the information for primary tracks
    //       because it does not make any sense for kalman or global ones
    //
    //

    // Loop over TOF-matching information
    for (Int_t iMatch = 0; iMatch < fTofMatching->GetEntriesFast(); iMatch++) {

      // Retrieve TOF-matching information
      MpdTofMatchingData* dataMatch = (MpdTofMatchingData*) fTofMatching->UncheckedAt(iMatch);

      // TOF matching information should exist
      if ( !dataMatch || (dataMatch->GetKFTrackIndex() != iKalmanTrk) ) continue;

      // Create barrel TOF traits
      MpdMiniBTofPidTraits* bTofPidTraits =
	new ((*miniBTofTraits)[miniBTofTraits->GetEntriesFast()]) MpdMiniBTofPidTraits();
    
      miniTrack->setBTofPidTraitsIndex( miniBTofTraits->GetEntriesFast() - 1 ); 

      bTofPidTraits->setTrackIndex( miniTracks->GetEntriesFast() - 1 );
      bTofPidTraits->setHitIndex( dataMatch->GetTofHitIndex() );
      bTofPidTraits->setBeta( dataMatch->GetBeta() );
      bTofPidTraits->setMomentum( dataMatch->GetMomentum() );

      // Increment number of TOF-matched tracks
      nTofMatched++;
      
      break;
    } // for (Int_t iMatch = 0; iMatch < fTofMatching->GetEntriesFast(); iMatch++)
    
  } // for (Int_t iKalmanTrk = 0; iKalmanTrk < fTpcTracks->GetEntriesFast(); iKalmanTrk++)

  
  //
  // Fill MpdMiniEvent with reference multiplicity values
  //
  MpdMiniEvent* miniEvent = (MpdMiniEvent*)fMiniArrays[MpdMiniArrays::Event]->At(0);
  miniEvent->setRefMultNeg( refMultNeg );
  miniEvent->setRefMultPos( refMultPos );
  miniEvent->setRefMultHalfNegEast( refMultHalfNegEast );
  miniEvent->setRefMultHalfPosEast( refMultHalfPosEast );
  miniEvent->setRefMultHalfNegWest( refMultHalfNegWest );
  miniEvent->setRefMultHalfPosWest( refMultHalfPosWest );
  miniEvent->setRefMult2NegEast( refMult2NegEast );
  miniEvent->setRefMult2PosEast( refMult2PosEast );
  miniEvent->setRefMult2NegWest( refMult2NegWest );
  miniEvent->setRefMult2PosWest( refMult2PosWest );
  miniEvent->setGRefMult( grefMult );
  miniEvent->setNumberOfBTOFMatch( nTofMatched );
  miniEvent->setNumberOfBECalMatch( nBECalMatched );
}

//_________________
void MpdMiniDstFillTask::refit2Vp(MpdMiniTrack* miniTrack, Int_t iTpcKalmanTrack,
				  MpdVertex* vtx) {
  
  // Get primary tracks from event
  // Done by smooth tracks from primary vertex (update momentum and track length -
  // covariance matrix is not updated !!!)
  // Got from MpdKfPrimaryVertexFinder::Smooth()
  MpdKalmanHit hit;
  TMatrixD c(3, 3), xk(3, 1), ck0(5, 1);
  TMatrixD a(5, 3), b(5, 3);
  TVector3 vert;
  vtx->Position(vert);
  xk(0, 0) = vert.X();
  xk(1, 0) = vert.Y();
  xk(2, 0) = vert.Z();
  Double_t rad = vert.Pt();

  MpdKalmanTrack* track =
    (MpdKalmanTrack*) fTpcTracks->UncheckedAt(iTpcKalmanTrack);
  MpdKalmanTrack* trVert = new MpdKalmanTrack(*track);

  MpdKalmanTrack track1 = *track;
  track1.SetParamNew(*track1.GetParam());
  track1.SetPos(track1.GetPosNew());
  track1.ReSetWeight();
  track1.SetLength(0.);
  TMatrixD g = *track1.GetWeight(); // track weight matrix

  if (track->GetNode() == "") {
    hit.SetType(MpdKalmanHit::kFixedR);
    hit.SetPos(track->GetPos());
  } else {
    hit.SetType(MpdKalmanHit::kFixedP);
    TString detName = track->GetNode();
    if (track->GetUniqueID()) {
      // ITS
      detName = detName(16, detName.Length());
      detName += "#0";
    }
    MpdKalmanGeoScheme *geo = MpdKalmanFilter::Instance()->GetGeo();
    hit.SetDetectorID(geo->DetId(detName));

    // Find distance from the current track position to the last point (plane) -
    // to define direction (mainly for ITS)
    TVector3 pos = geo->GlobalPos(&hit);
    TVector3 norm = geo->Normal(&hit);
    Double_t v7[7] = {0.0};
    track1.SetNode("");
    MpdKalmanFilter::Instance()->SetGeantParamB(&track1, v7, 1);
    Double_t d = -(pos * norm); // Ax+By+Cz+D=0, A=nx, B=ny, C=nz
    TVector3 v3(v7[0], v7[1], v7[2]);
    d += v3 * norm;
    if (d < 0) {
      track1.SetDirection(MpdKalmanTrack::kOutward);
    }
  } // else

  MpdKalmanFilter::Instance()->PropagateToHit(&track1, &hit, kTRUE, kTRUE);

  //TMatrixD* par2 = track->GetParamNew();
  //cout << par2 << endl;
  computeAandB(xk, track, track1, a, b, ck0); // compute matrices of derivatives

  // W = (Bt*G*B)'
  TMatrixD tmp(g, TMatrixD::kMult, b);
  TMatrixD w(b, TMatrixD::kTransposeMult, tmp);
  w.Invert();

  TMatrixD m = *track1.GetParamNew();
  m -= ck0; // m-ck0

  // qk = W*Bt*G*(m-ck0-A*xk)
  TMatrixD tmp21(a, TMatrixD::kMult, xk);
  tmp21 *= -1;
  tmp21 += m; // m-ck0-A*xk
  TMatrixD tmp22(g, TMatrixD::kMult, tmp21);
  TMatrixD tmp23(b, TMatrixD::kTransposeMult, tmp22);
  TMatrixD qk(w, TMatrixD::kMult, tmp23);

  // Update momentum and last coordinate
  TMatrixD* parPointer = nullptr;

  if (track->GetParamNew())
    parPointer = track->GetParamNew();
  else
    parPointer = &m;

  TMatrixD par = *parPointer;
  for (Int_t i = 0; i < 3; ++i) par(i + 2, 0) = qk(i, 0);
  par(0, 0) = rad * vert.Phi();
  par(1, 0) = vert.Z();
  trVert->SetParam(par);
  trVert->SetPosNew(rad);

  Double_t pT = 1. / trVert->GetParam(4); // Signed pT
  Double_t phi = trVert->GetParam(2);
  Double_t theta = TMath::PiOver2() - trVert->GetParam(3);

  Double_t Px = TMath::Abs(pT) * TMath::Cos(phi);
  Double_t Py = TMath::Abs(pT) * TMath::Sin(phi);

  Double_t Pz = -1.;
  if (TMath::Sin(theta) != 0.) {
    Pz = TMath::Abs(pT) / TMath::Tan(theta);
  }

  miniTrack->setPrimaryMomentum(TVector3(Px, Py, Pz));
}

//_________________
void MpdMiniDstFillTask::computeAandB(TMatrixD &xk0, const MpdKalmanTrack *track,
				      const MpdKalmanTrack &trackM,
				      TMatrixD &a, TMatrixD &b, TMatrixD &ck0) {

  // Compute matrices of derivatives w.r.t. vertex coordinates and track momentum
  Double_t vert0[3], zero[3] = {0}, *vert = xk0.GetMatrixArray();
  for (Int_t i = 0; i < 3; ++i) vert0[i] = vert[i];

  MpdKalmanTrack trackk = *track;
  trackk.SetPos( trackk.GetPosNew() );
  //trackk.GetParam()->Print();
  // Propagate track to PCA w.r.t. point xk0
  MpdKalmanFilter::Instance()->FindPca( &trackk, vert0 );
  //MpdKalmanFilter::Instance()->FindPca(&trackk,zero); // just for test
  //std::cout << trackk.GetPosNew() << std::endl;
  trackk.SetParam( *trackk.GetParamNew() );
  //trackk.GetParam()->Print();

  // Put track at xk0
  Double_t r = TMath::Sqrt(vert0[0] * vert0[0] + vert0[1] * vert0[1]);
  Double_t phi = trackk.GetParamNew(2); // track Phi
  if (r > 1.e-7) phi = TMath::ATan2(vert0[1], vert0[0]);
  trackk.SetPos( r );
  trackk.SetParam( 0, r * phi );
  trackk.SetParam( 1, vert0[2] );
  trackk.SetNode("");
  MpdKalmanTrack track0 = trackk;

  // Propagate track to chosen radius
  MpdKalmanHit hit;
  //hit.SetR(35.);
  //hit = *(MpdKalmanHitR*)track->GetTrHits()->Last();
  if (track->GetNode() == "") {
    hit.SetType( MpdKalmanHit::kFixedR );
    //hit.SetR(35.);
    //hit = *(MpdKalmanHitR*)track->GetTrHits()->Last();
    hit.SetPos( track->GetPos() );
    MpdKalmanFilter::Instance()->PropagateParamR( &trackk, &hit, kFALSE );
    //trackk.GetParamNew()->Print();
    Proxim( trackM, trackk );
  } else {
    hit.SetType( MpdKalmanHit::kFixedP );
    TString detName = track->GetNode();
    if (track->GetUniqueID()) {
      // ITS
      detName = detName( 16, detName.Length() );
      detName += "#0";
    }
    MpdKalmanGeoScheme *geo = MpdKalmanFilter::Instance()->GetGeo();
    hit.SetDetectorID( geo->DetId(detName) );
    // Find distance from the current track position to the last point (plane) -
    // to define direction (mainly for ITS)
    TVector3 pos = geo->GlobalPos( &hit );
    TVector3 norm = geo->Normal( &hit );
    Double_t v7[7] = {0.0};
    MpdKalmanFilter::Instance()->SetGeantParamB( &trackk, v7, 1 );
    Double_t d = -(pos * norm); // Ax+By+Cz+D=0, A=nx, B=ny, C=nz
    TVector3 v3(v7[0], v7[1], v7[2]);
    d += v3 * norm;
    if (d < 0) trackk.SetDirection(MpdKalmanTrack::kOutward);
    MpdKalmanFilter::Instance()->PropagateParamP(&trackk, &hit, kFALSE, kTRUE);
    track0.SetDirection( trackk.GetDirection() );
  }

  //Double_t shift = 0.01; // 100 um coordinate shift
  Double_t shift = 0.1; // 1 mm coordinate shift
  for (Int_t i = 0; i < 3; ++i) {
    MpdKalmanTrack track1 = track0;
    vert0[i] += shift;
    if (i > 0) vert0[i - 1] -= shift;
    r = TMath::Sqrt(vert0[0] * vert0[0] + vert0[1] * vert0[1]);
    if (r > 1.e-7) phi = TMath::ATan2(vert0[1], vert0[0]);
    else phi = track0.GetParamNew(2); // track Phi
    track1.SetPos( r );
    track1.SetParam( 0, r * phi );
    track1.SetParam( 1, vert0[2] );
    if (track->GetNode() == "") {
      MpdKalmanFilter::Instance()->PropagateParamR( &track1, &hit, kFALSE );
      Proxim( trackk, track1 );
      //proxim(track1,trackk);
    } else MpdKalmanFilter::Instance()->PropagateParamP( &track1, &hit, kFALSE, kTRUE );
    // Derivatives
    for (Int_t j = 0; j < 5; ++j) {
      a(j, i) = (track1.GetParamNew(j) - trackk.GetParamNew(j)) / shift;
    }
  } // for (Int_t i = 0; i < 3; ++i)

  for (Int_t i = 0; i < 3; ++i) {
    MpdKalmanTrack track1 = track0;
    Int_t j = i + 2;
    shift = (*track->GetCovariance())(j, j);
    shift = TMath::Sqrt( shift );
    if (j == 4) shift *= TMath::Sign(1., -track0.GetParamNew(j)); // 1/p
    track1.SetParam( j, track0.GetParamNew(j) + shift );
    //if (j == 2 && track1.GetParamNew(j)*TMath::Sign(1.,track1.GetParamNew(j)) > TMath::Pi())
    //track1.SetParam(j,track0.GetParamNew(j)-shift);
    if (track->GetNode() == "") {
      MpdKalmanFilter::Instance()->PropagateParamR( &track1, &hit, kFALSE );
      Proxim( trackk, track1 );
      //proxim(track1,trackk);
    } else MpdKalmanFilter::Instance()->PropagateParamP( &track1, &hit, kFALSE, kTRUE );
    // Derivatives
    for (Int_t k = 0; k < 5; ++k) {
      b(k, i) = (track1.GetParamNew(k) - trackk.GetParamNew(k)) / shift;
    }
  } // for (Int_t i = 0; i < 3; ++i)

  TMatrixD qk0(3, 1);
  for (Int_t i = 0; i < 3; ++i) qk0(i, 0) = track0.GetParamNew(i + 2);
  //qk0.Print();
  ck0 = *trackk.GetParamNew();
  ck0 -= TMatrixD(a, TMatrixD::kMult, xk0);
  ck0 -= TMatrixD(b, TMatrixD::kMult, qk0);
}

//________________
void MpdMiniDstFillTask::Proxim(const MpdKalmanTrack &track0, MpdKalmanTrack &track) {

  if (track0.GetType() != MpdKalmanTrack::kBarrel) {
    std::cout << " !!! Implemented only for kBarrel tracks !!!" << std::endl;
    exit(0);
  }

  Double_t tmp = track.GetParamNew(0);
  Double_t phi0 = track0.GetParamNew(0) / track0.GetPosNew();
  Double_t phi = track.GetParamNew(0) / track.GetPosNew();
  phi = MpdKalmanFilter::Instance()->Proxim(phi0, phi);
  TMatrixD *par = track.GetParamNew();
  (*par)(0, 0) = phi * track.GetPosNew();
  phi0 = track0.GetParamNew(2);
  phi = track.GetParamNew(2);
  phi = MpdKalmanFilter::Instance()->Proxim(phi0, phi);
  (*par)(2, 0) = phi;
  track.SetParamNew(*par);
}

//_________________
void MpdMiniDstFillTask::fillCovMatrix(MpdTpcKalmanTrack* tpcTrack, MpdMiniTrackCovMatrix* miniTrackCovMatrix) {
  
  miniTrackCovMatrix->setPti(1. / tpcTrack->GetParam(4));

  const Double_t* matrixElements = tpcTrack->GetCovariance()->GetMatrixArray();
  const Int_t nElements = tpcTrack->GetCovariance()->GetNoElements();

  std::vector <Float_t> sigmas;
  std::vector <Float_t> correlations;

  Int_t shift = Int_t(TMath::Sqrt(nElements));

  for (Int_t iEle = 0; iEle < nElements; iEle++) {
    if (iEle % Int_t(TMath::Sqrt(nElements) + 1) == 0) {
      sigmas.push_back(matrixElements[iEle]);

      for (Int_t jEle = 1; jEle < shift; jEle++) {
	correlations.push_back(matrixElements[iEle + jEle]);
      }

      shift--;
    } // if (iEle % Int_t(TMath::Sqrt(nElements) + 1) == 0)
  } // for (Int_t iEle = 0; iEle < nElements; iEle++)

  miniTrackCovMatrix->setSigmas(sigmas);
  miniTrackCovMatrix->setCorrelations(correlations);
}

//_________________
Int_t MpdMiniDstFillTask::miniMcIdxFromMcIdx(Int_t mcIdx) {
  Int_t ret = -1; // Not found
  for (UInt_t i=0; i<fMcTrk2MiniMcTrk.size(); i++) {
    if ( fMcTrk2MiniMcTrk.at( i ).first == mcIdx ) {
      ret = fMcTrk2MiniMcTrk.at( i ).second;
      break;
    }
  }
  return ret;
}
