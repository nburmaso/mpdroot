//
// Allows to read mcDst file or a list of mcDst files
//

// C++ headers
#include <string>
#include <sstream>
#include <iostream>
#include <fstream>
#include <assert.h>

// McDst headers
#include "MpdMcDst.h"
#include "MpdMcDstReader.h"
#include "MpdMcEvent.h"
#include "MpdMcParticle.h"
#include "MpdMcRun.h"
#include "MpdMcArrays.h"

// ROOT headers
#include "TRegexp.h"

ClassImp(MpdMcDstReader)

//_________________
MpdMcDstReader::MpdMcDstReader(const Char_t* inFileName) :
  mMcDst(new MpdMcDst()), mMcRun(nullptr), mChain(nullptr), mTree(nullptr),
  mEventCounter(0), mMcArrays{}, mStatusArrays{} {
  // Constructor
  streamerOff();
  createArrays();
  std::fill_n(mStatusArrays, sizeof(mStatusArrays) / sizeof(mStatusArrays[0]), 1);
  mInputFileName = inFileName;
}

//_________________
MpdMcDstReader::~MpdMcDstReader() {
  // Destructor
  if(mChain) {
    delete mChain;
  }
  if(mMcDst) {
    delete mMcDst;
  }
}

//_________________
void MpdMcDstReader::clearArrays() {
  // Clear arrays
  for(Int_t iArr=0; iArr<MpdMcArrays::NAllMpdMcArrays; iArr++) {
    mMcArrays[iArr]->Clear();
  }
}

//_________________
void MpdMcDstReader::setStatus(const Char_t *branchNameRegex, Int_t enable) {
  // Set branch status
  if(strncmp(branchNameRegex, "Mc", 2) == 0) {
    // Ignore first "Mc"
    branchNameRegex += 2;
  }

  TRegexp re(branchNameRegex, 1);
  for(Int_t iArr=0; iArr<MpdMcArrays::NAllMpdMcArrays; iArr++) {
    Ssiz_t len;
    if(re.Index(MpdMcArrays::mcArrayNames[iArr], &len) < 0) continue;
    std::cout << "[WARNING] MpdMcDstReader::SetStatus " << enable
	      << " to " << MpdMcArrays::mcArrayNames[iArr] << std::endl;
    mStatusArrays[iArr] = enable;
  }

  setBranchAddresses(mChain);
}

//_________________
void MpdMcDstReader::setBranchAddresses(TChain *chain) {
  // Set addresses of branches listed in mcArrays
  if (!chain) return;
  chain->SetBranchStatus("*", 0);
  TString ts;
  for (Int_t i = 0; i < MpdMcArrays::NAllMpdMcArrays; ++i) {
    if (mStatusArrays[i] == 0) continue;
    char const* bname = MpdMcArrays::mcArrayNames[i];
    TBranch* tb = chain->GetBranch(bname);
    if (!tb) {
      std::cout << "[WARNING] setBranchAddress: Branch name " << bname
                << " does not exist!" << std::endl;
      continue;
    }
    ts = bname;
    ts += "*";
    chain->SetBranchStatus(ts, 1);
    chain->SetBranchAddress(bname, mMcArrays + i);
    assert(tb->GetAddress() == (char*)(mMcArrays + i));
  }
  mTree = mChain->GetTree();
}

//_________________
void MpdMcDstReader::streamerOff() {
  MpdMcEvent::Class()->IgnoreTObjectStreamer();
  MpdMcParticle::Class()->IgnoreTObjectStreamer();
}

//_________________
void MpdMcDstReader::createArrays() {
  // Create arrays
  for(Int_t iArr=0; iArr<MpdMcArrays::NAllMpdMcArrays; iArr++) {
    mMcArrays[iArr] = new TClonesArray(MpdMcArrays::mcArrayTypes[iArr],
				       MpdMcArrays::mcArraySizes[iArr]);
  }
  mMcDst->set(mMcArrays);
}

//_________________
void MpdMcDstReader::Finish() {
  // Finish and clean everything
  if(mChain) {
    delete mChain;
  }
  mChain = nullptr;
}

//_________________
void MpdMcDstReader::Init() {
  // McDst initialization
  if(!mChain) {
    mChain = new TChain("McDst");
  }

  std::string const dirFile = mInputFileName.Data();

  if( dirFile.find(".list") != std::string::npos ||
      dirFile.find(".lis") != std::string::npos ) {

    std::ifstream inputStream( dirFile.c_str() );

    if(!inputStream) {
      std::cout << "[ERROR] Cannot open list file " << dirFile << std::endl;
    }

    Int_t nFile = 0;
    std::string file;
    while(getline(inputStream, file)) {
      if(file.find(".mcDst.root") != std::string::npos) {
        TFile* ftmp = TFile::Open(file.c_str());
        if(ftmp && !ftmp->IsZombie() && ftmp->GetNkeys()) {
          std::cout << " Read in mcDst file " << file << std::endl;
          mChain->Add(file.c_str());
          ++nFile;
        } //if(ftmp && !ftmp->IsZombie() && ftmp->GetNkeys())

        if (ftmp) {
	  ftmp->Close();
	} // if (ftmp)
      } //if(file.find(".mcDst.root") != std::string::npos)
    } //while (getline(inputStream, file))

    std::cout << " Total " << nFile << " files have been read in. " << std::endl;
  } //if(dirFile.find(".list") != std::string::npos || dirFile.find(".lis" != string::npos))
  else if(dirFile.find(".mcDst.root") != std::string::npos) {
    mChain->Add( dirFile.c_str() );
  }
  else {
    std::cout << "[WARNING] No good input file to read ... " << std::endl;
  }

  if(mChain) {
    setBranchAddresses(mChain);
    mChain->SetCacheSize(50e6);
    mChain->AddBranchToCache("*");
    mMcDst->set(mMcArrays);
    mMcRun = (MpdMcRun*)mChain->GetFile()->Get("run");
  }
}

//_________________
Bool_t MpdMcDstReader::loadEntry(Long64_t iEntry __attribute__((unused))) {
  // Read McDst entry
  Int_t mStatusRead = true; // true - okay, false - nothing to read

  if (!mChain) {
    std::cout << "[WARNING] No input files ... ! EXIT" << std::endl;
    mStatusRead = false;
    return mStatusRead;
  }

  Int_t bytes = mChain->GetEntry(mEventCounter++);
  Int_t nCycles = 0;
  while( bytes <= 0) {
    if( mEventCounter >= mChain->GetEntriesFast() ) {
    }

    std::cout << "[WARNING] Encountered invalid entry or I/O error while reading entry "
	      << mEventCounter << " from \"" << mChain->GetName() << "\" input tree\n";
    bytes = mChain->GetEntry(mEventCounter++);
    nCycles++;
    std::cout << "[WARNING] Not input has been found for: " << nCycles << " times" << std::endl;
    if(nCycles >= 10) {
      std::cout << "[ERROR] Terminating MpdMcDstReader::loadEntry(Long64_t) after "
		<< nCycles << " times!" << std::endl;
      mStatusRead = false;
      break;
    }
  }
  return mStatusRead;
}
