/* 
 * File:   MpdMcDstGenerator.h
 * Author: Pavel Batyuk
 *
 * Created on November 13, 2019
 */

#ifndef MPDMCDSTGENERATOR_H
#define MPDMCDSTGENERATOR_H

#include <iostream>
#include <FairGenerator.h>
#include <FairPrimaryGenerator.h>
#include <FairMCEventHeader.h>
#include <FairRunSim.h>

#include <MpdMcDstReader.h>
#include <MpdMcEvent.h>
#include <MpdMcArrays.h>
#include <MpdMcParticle.h>
#include <MpdGenTrackTask.h>
#include <MpdGenTrack.h>

#include <TClonesArray.h>

using namespace std;
using namespace TMath;

class MpdMcDstGenerator : public FairGenerator {
public:

    MpdMcDstGenerator();
    MpdMcDstGenerator(TString fileName);
    ~MpdMcDstGenerator();

    Bool_t ReadEvent(FairPrimaryGenerator* primGen);

    void SkipEvents(Int_t ev) {
        fEventNumber = ev;
        cout << "NUMBER OF SKIPPED EVENTS = " << ev << endl;
    }

private:
    Long64_t fEventNumber; //!
    McDstReader* myReader;
    
    TClonesArray* fGenTracks;
    
    MpdMcDstGenerator(const MpdMcDstGenerator&);
    MpdMcDstGenerator& operator=(const MpdMcDstGenerator&);

    ClassDef(MpdMcDstGenerator, 1);
};
#endif