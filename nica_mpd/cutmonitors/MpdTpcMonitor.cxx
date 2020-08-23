/*
 * MpdTpcMonitor.cxx
 *
 *  Created on: 20 sie 2020
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdTpcMonitor.h"
#include "NicaMpdTrack.h"
#include "NicaDataFormatManager.h"
#include "NicaComplexTrack.h"

MpdTpcMonitor::MpdTpcMonitor() :NicaPropertyMonitorXY("p [GeV/c]","dEdX [AU]",kTrackUpdate),fType(kReco){
    SetXaxis(100, 0, 4);
    SetYaxis(100,0,1E+5);
}

Bool_t MpdTpcMonitor::Init(Int_t task_id) {
    const NicaEvent *ev = NicaDataFormatManager::Instance()->GetFormat(task_id, kNonBuffered);
    fType = ev->GetFormatType();
    return NicaPropertyMonitorXY::Init(task_id);
}

void MpdTpcMonitor::Update(Bool_t passed, TObject *obj) {
    switch(fType){
    case kReco:{
        NicaMpdTrack *track = static_cast<NicaMpdTrack*>(obj);
        if(passed){
            fHistoPassed->Fill(track->GetMomentum()->P(),track->GetTpcTrack()->GetDeDx());
        }else{
            fHistoFailed->Fill(track->GetMomentum()->P(),track->GetTpcTrack()->GetDeDx());
        }
    }break;
    case kComplexReco:{
        NicaComplexTrack *complex_track = static_cast<NicaComplexTrack*>(obj);
        NicaMpdTrack *track = static_cast<NicaMpdTrack*>(complex_track->GetRealTrack());
        if(passed){
            fHistoPassed->Fill(track->GetMomentum()->P(),track->GetTpcTrack()->GetDeDx());
        }else{
            fHistoFailed->Fill(track->GetMomentum()->P(),track->GetTpcTrack()->GetDeDx());
        }
    }break;
    default:
        break;
    }
}

MpdTpcMonitor::MpdTpcMonitor(const MpdTpcMonitor &other) : NicaPropertyMonitorXY(other){
    fType  = other.fType;
}

MpdTpcMonitor::~MpdTpcMonitor() {
    // TODO Auto-generated destructor stub
}

