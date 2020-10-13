/*
 * MpdMiniDstQA.cxx
 *
 *  Created on: 11 paź 2020
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#include "MpdMiniDstQA.h"

MpdMiniDstBaseQAEvent::MpdMiniDstBaseQAEvent(TString name, Int_t dim1,
                                             Int_t dim2, Int_t dim3)
    : NicaQAPlot(name, dim1, dim2, dim3, ENicaCutUpdate::kEventUpdate) {}

MpdMiniDstBaseQAEvent::MpdMiniDstBaseQAEvent() {}

MpdMiniDstBaseQATrack::MpdMiniDstBaseQATrack(TString name, Int_t dim1,
                                             Int_t dim2, Int_t dim3)
    : NicaQAPlot(name, dim1, dim2, dim3, ENicaCutUpdate::kTrackUpdate),
      fRecoEvent(nullptr),
      fSimEvent(nullptr) {}

MpdMiniDstBaseQATrack::MpdMiniDstBaseQATrack()
    : fRecoEvent(nullptr), fSimEvent(nullptr) {}
