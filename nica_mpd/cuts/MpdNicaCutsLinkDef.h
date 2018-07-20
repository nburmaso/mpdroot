/*
 * MpdNicaCutsLinkDef.h
 *
 *  Created on: 10 kwi 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef INTERFACES_MPDROOT_CUTS_MPDNICACUTSLINKDEF_H_
#define INTERFACES_MPDROOT_CUTS_MPDNICACUTSLINKDEF_H_

#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

#pragma link C++ class MpdTpcProbCut+;
#pragma link C++ class MpdTrackChargeCut+;
#pragma link C++ class MpdTrackFirstPointCut+;
#pragma link C++ class MpdTcpCalibratedCut+;
#pragma link C++ class MpdTwoTrackSharedQualityCut+;
#pragma link C++ class MpdNSigmaCut+;
#pragma link C++ class MpdTwoTrackSharedPadsCut+;
#pragma link C++ class MpdPairPadsCuts+;
#pragma link C++ class MpdDeltaEtaDeltaPhiStarCut+;
#pragma link C++ class MpdPairKalmanPadsCuts+;
#endif

#endif /* INTERFACES_MPDROOT_CUTS_MPDNICACUTSLINKDEF_H_ */
