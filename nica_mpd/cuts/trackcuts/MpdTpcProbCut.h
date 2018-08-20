/*
 * MpdTpcProbCut.h
 *
 *  Created on: 12 kwi 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef INTERFACES_MPDROOT_CUTS_TRACKCUTS_MPDTPCPROBCUT_H_
#define INTERFACES_MPDROOT_CUTS_TRACKCUTS_MPDTPCPROBCUT_H_

#include "NicaTrackCut.h"

class MpdTpcProbCut : public NicaTrackCut{
public:
	MpdTpcProbCut();
	virtual Bool_t Pass(NicaTrack *track);
	virtual Bool_t Init(Int_t task_id);
	static Int_t PidPion(){return 0;};
	static Int_t PidProton(){return 1;}
	static Int_t PidKaon(){return 2;};
	static Int_t PidElectron(){return 3;};
	virtual ~MpdTpcProbCut();
	ClassDef(MpdTpcProbCut,1)
};

#endif /* INTERFACES_MPDROOT_CUTS_TRACKCUTS_MPDTPCPROBCUT_H_ */
