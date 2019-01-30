/*
 * MpdPairCutModularTpc.h
 *
 *  Created on: 22 lis 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef MPDROOT_NICA_MPD_CUTS_PAIRCUTS_TPC_MODULAR_MPDMODULARTPCPAIRCUT_H_
#define MPDROOT_NICA_MPD_CUTS_PAIRCUTS_TPC_MODULAR_MPDMODULARTPCPAIRCUT_H_
#include "NicaTwoTrackCut.h"
class MpdModularTpcPairCut : public NicaTwoTrackCut{
public:
	MpdModularTpcPairCut(Int_t size);
	virtual Bool_t Init(Int_t task_id);
	virtual ~MpdModularTpcPairCut();
	ClassDef(MpdModularTpcPairCut,1)
};

#endif /* MPDROOT_NICA_MPD_CUTS_PAIRCUTS_TPC_MODULAR_MPDMODULARTPCPAIRCUT_H_ */
