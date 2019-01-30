/*
 * NicaMpdTrackTpcPads.h
 *
 *  Created on: 2 lip 2018
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef MPDROOT_NICA_MPD_FORMAT_MPDEVENT_NICAMPDTRACKTPCPADS_H_
#define MPDROOT_NICA_MPD_FORMAT_MPDEVENT_NICAMPDTRACKTPCPADS_H_

#include "NicaMpdTrack.h"
#include "NicaTpcSectorGeo.h"

#define NICAMPDTRACKTPCPADS_CALCULATEMPDPAD_ON_DEMAND

class NicaMpdTrackTpcPads : public NicaMpdTrack{
	Float_t fPaths[53];
	Int_t fPadID[53];
	/**
	 * fPadsNo[0] - first pad, fPadsNo[1] - last pad
	 */
	Short_t fPadsNo[2];
	NicaTpcSectorGeo *fSec = NULL;
protected:
	/**
	 *
	 * @return true if pads were calculated
	 */
	Bool_t PadsCalculated()const;
public:
	NicaMpdTrackTpcPads();
	NicaMpdTrackTpcPads(const NicaMpdTrackTpcPads &other);
	NicaMpdTrackTpcPads &operator=(const NicaMpdTrackTpcPads &other);
	Int_t GetPadsNo()const{return fPadsNo[1]-fPadsNo[0];};
	Int_t GetFirstGoodPad()const{return fPadsNo[0];};
	Int_t GetFirstBadPad()const{return fPadsNo[1];};
	Int_t GetMaxPadsNo()const{return 53;};
	Float_t GetPathAt(Int_t lay)const{return fPaths[lay];};
	Float_t GetPhi(Int_t lay)const;
	Float_t GetR(Int_t lay)const;
	Float_t GetZ(Int_t lay)const;
	Int_t GetPadID(Int_t lay)const{return fPadID[lay];};
	/**
	 * calculate pads and pad id's this is usually used for pair-like cuts
	 * so we don't need to calculate them for each track in event (what is usually done when Update is called
	 * to build NicaEvent-based class from root file). We need them only for tracks that passed cuts and are
	 * taken into pair analysis. So this method should be called by NicaTwoTrack based class or analysis class.
	 * You can calculate those values by commenting #define NICAMPDTRACKTPCPADS_CALCULATEMPDPAD_ON_DEMAND.
	 * To avoid double counting fPadsNo for tracks without calculations is equal to -2
	 * @param shift - shift  by - vertex
	 */
	void CalculatePads(Bool_t shift=kFALSE);
	virtual void Update(MpdTrack* track);
	virtual void CopyData(NicaTrack *other);
	virtual ~NicaMpdTrackTpcPads();
	ClassDef(NicaMpdTrackTpcPads,1)
};

#endif /* MPDROOT_NICA_MPD_FORMAT_MPDEVENT_NICAMPDTRACKTPCPADS_H_ */
