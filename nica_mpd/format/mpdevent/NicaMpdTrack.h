/*
 * NicaMpdTrack.h
 *
 *  Created on: 28 mar 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef NICAMPDTRACK_H_
#define NICAMPDTRACK_H_

#include "NicaTrack.h"
#include "MpdTrack.h"
#include "NicaMpdTrack.h"
#include "NicaExpTrack.h"


class NicaMpdTrack : public NicaExpTrack{
	TVector3 *fFirstPoint, *fLastPoint;
	NicaTpcTrack *fTpcTrack;
	NicaToFTrack *fToFTrack;
	ULong64_t fHitsMap;
protected:
	inline NicaTpcTrack *GetTpcTrack()const{return fTpcTrack;};
	inline NicaToFTrack *GetToFTrack()const{return fToFTrack;};
public:
	NicaMpdTrack();
	NicaMpdTrack(const NicaMpdTrack &other);
	NicaMpdTrack &operator=(const NicaMpdTrack &other);
	inline TVector3 *GetLastPoint()const{return fLastPoint;};
	inline TVector3 *GetFirstPoint()const{return fFirstPoint;};
	TObject *GetDetTrack(const UInt_t detID)const;
	inline Bool_t HaveHit(Int_t layer)const{return TESTBIT(fHitsMap,layer);};
	virtual void Update(MpdTrack* track);
	void PrintHitMaps()const;
	virtual void CopyData(NicaTrack *other);
	virtual ~NicaMpdTrack();
	ClassDef(NicaMpdTrack,1)
};

#endif /* NICAMPDTRACK_H_ */
