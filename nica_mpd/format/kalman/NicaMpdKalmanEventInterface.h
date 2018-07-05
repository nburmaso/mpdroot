/*
 * NicaMpdKalmanEventInterface.h
 *
 *  Created on: 3 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef INTERFACES_MPDROOT_FORMAT_NICAMPDKALMANEVENTINTERFACE_H_
#define INTERFACES_MPDROOT_FORMAT_NICAMPDKALMANEVENTINTERFACE_H_

#include <TClonesArray.h>
#include "NicaEventInterface.h"
class NicaMpdKalmanEvent;
class NicaMpdDstKalmanEventInterface;
class NicaMpdKalmanEventInterface : public NicaEventInterface{
friend class NicaMpdKalmanEvent;
friend class NicaMpdDstKalmanEventInterface;
	TClonesArray *fTracks;
protected:
	virtual void FastCopy(NicaEventInterface *s);
	virtual void ConnectToTree();
	virtual void Register(Bool_t write);
public:
	NicaMpdKalmanEventInterface();
	NicaMpdKalmanEventInterface(const NicaMpdKalmanEventInterface &other);
	virtual void Compress(Int_t *map, Int_t map_size);
	virtual void Copy(NicaEventInterface *s);
	virtual void CopyAndCompress(NicaEventInterface *s, Int_t *map, Int_t map_size);
	virtual void FillTrackInterface(NicaTrackInterface *track, Int_t index) ;
	virtual Bool_t ExistInTree()const;
	virtual Int_t GetTotalTrackNo() const ;
	virtual NicaTrackInterface *GetTrackInterface()const;
	virtual TObject* GetRawTrackPointer(Int_t index)const;
	virtual TObject *GetRawEventPointer()const ;
	virtual ~NicaMpdKalmanEventInterface();
	ClassDef(NicaMpdKalmanEventInterface,1)
};

#endif /* INTERFACES_MPDROOT_FORMAT_NICAMPDKALMANEVENTINTERFACE_H_ */
