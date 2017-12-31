/*
 * NicaMpdDstKalmanInterface.h
 *
 *  Created on: 3 sie 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef INTERFACES_MPDROOT_FORMAT_KALMANDST_NICAMPDDSTKALMANEVENTINTERFACE_H_
#define INTERFACES_MPDROOT_FORMAT_KALMANDST_NICAMPDDSTKALMANEVENTINTERFACE_H_
#include "NicaMpdDstKalmanEventInterface.h"
#include "NicaMpdKalmanEventInterface.h"
#include "NicaMpdEventInterface.h"
class NicaMpdDstKalmanEventInterface : public NicaExpEventInterface{
	enum kTrackType{
		kAllTracks,
		kPrimaryTracks,
		kGlobalTracks
	};
	kTrackType fMode;
	NicaMpdEventInterface *fMpdInterface;
	NicaMpdKalmanEventInterface *fKalmanInterface;
	MpdEvent *fEvent;
	TClonesArray *fKalmanTracks;
protected:
	virtual void ConnectToTree();
	virtual void FastCopy(NicaEventInterface *s);
	void Register(Bool_t write);
public:
	NicaMpdDstKalmanEventInterface();
	void OnlyPrimary();
	void OnlyGlobal();
	virtual void Copy(NicaEventInterface *s);
	virtual void Compress(Int_t *map, Int_t map_size);
	virtual void CopyAndCompress(NicaEventInterface *s, Int_t *map, Int_t map_size);
	virtual void FillTrackInterface(NicaTrackInterface *track, Int_t index);
	virtual Bool_t ExistInTree()const;
	virtual Int_t GetTotalTrackNo() const;
	virtual TClonesArray *GetKalmans()const{return fKalmanTracks;};
	virtual TObject *GetRawEventPointer()const {return fEvent;};
	virtual NicaTrackInterface *GetTrackInterface()const;
	virtual TObject* GetRawTrackPointer(Int_t index)const;
	/** GETTERS AND SETTERS **/
	virtual void SetRunInfoId(Int_t i);
	virtual void SetMagneticField(TVector3 mag);
	virtual Int_t GetRunInfoId()const;;
	virtual Double_t GetPhi()const{return 0;};
	virtual Double_t GetPhiError()const{return 0;};
	virtual TVector3 GetMagneticField()const;
	virtual TLorentzVector GetVertexError()const;
	virtual TLorentzVector GetVertex()const;
	virtual ~NicaMpdDstKalmanEventInterface();
	ClassDef(NicaMpdDstKalmanEventInterface,1)
};

#endif /* INTERFACES_MPDROOT_FORMAT_KALMANDST_NICAMPDDSTKALMANEVENTINTERFACE_H_ */
