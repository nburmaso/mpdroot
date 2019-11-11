/*
 * NicaGeneratorWriteFairMCFreez.h
 *
 *  Created on: 13 wrz 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */
#ifndef NICAGENERATORWRITEFAIRMCFREEZ_H_
#define NICAGENERATORWRITEFAIRMCFREEZ_H_
#include <TClonesArray.h>

#include "NicaGeneratorWriteFairMC.h"
class NicaGeneratorWriteFairMCFreez : public NicaGeneratorWriteFairMC{
protected:
	TClonesArray *fFreez;
public:
	NicaGeneratorWriteFairMCFreez();
	virtual InitStatus Init();
	virtual void ClearEvent();
	void AddParticle();
	virtual ~NicaGeneratorWriteFairMCFreez();
	ClassDef(NicaGeneratorWriteFairMCFreez,1)
};

#endif /* NICAGENERATORWRITEFAIRMCFREEZ_H_ */
