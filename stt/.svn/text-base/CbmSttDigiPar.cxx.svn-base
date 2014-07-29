//*-- AUTHOR Denis Bertini
//*-- modified: 19/06/2005 by Denis Bertini
//*-- created : 18/06/2005

/////////////////////////////////////////////////////////////
// CbmSttDigiPar
//
// Container class for Digitisation parameters
//
/////////////////////////////////////////////////////////////

using namespace std;
#include "CbmSttDigiPar.h"
#include "FairRuntimeDb.h"
#include "FairParIo.h"
#include "FairDetParIo.h"
#include "TClass.h"
#include <iostream> 
#include <iomanip>

ClassImp(CbmSttDigiPar)

CbmSttDigiPar::CbmSttDigiPar(const char* name,const char* title,
			     const char* context)
    : FairParSet(name,title,context)
{
  // constructor does nothing yet
  detName="Stt";
}

CbmSttDigiPar::~CbmSttDigiPar() 
{
    // destructor
}

Bool_t CbmSttDigiPar::init(FairParIo* inp)
{
    // intitializes the container from an input
    cout << "-I- CbmSttDigiPar::init " << endl;
    
    FairDetParIo* input=inp->getDetParIo("CbmSttParIo");
    cout << "-I- CbmSttDigiPar::init " << input << endl;

    if (input) return (input->init(this));
    return kFALSE;
}

Int_t CbmSttDigiPar::write(FairParIo* output)
{
    // writes the container to an output
    FairDetParIo* out=output->getDetParIo("CbmSttParIo");
    if (out) return out->write(this);
    return -1;
}

void CbmSttDigiPar::clear() 
{
    // clears the container
    status=kFALSE;
    resetInputVersions();
}

void CbmSttDigiPar::printParam() 
{
    // prints the calibration parameters
    // to be done
}

void CbmSttDigiPar::readline(const char *buf, Int_t *set, fstream *f) 
{
}

void CbmSttDigiPar::readline(const char *buf, Int_t *set) 
{
}

void CbmSttDigiPar::putAsciiHeader(TString& header) 
{
}

Bool_t CbmSttDigiPar::writeline(char *buf, Int_t mod, Int_t strip) 
{
    return kTRUE;
}
