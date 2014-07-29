#ifndef CBMSTTDIGIPAR_H
#define CBMSTTDIGIPAR_H

#include "TObject.h"
#include "FairParSet.h"
#include <fstream>


class CbmSttDigiPar : public FairParSet
{
 protected:
 public:
    CbmSttDigiPar(const char* name="SttDigiPar",
		  const char* title="Stt Digitisation Parameters",
		  const char* context="TestDefaultContext");
    ~CbmSttDigiPar();
    Int_t getSize() { return 0; }
    Bool_t init(FairParIo* input);
    Int_t write(FairParIo* output);
    void clear();
    void printParam();
    void readline(const char*, Int_t*);
    void readline(const char*,Int_t*,fstream *);
    void putAsciiHeader(TString&);
    Bool_t writeline(char*, Int_t, Int_t);
    ClassDef(CbmSttDigiPar,1) // Container for the Stt Digitisation parameters
};
	
#endif  /*!CBMSTTDIGIPAR_H*/







