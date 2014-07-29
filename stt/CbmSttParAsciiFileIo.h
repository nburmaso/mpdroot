#ifndef CBMSTTPARASCIIFILEIO_H
#define CBMSTTPARASCIIFILEIO_H

using namespace std;
#include <fstream> 

#include "TObject.h"
#include "TArrayI.h"
#include "FairDetParAsciiFileIo.h"

class FairParSet;

class CbmSttParAsciiFileIo : public FairDetParAsciiFileIo {
public:
  CbmSttParAsciiFileIo(fstream*);
  ~CbmSttParAsciiFileIo() {}
  Bool_t init(FairParSet*);
  Int_t write(FairParSet*);
  template<class T> Bool_t read(T*, Int_t*, Bool_t needsClear=kFALSE);
  template<class T> Int_t writeFile2(T*);
  ClassDef(CbmSttParAsciiFileIo,0) // Class for STT parameter I/O from Ascii files
};

#endif  /* !CBMSTTPARASCIIFILEIO_H */







