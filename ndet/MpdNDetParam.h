/** I am desperate for usable parameter handle class.
 ** So we will have it only for neutron detector. :(**/
#ifndef MPDNDETPARAM_H
#define MPDNDETPARAM_H

#include "TObjArray.h"
#include <list>
#include "TString.h"
#include "TObjString.h"
#include "TNamed.h"
#include "TMap.h"


class MpdNDetParam : public TNamed
{
public:
  /** This is ROOT requirement **/
  MpdNDetParam() {};
  /** Text file constructor **/
  MpdNDetParam(const char* name, const char* filename);	
  virtual ~MpdNDetParam();

  void DumpContainer() const;
  void DumpContainer(std::ostream& stream) const;

  /** Various getters **/
  /** key must be lower case. For example, if have in
   ** configuration file AaaA=90, then you should call 
   ** GetVariableStrict("aaaa").
   ** If variable not found, will generate Fatal **/
  TString GetString(const char* key) const;
  Double_t GetDouble(const char* key) const;
  Int_t GetInteger(const char* key) const;

  void AddVariable(const char* key, const char* value);
  Bool_t Differs(const MpdNDetParam* info) const;
private:
  /** A map containing all variables
   ** This variable should be saved in parameter file **/
  TMap fVariables;

  Int_t fSuccess;

  TString fFileName;
public:
  /** DO NOT USE THIS UNLESS YOU ARE COMPLETELY SURE.
   ** key must be lower case. For example, if have in
   ** configuration file AaaA=90, then you should call 
   ** GetVariableStrict("aaaa").
   ** If variable not found, will return -1111 **/
  Double_t GetDoubleSlack(const char* key) const;
  Int_t GetIntegerSlack(const char* key) const;
  TString GetStringSlack(const char* key) const;

  ClassDef(MpdNDetParam,1);
};

#endif
