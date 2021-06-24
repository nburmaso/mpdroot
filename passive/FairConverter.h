/********************************************************************************
 *    Copyright (C) 2014 GSI Helmholtzzentrum fuer Schwerionenforschung GmbH    *
 *                                                                              *
 *              This software is distributed under the terms of the             * 
 *         GNU Lesser General Public Licence version 3 (LGPL) version 3,        *  
 *                  copied verbatim in the file "LICENSE"                       *
 ********************************************************************************/
#ifndef CONVERTER_H
#define CONVERTER_H

#include "FairModule.h" // for FairModule

#include "Rtypes.h" // for FairPipe::Class, ClassDef, etc
#include <string>   // for string

class FairConverter : public FairModule
{
 public:
  explicit FairConverter(const char* name, const char* Title = "PND Converter") {};
  FairConverter() = default;

  ~FairConverter() override = default;
  void ConstructGeometry() override;
  void ConstructASCIIGeometry() override;

  ClassDef(FairConverter, 1) // PNDCONVERTER
};

#endif //PIPE_H
