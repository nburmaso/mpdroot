// -------------------------------------------------------------------------
// -----                      MpdDbGenerateClasses header file         -----
// -----                  Created 18/08/15  by K. Gertsenberger        -----
// -------------------------------------------------------------------------

/** MpdDbGenerateClasses.h
 *@author K.Gertsenberger <gertsen@jinr.ru>
 **
 ** Class for generating MPD DB classes
 **/

#ifndef MPDDBGENERATECLASSES_H
#define MPDDDGENERATECLASSES_H 1

#include "TString.h"

struct structColumnInfo
{
    TString strColumnName;
    // the same as strColumnName, except pointers: *strColumnName
    TString strColumnValue;
    TString strVariableType;
    // the same as strVariableType, except pointers: strVariableType without '*' at the end
    TString strVariableTypePointer;
    TString strStatementType;
    TString strPrintfType;
    TString strVariableName;
    TString strTempVariableName;
    TString strShortVariableName;
    bool isIdentity;
    bool isPrimary;
    bool isCompositeKey;
    bool isUnique;
    bool isNullable;
    bool isBinary;
    bool isDateTime;
};

class MpdDbGenerateClasses
{
 public:
    MpdDbGenerateClasses();             // Constructor
    virtual ~MpdDbGenerateClasses();    // Destructor

    // generate C++ classess - wrappers for DB tables
    int GenerateClasses(TString connection_string = "", TString class_prefix = "MpdDb", bool isOnlyUpdate = false);

  ClassDef(MpdDbGenerateClasses,1);
};

#endif
