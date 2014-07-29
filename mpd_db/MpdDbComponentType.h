// -------------------------------------------------------------------------
// -----                      MpdDbComponentType header file           -----
// -----                  Created 12/02/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------

/** MpdDbComponentType.h
 *@author K.Gertsenberger <gertsen@jinr.ru>
 **
 ** Class for the component type in MPD_DB database
 **/

#ifndef MPDDBCOMPONENTTYPE_H
#define MPDDBCOMPONENTTYPE_H 1

#include <TSQLServer.h>
#include <TSQLResult.h>
#include <TSQLRow.h>
#include <TSQLStatement.h>

#include "TString.h"

#include "MpdDbConnection.h"

class MpdDbComponentType
{
 private:
    MpdDbConnection* connectionMpdDb;

    TString strTypeName, strTypeDescription;

    MpdDbComponentType(MpdDbConnection* connMpdDb, TString type_name, TString description);

 public:
    virtual ~MpdDbComponentType(); // Destructor

    // статические методы класса
    static MpdDbComponentType* GetComponentType(char* type_name);
    static MpdDbComponentType* CreateComponentType(char* type_name, char* description = 0x00);
    static int DeleteComponentType(char* type_name);

    TString GetTypeName(){return strTypeName;}
    TString GetDescription(){return strTypeDescription;}

    int Rename(char* type_name);
    int SetDescription(char* description);

  ClassDef(MpdDbComponentType,1);
};

#endif
