// -------------------------------------------------------------------------
// -----                      MpdDbGeoParameter header file            -----
// -----                  Created 13/02/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------

/** MpdDbGeoParameter.h
 *@author K.Gertsenberger <gertsen@jinr.ru>
 **
 ** Class for the table Geo_parameter in MPD_DB database
 **/

#ifndef MPDDBGEOPARAMETER_H
#define MPDDBGEOPARAMETER_H 1

#include <TSQLServer.h>
#include <TSQLResult.h>
#include <TSQLRow.h>
#include <TSQLStatement.h>

#include "TString.h"
#include "TTimeStamp.h"
#include "TDatime.h"

#include "MpdDbConnection.h"

class MpdDbGeoParameter
{
 private:
    MpdDbConnection* connectionMpdDb;

    int iComponentId, iParameterType;
    float fParameterValue;

    MpdDbGeoParameter(MpdDbConnection* connMpdDb, int component_id, int parameter_type, float parameter_value);

 public:
    virtual ~MpdDbGeoParameter(); // Destructor

    // статические методы класса
    static MpdDbGeoParameter* GetGeoParameter(char* component_name, int parameter_type);
    static MpdDbGeoParameter* CreateGeoParameter(char* component_name, int parameter_type, float parameter_value);
    static int DeleteGeoParameter(char* component_name, int parameter_type);

    TString GetComponentName();
    int GetType(){return iParameterType;}
    float GetValue(){return fParameterValue;}

    void UpdateFromDB();

    int SetComponentName(char* component_name);
    int SetType(int parameter_type);
    int SetValue(float parameter_value);

  ClassDef(MpdDbGeoParameter,1);
};

#endif
