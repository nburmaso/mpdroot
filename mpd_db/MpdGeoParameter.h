// -------------------------------------------------------------------------
// -----                      MpdGeoParameter header file              -----
// -----                  Created 21/08/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------

/** MpdGeoParameter.h
 *@author K.Gertsenberger <gertsen@jinr.ru>
 **
 ** Class for the table geo_parameters in MPD_GEO database
 **/

#ifndef MPDGEOPARAMETER_H
#define MPDGEOPARAMETER_H 1

#include <TSQLServer.h>
#include <TSQLResult.h>
#include <TSQLRow.h>
#include <TSQLStatement.h>

#include "TString.h"
#include "TTimeStamp.h"
#include "TDatime.h"

#include "MpdDbConnection.h"

class MpdGeoParameter
{
 private:
    MpdDbConnection* connectionMpdGeo;

    enum enumParameterType{DoubleValue = 0, IntValue = 1};
    int iParameterId;
    enumParameterType eParameterType;
    double fParameterValue;
    TString strDetectorName, strParameterName, strDescription;

    MpdGeoParameter(MpdDbConnection* connMpdGeo, int parameter_id, TString detector_name, TString parameter_name, enumParameterType parameter_type, double parameter_value, TString description);

 public:
    virtual ~MpdGeoParameter(); // Destructor

    // статические методы класса
    static MpdGeoParameter* GetGeoParameter(char* detector_name, char* parameter_name);
    static MpdGeoParameter* CreateGeoParameter(char* detector_name, char* parameter_name, int parameter_type, float parameter_value, char* description = NULL);
    static int DeleteGeoParameter(char* detector_name, char* parameter_name);

    static double GetValue(char* detector_name, char* parameter_name);
    static int GetValueInt(char* detector_name, char* parameter_name);
    static double GetValueDouble(char* detector_name, char* parameter_name){return GetValue(detector_name, parameter_name);}

    static int ShowParameterList(char* detector_name = NULL);

    TString GetDetectorName(){return strDetectorName;}
    TString GetParameterName(){return strParameterName;}
    int GetType(){return (int)eParameterType;}
    double GetValue(){return fParameterValue;}
    TString GetDescription(){return strDescription;}

    void UpdateFromDB();

    int SetDetectorName(char* detector_name);
    int SetParameterName(char* parameter_name);
    int SetType(int parameter_type);
    int SetValue(double parameter_value);
    int SetDescription(char* description);

  ClassDef(MpdGeoParameter,1);
};

#endif
