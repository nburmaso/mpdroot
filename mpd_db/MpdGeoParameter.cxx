// -------------------------------------------------------------------------
// -----                      MpdGeoParameter header file            -----
// -----                  Created 26/07/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------
#include "MpdGeoParameter.h"

#include <iostream>
using namespace std;

// -----   Constructor with MPD connection   -------------------------------
MpdGeoParameter::MpdGeoParameter(MpdDbConnection* connMpdGeo, int parameter_id, TString detector_name, TString parameter_name, enumParameterType parameter_type, double parameter_value, TString desc)
{
    connectionMpdGeo = connMpdGeo;

    iParameterId = parameter_id;
    strDetectorName = detector_name;
    strParameterName = parameter_name;
    eParameterType = parameter_type;
    fParameterValue = parameter_value;
    strDescription = desc;
}
// -------------------------------------------------------------------------

// -----   Getting geometry parameter from database   -------------------------------
MpdGeoParameter* MpdGeoParameter::GetGeoParameter(char* detector_name, char* parameter_name)
{
    if ((detector_name == 0x00) || (detector_name[0] == '\0')){
        cout<<"Detector's name can't be empty"<<endl;
        return 0x00;
    }
    if ((parameter_name == 0x00) || (parameter_name[0] == '\0')){
        cout<<"Parameter name can't be empty"<<endl;
        return 0x00;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_GEO);
    if (connMpdDb == 0x00) return 0x00;

    TSQLServer* mpd_geo = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_geo->Statement("select p.parameter_id, p.detector_name, p.parameter_name, p.parameter_type, p.parameter_value, p.description "
                                             "from geo_parameter p "
                                             "where lower(p.detector_name) = $1 and lower(p.parameter_name) = $2");
    TString detName = detector_name;
    detName.ToLower();
    stmt->SetString(0, detName);
    TString parName = parameter_name;
    parName.ToLower();
    stmt->SetString(1, parName);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<detector_name<<":"<<parameter_name<<" - parameter wasn't found!"<<endl;

            delete stmt;
            delete connMpdDb;

            return 0x00;
        }
    }
    else{
        delete stmt;
        delete connMpdDb;

        return 0x00;
    }

    // получение параметров и создание нового объекта
    int parameter_id = stmt->GetInt(0);
    TString strDetName = stmt->GetString(1);
    TString strParName = stmt->GetString(2);
    enumParameterType parameter_type = (enumParameterType)stmt->GetInt(3);
    double parameter_value = stmt->GetDouble(4);
    TString strDesc;
    if (stmt->IsNull(5)) strDesc = "";
    else strDesc = stmt->GetString(5);

    delete stmt;

    return new MpdGeoParameter(connMpdDb, parameter_id, strDetName, strParName, parameter_type, parameter_value, strDesc);
}

// -----   Creating new geometry parameter in database   -------------------------------
MpdGeoParameter* MpdGeoParameter::CreateGeoParameter(char* detector_name, char* parameter_name, int parameter_type, float parameter_value, char* description)
{
    if ((detector_name == 0x00) || (detector_name[0] == '\0')){
        cout<<"Detector's name can't be empty"<<endl;
        return 0x00;
    }
    if ((parameter_name == 0x00) || (parameter_name[0] == '\0')){
        cout<<"Parameter name can't be empty"<<endl;
        return 0x00;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_GEO);
    if (connMpdDb == 0x00) return 0x00;

    TSQLServer* mpd_geo = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_geo->Statement("insert into geo_parameter(detector_name, parameter_name, parameter_type, parameter_value, description) "
                                             "values ($1, $2, $3, $4, $5)");

    stmt->SetString(0, detector_name);
    stmt->SetString(1, parameter_name);
    stmt->SetInt(2, parameter_type);
    stmt->SetDouble(3, parameter_value);
    TString strDesc = "";
    if ((description != 0x00) && (description[0] != '\0')){
        stmt->SetString(4, description);
        strDesc = description;
    }
    else
        stmt->SetNull(4);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        delete connMpdDb;

        return 0x00;
    }

    delete stmt;

    // get serial identificator for new parameter
    stmt = mpd_geo->Statement("select parameter_id "
                              "from geo_parameter "
                              "where detector_name = $1 and parameter_name = $2");
    stmt->SetString(0, detector_name);
    stmt->SetString(1, parameter_name);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<detector_name<<":"<<parameter_name<<" - parameter wasn't found!"<<endl;

            delete stmt;
            delete connMpdDb;

            return 0x00;
        }
    }
    else{
        delete stmt;
        delete connMpdDb;

        return 0x00;
    }

    int parameter_id = stmt->GetInt(0);

    delete stmt;

    return new MpdGeoParameter(connMpdDb, parameter_id, detector_name, parameter_name, (enumParameterType)parameter_type, parameter_value, strDesc);
}

int MpdGeoParameter::DeleteGeoParameter(char* detector_name, char* parameter_name)
{
    if ((detector_name == 0x00) || (detector_name[0] == '\0')){
        cout<<"Detector's name can't be empty"<<endl;
        return 0x00;
    }
    if ((parameter_name == 0x00) || (parameter_name[0] == '\0')){
        cout<<"Parameter name can't be empty"<<endl;
        return 0x00;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_GEO);
    if (connMpdDb == 0x00) return -1;

    TSQLServer* mpd_geo = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_geo->Statement("select p.parameter_id "
                                             "from geo_parameter p "
                                             "where lower(p.detector_name) = $1 and lower(p.parameter_name) = $2");

    TString detName = detector_name;
    detName.ToLower();
    stmt->SetString(0, detName);
    TString parName = parameter_name;
    parName.ToLower();
    stmt->SetString(1, parName);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<detector_name<<":"<<parameter_name<<" - parameter wasn't found!"<<endl;

            delete stmt;
            delete connMpdDb;

            return 0x00;
        }
    }
    else{
        delete stmt;
        delete connMpdDb;

        return 0x00;
    }

    int parameter_id = stmt->GetInt(0);

    delete stmt;

    stmt = mpd_geo->Statement("delete from geo_parameter "
                              "where parameter_id = $1");

    stmt->SetInt(0, parameter_id);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        delete connMpdDb;

        return -2;
    }

    delete stmt;
    delete connMpdDb;

    return 0;
}

int MpdGeoParameter::ShowParameterList(char* detector_name){
    bool isDetectorSet = true;
    if ((detector_name == 0x00) || (detector_name[0] == '\0')){
        isDetectorSet = false;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_GEO);
    if (connMpdDb == 0x00) return -1;

    TSQLServer* mpd_geo = connMpdDb->GetSQLServer();

    TSQLStatement* stmt;
    if (isDetectorSet){
        stmt = mpd_geo->Statement("select p.detector_name, p.parameter_name, p.parameter_type, p.parameter_value, p.description "
                                  "from geo_parameter p "
                                  "where lower(p.detector_name) = $1 "
                                  "order by p.parameter_name");
        TString detName = detector_name;
        detName.ToLower();
        stmt->SetString(0, detName);
    }
    else
        stmt = mpd_geo->Statement("select p.detector_name, p.parameter_name, p.parameter_type, p.parameter_value, p.description "
                                  "from geo_parameter p "
                                  "order by p.detector_name, p.parameter_name");

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        while (stmt->NextResultRow()){
            TString strDetName = stmt->GetString(0), strParName = stmt->GetString(1);
            enumParameterType parameter_type = (enumParameterType)stmt->GetInt(2);
            double parameter_value = stmt->GetDouble(3);
            TString strDesc = stmt->GetString(4);

            cout<<"Detector: "<<strDetName<<endl<<" Parameter: "<<(parameter_type == DoubleValue ? "double" : "int")<<" "<<strParName<<" = "<<parameter_value
               <<" // "<<strDesc<<endl;
        }
    }
    else{
        delete stmt;
        delete connMpdDb;

        return 0x00;
    }

    delete stmt;
    delete connMpdDb;
}

double MpdGeoParameter::GetValue(char* detector_name, char* parameter_name)
{
    if ((detector_name == 0x00) || (detector_name[0] == '\0')){
        cout<<"Detector's name can't be empty"<<endl;
        return 0;
    }
    if ((parameter_name == 0x00) || (parameter_name[0] == '\0')){
        cout<<"Parameter name can't be empty"<<endl;
        return 0;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_GEO);
    if (connMpdDb == 0x00) return -1;

    TSQLServer* mpd_geo = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_geo->Statement("select p.parameter_value, p.parameter_type "
                                             "from geo_parameter p "
                                             "where lower(p.detector_name) = $1 and lower(p.parameter_name) = $2");

    TString detName = detector_name;
    detName.ToLower();
    stmt->SetString(0, detName);
    TString parName = parameter_name;
    parName.ToLower();
    stmt->SetString(1, parName);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<detector_name<<":"<<parameter_name<<" - parameter wasn't found!"<<endl;

            delete stmt;
            delete connMpdDb;

            return 0x00;
        }
    }
    else{
        delete stmt;
        delete connMpdDb;

        return 0x00;
    }

    // checking for parameter type
    int parameter_type = stmt->GetInt(1);
    if (parameter_type != 0)
        cout<<"Warning: parameter "<<detector_name<<":"<<parameter_name<<" is not double!"<<endl;

    double parameter_value = stmt->GetDouble(0);

    delete stmt;
    delete connMpdDb;

    return parameter_value;
}

int MpdGeoParameter::GetValueInt(char* detector_name, char* parameter_name)
{
    if ((detector_name == 0x00) || (detector_name[0] == '\0')){
        cout<<"Detector's name can't be empty"<<endl;
        return 0;
    }
    if ((parameter_name == 0x00) || (parameter_name[0] == '\0')){
        cout<<"Parameter name can't be empty"<<endl;
        return 0;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_GEO);
    if (connMpdDb == 0x00) return -1;

    TSQLServer* mpd_geo = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_geo->Statement("select p.parameter_value, p.parameter_type "
                                             "from geo_parameter p "
                                             "where lower(p.detector_name) = $1 and lower(p.parameter_name) = $2");

    TString detName = detector_name;
    detName.ToLower();
    stmt->SetString(0, detName);
    TString parName = parameter_name;
    parName.ToLower();
    stmt->SetString(1, parName);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<detector_name<<":"<<parameter_name<<" - parameter wasn't found!"<<endl;

            delete stmt;
            delete connMpdDb;

            return 0x00;
        }
    }
    else{
        delete stmt;
        delete connMpdDb;

        return 0x00;
    }

    // checking for parameter type
    int parameter_type = stmt->GetInt(1);
    if (parameter_type != 1)
        cout<<"Warning: parameter "<<detector_name<<":"<<parameter_name<<" is not int!"<<endl;

    int parameter_value = stmt->GetInt(0);

    delete stmt;
    delete connMpdDb;

    return parameter_value;
}

void MpdGeoParameter::UpdateFromDB()
{
    if (!connectionMpdGeo){
        cout<<"Connection object is null"<<endl;
        return;
    }

    TSQLServer* mpd_geo = connectionMpdGeo->GetSQLServer();

    TSQLStatement* stmt = mpd_geo->Statement("select p.detector_name, p.parameter_name, p.parameter_type, p.parameter_value, p.description "
                                             "from geo_parameter p "
                                             "where p.parameter_id = $1");
    stmt->SetInt(0, iParameterId);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<"Geometry parameter wasn't found!"<<endl;
            delete stmt;
            return;
        }
    }
    else{
        delete stmt;
        return;
    }

    // получение параметров
    strDetectorName = stmt->GetString(0);
    strParameterName = stmt->GetString(1);
    eParameterType = (enumParameterType)stmt->GetInt(2);
    fParameterValue= stmt->GetDouble(3);
    strDescription = stmt->GetString(4);

    delete stmt;

    return;
}

int MpdGeoParameter::SetDetectorName(char* detector_name)
{
    if (!connectionMpdGeo){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    if ((detector_name == 0x00) || (detector_name[0] == '\0')){
        cout<<"Detector's name can't be empty"<<endl;
        return -3;
    }

    TSQLServer* mpd_geo = connectionMpdGeo->GetSQLServer();

    TSQLStatement* stmt = mpd_geo->Statement("update geo_parameter "
                                             "set detector_name = $1 "
                                             "where parameter_id = $2");
    stmt->SetString(0, detector_name);
    stmt->SetInt(1, iParameterId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    strDetectorName = detector_name;

    delete stmt;

    return 0;
}

int MpdGeoParameter::SetParameterName(char* parameter_name)
{
    if (!connectionMpdGeo){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    if ((parameter_name == 0x00) || (parameter_name[0] == '\0')){
        cout<<"Parameter name can't be empty"<<endl;
        return -3;
    }

    TSQLServer* mpd_geo = connectionMpdGeo->GetSQLServer();

    TSQLStatement* stmt = mpd_geo->Statement("update geo_parameter "
                                             "set parameter_name = $1 "
                                             "where parameter_id = $2");
    stmt->SetString(0, parameter_name);
    stmt->SetInt(1, iParameterId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    strParameterName = parameter_name;

    delete stmt;

    return 0;
}

int MpdGeoParameter::SetType(int parameter_type)
{
    if (!connectionMpdGeo){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_geo = connectionMpdGeo->GetSQLServer();

    TSQLStatement* stmt = mpd_geo->Statement("update geo_parameter "
                                             "set parameter_type = $1 "
                                             "where parameter_id = $2");
    stmt->SetInt(0, parameter_type);
    stmt->SetInt(1, iParameterId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    eParameterType = (enumParameterType)parameter_type;

    delete stmt;

    return 0;
}

int MpdGeoParameter::SetValue(double parameter_value)
{
    if (!connectionMpdGeo){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_geo = connectionMpdGeo->GetSQLServer();

    TSQLStatement* stmt = mpd_geo->Statement("update geo_parameter "
                                             "set parameter_value = $1 "
                                             "where parameter_id = $2");
    stmt->SetDouble(0, parameter_value);
    stmt->SetInt(1, iParameterId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    fParameterValue = parameter_value;

    delete stmt;

    return 0;
}

int MpdGeoParameter::SetDescription(char* description)
{
    if (!connectionMpdGeo){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_geo = connectionMpdGeo->GetSQLServer();

    TSQLStatement* stmt = mpd_geo->Statement("update geo_parameter "
                                             "set description = $1 "
                                             "where parameter_id = $2");

    if ((description != 0x00) && (description[0] != '\0')){
        stmt->SetString(0, description);
    }
    else
        stmt->SetNull(0);
    stmt->SetInt(1, iParameterId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    strDescription = description;

    delete stmt;

    return 0;
}

// -------------------------------------------------------------------
MpdGeoParameter::~MpdGeoParameter()
{
    if (connectionMpdGeo)
        delete connectionMpdGeo;
}

// -------------------------------------------------------------------
ClassImp(MpdGeoParameter)
