// -------------------------------------------------------------------------
// -----                      MpdDbGeoParameter header file            -----
// -----                  Created 13/02/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------
#include "MpdDbGeoParameter.h"

#include <iostream>
using namespace std;

// -----   Constructor with MPD connection   -------------------------------
MpdDbGeoParameter::MpdDbGeoParameter(MpdDbConnection* connMpdDb, int component_id, int parameter_type, float parameter_value)
{
    connectionMpdDb = connMpdDb;

    iComponentId = component_id;
    iParameterType = parameter_type;
    fParameterValue = parameter_value;

}
// -------------------------------------------------------------------------

// -----   Getting geometry parameter from database   -------------------------------
MpdDbGeoParameter* MpdDbGeoParameter::GetGeoParameter(char* component_name, int parameter_type)
{
    if ((component_name == 0x00) || (component_name[0] == '\0')){
        cout<<"Component name can't be empty"<<endl;
        return 0x00;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return 0x00;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select p.component_id, p.parameter_type, p.parameter_value "
                                            "from geo_parameter p join component c on p.component_id = c.component_id "
                                            "where lower(c.component_name) = $1 and p.parameter_type = $2");
    TString compName = component_name;
    compName.ToLower();
    stmt->SetString(0, compName);
    stmt->SetInt(1, parameter_type);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<component_name<<":"<<parameter_type<<" - parameter wasn't found!"<<endl;

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
    int component_id = stmt->GetInt(0);
    float parameter_value = stmt->GetDouble(2);

    delete stmt;

    return new MpdDbGeoParameter(connMpdDb, component_id, parameter_type, parameter_value);
}

// -----   Creating new geometry parameter in database   -------------------------------
MpdDbGeoParameter* MpdDbGeoParameter::CreateGeoParameter(char* component_name, int parameter_type, float parameter_value)
{
    if ((component_name == 0x00) || (component_name[0] == '\0')){
        cout<<"Component name can't be empty"<<endl;
        return 0x00;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return 0x00;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select component_id "
                                            "from component "
                                            "where lower(component_name) = $1");
    TString compName = component_name;
    compName.ToLower();
    stmt->SetString(0, compName);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<component_name<<" component wasn't found!"<<endl;

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

    int component_id = stmt->GetInt(0);

    delete stmt;

    stmt = mpd_db->Statement("insert into geo_parameter(component_id, parameter_type, parameter_value) "
                             "values ($1, $2, $3)");

    stmt->SetInt(0, component_id);
    stmt->SetInt(1, parameter_type);
    stmt->SetDouble(2, parameter_value);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        delete connMpdDb;

        return 0x00;
    }

    delete stmt;

    return new MpdDbGeoParameter(connMpdDb, component_id, parameter_type, parameter_value);
}

int MpdDbGeoParameter::DeleteGeoParameter(char* component_name, int parameter_type)
{
    if ((component_name == 0x00) || (component_name[0] == '\0')){
        cout<<"Component name can't be empty"<<endl;
        return -4;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return -1;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select p.component_id "
                                            "from geo_parameter p join component c on p.component_id = c.component_id "
                                            "where lower(c.component_name) = $1 and p.parameter_type = $2");

    TString compName = component_name;
    compName.ToLower();
    stmt->SetString(0, compName);
    stmt->SetInt(1, parameter_type);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<component_name<<":"<<parameter_type<<" - parameter wasn't found!"<<endl;

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

    int component_id = stmt->GetInt(0);

    delete stmt;

    stmt = mpd_db->Statement("delete from geo_parameter "
                             "where component_id = $1 and parameter_type = $2");

    stmt->SetInt(0, component_id);
    stmt->SetInt(1, parameter_type);

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

TString MpdDbGeoParameter::GetComponentName()
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return "";
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select component_name "
                                            "from component "
                                            "where component_id = $1");
    stmt->SetInt(0, iComponentId);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<iComponentId<<": component wasn't found!"<<endl;

            delete stmt;

            return "";
        }
    }
    else{
        delete stmt;

        return "";
    }

    TString component_name = stmt->GetString(0);

    delete stmt;

    return component_name;
}

void MpdDbGeoParameter::UpdateFromDB()
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select parameter_value "
                                            "from geo_parameter "
                                            "where component_id = $1 and parameter_type = $2");
    stmt->SetInt(0, iComponentId);
    stmt->SetInt(1, iParameterType);

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
    fParameterValue= stmt->GetDouble(0);

    delete stmt;

    return;
}

int MpdDbGeoParameter::SetComponentName(char* component_name)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    if ((component_name == 0x00) || (component_name[0] == '\0')){
        cout<<"Component name can't be empty"<<endl;
        return -3;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select component_id "
                                            "from component "
                                            "where lower(component_name) = $1");
    TString compName = component_name;
    compName.ToLower();
    stmt->SetString(0, compName);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<component_name<<": component wasn't found!"<<endl;

            delete stmt;
            return -10;
        }
    }
    else{
        delete stmt;
        return -11;
    }

    int component_id = stmt->GetInt(0);

    delete stmt;

    stmt = mpd_db->Statement("update geo_parameter "
                             "set component_id = $1 "
                             "where component_id = $2 and parameter_type = $3");
    stmt->SetInt(0, component_id);
    stmt->SetInt(1, iComponentId);
    stmt->SetInt(2, iParameterType);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    iComponentId = component_id;

    delete stmt;

    return 0;
}

int MpdDbGeoParameter::SetType(int parameter_type)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update geo_parameter "
                                            "set parameter_type = $1 "
                                            "where component_id = $2 and parameter_type = $3");
    stmt->SetInt(0, parameter_type);
    stmt->SetInt(1, iComponentId);
    stmt->SetInt(2, iParameterType);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    iParameterType = parameter_type;

    delete stmt;

    return 0;
}

int MpdDbGeoParameter::SetValue(float parameter_value)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update geo_parameter "
                                            "set parameter_value = $1 "
                                            "where component_id = $2 and parameter_type = $3");
    stmt->SetDouble(0, parameter_value);
    stmt->SetInt(1, iComponentId);
    stmt->SetInt(2, iParameterType);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    fParameterValue = parameter_value;

    delete stmt;

    return 0;
}

// -------------------------------------------------------------------
MpdDbGeoParameter::~MpdDbGeoParameter()
{
    if (connectionMpdDb)
        delete connectionMpdDb;
}

// -------------------------------------------------------------------
ClassImp(MpdDbGeoParameter)
