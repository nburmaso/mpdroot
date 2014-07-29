// -------------------------------------------------------------------------
// -----                      MpdDbComponentType header file           -----
// -----                  Created 12/02/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------
#include "MpdDbComponentType.h"

#include <iostream>
using namespace std;

// -----   Constructor with MPD connection   -------------------------------
MpdDbComponentType::MpdDbComponentType(MpdDbConnection* connMpdDb, TString type_name, TString description)
{
    connectionMpdDb = connMpdDb;

    strTypeName = type_name;
    strTypeDescription = description;
}
// -------------------------------------------------------------------------

// -----   Getting component type from database   -------------------------------
MpdDbComponentType* MpdDbComponentType::GetComponentType(char* type_name)
{
    if ((type_name == 0x00) || (type_name[0] == '\0')){
        cout<<"Component type can't be empty"<<endl;
        return 0x00;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return 0x00;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select type_name, description "
                                            "from component_type "
                                            "where lower(type_name) = $1");
    TString typeName = type_name;
    typeName.ToLower();
    stmt->SetString(0, typeName);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<type_name<<": component type wasn't found!"<<endl;

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

    TString strName = stmt->GetString(0);
    TString strDesc;
    if (stmt->IsNull(1)) strDesc = "";
    else strDesc = stmt->GetString(1);

    delete stmt;

    return new MpdDbComponentType(connMpdDb, strName, strDesc);
}

// -----   Creating new component type in database   -------------------------------
MpdDbComponentType* MpdDbComponentType::CreateComponentType(char* type_name, char* description)
{
    if ((type_name == 0x00) || (type_name[0] == '\0')){
        cout<<"Component type can't be empty"<<endl;
        return 0x00;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return 0x00;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("insert into component_type(type_name, description) "
                                            "values ($1, $2)");
    stmt->SetString(0, type_name);
    if ((description != 0x00) && (description[0] != '\0'))
        stmt->SetString(1, description);
    else
        stmt->SetNull(1);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        delete connMpdDb;

        return 0x00;
    }

    delete stmt;

    return new MpdDbComponentType(connMpdDb, type_name, description);
}

int MpdDbComponentType::DeleteComponentType(char* type_name)
{
    if ((type_name == 0x00) || (type_name[0] == '\0')){
        cout<<"Type name can't be empty"<<endl;
        return -3;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return -1;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("delete from component_type "
                                            "where lower(type_name) = $1");

    TString typeName = type_name;
    typeName.ToLower();
    stmt->SetString(0, typeName);

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

int MpdDbComponentType::Rename(char* type_name)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    if ((type_name == 0x00) || (type_name[0] == '\0')){
        cout<<"Type name can't be empty"<<endl;
        return -3;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update component_type "
                                            "set type_name = $1 "
                                            "where type_name = $2");
    stmt->SetString(0, type_name);
    stmt->SetString(1, strTypeName);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    strTypeName = type_name;

    delete stmt;

    return 0;
}

int MpdDbComponentType::SetDescription(char* description)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update component_type "
                                            "set description = $1 "
                                            "where type_name = $2");
    bool isNull = false;
    if ((description != 0x00) && (description[0] != '\0'))
        stmt->SetString(0, description);
    else
    {
        stmt->SetNull(0);
        isNull = true;
    }
    stmt->SetString(1, strTypeName);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    if (isNull) strTypeDescription = "";
    else strTypeDescription = description;

    delete stmt;

    return 0;
}
  
// -------------------------------------------------------------------
MpdDbComponentType::~MpdDbComponentType()
{
    if (connectionMpdDb)
        delete connectionMpdDb;
}

// -------------------------------------------------------------------
ClassImp(MpdDbComponentType)
