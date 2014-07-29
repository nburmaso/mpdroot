// -------------------------------------------------------------------------
// -----                      MpdDbGeoShape header file                -----
// -----                  Created 12/02/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------
#include "MpdDbGeoShape.h"

#include <iostream>
using namespace std;

// -----   Constructor with MPD connection   -------------------------------
MpdDbGeoShape::MpdDbGeoShape(MpdDbConnection* connMpdDb, TString shape_name, TString description)
{
    connectionMpdDb = connMpdDb;

    strShapeName = shape_name;
    strShapeDescription = description;
}
// -------------------------------------------------------------------------

// -----   Getting geometry shape from database   -------------------------------
MpdDbGeoShape* MpdDbGeoShape::GetShape(char* shape_name)
{
    if ((shape_name == 0x00) || (shape_name[0] == '\0')){
        cout<<"Shape name can't be empty"<<endl;
        return 0x00;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return 0x00;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select shape_name, description "
                                            "from geo_shape "
                                            "where lower(shape_name) = $1");
    TString shapeName = shape_name;
    shapeName.ToLower();
    stmt->SetString(0, shapeName);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<shape_name<<": geometry shape wasn't found!"<<endl;

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

    return new MpdDbGeoShape(connMpdDb, strName, strDesc);
}

// -----   Creating new geometry shape in database   -------------------------------
MpdDbGeoShape* MpdDbGeoShape::CreateShape(char* shape_name, char* description)
{
    if ((shape_name == 0x00) || (shape_name[0] == '\0')){
        cout<<"Shape name can't be empty"<<endl;
        return 0x00;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return 0x00;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("insert into geo_shape(shape_name, description) "
                                            "values ($1, $2)");
    stmt->SetString(0, shape_name);
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

    return new MpdDbGeoShape(connMpdDb, shape_name, description);
}

int MpdDbGeoShape::DeleteGeoShape(char* shape_name)
{
    if ((shape_name == 0x00) || (shape_name[0] == '\0')){
        cout<<"Shape name can't be empty"<<endl;
        return -3;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return -1;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("delete from geo_shape "
                                            "where lower(shape_name) = $1");

    TString shapeName = shape_name;
    shapeName.ToLower();
    stmt->SetString(0, shapeName);

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

int MpdDbGeoShape::Rename(char* shape_name)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    if ((shape_name == 0x00) || (shape_name[0] == '\0')){
        cout<<"Shape name can't be empty"<<endl;
        return -3;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update geo_shape "
                                            "set shape_name = $1 "
                                            "where shape_name = $2");
    stmt->SetString(0, shape_name);
    stmt->SetString(1, strShapeName);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    strShapeName = shape_name;

    delete stmt;

    return 0;
}

int MpdDbGeoShape::SetDescription(char* description)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update geo_shape "
                                            "set description = $1 "
                                            "where shape_name = $2");
    bool isNull = false;
    if ((description != 0x00) && (description[0] != '\0'))
        stmt->SetString(0, description);
    else
    {
        stmt->SetNull(0);
        isNull = true;
    }
    stmt->SetString(1, strShapeName);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    if (isNull) strShapeDescription = "";
    else strShapeDescription = description;

    delete stmt;

    return 0;
}
  
// -------------------------------------------------------------------
MpdDbGeoShape::~MpdDbGeoShape()
{
    if (connectionMpdDb)
        delete connectionMpdDb;
}

// -------------------------------------------------------------------
ClassImp(MpdDbGeoShape)
