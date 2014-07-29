// -------------------------------------------------------------------------
// -----                      MpdDbGeoPoint header file                -----
// -----                  Created 13/02/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------
#include "MpdDbGeoPoint.h"

#include <iostream>
using namespace std;

// -----   Constructor with MPD connection   -------------------------------
MpdDbGeoPoint::MpdDbGeoPoint(MpdDbConnection* connMpdDb, int component_id, int point_pos, float x, float y, float z)
{
    connectionMpdDb = connMpdDb;

    iComponentId = component_id;
    iPointPos = point_pos;
    fX = x, fY = y, fZ = z;

}
// -------------------------------------------------------------------------

// -----   Getting geometry point from database   -------------------------------
MpdDbGeoPoint* MpdDbGeoPoint::GetGeoPoint(char* component_name, int position)
{
    if ((component_name == 0x00) || (component_name[0] == '\0')){
        cout<<"Component name can't be empty"<<endl;
        return 0x00;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return 0x00;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select p.component_id, p.point_pos, p.x, p.y, p.z "
                                            "from geo_point p join component c on p.component_id = c.component_id "
                                            "where lower(c.component_name) = $1 and p.point_pos = $2");
    TString compName = component_name;
    compName.ToLower();
    stmt->SetString(0, compName);
    stmt->SetInt(1, position);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<component_name<<":"<<position<<" - point wasn't found!"<<endl;

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
    float x = stmt->GetDouble(2), y = stmt->GetDouble(3), z = stmt->GetDouble(4);

    delete stmt;

    return new MpdDbGeoPoint(connMpdDb, component_id, position, x, y, z);
}

// -----   Creating new geometry point in database   -------------------------------
MpdDbGeoPoint* MpdDbGeoPoint::CreateGeoPoint(char* component_name, int position, float x, float y, float z)
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

    stmt = mpd_db->Statement("insert into geo_point(component_id, point_pos, x, y, z) "
                             "values ($1, $2, $3, $4, $5)");

    stmt->SetInt(0, component_id);
    stmt->SetInt(1, position);
    stmt->SetDouble(2, x);
    stmt->SetDouble(3, y);
    stmt->SetDouble(4, z);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        delete connMpdDb;

        return 0x00;
    }

    delete stmt;

    return new MpdDbGeoPoint(connMpdDb, component_id, position, x, y, z);
}

int MpdDbGeoPoint::DeleteGeoPoint(char* component_name, int position)
{
    if ((component_name == 0x00) || (component_name[0] == '\0')){
        cout<<"Component name can't be empty"<<endl;
        return -4;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return -1;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select p.component_id "
                                            "from geo_point p join component c on p.component_id = c.component_id "
                                            "where lower(c.component_name) = $1 and p.point_pos = $2");

    TString compName = component_name;
    compName.ToLower();
    stmt->SetString(0, compName);
    stmt->SetInt(1, position);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<component_name<<":"<<position<<" - point wasn't found!"<<endl;

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

    stmt = mpd_db->Statement("delete from geo_point "
                             "where component_id = $1 and point_pos = $2");

    stmt->SetInt(0, component_id);
    stmt->SetInt(1, position);

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

TString MpdDbGeoPoint::GetComponentName()
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

void MpdDbGeoPoint::UpdateFromDB()
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select x, y, z "
                                            "from geo_point "
                                            "where component_id = $1 and point_pos = $2");
    stmt->SetInt(0, iComponentId);
    stmt->SetInt(1, iPointPos);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<"Geometry point wasn't found!"<<endl;
            delete stmt;
            return;
        }
    }
    else{
        delete stmt;
        return;
    }

    // получение параметров
    fX = stmt->GetDouble(0);
    fY = stmt->GetDouble(1);
    fZ = stmt->GetDouble(2);

    delete stmt;

    return;
}

int MpdDbGeoPoint::SetComponentName(char* component_name)
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

    stmt = mpd_db->Statement("update geo_point "
                             "set component_id = $1 "
                             "where component_id = $2 and point_pos = $3");
    stmt->SetInt(0, component_id);
    stmt->SetInt(1, iComponentId);
    stmt->SetInt(2, iPointPos);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    iComponentId = component_id;

    delete stmt;

    return 0;
}

int MpdDbGeoPoint::SetPos(int position)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update geo_point "
                                            "set point_pos = $1 "
                                            "where component_id = $2 and point_pos = $3");
    stmt->SetInt(0, position);
    stmt->SetInt(1, iComponentId);
    stmt->SetInt(2, iPointPos);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    iPointPos = position;

    delete stmt;

    return 0;
}

int MpdDbGeoPoint::SetX(float x)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update geo_point "
                                            "set x = $1 "
                                            "where component_id = $2 and point_pos = $3");
    stmt->SetDouble(0, x);
    stmt->SetInt(1, iComponentId);
    stmt->SetInt(2, iPointPos);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    fX = x;

    delete stmt;

    return 0;
}

int MpdDbGeoPoint::SetY(float y)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update geo_point "
                                            "set y = $1 "
                                            "where component_id = $2 and point_pos = $3");
    stmt->SetDouble(0, y);
    stmt->SetInt(1, iComponentId);
    stmt->SetInt(2, iPointPos);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    fY = y;

    delete stmt;

    return 0;
}

int MpdDbGeoPoint::SetZ(float z)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update geo_point "
                                            "set z = $1 "
                                            "where component_id = $2 and point_pos = $3");
    stmt->SetDouble(0, z);
    stmt->SetInt(1, iComponentId);
    stmt->SetInt(2, iPointPos);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    fZ = z;

    delete stmt;

    return 0;
}

// -------------------------------------------------------------------
MpdDbGeoPoint::~MpdDbGeoPoint()
{
    if (connectionMpdDb)
        delete connectionMpdDb;
}

// -------------------------------------------------------------------
ClassImp(MpdDbGeoPoint)
