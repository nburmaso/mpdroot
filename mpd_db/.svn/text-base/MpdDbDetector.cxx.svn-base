// -------------------------------------------------------------------------
// -----                      MpdDbDetector header file                -----
// -----                  Created 28/01/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------
#include "MpdDbDetector.h"

#include <iostream>
using namespace std;

// -----   Constructor with MPD connection   -------------------------------
MpdDbDetector::MpdDbDetector(MpdDbConnection* connMpdDb, int detector_id, TString detector_name, TString description)
{
    connectionMpdDb = connMpdDb;

    iDetectorId = detector_id;
    strDetectorName = detector_name;
    strDetectorDescription = description;
}
// -------------------------------------------------------------------------

// -----   Getting detector from database   -------------------------------
MpdDbDetector* MpdDbDetector::GetDetector(char* detector_name)
{
    /*TString sql = TString::Format("select detector_id, detector_name, description "
                                  "from detector "
                                  "where detector_name = %s", detector_name);

    TSQLResult* res = mpd_db->Query(sql);

    int nrows = res->GetRowCount();
    if (nrows == 0){
        cout<<detector_name<<" detector wasn't found"<<endl;
        return 0x00;
    }

    TSQLRow* row = res->Next();
    TString detector_id = row->GetField(0);
    TString description = row->GetField(2);

    delete row;
    delete res;*/

    if ((detector_name == 0x00) || (detector_name[0] == '\0')){
        cout<<"Detector name can't be empty"<<endl;
        return 0x00;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return 0x00;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select detector_id, detector_name, description "
                                            "from detector "
                                            "where lower(detector_name) = $1");
    TString detName = detector_name;
    detName.ToLower();
    stmt->SetString(0, detName);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<detector_name<<" detector wasn't found!"<<endl;

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

    int detector_id = stmt->GetInt(0);
    TString strDetName = stmt->GetString(1);
    TString strDesc;
    if (stmt->IsNull(2)) strDesc = "";
    else strDesc = stmt->GetString(2);

    delete stmt;

    return new MpdDbDetector(connMpdDb, detector_id, strDetName, strDesc);
}

// -----   Creating new detector in database   -------------------------------
MpdDbDetector* MpdDbDetector::CreateDetector(char* detector_name, char* description)
{
    if ((detector_name == 0x00) || (detector_name[0] == '\0')){
        cout<<"Detector name can't be empty"<<endl;
        return 0x00;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return 0x00;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("insert into detector(detector_name, description) "
                                            "values ($1, $2)");
    stmt->SetString(0, detector_name);
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

    // get serial identificator for new detector
    stmt = mpd_db->Statement("select detector_id "
                             "from detector "
                             "where detector_name = $1");
    stmt->SetString(0, detector_name);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<detector_name<<" detector wasn't found!"<<endl;

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

    int detector_id = stmt->GetInt(0);

    delete stmt;

    return new MpdDbDetector(connMpdDb, detector_id, detector_name, description);
}

int MpdDbDetector::DeleteDetector(char* detector_name)
{
    if ((detector_name == 0x00) || (detector_name[0] == '\0')){
        cout<<"Detector name can't be empty"<<endl;
        return -3;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return -1;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("delete from detector "
                                            "where lower(detector_name) = $1");

    TString detName = detector_name;
    detName.ToLower();
    stmt->SetString(0, detName);

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

void MpdDbDetector::UpdateFromDB()
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select detector_name, description "
                                            "from detector "
                                            "where detector_id = $1");
    stmt->SetInt(0, iDetectorId);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<"Detector (with old id) wasn't found!"<<endl;
            delete stmt;
            return;
        }
    }
    else{
        delete stmt;
        return;
    }

    strDetectorName = stmt->GetString(0);
    strDetectorDescription = stmt->GetString(1);

    delete stmt;

    return;
}

int MpdDbDetector::SetName(char* detector_name)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    if ((detector_name == 0x00) || (detector_name[0] == '\0')){
        cout<<"Detector name can't be empty"<<endl;
        return -3;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update detector "
                                            "set detector_name = $1 "
                                            "where detector_id = $2");
    stmt->SetString(0, detector_name);
    stmt->SetInt(1, iDetectorId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    strDetectorName = detector_name;

    delete stmt;

    return 0;
}

int MpdDbDetector::SetDescription(char* description)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update detector "
                                            "set description = $1 "
                                            "where detector_id = $2");
    bool isNull = false;
    if ((description != 0x00) && (description[0] != '\0'))
        stmt->SetString(0, description);
    else
    {
        stmt->SetNull(0);
        isNull = true;
    }
    stmt->SetInt(1, iDetectorId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    if (isNull) strDetectorDescription = "";
    else strDetectorDescription = description;

    delete stmt;

    return 0;
}

/*int MpdDbDetector::DeleteDetector()
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("delete from detector "
                                            "where detector_id = $1");
    stmt->SetInt(0, iDetectorId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    delete stmt;

    return 0;
}*/
  
// -------------------------------------------------------------------
MpdDbDetector::~MpdDbDetector()
{
    if (connectionMpdDb)
        delete connectionMpdDb;
}

// -------------------------------------------------------------------
ClassImp(MpdDbDetector)
