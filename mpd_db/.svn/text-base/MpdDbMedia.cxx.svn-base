// -------------------------------------------------------------------------
// -----                      MpdDbMedia header file                   -----
// -----                  Created 12/02/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------
#include "MpdDbMedia.h"

#include <iostream>
using namespace std;

// -----   Constructor with MPD connection   -------------------------------
MpdDbMedia::MpdDbMedia(MpdDbConnection* connMpdDb, TString media_name, TString description)
{
    connectionMpdDb = connMpdDb;

    strMediaName = media_name;
    strMediaDescription = description;
}
// -------------------------------------------------------------------------

// -----   Getting media object from database   -------------------------------
MpdDbMedia* MpdDbMedia::GetMedia(char* media_name)
{
    if ((media_name == 0x00) || (media_name[0] == '\0')){
        cout<<"Media name can't be empty"<<endl;
        return 0x00;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return 0x00;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select media_name, description "
                                            "from media "
                                            "where lower(media_name) = $1");
    TString medName = media_name;
    medName.ToLower();
    stmt->SetString(0, medName);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<media_name<<": media wasn't found!"<<endl;

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

    TString strMedName = stmt->GetString(0);
    TString strDesc;
    if (stmt->IsNull(1)) strDesc = "";
    else strDesc = stmt->GetString(1);

    delete stmt;

    return new MpdDbMedia(connMpdDb, strMedName, strDesc);
}

// -----   Creating new media in database   -------------------------------
MpdDbMedia* MpdDbMedia::CreateMedia(char* media_name, char* description)
{
    if ((media_name == 0x00) || (media_name[0] == '\0')){
        cout<<"Media name can't be empty"<<endl;
        return 0x00;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return 0x00;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("insert into media(media_name, description) "
                                            "values ($1, $2)");
    stmt->SetString(0, media_name);
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

    return new MpdDbMedia(connMpdDb, media_name, description);
}

int MpdDbMedia::DeleteMedia(char* media_name)
{
    if ((media_name == 0x00) || (media_name[0] == '\0')){
        cout<<"Media name can't be empty"<<endl;
        return -3;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return -1;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("delete from media "
                                            "where lower(media_name) = $1");

    TString medName = media_name;
    medName.ToLower();
    stmt->SetString(0, medName);

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

int MpdDbMedia::Rename(char* media_name)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    if ((media_name == 0x00) || (media_name[0] == '\0')){
        cout<<"Media name can't be empty"<<endl;
        return -3;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update media "
                                            "set media_name = $1 "
                                            "where media_name = $2");
    stmt->SetString(0, media_name);
    stmt->SetString(1, strMediaName);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    strMediaName = media_name;

    delete stmt;

    return 0;
}

int MpdDbMedia::SetDescription(char* description)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update media "
                                            "set description = $1 "
                                            "where media_name = $2");
    bool isNull = false;
    if ((description != 0x00) && (description[0] != '\0'))
        stmt->SetString(0, description);
    else
    {
        stmt->SetNull(0);
        isNull = true;
    }
    stmt->SetString(1, strMediaName);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    if (isNull) strMediaDescription = "";
    else strMediaDescription = description;

    delete stmt;

    return 0;
}
  
// -------------------------------------------------------------------
MpdDbMedia::~MpdDbMedia()
{
    if (connectionMpdDb)
        delete connectionMpdDb;
}

// -------------------------------------------------------------------
ClassImp(MpdDbMedia)
