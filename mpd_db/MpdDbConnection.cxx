// -------------------------------------------------------------------------
// -----                      MpdDbConnection header file              -----
// -----                  Created 28/01/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------
#include "MpdDbConnection.h"

#include <iostream>
using namespace std;

// -----   Constructor with connection   ----------------------
MpdDbConnection::MpdDbConnection(TSQLServer* pSQLServer)
{
    mpd_db = pSQLServer;
}

// -------------------------------------------------------------------
MpdDbConnection* MpdDbConnection::Open(MpdConnectionType database_type)
{
    TSQLServer* pSQLServer = 0x00;

    switch (database_type)
    {
        case MPD_DB:
            pSQLServer = TSQLServer::Connect("pgsql://nc11.jinr.ru/mpd_db", "mpd", "cbmsoft");
            break;
        case MPD_GEO:
            pSQLServer = TSQLServer::Connect("pgsql://nc11.jinr.ru/mpd_geo", "mpd", "cbmsoft");
            break;
        case UNIFIED_DB:
            pSQLServer = TSQLServer::Connect("pgsql://nc13.jinr.ru/mpd_db", "mpd", "mpdsoft");
            break;
        default:
            cout<<"Incorrect database connection type!"<<endl;
    }

    if (pSQLServer == 0x00)
    {
        cout<<"Connection wasn't established"<<endl;
        return 0x00;
    }
    //else
    //    cout<<"Server info: "<<pSQLServer->ServerInfo()<<endl;

    return new MpdDbConnection(pSQLServer);
}

// -------------------------------------------------------------------
MpdDbConnection* MpdDbConnection::Open(TString strDBName, TString strUID, TString strPassword)
{
    TSQLServer* pSQLServer = TSQLServer::Connect(strDBName, strUID, strPassword);
    if (pSQLServer == 0x00)
    {
        cout<<"Connection wasn't established"<<endl;
        return 0x00;
    }
    else
        cout<<"Server info: "<<pSQLServer->ServerInfo()<<endl;

    return new MpdDbConnection(pSQLServer);
}

// -------------------------------------------------------------------
MpdDbConnection::~MpdDbConnection()
{
    if (mpd_db)
        delete mpd_db;
}

// -------------------------------------------------------------------
ClassImp(MpdDbConnection);
