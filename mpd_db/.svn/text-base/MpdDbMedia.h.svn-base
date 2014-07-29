// -------------------------------------------------------------------------
// -----                      MpdDbMedia header file                   -----
// -----                  Created 12/02/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------

/** MpdDbMedia.h
 *@author K.Gertsenberger <gertsen@jinr.ru>
 **
 ** Class for the media in MPD_DB database
 **/

#ifndef MPDDBMEDIA_H
#define MPDDBMEDIA_H 1

#include <TSQLServer.h>
#include <TSQLResult.h>
#include <TSQLRow.h>
#include <TSQLStatement.h>

#include "TString.h"

#include "MpdDbConnection.h"

class MpdDbMedia
{
 private:
    MpdDbConnection* connectionMpdDb;

    TString strMediaName, strMediaDescription;

    MpdDbMedia(MpdDbConnection* connMpdDb, TString media_name, TString description);

 public:
    virtual ~MpdDbMedia(); // Destructor

    // статические методы класса
    static MpdDbMedia* GetMedia(char* media_name);
    static MpdDbMedia* CreateMedia(char* media_name, char* description = 0x00);
    static int DeleteMedia(char* media_name);

    TString GetName(){return strMediaName;}
    TString GetDescription(){return strMediaDescription;}

    int Rename(char* media_name);
    int SetDescription(char* description);

  ClassDef(MpdDbMedia,1);
};

#endif
