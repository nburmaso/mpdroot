// -------------------------------------------------------------------------
// -----                      MpdDbDetector header file                -----
// -----                  Created 28/01/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------

/** MpdDbDetector.h
 *@author K.Gertsenberger <gertsen@jinr.ru>
 **
 ** Class for the table Detector in MPD_DB database
 **/

#ifndef MPDDBDETECTOR_H
#define MPDDBDETECTOR_H 1

#include <TSQLServer.h>
#include <TSQLResult.h>
#include <TSQLRow.h>
#include <TSQLStatement.h>

#include "TString.h"

#include "MpdDbConnection.h"

class MpdDbDetector
{
 private:
    MpdDbConnection* connectionMpdDb;

    int iDetectorId;
    TString strDetectorName, strDetectorDescription;

    MpdDbDetector(MpdDbConnection* connMpdDb, int detector_id, TString detector_name, TString description);

 public:
    virtual ~MpdDbDetector(); // Destructor

    // статические методы класса
    static MpdDbDetector* GetDetector(char* detector_name);
    static MpdDbDetector* CreateDetector(char* detector_name, char* description = 0x00);
    static int DeleteDetector(char* detector_name);

    TString GetName(){return strDetectorName;}
    TString GetDescription(){return strDetectorDescription;}

    void UpdateFromDB();

    int SetName(char* detector_name);
    int SetDescription(char* description);

  ClassDef(MpdDbDetector,1);
};

#endif
