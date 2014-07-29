// -------------------------------------------------------------------------
// -----                      MpdDbGeoShape header file                -----
// -----                  Created 12/02/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------

/** MpdDbGeoShape.h
 *@author K.Gertsenberger <gertsen@jinr.ru>
 **
 ** Class for the geometry shape in MPD_DB database
 **/

#ifndef MPDDBGEOSHAPE_H
#define MPDDBGEOSHAPE_H 1

#include <TSQLServer.h>
#include <TSQLResult.h>
#include <TSQLRow.h>
#include <TSQLStatement.h>

#include "TString.h"

#include "MpdDbConnection.h"

class MpdDbGeoShape
{
 private:
    MpdDbConnection* connectionMpdDb;

    TString strShapeName, strShapeDescription;

    MpdDbGeoShape(MpdDbConnection* connMpdDb, TString shape_name, TString description);

 public:
    virtual ~MpdDbGeoShape(); // Destructor

    // статические методы класса
    static MpdDbGeoShape* GetShape(char* shape_name);
    static MpdDbGeoShape* CreateShape(char* shape_name, char* description = 0x00);
    static int DeleteGeoShape(char* shape_name);

    TString GetName(){return strShapeName;}
    TString GetDescription(){return strShapeDescription;}

    int Rename(char* shape_name);
    int SetDescription(char* description);

  ClassDef(MpdDbGeoShape,1);
};

#endif
