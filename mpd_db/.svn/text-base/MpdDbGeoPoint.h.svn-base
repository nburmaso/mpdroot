// -------------------------------------------------------------------------
// -----                      MpdDbGeoPoint header file                -----
// -----                  Created 13/02/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------

/** MpdDbGeoPoint.h
 *@author K.Gertsenberger <gertsen@jinr.ru>
 **
 ** Class for the table Geo_point in MPD_DB database
 **/

#ifndef MPDDBGEOPOINT_H
#define MPDDBGEOPOINT_H 1

#include <TSQLServer.h>
#include <TSQLResult.h>
#include <TSQLRow.h>
#include <TSQLStatement.h>

#include "TString.h"
#include "TTimeStamp.h"
#include "TDatime.h"

#include "MpdDbConnection.h"

class MpdDbGeoPoint
{
 private:
    MpdDbConnection* connectionMpdDb;

    int iComponentId, iPointPos;
    float fX, fY, fZ;

    MpdDbGeoPoint(MpdDbConnection* connMpdDb, int component_id, int point_pos, float x, float y, float z);

 public:
    virtual ~MpdDbGeoPoint(); // Destructor

    // статические методы класса
    static MpdDbGeoPoint* GetGeoPoint(char* component_name, int position);
    static MpdDbGeoPoint* CreateGeoPoint(char* component_name, int position, float x, float y, float z);
    static int DeleteGeoPoint(char* component_name, int position);

    TString GetComponentName();
    int GetPos(){return iPointPos;}
    float GetX(){return fX;}
    float GetY(){return fY;}
    float GetZ(){return fZ;}

    void UpdateFromDB();

    int SetComponentName(char* component_name);
    int SetPos(int position);
    int SetX(float x);
    int SetY(float x);
    int SetZ(float x);

  ClassDef(MpdDbGeoPoint,1);
};

#endif
