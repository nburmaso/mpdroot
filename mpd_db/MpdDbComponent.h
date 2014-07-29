// -------------------------------------------------------------------------
// -----                      MpdDbComponent header file               -----
// -----                  Created 28/01/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------

/** MpdDbComponent.h
 *@author K.Gertsenberger <gertsen@jinr.ru>
 **
 ** Class for the table Component in MPD_DB database
 **/

#ifndef MPDDBCOMPONENT_H
#define MPDDBCOMPONENT_H 1

#include <TSQLServer.h>
#include <TSQLResult.h>
#include <TSQLRow.h>
#include <TSQLStatement.h>

#include "TString.h"
#include "TTimeStamp.h"
#include "TDatime.h"

#include "MpdDbConnection.h"

class MpdDbComponent
{
 private:
    MpdDbConnection* connectionMpdDb;

    int iComponentId, iDetectorId, iComponentParentId, iManufacturerId;
    TString strComponentName, strTypeName, strMediaName, strShapeName, strComponentDescription;
    bool isVirtual;
    TDatime tsBeginPart;
    TDatime* tsEndPart;
    TDatime* dateProductionDate;

    MpdDbComponent(MpdDbConnection* connMpdDb, int component_id, int detector_id, int parent_component_id, TString component_name,
                   TString shape_name, TString media_name, bool is_virtual, TDatime begin_part,
                   TDatime* end_part, TString type_name, int manufacturer_id, TDatime* production_date, TString description);

 public:
    virtual ~MpdDbComponent(); // Destructor

    // статические методы класса
    static MpdDbComponent* GetComponent(char* detector_name, char* component_name);
    static MpdDbComponent* CreateComponent(char* detector_name, char* parent_component_name, char* component_name,
                                           char* shape_name, char* media_name, bool is_virtual,
                                           char* type_name = 0x00, char* manufacturer_name = 0x00, TDatime* production_date = 0x00, char* description = 0x00);
    //static int DeleteComponent(char* detector_name, char* component_name);

    TString GetDetectorName();
    TString GetParentComponentName();
    TString GetName(){return strComponentName;}
    TString GetTypeName(){return strTypeName;}
    TString GetMediaName(){return strMediaName;}
    TString GetShape(){return strShapeName;}
    bool isVirtualComponent(){return isVirtual;}
    TDatime GetBeginPart(){return tsBeginPart;}
    TDatime* GetEndPart(){return tsEndPart;}
    TString GetManufacturerName();
    TDatime* GetProductionDate(){return dateProductionDate;}
    TString GetDescription(){return strComponentDescription;}

    void UpdateFromDB();

    int SetDetectorName(char* detector_name);
    int SetParentComponentName(char* parent_component_name);
    int SetName(char* component_name);
    int SetTypeName(char* type_name);
    int SetMediaName(char* media_name);
    int SetShapeName(char* shape_name);
    int SetVirtual(bool is_virtual = true);
    int SetEndPart(TDatime* end_part);
    int SetManufacturerName(char* manufacturer_name);
    int SetProductionDate(TDatime* production_date);
    int SetDescription(char* description);

    int ChangeBeginPart(TDatime begin_part);
    int ClosePart();

  ClassDef(MpdDbComponent,1);
};

#endif
