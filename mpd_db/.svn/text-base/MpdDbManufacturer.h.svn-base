// -------------------------------------------------------------------------
// -----                      MpdDbManufacturer header file            -----
// -----                  Created 13/02/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------

/** MpdDbManufacturer.h
 *@author K.Gertsenberger <gertsen@jinr.ru>
 **
 ** Class for the table Manufacturer in MPD_DB database
 **/

#ifndef MPDDBMANUFACTURER_H
#define MPDDBMANUFACTURER_H 1

#include <TSQLServer.h>
#include <TSQLResult.h>
#include <TSQLRow.h>
#include <TSQLStatement.h>

#include "TString.h"
#include "TTimeStamp.h"
#include "TDatime.h"

#include "MpdDbConnection.h"

class MpdDbManufacturer
{
 private:
    MpdDbConnection* connectionMpdDb;

    int iManufacturerId, iManufacturerParentId;
    TString strManufacturerName, strManufacturerDescription;
    bool isLaboratory_;

    MpdDbManufacturer(MpdDbConnection* connMpdDb, int manufacturer_id, int manufacturer_parent_id, TString manufacturer_name,
                      bool is_laboratory, TString description);

 public:
    virtual ~MpdDbManufacturer(); // Destructor

    // статические методы класса
    static MpdDbManufacturer* GetManufacturer(char* manufacturer_name);
    static MpdDbManufacturer* CreateManufacturer(char* manufacturer_name, char* manufacturer_parent_name, bool is_laboratory, char* description = 0x00);
    static int DeleteManufacturer(char* manufacturer_name);

    TString GetParentName();
    TString GetName(){return strManufacturerName;}
    bool isLaboratory(){return isLaboratory_;}
    TString GetDescription(){return strManufacturerDescription;}

    void UpdateFromDB();

    int SetName(char* manufacturer_name);
    int SetParent(char* manufacturer_parent_name);
    int SetLaboratory(bool is_laboratory = true);
    int SetDescription(char* description);

  ClassDef(MpdDbManufacturer,1);
};

#endif
