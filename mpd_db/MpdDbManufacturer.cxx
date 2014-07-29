// -------------------------------------------------------------------------
// -----                      MpdDbManufacturer header file            -----
// -----                  Created 13/02/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------
#include "MpdDbManufacturer.h"

#include <iostream>
using namespace std;

// -----   Constructor with MPD connection   -------------------------------
MpdDbManufacturer::MpdDbManufacturer(MpdDbConnection* connMpdDb, int manufacturer_id, int manufacturer_parent_id, TString manufacturer_name,
                                     bool is_laboratory, TString description)
{
    connectionMpdDb = connMpdDb;

    iManufacturerId = manufacturer_id;
    iManufacturerParentId = manufacturer_parent_id;
    strManufacturerName = manufacturer_name;
    isLaboratory_ = is_laboratory;
    strManufacturerDescription = description;
}
// -------------------------------------------------------------------------

// -----   Getting manufacturer from database   -------------------------------
MpdDbManufacturer* MpdDbManufacturer::GetManufacturer(char* manufacturer_name)
{
    if ((manufacturer_name == 0x00) || (manufacturer_name[0] == '\0')){
        cout<<"Manufacturer name can't be empty"<<endl;
        return 0x00;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return 0x00;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select manufacturer_id, manufacturer_parent_id, manufacturer_name, isLaboratory, description "
                                            "from manufacturer "
                                            "where lower(manufacturer_name) = $1");
    TString manName = manufacturer_name;
    manName.ToLower();
    stmt->SetString(0, manName);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<manufacturer_name<<" manufacturer wasn't found!"<<endl;

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
    int manufacturer_id = stmt->GetInt(0);

    int manufacturer_parent_id = -1;
    if (!stmt->IsNull(1)) manufacturer_parent_id = stmt->GetInt(1);

    TString strName = stmt->GetString(2);

    bool is_laboratory = (bool)stmt->GetInt(3);

    TString description = "";
    if (!stmt->IsNull(4)) description = stmt->GetString(4);

    delete stmt;

    return new MpdDbManufacturer(connMpdDb, manufacturer_id, manufacturer_parent_id, strName, is_laboratory, description);
}

// -----   Creating new manufacturer in database   -------------------------------
MpdDbManufacturer* MpdDbManufacturer::CreateManufacturer(char* manufacturer_name, char* manufacturer_parent_name, bool is_laboratory, char* description)
{
    if ((manufacturer_name == 0x00) || (manufacturer_name[0] == '\0')){
        cout<<"Manufacturer name can't be empty"<<endl;
        return 0x00;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return 0x00;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt;

    int manufacturer_parent_id = -1;
    if ((manufacturer_parent_name != 0x00) && (manufacturer_parent_name[0] != '\0')){
        stmt = mpd_db->Statement("select manufacturer_id "
                                 "from manufacturer "
                                 "where lower(manufacturer_name) = $1");
        TString parName = manufacturer_parent_name;
        parName.ToLower();
        stmt->SetString(0, parName);

        // process statement
        if (stmt->Process()){
            // store result of statement in buffer
            stmt->StoreResult();

            // extract rows one after another
            if (!stmt->NextResultRow()){
                cout<<manufacturer_parent_name<<": parent manufacturer wasn't found!"<<endl;

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

        manufacturer_parent_id = stmt->GetInt(0);

        delete stmt;
    }

    stmt = mpd_db->Statement("insert into manufacturer(manufacturer_parent_id, manufacture_name, isLaboratory, description) "
                             "values ($1, $2, $3, $4)");

    if (manufacturer_parent_id != -1)
        stmt->SetInt(0, manufacturer_parent_id);
    else
        stmt->SetNull(0);

    stmt->SetString(1, manufacturer_name);

    stmt->SetInt(2, (int)is_laboratory);

    if ((description != 0x00) && (description[0] != '\0'))
        stmt->SetString(3, description);
    else
        stmt->SetNull(3);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        delete connMpdDb;

        return 0x00;
    }

    delete stmt;

    // get serial identificator for new manufacturer
    mpd_db->Statement("select manufacturer_id "
                      "from manufacturer "
                      "where manufacturer_name = $1");

    stmt->SetString(0, manufacturer_name);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<manufacturer_name<<": new manufacturer wasn't found!"<<endl;

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

    int manufacturer_id = stmt->GetInt(0);

    delete stmt;

    return new MpdDbManufacturer(connMpdDb, manufacturer_id, manufacturer_parent_id, manufacturer_name, is_laboratory, description);
}

int MpdDbManufacturer::DeleteManufacturer(char* manufacturer_name)
{
    if ((manufacturer_name == 0x00) || (manufacturer_name[0] == '\0')){
        cout<<"Manufacturer name can't be empty"<<endl;
        return -3;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return -1;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select manufacturer_id "
                                            "from manufacturer "
                                            "where lower(manufacturer_name) = $1");
    TString manName = manufacturer_name;
    manName.ToLower();
    stmt->SetString(0, manName);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<manufacturer_name<<": manufacturer wasn't found!"<<endl;

            delete stmt;
            delete connMpdDb;

            return -10;
        }
    }
    else{
        delete stmt;
        delete connMpdDb;

        return -11;
    }

    int manufacturer_id = stmt->GetInt(0);

    delete stmt;

    stmt = mpd_db->Statement("delete from manufacturer "
                             "where manufacturer_id = $1");

    stmt->SetInt(0, manufacturer_id);

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

TString MpdDbManufacturer::GetParentName()
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return "";
    }

    if (iManufacturerParentId == -1) return "";

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select manufacturer_name "
                                            "from manufacturer "
                                            "where manufacturer_id = $1");
    stmt->SetInt(0, iManufacturerParentId);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<iManufacturerParentId<<": parent manufacturer wasn't found!"<<endl;

            delete stmt;

            return "";
        }
    }
    else{
        delete stmt;

        return "";
    }

    TString manufacturer_parent_name = stmt->GetString(0);

    delete stmt;

    return manufacturer_parent_name;
}

void MpdDbManufacturer::UpdateFromDB()
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select manufacturer_parent_id, manufacturer_name, isLaboratory, description "
                                            "from manufacturer "
                                            "where manufacturer_id = $1");
    stmt->SetInt(0, iManufacturerId);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<"Manufacturer (with old id) wasn't found!"<<endl;
            delete stmt;
            return;
        }
    }
    else{
        delete stmt;
        return;
    }

    // получение параметров
    if (!stmt->IsNull(0)) iManufacturerParentId = stmt->GetInt(0);
    else{
        iManufacturerParentId = -1;
    }

    strManufacturerName = stmt->GetString(1);

    isLaboratory_ = (bool)stmt->GetInt(2);

    if (!stmt->IsNull(3)) strManufacturerDescription = stmt->GetString(3);
    else strManufacturerDescription = "";

    delete stmt;

    return;
}

int MpdDbManufacturer::SetParent(char* manufacturer_parent_name)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    bool isNull = false;
    int manufacturer_id = -1;
    if ((manufacturer_parent_name != 0x00) && (manufacturer_parent_name[0] != '\0')){
        TSQLStatement* stmt = mpd_db->Statement("select manufacturer_id "
                                                "from manufacturer "
                                                "where lower(manufacturer_name) = $1");
        TString parName = manufacturer_parent_name;
        parName.ToLower();
        stmt->SetString(0, parName);

        // process statement
        if (stmt->Process()){
            // store result of statement in buffer
            stmt->StoreResult();

            // extract rows one after another
            if (!stmt->NextResultRow()){
                cout<<manufacturer_parent_name<<": parent manufacturer wasn't found!"<<endl;

                delete stmt;
                return -10;
            }
        }
        else{
            delete stmt;
            return -11;
        }

        manufacturer_id = stmt->GetInt(0);

        delete stmt;
    }
    else isNull = true;

    TSQLStatement* stmt = mpd_db->Statement("update manufacturer "
                                            "set manufacturer_parent_id = $1 "
                                            "where manufacturer_id = $2");
    if (isNull) stmt->SetNull(0);
    else stmt->SetInt(0, manufacturer_id);
    stmt->SetInt(1, iManufacturerId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    iManufacturerParentId = manufacturer_id;

    delete stmt;

    return 0;
}

int MpdDbManufacturer::SetName(char* manufacturer_name)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    if ((manufacturer_name == 0x00) || (manufacturer_name[0] == '\0')){
        cout<<"Manufacturer name can't be empty"<<endl;
        return -3;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update manufacturer "
                                            "set manufacturer_name = $1 "
                                            "where manufacturer_id = $2");
    stmt->SetString(0, manufacturer_name);
    stmt->SetInt(1, iManufacturerId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    strManufacturerName = manufacturer_name;

    delete stmt;

    return 0;
}

int MpdDbManufacturer::SetLaboratory(bool is_laboratory)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update manufacturer "
                                            "set is_laboratory = $1 "
                                            "where manufacturer_id = $2");
    stmt->SetInt(0, is_laboratory);
    stmt->SetInt(1, iManufacturerId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    isLaboratory_ = is_laboratory;

    delete stmt;

    return 0;
}

int MpdDbManufacturer::SetDescription(char* description)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update manufacturer "
                                            "set description = $1 "
                                            "where manufacturer_id = $2");
    bool isNull = false;
    if ((description != 0x00) && (description[0] != '\0'))
        stmt->SetString(0, description);
    else
    {
        stmt->SetNull(0);
        isNull = true;
    }
    stmt->SetInt(1, iManufacturerId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    if (isNull) strManufacturerDescription = "";
    else strManufacturerDescription = description;

    delete stmt;

    return 0;
}

// -------------------------------------------------------------------
MpdDbManufacturer::~MpdDbManufacturer()
{
    if (connectionMpdDb)
        delete connectionMpdDb;
}

// -------------------------------------------------------------------
ClassImp(MpdDbManufacturer)
