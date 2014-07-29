// -------------------------------------------------------------------------
// -----                      MpdDbComponent header file               -----
// -----                  Created 28/01/13  by K. Gertsenberger        -----
// -------------------------------------------------------------------------
#include "MpdDbComponent.h"

#include <iostream>
using namespace std;

// -----   Constructor with MPD connection   -------------------------------
MpdDbComponent::MpdDbComponent(MpdDbConnection* connMpdDb, int component_id, int detector_id, int parent_component_id, TString component_name,
                               TString shape_name, TString media_name, bool is_virtual, TDatime begin_part,
                               TDatime* end_part, TString type_name, int manufacturer_id, TDatime* production_date, TString description)
{
    connectionMpdDb = connMpdDb;

    iComponentId = component_id;
    iDetectorId = detector_id;
    iComponentParentId = parent_component_id;
    iManufacturerId = manufacturer_id;
    strComponentName = component_name;
    strTypeName = type_name;
    strMediaName = media_name;
    strShapeName = shape_name;
    strComponentDescription = description;
    isVirtual = is_virtual;
    tsBeginPart = begin_part;

    tsEndPart = end_part;
    dateProductionDate = production_date;
}
// -------------------------------------------------------------------------

// -----   Getting detector from database   -------------------------------
MpdDbComponent* MpdDbComponent::GetComponent(char* detector_name, char* component_name)
{
    if ((detector_name == 0x00) || (detector_name[0] == '\0')){
        cout<<"Detector name can't be empty"<<endl;
        return 0x00;
    }

    if ((component_name == 0x00) || (component_name[0] == '\0')){
        cout<<"Component name can't be empty"<<endl;
        return 0x00;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return 0x00;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select c.component_id, c.detector_id, c.component_parent_id, c.component_name, c.type_name, c.media_name, "
                                            "c.shape_name, c.isVirtual, c.begin_part, c.end_part, c.manufacturer_id, c.production_date, c.description "
                                            "from detector d join component c on d.detector_id = c.detector_id "
                                            "where lower(d.detector_name) = $1 and lower(c.component_name) = $2");
    TString detName = detector_name;
    detName.ToLower();
    stmt->SetString(0, detName);
    TString compName = component_name;
    compName.ToLower();
    stmt->SetString(1, compName);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<component_name<<" component wasn't found!"<<endl;

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
    int component_id = stmt->GetInt(0), detector_id = stmt->GetInt(1);

    int component_parent_id = -1, manufacturer_id = -1;
    if (!stmt->IsNull(2)) component_parent_id = stmt->GetInt(2);

    TString strName = stmt->GetString(3);

    TString type_name = "", media_name = "", shape_name = "", description = "";
    if (!stmt->IsNull(4)) type_name = stmt->GetString(4);
    if (!stmt->IsNull(5)) media_name = stmt->GetString(5);
    if (!stmt->IsNull(6)) shape_name = stmt->GetString(6);

    bool is_virtual = (bool)stmt->GetInt(7);

    TDatime begin_part = stmt->GetTimestamp(8);
    TDatime* end_part = 0x00;
    if (!stmt->IsNull(9)) end_part = new TDatime(stmt->GetTimestamp(9));

    if (!stmt->IsNull(10)) manufacturer_id = stmt->GetInt(10);

    TDatime* production_date = 0x00;
    if (!stmt->IsNull(11)) production_date = new TDatime(stmt->GetDatime(11));

    if (!stmt->IsNull(12)) description = stmt->GetString(12);

    delete stmt;

    return new MpdDbComponent(connMpdDb, component_id, detector_id, component_parent_id, strName,
                              shape_name, media_name, is_virtual, begin_part, end_part, type_name,
                              manufacturer_id, production_date, description);
}

// -----   Creating new component in database   -------------------------------
MpdDbComponent* MpdDbComponent::CreateComponent(char* detector_name, char* parent_component_name, char* component_name,
                                                char* shape_name, char* media_name, bool is_virtual,
                                                char* type_name, char* manufacturer_name, TDatime* production_date, char* description)
{
    if ((detector_name == 0x00) || (detector_name[0] == '\0')){
        cout<<"Detector name can't be empty"<<endl;
        return 0x00;
    }

    if ((component_name == 0x00) || (component_name[0] == '\0')){
        cout<<"Component name can't be empty"<<endl;
        return 0x00;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return 0x00;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select detector_id "
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

    int detector_id = stmt->GetInt(0), component_parent_id = -1, manufacturer_id = -1;

    delete stmt;

    if ((parent_component_name != 0x00) && (parent_component_name[0] != '\0')){
        stmt = mpd_db->Statement("select c.component_id "
                                 "from detector d join component c on d.detector_id = c.detector_id "
                                 "where lower(d.detector_name) = $1 and lower(c.component_name) = $2");
        stmt->SetString(0, detName);
        TString compName = parent_component_name;
        compName.ToLower();
        stmt->SetString(1, compName);

        // process statement
        if (stmt->Process()){
            // store result of statement in buffer
            stmt->StoreResult();

            // extract rows one after another
            if (!stmt->NextResultRow()){
                cout<<parent_component_name<<": parent component wasn't found!"<<endl;

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

        component_parent_id = stmt->GetInt(0);

        delete stmt;
    }

    if ((manufacturer_name != 0x00) && (manufacturer_name[0] != '\0')){
        stmt = mpd_db->Statement("select manufacturer_id "
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

                return 0x00;
            }
        }
        else{
            delete stmt;
            delete connMpdDb;

            return 0x00;
        }

        manufacturer_id = stmt->GetInt(0);

        delete stmt;
    }

    stmt = mpd_db->Statement("insert into component(detector_id, component_parent_id, component_name, type_name, media_name, "
                              "shape_name, isVirtual, begin_part, manufacturer_id, production_date, description) "
                              "values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)");

    stmt->SetInt(0, detector_id);

    if (component_parent_id != -1)
        stmt->SetInt(1, component_parent_id);
    else
        stmt->SetNull(1);

    stmt->SetString(2, component_name);

    if ((type_name != 0x00) && (type_name[0] != '\0'))
        stmt->SetString(3, type_name);
    else
        stmt->SetNull(3);

    if ((media_name != 0x00) && (media_name[0] != '\0'))
        stmt->SetString(4, media_name);
    else
        stmt->SetNull(4);

    if ((shape_name != 0x00) && (shape_name[0] != '\0'))
        stmt->SetString(5, shape_name);
    else
        stmt->SetNull(5);

    stmt->SetInt(6, (int)is_virtual);

    TDatime begin_part;
    stmt->SetTimestamp(7, begin_part);

    if (manufacturer_id != -1)
        stmt->SetInt(8, manufacturer_id);
    else
        stmt->SetNull(8);

    if (production_date != 0x00)
        stmt->SetDatime(9, *production_date);
    else
        stmt->SetNull(9);

    if ((description != 0x00) && (description[0] != '\0'))
        stmt->SetString(10, description);
    else
        stmt->SetNull(10);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        delete connMpdDb;

        return 0x00;
    }

    delete stmt;

    // get serial identificator for new component
    mpd_db->Statement("select c.component_id "
                      "from detector d join component c on d.detector_id = c.detector_id "
                      "where lower(d.detector_name) = $1 and c.component_name = $2");

    stmt->SetString(0, detName);
    stmt->SetString(1, component_name);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<component_name<<": new component wasn't found!"<<endl;

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

    int component_id = stmt->GetInt(0);

    delete stmt;

    return new MpdDbComponent(connMpdDb, component_id, detector_id, component_parent_id, component_name,
                              shape_name, media_name, is_virtual, begin_part, 0x00, type_name,
                              manufacturer_id, production_date, description);
}

/*int MpdDbComponent::DeleteComponent(char* detector_name, char* component_name)
{
    if ((detector_name == 0x00) || (detector_name[0] == '\0')){
        cout<<"Detector name can't be empty"<<endl;
        return -3;
    }
    if ((component_name == 0x00) || (component_name[0] == '\0')){
        cout<<"Component name can't be empty"<<endl;
        return -4;
    }

    MpdDbConnection* connMpdDb = MpdDbConnection::Open(MPD_DB);
    if (connMpdDb == 0x00) return -1;

    TSQLServer* mpd_db = connMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select c.component_id "
                                            "from detector d join component c on d.detector_id = c.detector_id "
                                            "where lower(d.detector_name) = $1 and lower(c.component_name) = $2");

    TString detName = detector_name;
    detName.ToLower();
    stmt->SetString(0, detName);
    TString compName = component_name;
    compName.ToLower();
    stmt->SetString(1, compName);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<component_name<<": component wasn't found!"<<endl;

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

    int component_id = stmt->GetInt(0);

    delete stmt;

    stmt = mpd_db->Statement("delete from component "
                             "where component_id = $1");

    stmt->SetInt(0, component_id);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        delete connMpdDb;

        return -2;
    }

    delete stmt;
    delete connMpdDb;

    return 0;
}*/

TString MpdDbComponent::GetDetectorName()
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return "";
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select detector_name "
                                            "from detector "
                                            "where detector_id = $1");
    stmt->SetInt(0, iDetectorId);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<iDetectorId<<": detector wasn't found!"<<endl;

            delete stmt;

            return "";
        }
    }
    else{
        delete stmt;

        return "";
    }

    TString detector_name = stmt->GetString(0);

    delete stmt;

    return detector_name;
}

TString MpdDbComponent::GetParentComponentName()
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return "";
    }

    if (iComponentParentId == -1) return "";

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select component_name "
                                            "from component "
                                            "where component_id = $1");
    stmt->SetInt(0, iComponentParentId);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<iComponentParentId<<": parent component wasn't found!"<<endl;

            delete stmt;

            return "";
        }
    }
    else{
        delete stmt;

        return "";
    }

    TString parent_component_name = stmt->GetString(0);

    delete stmt;

    return parent_component_name;
}

TString MpdDbComponent::GetManufacturerName()
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return "";
    }

    if (iManufacturerId == -1) return "";

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select manufacturer_name "
                                            "from manufacturer "
                                            "where manufacturer_id = $1");
    stmt->SetInt(0, iManufacturerId);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<iManufacturerId<<": manufacturer wasn't found!"<<endl;

            delete stmt;

            return "";
        }
    }
    else{
        delete stmt;

        return "";
    }

    TString manufacturer_name = stmt->GetString(0);

    delete stmt;

    return manufacturer_name;
}

void MpdDbComponent::UpdateFromDB()
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("select component_id, detector_id, component_parent_id, component_name, type_name, media_name, "
                                            "shape_name, isVirtual, begin_part, end_part, manufacturer_id, production_date, description "
                                            "from component "
                                            "where component_id = $1");
    stmt->SetInt(0, iComponentId);

    // process statement
    if (stmt->Process()){
        // store result of statement in buffer
        stmt->StoreResult();

        // extract rows one after another
        if (!stmt->NextResultRow()){
            cout<<"Component (with old id) wasn't found!"<<endl;
            delete stmt;
            return;
        }
    }
    else{
        delete stmt;
        return;
    }

    // получение параметров
    iDetectorId = stmt->GetInt(1);

    if (!stmt->IsNull(2)) iComponentParentId = stmt->GetInt(2);
    else{
        iComponentParentId = -1;
    }

    strComponentName = stmt->GetString(3);

    if (!stmt->IsNull(4)) strTypeName = stmt->GetString(4);
    else strTypeName = "";
    if (!stmt->IsNull(5)) strMediaName = stmt->GetString(5);
    else strMediaName = "";
    if (!stmt->IsNull(6)) strShapeName = stmt->GetString(6);
    else strShapeName = "";

    isVirtual = (bool)stmt->GetInt(7);

    tsBeginPart = stmt->GetTimestamp(8);

    if (tsEndPart)
        delete tsEndPart;
    if (!stmt->IsNull(9)) tsEndPart = new TDatime(stmt->GetTimestamp(9));
    else tsEndPart = 0x00;

    if (!stmt->IsNull(10)) iManufacturerId = stmt->GetInt(10);
    else iManufacturerId = -1;

    if (dateProductionDate)
        delete dateProductionDate;
    if (!stmt->IsNull(11)) dateProductionDate = new TDatime(stmt->GetDatime(11));
    else dateProductionDate = 0x00;

    if (!stmt->IsNull(12)) strComponentDescription = stmt->GetString(12);
    else strComponentDescription = "";

    delete stmt;

    return;
}

int MpdDbComponent::SetDetectorName(char* detector_name)
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

    TSQLStatement* stmt = mpd_db->Statement("select detector_id "
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
            cout<<detector_name<<": detector wasn't found!"<<endl;

            delete stmt;
            return -10;
        }
    }
    else{
        delete stmt;
        return -11;
    }

    int detector_id = stmt->GetInt(0);

    delete stmt;

    stmt = mpd_db->Statement("update component "
                             "set detector_id = $1 "
                             "where component_id = $2");
    stmt->SetInt(0, detector_id);
    stmt->SetInt(1, iComponentId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    iDetectorId = detector_id;

    delete stmt;

    return 0;
}

int MpdDbComponent::SetParentComponentName(char* parent_component_name)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    bool isNull = false;
    int component_id = -1;
    if ((parent_component_name != 0x00) && (parent_component_name[0] != '\0')){
        TSQLStatement* stmt = mpd_db->Statement("select c.component_id "
                                                "from detector d join component c on d.detector_id = c.detector_id "
                                                "where d.detector_id = $1 and lower(c.component_name) = $2");
        stmt->SetInt(0, iDetectorId);
        TString compName = parent_component_name;
        compName.ToLower();
        stmt->SetString(1, compName);

        // process statement
        if (stmt->Process()){
            // store result of statement in buffer
            stmt->StoreResult();

            // extract rows one after another
            if (!stmt->NextResultRow()){
                cout<<parent_component_name<<": component wasn't found!"<<endl;

                delete stmt;
                return -10;
            }
        }
        else{
            delete stmt;
            return -11;
        }

        component_id = stmt->GetInt(0);

        delete stmt;
    }
    else isNull = true;

    TSQLStatement* stmt = mpd_db->Statement("update component "
                                            "set component_parent_id = $1 "
                                            "where component_id = $2");
    if (isNull) stmt->SetNull(0);
    else stmt->SetInt(0, component_id);
    stmt->SetInt(1, iComponentId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    iComponentParentId = component_id;

    delete stmt;

    return 0;
}

int MpdDbComponent::SetName(char* component_name)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    if ((component_name == 0x00) || (component_name[0] == '\0')){
        cout<<"Component name can't be empty"<<endl;
        return -3;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update component "
                                            "set component_name = $1 "
                                            "where component_id = $2");
    stmt->SetString(0, component_name);
    stmt->SetInt(1, iComponentId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    strComponentName = component_name;

    delete stmt;

    return 0;
}

int MpdDbComponent::SetTypeName(char* type_name)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update component "
                                            "set type_name = $1 "
                                            "where component_id = $2");
    bool isNull = false;
    if ((type_name != 0x00) && (type_name[0] != '\0'))
        stmt->SetString(0, type_name);
    else
    {
        stmt->SetNull(0);
        isNull = true;
    }
    stmt->SetInt(1, iComponentId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    if (isNull) strTypeName = "";
    else strTypeName = type_name;

    delete stmt;

    return 0;
}

int MpdDbComponent::SetMediaName(char* media_name)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update component "
                                            "set media_name = $1 "
                                            "where component_id = $2");
    bool isNull = false;
    if ((media_name != 0x00) && (media_name[0] != '\0'))
        stmt->SetString(0, media_name);
    else
    {
        stmt->SetNull(0);
        isNull = true;
    }
    stmt->SetInt(1, iComponentId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    if (isNull) strMediaName = "";
    else strMediaName = media_name;

    delete stmt;

    return 0;
}

int MpdDbComponent::SetShapeName(char* shape_name)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update component "
                                            "set shape_name = $1 "
                                            "where component_id = $2");
    bool isNull = false;
    if ((shape_name != 0x00) && (shape_name[0] != '\0'))
        stmt->SetString(0, shape_name);
    else
    {
        stmt->SetNull(0);
        isNull = true;
    }
    stmt->SetInt(1, iComponentId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    if (isNull) strShapeName = "";
    else strShapeName = shape_name;

    delete stmt;

    return 0;
}

int MpdDbComponent::SetVirtual(bool is_virtual)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update component "
                                            "set is_virtual = $1 "
                                            "where component_id = $2");
    stmt->SetInt(0, is_virtual);
    stmt->SetInt(1, iComponentId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    isVirtual = is_virtual;

    delete stmt;

    return 0;
}

int MpdDbComponent::ChangeBeginPart(TDatime begin_part)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update component "
                                            "set begin_part = $1 "
                                            "where component_id = $2");
    stmt->SetTimestamp(0, begin_part);
    stmt->SetInt(1, iComponentId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    tsBeginPart = begin_part;

    delete stmt;

    return 0;
}

int MpdDbComponent::SetEndPart(TDatime* end_part)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update component "
                                            "set end_part = $1 "
                                            "where component_id = $2");
    if (end_part != 0x00)
        stmt->SetTimestamp(0, *end_part);
    else
        stmt->SetNull(0);
    stmt->SetInt(1, iComponentId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    if (tsEndPart)
        delete tsEndPart;

    tsEndPart = end_part;

    delete stmt;

    return 0;
}

int MpdDbComponent::ClosePart()
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    if (tsEndPart){
        cout<<"This component is not detector's part"<<endl;
        return -3;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TDatime* now = new TDatime();

    TSQLStatement* stmt = mpd_db->Statement("update component "
                                            "set end_part = $1 "
                                            "where component_id = $2");
    stmt->SetTimestamp(0, *now);
    stmt->SetInt(1, iComponentId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        delete now;
        return -2;
    }

    tsEndPart = now;

    delete stmt;

    return 0;
}

int MpdDbComponent::SetManufacturerName(char* manufacturer_name)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    bool isNull = false;
    int manufacturer_id = -1;
    if ((manufacturer_name != 0x00) && (manufacturer_name[0] != '\0')){
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

    TSQLStatement* stmt = mpd_db->Statement("update component "
                                            "set manufacturer_id = $1 "
                                            "where component_id = $2");
    if (isNull) stmt->SetNull(0);
    else stmt->SetInt(0, manufacturer_id);
    stmt->SetInt(1, iComponentId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    iManufacturerId = manufacturer_id;

    delete stmt;

    return 0;
}

int MpdDbComponent::SetProductionDate(TDatime* production_date)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update component "
                                            "set production_date = $1 "
                                            "where component_id = $2");
    if (production_date != 0x00)
        stmt->SetDatime(0, *production_date);
    else
        stmt->SetNull(0);
    stmt->SetInt(1, iComponentId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    if (dateProductionDate)
        delete dateProductionDate;

    dateProductionDate = production_date;

    delete stmt;

    return 0;
}

int MpdDbComponent::SetDescription(char* description)
{
    if (!connectionMpdDb){
        cout<<"Connection object is null"<<endl;
        return -1;
    }

    TSQLServer* mpd_db = connectionMpdDb->GetSQLServer();

    TSQLStatement* stmt = mpd_db->Statement("update component "
                                            "set description = $1 "
                                            "where component_id = $2");
    bool isNull = false;
    if ((description != 0x00) && (description[0] != '\0'))
        stmt->SetString(0, description);
    else
    {
        stmt->SetNull(0);
        isNull = true;
    }
    stmt->SetInt(1, iComponentId);

    // process statement
    if (!stmt->Process()){
        delete stmt;
        return -2;
    }

    if (isNull) strComponentDescription = "";
    else strComponentDescription = description;

    delete stmt;

    return 0;
}

// -------------------------------------------------------------------
MpdDbComponent::~MpdDbComponent()
{
    if (connectionMpdDb)
        delete connectionMpdDb;

    if (tsEndPart)
        delete tsEndPart;

    if (dateProductionDate)
        delete dateProductionDate;
}

// -------------------------------------------------------------------
ClassImp(MpdDbComponent)
