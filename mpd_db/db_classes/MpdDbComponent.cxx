// ----------------------------------------------------------------------
//                    MpdDbComponent cxx file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

#include "TSQLServer.h" 
#include "TSQLStatement.h" 

#include "MpdDbComponent.h" 

#include <iostream>
using namespace std;

// -----   Constructor with database connection   -----------------------
MpdDbComponent::MpdDbComponent(MpdDbConnection* connUniDb, TString component_name, TString* manufacturer_name, TString* responsible_person, TString* description)
{
	connectionUniDb = connUniDb;

	str_component_name = component_name;
	str_manufacturer_name = manufacturer_name;
	str_responsible_person = responsible_person;
	str_description = description;
}

// -----   Destructor   -------------------------------------------------
MpdDbComponent::~MpdDbComponent()
{
	if (connectionUniDb)
		delete connectionUniDb;
	if (str_manufacturer_name)
		delete str_manufacturer_name;
	if (str_responsible_person)
		delete str_responsible_person;
	if (str_description)
		delete str_description;
}

// -----   Creating new record in class table ---------------------------
MpdDbComponent* MpdDbComponent::CreateComponent(TString component_name, TString* manufacturer_name, TString* responsible_person, TString* description)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"insert into component(component_name, manufacturer_name, responsible_person, description) "
		"values ($1, $2, $3, $4)");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetString(0, component_name);
	if (manufacturer_name == NULL)
		stmt->SetNull(1);
	else
		stmt->SetString(1, *manufacturer_name);
	if (responsible_person == NULL)
		stmt->SetNull(2);
	else
		stmt->SetString(2, *responsible_person);
	if (description == NULL)
		stmt->SetNull(3);
	else
		stmt->SetString(3, *description);

	// inserting new record to DB
	if (!stmt->Process())
	{
		cout<<"Error: inserting new record to DB has been failed"<<endl;
		delete stmt;
		delete connUniDb;
		return 0x00;
	}

	delete stmt;

	return new MpdDbComponent(connUniDb, component_name, manufacturer_name, responsible_person, description);
}

// -----   Get table record from database ---------------------------
MpdDbComponent* MpdDbComponent::GetComponent(TString component_name)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select component_name, manufacturer_name, responsible_person, description "
		"from component "
		"where lower(component_name) = lower($1)");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetString(0, component_name);

	// get table record from DB
	if (!stmt->Process())
	{
		cout<<"Error: getting record from DB has been failed"<<endl;

		delete stmt;
		delete connUniDb;
		return 0x00;
	}

	// store result of statement in buffer
	stmt->StoreResult();

	// extract row
	if (!stmt->NextResultRow())
	{
		cout<<"Error: table record wasn't found"<<endl;

		delete stmt;
		delete connUniDb;
		return 0x00;
	}

	TString tmp_component_name;
	tmp_component_name = stmt->GetString(0);
	TString* tmp_manufacturer_name;
	if (stmt->IsNull(1)) tmp_manufacturer_name = NULL;
	else
		tmp_manufacturer_name = new TString(stmt->GetString(1));
	TString* tmp_responsible_person;
	if (stmt->IsNull(2)) tmp_responsible_person = NULL;
	else
		tmp_responsible_person = new TString(stmt->GetString(2));
	TString* tmp_description;
	if (stmt->IsNull(3)) tmp_description = NULL;
	else
		tmp_description = new TString(stmt->GetString(3));

	delete stmt;

	return new MpdDbComponent(connUniDb, tmp_component_name, tmp_manufacturer_name, tmp_responsible_person, tmp_description);
}

// -----   Delete record from class table ---------------------------
int MpdDbComponent::DeleteComponent(TString component_name)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"delete from component "
		"where lower(component_name) = lower($1)");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetString(0, component_name);

	// delete table record from DB
	if (!stmt->Process())
	{
		cout<<"Error: deleting record from DB has been failed"<<endl;

		delete stmt;
		delete connUniDb;
		return -1;
	}

	delete stmt;
	delete connUniDb;
	return 0;
}

// -----   Print all table records ---------------------------------
int MpdDbComponent::PrintAll()
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select component_name, manufacturer_name, responsible_person, description "
		"from component");
	TSQLStatement* stmt = uni_db->Statement(sql);

	// get table record from DB
	if (!stmt->Process())
	{
		cout<<"Error: getting all records from DB has been failed"<<endl;

		delete stmt;
		delete connUniDb;
		return -1;
	}

	// store result of statement in buffer
	stmt->StoreResult();

	// print rows
	while (stmt->NextResultRow())
	{
		TString tmp_component_name;
		tmp_component_name = stmt->GetString(0);
		TString* tmp_manufacturer_name;
		if (stmt->IsNull(1)) tmp_manufacturer_name = NULL;
		else
			tmp_manufacturer_name = new TString(stmt->GetString(1));
		TString* tmp_responsible_person;
		if (stmt->IsNull(2)) tmp_responsible_person = NULL;
		else
			tmp_responsible_person = new TString(stmt->GetString(2));
		TString* tmp_description;
		if (stmt->IsNull(3)) tmp_description = NULL;
		else
			tmp_description = new TString(stmt->GetString(3));

		cout<<"Table 'component'";
		cout<<". component_name: "<<tmp_component_name<<". manufacturer_name: "<<(*tmp_manufacturer_name)<<". responsible_person: "<<(*tmp_responsible_person)<<". description: "<<(*tmp_description)<<endl;
	}

	delete stmt;
	delete connUniDb;

	return 0;
}


// Setters functions
int MpdDbComponent::SetComponentName(TString component_name)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update component "
		"set component_name = $1 "
		"where component_name = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetString(0, component_name);
	stmt->SetString(1, str_component_name);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	str_component_name = component_name;

	delete stmt;
	return 0;
}

int MpdDbComponent::SetManufacturerName(TString* manufacturer_name)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update component "
		"set manufacturer_name = $1 "
		"where component_name = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	if (manufacturer_name == NULL)
		stmt->SetNull(0);
	else
		stmt->SetString(0, *manufacturer_name);
	stmt->SetString(1, str_component_name);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	if (manufacturer_name)
		delete manufacturer_name;
	str_manufacturer_name = manufacturer_name;

	delete stmt;
	return 0;
}

int MpdDbComponent::SetResponsiblePerson(TString* responsible_person)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update component "
		"set responsible_person = $1 "
		"where component_name = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	if (responsible_person == NULL)
		stmt->SetNull(0);
	else
		stmt->SetString(0, *responsible_person);
	stmt->SetString(1, str_component_name);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	if (responsible_person)
		delete responsible_person;
	str_responsible_person = responsible_person;

	delete stmt;
	return 0;
}

int MpdDbComponent::SetDescription(TString* description)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update component "
		"set description = $1 "
		"where component_name = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	if (description == NULL)
		stmt->SetNull(0);
	else
		stmt->SetString(0, *description);
	stmt->SetString(1, str_component_name);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	if (description)
		delete description;
	str_description = description;

	delete stmt;
	return 0;
}

// -----   Print current record ---------------------------------------
void MpdDbComponent::Print()
{
	cout<<"Table 'component'";
	cout<<". component_name: "<<str_component_name<<". manufacturer_name: "<<(*str_manufacturer_name)<<". responsible_person: "<<(*str_responsible_person)<<". description: "<<(*str_description)<<endl;

	return;
}

// -------------------------------------------------------------------
ClassImp(MpdDbComponent);
