// ----------------------------------------------------------------------
//                    MpdDbSessionComponent cxx file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

#include "TSQLServer.h" 
#include "TSQLStatement.h" 

#include "MpdDbSessionComponent.h" 

#include <iostream>
using namespace std;

// -----   Constructor with database connection   -----------------------
MpdDbSessionComponent::MpdDbSessionComponent(MpdDbConnection* connUniDb, int session_number, TString component_name, int* map_id)
{
	connectionUniDb = connUniDb;

	i_session_number = session_number;
	str_component_name = component_name;
	i_map_id = map_id;
}

// -----   Destructor   -------------------------------------------------
MpdDbSessionComponent::~MpdDbSessionComponent()
{
	if (connectionUniDb)
		delete connectionUniDb;
	if (i_map_id)
		delete i_map_id;
}

// -----   Creating new record in class table ---------------------------
MpdDbSessionComponent* MpdDbSessionComponent::CreateSessionComponent(int session_number, TString component_name, int* map_id)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"insert into session_component(session_number, component_name, map_id) "
		"values ($1, $2, $3)");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, session_number);
	stmt->SetString(1, component_name);
	if (map_id == NULL)
		stmt->SetNull(2);
	else
		stmt->SetInt(2, *map_id);

	// inserting new record to DB
	if (!stmt->Process())
	{
		cout<<"Error: inserting new record to DB has been failed"<<endl;
		delete stmt;
		delete connUniDb;
		return 0x00;
	}

	delete stmt;

	return new MpdDbSessionComponent(connUniDb, session_number, component_name, map_id);
}

// -----   Get table record from database ---------------------------
MpdDbSessionComponent* MpdDbSessionComponent::GetSessionComponent(int session_number)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select session_number, component_name, map_id "
		"from session_component "
		"where session_number = $1");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, session_number);

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

	int tmp_session_number;
	tmp_session_number = stmt->GetInt(0);
	TString tmp_component_name;
	tmp_component_name = stmt->GetString(1);
	int* tmp_map_id;
	if (stmt->IsNull(2)) tmp_map_id = NULL;
	else
		tmp_map_id = new int(stmt->GetInt(2));

	delete stmt;

	return new MpdDbSessionComponent(connUniDb, tmp_session_number, tmp_component_name, tmp_map_id);
}

// -----   Delete record from class table ---------------------------
int MpdDbSessionComponent::DeleteSessionComponent(int session_number)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"delete from session_component "
		"where session_number = $1");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, session_number);

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
int MpdDbSessionComponent::PrintAll()
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select session_number, component_name, map_id "
		"from session_component");
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
		int tmp_session_number;
		tmp_session_number = stmt->GetInt(0);
		TString tmp_component_name;
		tmp_component_name = stmt->GetString(1);
		int* tmp_map_id;
		if (stmt->IsNull(2)) tmp_map_id = NULL;
		else
			tmp_map_id = new int(stmt->GetInt(2));

		cout<<"Table 'session_component'";
		cout<<". session_number: "<<tmp_session_number<<". component_name: "<<tmp_component_name<<". map_id: "<<(*tmp_map_id)<<endl;
	}

	delete stmt;
	delete connUniDb;

	return 0;
}


// Setters functions
int MpdDbSessionComponent::SetSessionNumber(int session_number)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update session_component "
		"set session_number = $1 "
		"where session_number = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, session_number);
	stmt->SetInt(1, i_session_number);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	i_session_number = session_number;

	delete stmt;
	return 0;
}

int MpdDbSessionComponent::SetComponentName(TString component_name)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update session_component "
		"set component_name = $1 "
		"where session_number = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetString(0, component_name);
	stmt->SetInt(1, i_session_number);

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

int MpdDbSessionComponent::SetMapId(int* map_id)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update session_component "
		"set map_id = $1 "
		"where session_number = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	if (map_id == NULL)
		stmt->SetNull(0);
	else
		stmt->SetInt(0, *map_id);
	stmt->SetInt(1, i_session_number);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	if (map_id)
		delete map_id;
	i_map_id = map_id;

	delete stmt;
	return 0;
}

// -----   Print current record ---------------------------------------
void MpdDbSessionComponent::Print()
{
	cout<<"Table 'session_component'";
	cout<<". session_number: "<<i_session_number<<". component_name: "<<str_component_name<<". map_id: "<<(*i_map_id)<<endl;

	return;
}

// -------------------------------------------------------------------
ClassImp(MpdDbSessionComponent);
