// ----------------------------------------------------------------------
//                    MpdDbRunParameter cxx file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

#include "TSQLServer.h" 
#include "TSQLStatement.h" 

#include "MpdDbRunParameter.h" 

#include <iostream>
using namespace std;

// -----   Constructor with database connection   -----------------------
MpdDbRunParameter::MpdDbRunParameter(MpdDbConnection* connUniDb, int run_number, TString component_name, int parameter_id, int value_id)
{
	connectionUniDb = connUniDb;

	i_run_number = run_number;
	str_component_name = component_name;
	i_parameter_id = parameter_id;
	i_value_id = value_id;
}

// -----   Destructor   -------------------------------------------------
MpdDbRunParameter::~MpdDbRunParameter()
{
	if (connectionUniDb)
		delete connectionUniDb;
}

// -----   Creating new record in class table ---------------------------
MpdDbRunParameter* MpdDbRunParameter::CreateRunParameter(int run_number, TString component_name, int parameter_id, int value_id)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"insert into run_parameter(run_number, component_name, parameter_id, value_id) "
		"values ($1, $2, $3, $4)");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, run_number);
	stmt->SetString(1, component_name);
	stmt->SetInt(2, parameter_id);
	stmt->SetInt(3, value_id);

	// inserting new record to DB
	if (!stmt->Process())
	{
		cout<<"Error: inserting new record to DB has been failed"<<endl;
		delete stmt;
		delete connUniDb;
		return 0x00;
	}

	delete stmt;

	return new MpdDbRunParameter(connUniDb, run_number, component_name, parameter_id, value_id);
}

// -----   Get table record from database ---------------------------
MpdDbRunParameter* MpdDbRunParameter::GetRunParameter(int run_number)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select run_number, component_name, parameter_id, value_id "
		"from run_parameter "
		"where run_number = $1");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, run_number);

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

	int tmp_run_number;
	tmp_run_number = stmt->GetInt(0);
	TString tmp_component_name;
	tmp_component_name = stmt->GetString(1);
	int tmp_parameter_id;
	tmp_parameter_id = stmt->GetInt(2);
	int tmp_value_id;
	tmp_value_id = stmt->GetInt(3);

	delete stmt;

	return new MpdDbRunParameter(connUniDb, tmp_run_number, tmp_component_name, tmp_parameter_id, tmp_value_id);
}

// -----   Delete record from class table ---------------------------
int MpdDbRunParameter::DeleteRunParameter(int run_number)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"delete from run_parameter "
		"where run_number = $1");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, run_number);

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
int MpdDbRunParameter::PrintAll()
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select run_number, component_name, parameter_id, value_id "
		"from run_parameter");
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
		int tmp_run_number;
		tmp_run_number = stmt->GetInt(0);
		TString tmp_component_name;
		tmp_component_name = stmt->GetString(1);
		int tmp_parameter_id;
		tmp_parameter_id = stmt->GetInt(2);
		int tmp_value_id;
		tmp_value_id = stmt->GetInt(3);

		cout<<"Table 'run_parameter'";
		cout<<". run_number: "<<tmp_run_number<<". component_name: "<<tmp_component_name<<". parameter_id: "<<tmp_parameter_id<<". value_id: "<<tmp_value_id<<endl;
	}

	delete stmt;
	delete connUniDb;

	return 0;
}


// Setters functions
int MpdDbRunParameter::SetRunNumber(int run_number)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update run_parameter "
		"set run_number = $1 "
		"where run_number = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, run_number);
	stmt->SetInt(1, i_run_number);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	i_run_number = run_number;

	delete stmt;
	return 0;
}

int MpdDbRunParameter::SetComponentName(TString component_name)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update run_parameter "
		"set component_name = $1 "
		"where run_number = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetString(0, component_name);
	stmt->SetInt(1, i_run_number);

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

int MpdDbRunParameter::SetParameterId(int parameter_id)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update run_parameter "
		"set parameter_id = $1 "
		"where run_number = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, parameter_id);
	stmt->SetInt(1, i_run_number);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	i_parameter_id = parameter_id;

	delete stmt;
	return 0;
}

int MpdDbRunParameter::SetValueId(int value_id)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update run_parameter "
		"set value_id = $1 "
		"where run_number = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, value_id);
	stmt->SetInt(1, i_run_number);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	i_value_id = value_id;

	delete stmt;
	return 0;
}

// -----   Print current record ---------------------------------------
void MpdDbRunParameter::Print()
{
	cout<<"Table 'run_parameter'";
	cout<<". run_number: "<<i_run_number<<". component_name: "<<str_component_name<<". parameter_id: "<<i_parameter_id<<". value_id: "<<i_value_id<<endl;

	return;
}

// -------------------------------------------------------------------
ClassImp(MpdDbRunParameter);
