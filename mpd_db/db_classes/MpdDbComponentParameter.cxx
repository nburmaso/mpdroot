// ----------------------------------------------------------------------
//                    MpdDbComponentParameter cxx file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

#include "TSQLServer.h" 
#include "TSQLStatement.h" 

#include "MpdDbComponentParameter.h" 

#include <iostream>
using namespace std;

// -----   Constructor with database connection   -----------------------
MpdDbComponentParameter::MpdDbComponentParameter(MpdDbConnection* connUniDb, int parameter_id, TString parameter_name, int parameter_type)
{
	connectionUniDb = connUniDb;

	i_parameter_id = parameter_id;
	str_parameter_name = parameter_name;
	i_parameter_type = parameter_type;
}

// -----   Destructor   -------------------------------------------------
MpdDbComponentParameter::~MpdDbComponentParameter()
{
	if (connectionUniDb)
		delete connectionUniDb;
}

// -----   Creating new record in class table ---------------------------
MpdDbComponentParameter* MpdDbComponentParameter::CreateComponentParameter(TString parameter_name, int parameter_type)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"insert into component_parameter(parameter_name, parameter_type) "
		"values ($1, $2)");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetString(0, parameter_name);
	stmt->SetInt(1, parameter_type);

	// inserting new record to DB
	if (!stmt->Process())
	{
		cout<<"Error: inserting new record to DB has been failed"<<endl;
		delete stmt;
		delete connUniDb;
		return 0x00;
	}

	delete stmt;

	// getting last inserted ID
	int parameter_id;
	TSQLStatement* stmt_last = uni_db->Statement("SELECT currval(pg_get_serial_sequence('component_parameter','parameter_id'))");

	// process getting last id
	if (stmt_last->Process())
	{
		// store result of statement in buffer
		stmt_last->StoreResult();

		// if there is no last id then exit with error
		if (!stmt_last->NextResultRow())
		{
			cout<<"Error: no last ID in DB!"<<endl;
			delete stmt_last;
			return 0x00;
		}
		else
		{
			parameter_id = stmt_last->GetInt(0);
			delete stmt_last;
		}
	}
	else
	{
		cout<<"Error: getting last ID has been failed!"<<endl;
		delete stmt_last;
		return 0x00;
	}

	return new MpdDbComponentParameter(connUniDb, parameter_id, parameter_name, parameter_type);
}

// -----   Get table record from database ---------------------------
MpdDbComponentParameter* MpdDbComponentParameter::GetComponentParameter(int parameter_id)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select parameter_id, parameter_name, parameter_type "
		"from component_parameter "
		"where parameter_id = $1");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, parameter_id);

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

	int tmp_parameter_id;
	tmp_parameter_id = stmt->GetInt(0);
	TString tmp_parameter_name;
	tmp_parameter_name = stmt->GetString(1);
	int tmp_parameter_type;
	tmp_parameter_type = stmt->GetInt(2);

	delete stmt;

	return new MpdDbComponentParameter(connUniDb, tmp_parameter_id, tmp_parameter_name, tmp_parameter_type);
}

// -----   Get table record from database for unique key--------------
MpdDbComponentParameter* MpdDbComponentParameter::GetComponentParameter(TString parameter_name)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select parameter_id, parameter_name, parameter_type "
		"from component_parameter "
		"where lower(parameter_name) = lower($1)");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetString(0, parameter_name);

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

	int tmp_parameter_id;
	tmp_parameter_id = stmt->GetInt(0);
	TString tmp_parameter_name;
	tmp_parameter_name = stmt->GetString(1);
	int tmp_parameter_type;
	tmp_parameter_type = stmt->GetInt(2);
	delete stmt;

	return new MpdDbComponentParameter(connUniDb, tmp_parameter_id, tmp_parameter_name, tmp_parameter_type);
}

// -----   Delete record from class table ---------------------------
int MpdDbComponentParameter::DeleteComponentParameter(int parameter_id)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"delete from component_parameter "
		"where parameter_id = $1");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, parameter_id);

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

// -----   Delete table record from database for unique key--------------
int MpdDbComponentParameter::DeleteComponentParameter(TString parameter_name)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"delete from component_parameter "
		"where lower(parameter_name) = lower($1)");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetString(0, parameter_name);

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
int MpdDbComponentParameter::PrintAll()
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select parameter_id, parameter_name, parameter_type "
		"from component_parameter");
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
		int tmp_parameter_id;
		tmp_parameter_id = stmt->GetInt(0);
		TString tmp_parameter_name;
		tmp_parameter_name = stmt->GetString(1);
		int tmp_parameter_type;
		tmp_parameter_type = stmt->GetInt(2);

		cout<<"Table 'component_parameter'";
		cout<<". parameter_id: "<<tmp_parameter_id<<". parameter_name: "<<tmp_parameter_name<<". parameter_type: "<<tmp_parameter_type<<endl;
	}

	delete stmt;
	delete connUniDb;

	return 0;
}


// Setters functions
int MpdDbComponentParameter::SetParameterName(TString parameter_name)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update component_parameter "
		"set parameter_name = $1 "
		"where parameter_id = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetString(0, parameter_name);
	stmt->SetInt(1, i_parameter_id);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	str_parameter_name = parameter_name;

	delete stmt;
	return 0;
}

int MpdDbComponentParameter::SetParameterType(int parameter_type)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update component_parameter "
		"set parameter_type = $1 "
		"where parameter_id = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, parameter_type);
	stmt->SetInt(1, i_parameter_id);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	i_parameter_type = parameter_type;

	delete stmt;
	return 0;
}

// -----   Print current record ---------------------------------------
void MpdDbComponentParameter::Print()
{
	cout<<"Table 'component_parameter'";
	cout<<". parameter_id: "<<i_parameter_id<<". parameter_name: "<<str_parameter_name<<". parameter_type: "<<i_parameter_type<<endl;

	return;
}

// -------------------------------------------------------------------
ClassImp(MpdDbComponentParameter);
