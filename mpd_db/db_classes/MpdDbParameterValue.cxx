// ----------------------------------------------------------------------
//                    MpdDbParameterValue cxx file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

#include "TSQLServer.h" 
#include "TSQLStatement.h" 

#include "MpdDbParameterValue.h" 

#include <iostream>
using namespace std;

// -----   Constructor with database connection   -----------------------
MpdDbParameterValue::MpdDbParameterValue(MpdDbConnection* connUniDb, int value_id)
{
	connectionUniDb = connUniDb;

	i_value_id = value_id;
}

// -----   Destructor   -------------------------------------------------
MpdDbParameterValue::~MpdDbParameterValue()
{
	if (connectionUniDb)
		delete connectionUniDb;
}

// -----   Creating new record in class table ---------------------------
MpdDbParameterValue* MpdDbParameterValue::CreateParameterValue()
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"insert into parameter_value() "
		"values ()");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();

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
	int value_id;
	TSQLStatement* stmt_last = uni_db->Statement("SELECT currval(pg_get_serial_sequence('parameter_value','value_id'))");

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
			value_id = stmt_last->GetInt(0);
			delete stmt_last;
		}
	}
	else
	{
		cout<<"Error: getting last ID has been failed!"<<endl;
		delete stmt_last;
		return 0x00;
	}

	return new MpdDbParameterValue(connUniDb, value_id);
}

// -----   Get table record from database ---------------------------
MpdDbParameterValue* MpdDbParameterValue::GetParameterValue(int value_id)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select value_id "
		"from parameter_value "
		"where value_id = $1");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, value_id);

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

	int tmp_value_id;
	tmp_value_id = stmt->GetInt(0);

	delete stmt;

	return new MpdDbParameterValue(connUniDb, tmp_value_id);
}

// -----   Delete record from class table ---------------------------
int MpdDbParameterValue::DeleteParameterValue(int value_id)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"delete from parameter_value "
		"where value_id = $1");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, value_id);

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
int MpdDbParameterValue::PrintAll()
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select value_id "
		"from parameter_value");
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
		int tmp_value_id;
		tmp_value_id = stmt->GetInt(0);

		cout<<"Table 'parameter_value'";
		cout<<". value_id: "<<tmp_value_id<<endl;
	}

	delete stmt;
	delete connUniDb;

	return 0;
}


// Setters functions
// -----   Print current record ---------------------------------------
void MpdDbParameterValue::Print()
{
	cout<<"Table 'parameter_value'";
	cout<<". value_id: "<<i_value_id<<endl;

	return;
}

// -------------------------------------------------------------------
ClassImp(MpdDbParameterValue);
