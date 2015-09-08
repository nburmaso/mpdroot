// ----------------------------------------------------------------------
//                    MpdDbDoubleValue cxx file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

#include "TSQLServer.h" 
#include "TSQLStatement.h" 

#include "MpdDbDoubleValue.h" 

#include <iostream>
using namespace std;

// -----   Constructor with database connection   -----------------------
MpdDbDoubleValue::MpdDbDoubleValue(MpdDbConnection* connUniDb, int value_id, double par_value)
{
	connectionUniDb = connUniDb;

	i_value_id = value_id;
	d_par_value = par_value;
}

// -----   Destructor   -------------------------------------------------
MpdDbDoubleValue::~MpdDbDoubleValue()
{
	if (connectionUniDb)
		delete connectionUniDb;
}

// -----   Creating new record in class table ---------------------------
MpdDbDoubleValue* MpdDbDoubleValue::CreateDoubleValue(int value_id, double par_value)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"insert into double_value(value_id, par_value) "
		"values ($1, $2)");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, value_id);
	stmt->SetDouble(1, par_value);

	// inserting new record to DB
	if (!stmt->Process())
	{
		cout<<"Error: inserting new record to DB has been failed"<<endl;
		delete stmt;
		delete connUniDb;
		return 0x00;
	}

	delete stmt;

	return new MpdDbDoubleValue(connUniDb, value_id, par_value);
}

// -----   Get table record from database ---------------------------
MpdDbDoubleValue* MpdDbDoubleValue::GetDoubleValue(int value_id)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select value_id, par_value "
		"from double_value "
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
	double tmp_par_value;
	tmp_par_value = stmt->GetDouble(1);

	delete stmt;

	return new MpdDbDoubleValue(connUniDb, tmp_value_id, tmp_par_value);
}

// -----   Delete record from class table ---------------------------
int MpdDbDoubleValue::DeleteDoubleValue(int value_id)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"delete from double_value "
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
int MpdDbDoubleValue::PrintAll()
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select value_id, par_value "
		"from double_value");
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
		double tmp_par_value;
		tmp_par_value = stmt->GetDouble(1);

		cout<<"Table 'double_value'";
		cout<<". value_id: "<<tmp_value_id<<". par_value: "<<tmp_par_value<<endl;
	}

	delete stmt;
	delete connUniDb;

	return 0;
}


// Setters functions
int MpdDbDoubleValue::SetValueId(int value_id)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update double_value "
		"set value_id = $1 "
		"where value_id = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, value_id);
	stmt->SetInt(1, i_value_id);

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

int MpdDbDoubleValue::SetParValue(double par_value)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update double_value "
		"set par_value = $1 "
		"where value_id = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetDouble(0, par_value);
	stmt->SetInt(1, i_value_id);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	d_par_value = par_value;

	delete stmt;
	return 0;
}

// -----   Print current record ---------------------------------------
void MpdDbDoubleValue::Print()
{
	cout<<"Table 'double_value'";
	cout<<". value_id: "<<i_value_id<<". par_value: "<<d_par_value<<endl;

	return;
}

// -------------------------------------------------------------------
ClassImp(MpdDbDoubleValue);
