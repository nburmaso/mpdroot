// ----------------------------------------------------------------------
//                    MpdDbExperimentSession cxx file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

#include "TSQLServer.h" 
#include "TSQLStatement.h" 

#include "MpdDbExperimentSession.h" 

#include <iostream>
using namespace std;

// -----   Constructor with database connection   -----------------------
MpdDbExperimentSession::MpdDbExperimentSession(MpdDbConnection* connUniDb, int session_number, TDatime start_date, TDatime* end_date)
{
	connectionUniDb = connUniDb;

	i_session_number = session_number;
	dt_start_date = start_date;
	dt_end_date = end_date;
}

// -----   Destructor   -------------------------------------------------
MpdDbExperimentSession::~MpdDbExperimentSession()
{
	if (connectionUniDb)
		delete connectionUniDb;
	if (dt_end_date)
		delete dt_end_date;
}

// -----   Creating new record in class table ---------------------------
MpdDbExperimentSession* MpdDbExperimentSession::CreateExperimentSession(int session_number, TDatime start_date, TDatime* end_date)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"insert into experiment_session(session_number, start_date, end_date) "
		"values ($1, $2, $3)");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, session_number);
	stmt->SetDatime(1, start_date);
	if (end_date == NULL)
		stmt->SetNull(2);
	else
		stmt->SetDatime(2, *end_date);

	// inserting new record to DB
	if (!stmt->Process())
	{
		cout<<"Error: inserting new record to DB has been failed"<<endl;
		delete stmt;
		delete connUniDb;
		return 0x00;
	}

	delete stmt;

	return new MpdDbExperimentSession(connUniDb, session_number, start_date, end_date);
}

// -----   Get table record from database ---------------------------
MpdDbExperimentSession* MpdDbExperimentSession::GetExperimentSession(int session_number)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select session_number, start_date, end_date "
		"from experiment_session "
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
	TDatime tmp_start_date;
	tmp_start_date = stmt->GetDatime(1);
	TDatime* tmp_end_date;
	if (stmt->IsNull(2)) tmp_end_date = NULL;
	else
		tmp_end_date = new TDatime(stmt->GetDatime(2));

	delete stmt;

	return new MpdDbExperimentSession(connUniDb, tmp_session_number, tmp_start_date, tmp_end_date);
}

// -----   Delete record from class table ---------------------------
int MpdDbExperimentSession::DeleteExperimentSession(int session_number)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"delete from experiment_session "
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
int MpdDbExperimentSession::PrintAll()
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select session_number, start_date, end_date "
		"from experiment_session");
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
		TDatime tmp_start_date;
		tmp_start_date = stmt->GetDatime(1);
		TDatime* tmp_end_date;
		if (stmt->IsNull(2)) tmp_end_date = NULL;
		else
			tmp_end_date = new TDatime(stmt->GetDatime(2));

		cout<<"Table 'experiment_session'";
		cout<<". session_number: "<<tmp_session_number<<". start_date: "<<tmp_start_date.AsSQLString()<<". end_date: "<<(*tmp_end_date).AsSQLString()<<endl;
	}

	delete stmt;
	delete connUniDb;

	return 0;
}


// Setters functions
int MpdDbExperimentSession::SetSessionNumber(int session_number)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update experiment_session "
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

int MpdDbExperimentSession::SetStartDate(TDatime start_date)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update experiment_session "
		"set start_date = $1 "
		"where session_number = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetDatime(0, start_date);
	stmt->SetInt(1, i_session_number);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	dt_start_date = start_date;

	delete stmt;
	return 0;
}

int MpdDbExperimentSession::SetEndDate(TDatime* end_date)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update experiment_session "
		"set end_date = $1 "
		"where session_number = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	if (end_date == NULL)
		stmt->SetNull(0);
	else
		stmt->SetDatime(0, *end_date);
	stmt->SetInt(1, i_session_number);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	if (end_date)
		delete end_date;
	dt_end_date = end_date;

	delete stmt;
	return 0;
}

// -----   Print current record ---------------------------------------
void MpdDbExperimentSession::Print()
{
	cout<<"Table 'experiment_session'";
	cout<<". session_number: "<<i_session_number<<". start_date: "<<dt_start_date.AsSQLString()<<". end_date: "<<(*dt_end_date).AsSQLString()<<endl;

	return;
}

// -------------------------------------------------------------------
ClassImp(MpdDbExperimentSession);
