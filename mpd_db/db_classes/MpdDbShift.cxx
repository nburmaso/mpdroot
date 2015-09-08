// ----------------------------------------------------------------------
//                    MpdDbShift cxx file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

#include "TSQLServer.h" 
#include "TSQLStatement.h" 

#include "MpdDbShift.h" 

#include <iostream>
using namespace std;

// -----   Constructor with database connection   -----------------------
MpdDbShift::MpdDbShift(MpdDbConnection* connUniDb, int shift_id, int session_number, TString fio, TDatime start_date, TDatime end_date, TString* responsibility)
{
	connectionUniDb = connUniDb;

	i_shift_id = shift_id;
	i_session_number = session_number;
	str_fio = fio;
	dt_start_date = start_date;
	dt_end_date = end_date;
	str_responsibility = responsibility;
}

// -----   Destructor   -------------------------------------------------
MpdDbShift::~MpdDbShift()
{
	if (connectionUniDb)
		delete connectionUniDb;
	if (str_responsibility)
		delete str_responsibility;
}

// -----   Creating new record in class table ---------------------------
MpdDbShift* MpdDbShift::CreateShift(int session_number, TString fio, TDatime start_date, TDatime end_date, TString* responsibility)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"insert into shift(session_number, fio, start_date, end_date, responsibility) "
		"values ($1, $2, $3, $4, $5)");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, session_number);
	stmt->SetString(1, fio);
	stmt->SetDatime(2, start_date);
	stmt->SetDatime(3, end_date);
	if (responsibility == NULL)
		stmt->SetNull(4);
	else
		stmt->SetString(4, *responsibility);

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
	int shift_id;
	TSQLStatement* stmt_last = uni_db->Statement("SELECT currval(pg_get_serial_sequence('shift','shift_id'))");

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
			shift_id = stmt_last->GetInt(0);
			delete stmt_last;
		}
	}
	else
	{
		cout<<"Error: getting last ID has been failed!"<<endl;
		delete stmt_last;
		return 0x00;
	}

	return new MpdDbShift(connUniDb, shift_id, session_number, fio, start_date, end_date, responsibility);
}

// -----   Get table record from database ---------------------------
MpdDbShift* MpdDbShift::GetShift(int shift_id)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select shift_id, session_number, fio, start_date, end_date, responsibility "
		"from shift "
		"where shift_id = $1");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, shift_id);

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

	int tmp_shift_id;
	tmp_shift_id = stmt->GetInt(0);
	int tmp_session_number;
	tmp_session_number = stmt->GetInt(1);
	TString tmp_fio;
	tmp_fio = stmt->GetString(2);
	TDatime tmp_start_date;
	tmp_start_date = stmt->GetDatime(3);
	TDatime tmp_end_date;
	tmp_end_date = stmt->GetDatime(4);
	TString* tmp_responsibility;
	if (stmt->IsNull(5)) tmp_responsibility = NULL;
	else
		tmp_responsibility = new TString(stmt->GetString(5));

	delete stmt;

	return new MpdDbShift(connUniDb, tmp_shift_id, tmp_session_number, tmp_fio, tmp_start_date, tmp_end_date, tmp_responsibility);
}

// -----   Delete record from class table ---------------------------
int MpdDbShift::DeleteShift(int shift_id)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"delete from shift "
		"where shift_id = $1");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, shift_id);

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
int MpdDbShift::PrintAll()
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select shift_id, session_number, fio, start_date, end_date, responsibility "
		"from shift");
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
		int tmp_shift_id;
		tmp_shift_id = stmt->GetInt(0);
		int tmp_session_number;
		tmp_session_number = stmt->GetInt(1);
		TString tmp_fio;
		tmp_fio = stmt->GetString(2);
		TDatime tmp_start_date;
		tmp_start_date = stmt->GetDatime(3);
		TDatime tmp_end_date;
		tmp_end_date = stmt->GetDatime(4);
		TString* tmp_responsibility;
		if (stmt->IsNull(5)) tmp_responsibility = NULL;
		else
			tmp_responsibility = new TString(stmt->GetString(5));

		cout<<"Table 'shift'";
		cout<<". shift_id: "<<tmp_shift_id<<". session_number: "<<tmp_session_number<<". fio: "<<tmp_fio<<". start_date: "<<tmp_start_date.AsSQLString()<<". end_date: "<<tmp_end_date.AsSQLString()<<". responsibility: "<<(*tmp_responsibility)<<endl;
	}

	delete stmt;
	delete connUniDb;

	return 0;
}


// Setters functions
int MpdDbShift::SetSessionNumber(int session_number)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update shift "
		"set session_number = $1 "
		"where shift_id = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, session_number);
	stmt->SetInt(1, i_shift_id);

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

int MpdDbShift::SetFio(TString fio)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update shift "
		"set fio = $1 "
		"where shift_id = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetString(0, fio);
	stmt->SetInt(1, i_shift_id);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	str_fio = fio;

	delete stmt;
	return 0;
}

int MpdDbShift::SetStartDate(TDatime start_date)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update shift "
		"set start_date = $1 "
		"where shift_id = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetDatime(0, start_date);
	stmt->SetInt(1, i_shift_id);

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

int MpdDbShift::SetEndDate(TDatime end_date)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update shift "
		"set end_date = $1 "
		"where shift_id = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetDatime(0, end_date);
	stmt->SetInt(1, i_shift_id);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	dt_end_date = end_date;

	delete stmt;
	return 0;
}

int MpdDbShift::SetResponsibility(TString* responsibility)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update shift "
		"set responsibility = $1 "
		"where shift_id = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	if (responsibility == NULL)
		stmt->SetNull(0);
	else
		stmt->SetString(0, *responsibility);
	stmt->SetInt(1, i_shift_id);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	if (responsibility)
		delete responsibility;
	str_responsibility = responsibility;

	delete stmt;
	return 0;
}

// -----   Print current record ---------------------------------------
void MpdDbShift::Print()
{
	cout<<"Table 'shift'";
	cout<<". shift_id: "<<i_shift_id<<". session_number: "<<i_session_number<<". fio: "<<str_fio<<". start_date: "<<dt_start_date.AsSQLString()<<". end_date: "<<dt_end_date.AsSQLString()<<". responsibility: "<<(*str_responsibility)<<endl;

	return;
}

// -------------------------------------------------------------------
ClassImp(MpdDbShift);
