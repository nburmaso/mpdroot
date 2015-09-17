// ----------------------------------------------------------------------
//                    MpdDbMap2dim cxx file 
//                      Generated 15-09-2015 
// ----------------------------------------------------------------------

#include "TSQLServer.h"
#include "TSQLStatement.h"

#include "MpdDbMap2dim.h"

#include <iostream>
using namespace std;

/* GENERATED CLASS MEMBERS (SHOULDN'T BE CHANGED MANUALLY) */
// -----   Constructor with database connection   -----------------------
MpdDbMap2dim::MpdDbMap2dim(MpdDbConnection* connUniDb, int map_id, int map_row, TString serial_hex, int channel, int f_channel, int channel_size, int x, int y, int is_connected)
{
	connectionUniDb = connUniDb;

	i_map_id = map_id;
	i_map_row = map_row;
	str_serial_hex = serial_hex;
	i_channel = channel;
	i_f_channel = f_channel;
	i_channel_size = channel_size;
	i_x = x;
	i_y = y;
	i_is_connected = is_connected;
}

// -----   Destructor   -------------------------------------------------
MpdDbMap2dim::~MpdDbMap2dim()
{
	if (connectionUniDb)
		delete connectionUniDb;
}

// -----   Creating new record in class table ---------------------------
MpdDbMap2dim* MpdDbMap2dim::CreateMap2dim(int map_id, TString serial_hex, int channel, int f_channel, int channel_size, int x, int y, int is_connected)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"insert into map_2dim(map_id, serial_hex, channel, f_channel, channel_size, x, y, is_connected) "
		"values ($1, $2, $3, $4, $5, $6, $7, $8)");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, map_id);
	stmt->SetString(1, serial_hex);
	stmt->SetInt(2, channel);
	stmt->SetInt(3, f_channel);
	stmt->SetInt(4, channel_size);
	stmt->SetInt(5, x);
	stmt->SetInt(6, y);
	stmt->SetInt(7, is_connected);

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
	int map_row;
	TSQLStatement* stmt_last = uni_db->Statement("SELECT currval(pg_get_serial_sequence('map_2dim','map_row'))");

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
			map_row = stmt_last->GetInt(0);
			delete stmt_last;
		}
	}
	else
	{
		cout<<"Error: getting last ID has been failed!"<<endl;
		delete stmt_last;
		return 0x00;
	}

	return new MpdDbMap2dim(connUniDb, map_id, map_row, serial_hex, channel, f_channel, channel_size, x, y, is_connected);
}

// -----   Get table record from database ---------------------------
MpdDbMap2dim* MpdDbMap2dim::GetMap2dim(int map_id, int map_row)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select map_id, map_row, serial_hex, channel, f_channel, channel_size, x, y, is_connected "
		"from map_2dim "
		"where map_id = %d and map_row = %d", map_id, map_row);
	TSQLStatement* stmt = uni_db->Statement(sql);

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

	int tmp_map_id;
	tmp_map_id = stmt->GetInt(0);
	int tmp_map_row;
	tmp_map_row = stmt->GetInt(1);
	TString tmp_serial_hex;
	tmp_serial_hex = stmt->GetString(2);
	int tmp_channel;
	tmp_channel = stmt->GetInt(3);
	int tmp_f_channel;
	tmp_f_channel = stmt->GetInt(4);
	int tmp_channel_size;
	tmp_channel_size = stmt->GetInt(5);
	int tmp_x;
	tmp_x = stmt->GetInt(6);
	int tmp_y;
	tmp_y = stmt->GetInt(7);
	int tmp_is_connected;
	tmp_is_connected = stmt->GetInt(8);

	delete stmt;

	return new MpdDbMap2dim(connUniDb, tmp_map_id, tmp_map_row, tmp_serial_hex, tmp_channel, tmp_f_channel, tmp_channel_size, tmp_x, tmp_y, tmp_is_connected);
}

// -----   Delete record from class table ---------------------------
int MpdDbMap2dim::DeleteMap2dim(int map_id, int map_row)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"delete from map_2dim "
		"where map_id = $1 and map_row = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, map_id);
	stmt->SetInt(1, map_row);

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
int MpdDbMap2dim::PrintAll()
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select map_id, map_row, serial_hex, channel, f_channel, channel_size, x, y, is_connected "
		"from map_2dim");
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
		int tmp_map_id;
		tmp_map_id = stmt->GetInt(0);
		int tmp_map_row;
		tmp_map_row = stmt->GetInt(1);
		TString tmp_serial_hex;
		tmp_serial_hex = stmt->GetString(2);
		int tmp_channel;
		tmp_channel = stmt->GetInt(3);
		int tmp_f_channel;
		tmp_f_channel = stmt->GetInt(4);
		int tmp_channel_size;
		tmp_channel_size = stmt->GetInt(5);
		int tmp_x;
		tmp_x = stmt->GetInt(6);
		int tmp_y;
		tmp_y = stmt->GetInt(7);
		int tmp_is_connected;
		tmp_is_connected = stmt->GetInt(8);

		cout<<"Table 'map_2dim'";
		cout<<". map_id: "<<tmp_map_id<<". map_row: "<<tmp_map_row<<". serial_hex: "<<tmp_serial_hex<<". channel: "<<tmp_channel<<". f_channel: "<<tmp_f_channel<<". channel_size: "<<tmp_channel_size<<". x: "<<tmp_x<<". y: "<<tmp_y<<". is_connected: "<<tmp_is_connected<<endl;
	}

	delete stmt;
	delete connUniDb;

	return 0;
}


// Setters functions
int MpdDbMap2dim::SetMapId(int map_id)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update map_2dim "
		"set map_id = $1 "
		"where map_id = $2 and map_row = $3");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, map_id);
	stmt->SetInt(1, i_map_id);
	stmt->SetInt(2, i_map_row);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	i_map_id = map_id;

	delete stmt;
	return 0;
}

int MpdDbMap2dim::SetSerialHex(TString serial_hex)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update map_2dim "
		"set serial_hex = $1 "
		"where map_id = $2 and map_row = $3");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetString(0, serial_hex);
	stmt->SetInt(1, i_map_id);
	stmt->SetInt(2, i_map_row);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	str_serial_hex = serial_hex;

	delete stmt;
	return 0;
}

int MpdDbMap2dim::SetChannel(int channel)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update map_2dim "
		"set channel = $1 "
		"where map_id = $2 and map_row = $3");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, channel);
	stmt->SetInt(1, i_map_id);
	stmt->SetInt(2, i_map_row);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	i_channel = channel;

	delete stmt;
	return 0;
}

int MpdDbMap2dim::SetFChannel(int f_channel)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update map_2dim "
		"set f_channel = $1 "
		"where map_id = $2 and map_row = $3");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, f_channel);
	stmt->SetInt(1, i_map_id);
	stmt->SetInt(2, i_map_row);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	i_f_channel = f_channel;

	delete stmt;
	return 0;
}

int MpdDbMap2dim::SetChannelSize(int channel_size)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update map_2dim "
		"set channel_size = $1 "
		"where map_id = $2 and map_row = $3");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, channel_size);
	stmt->SetInt(1, i_map_id);
	stmt->SetInt(2, i_map_row);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	i_channel_size = channel_size;

	delete stmt;
	return 0;
}

int MpdDbMap2dim::SetX(int x)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update map_2dim "
		"set x = $1 "
		"where map_id = $2 and map_row = $3");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, x);
	stmt->SetInt(1, i_map_id);
	stmt->SetInt(2, i_map_row);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	i_x = x;

	delete stmt;
	return 0;
}

int MpdDbMap2dim::SetY(int y)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update map_2dim "
		"set y = $1 "
		"where map_id = $2 and map_row = $3");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, y);
	stmt->SetInt(1, i_map_id);
	stmt->SetInt(2, i_map_row);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	i_y = y;

	delete stmt;
	return 0;
}

int MpdDbMap2dim::SetIsConnected(int is_connected)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update map_2dim "
		"set is_connected = $1 "
		"where map_id = $2 and map_row = $3");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, is_connected);
	stmt->SetInt(1, i_map_id);
	stmt->SetInt(2, i_map_row);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	i_is_connected = is_connected;

	delete stmt;
	return 0;
}

// -----   Print current record ---------------------------------------
void MpdDbMap2dim::Print()
{
	cout<<"Table 'map_2dim'";
	cout<<". map_id: "<<i_map_id<<". map_row: "<<i_map_row<<". serial_hex: "<<str_serial_hex<<". channel: "<<i_channel<<". f_channel: "<<i_f_channel<<". channel_size: "<<i_channel_size<<". x: "<<i_x<<". y: "<<i_y<<". is_connected: "<<i_is_connected<<endl;

	return;
}
/* END OF GENERATED CLASS PART (SHOULDN'T BE CHANGED MANUALLY) */

// -------------------------------------------------------------------
ClassImp(MpdDbMap2dim);
