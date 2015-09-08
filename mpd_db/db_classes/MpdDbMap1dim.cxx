// ----------------------------------------------------------------------
//                    MpdDbMap1dim cxx file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

#include "TSQLServer.h" 
#include "TSQLStatement.h" 

#include "MpdDbMap1dim.h" 

#include <iostream>
using namespace std;

// -----   Constructor with database connection   -----------------------
MpdDbMap1dim::MpdDbMap1dim(MpdDbConnection* connUniDb, int map_id, TString serial_hex, int plane, int map_group, int slot, int channel_low, int channel_high)
{
	connectionUniDb = connUniDb;

	i_map_id = map_id;
	str_serial_hex = serial_hex;
	i_plane = plane;
	i_map_group = map_group;
	i_slot = slot;
	i_channel_low = channel_low;
	i_channel_high = channel_high;
}

// -----   Destructor   -------------------------------------------------
MpdDbMap1dim::~MpdDbMap1dim()
{
	if (connectionUniDb)
		delete connectionUniDb;
}

// -----   Creating new record in class table ---------------------------
MpdDbMap1dim* MpdDbMap1dim::CreateMap1dim(int map_id, TString serial_hex, int plane, int map_group, int slot, int channel_low, int channel_high)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"insert into map_1dim(map_id, serial_hex, plane, map_group, slot, channel_low, channel_high) "
		"values ($1, $2, $3, $4, $5, $6, $7)");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, map_id);
	stmt->SetString(1, serial_hex);
	stmt->SetInt(2, plane);
	stmt->SetInt(3, map_group);
	stmt->SetInt(4, slot);
	stmt->SetInt(5, channel_low);
	stmt->SetInt(6, channel_high);

	// inserting new record to DB
	if (!stmt->Process())
	{
		cout<<"Error: inserting new record to DB has been failed"<<endl;
		delete stmt;
		delete connUniDb;
		return 0x00;
	}

	delete stmt;

	return new MpdDbMap1dim(connUniDb, map_id, serial_hex, plane, map_group, slot, channel_low, channel_high);
}

// -----   Get table record from database ---------------------------
MpdDbMap1dim* MpdDbMap1dim::GetMap1dim(int map_id)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select map_id, serial_hex, plane, map_group, slot, channel_low, channel_high "
		"from map_1dim "
		"where map_id = $1");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, map_id);

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
	TString tmp_serial_hex;
	tmp_serial_hex = stmt->GetString(1);
	int tmp_plane;
	tmp_plane = stmt->GetInt(2);
	int tmp_map_group;
	tmp_map_group = stmt->GetInt(3);
	int tmp_slot;
	tmp_slot = stmt->GetInt(4);
	int tmp_channel_low;
	tmp_channel_low = stmt->GetInt(5);
	int tmp_channel_high;
	tmp_channel_high = stmt->GetInt(6);

	delete stmt;

	return new MpdDbMap1dim(connUniDb, tmp_map_id, tmp_serial_hex, tmp_plane, tmp_map_group, tmp_slot, tmp_channel_low, tmp_channel_high);
}

// -----   Delete record from class table ---------------------------
int MpdDbMap1dim::DeleteMap1dim(int map_id)
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"delete from map_1dim "
		"where map_id = $1");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, map_id);

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
int MpdDbMap1dim::PrintAll()
{
	MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
	if (connUniDb == 0x00) return 0x00;

	TSQLServer* uni_db = connUniDb->GetSQLServer();

	TString sql = TString::Format(
		"select map_id, serial_hex, plane, map_group, slot, channel_low, channel_high "
		"from map_1dim");
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
		TString tmp_serial_hex;
		tmp_serial_hex = stmt->GetString(1);
		int tmp_plane;
		tmp_plane = stmt->GetInt(2);
		int tmp_map_group;
		tmp_map_group = stmt->GetInt(3);
		int tmp_slot;
		tmp_slot = stmt->GetInt(4);
		int tmp_channel_low;
		tmp_channel_low = stmt->GetInt(5);
		int tmp_channel_high;
		tmp_channel_high = stmt->GetInt(6);

		cout<<"Table 'map_1dim'";
		cout<<". map_id: "<<tmp_map_id<<". serial_hex: "<<tmp_serial_hex<<". plane: "<<tmp_plane<<". map_group: "<<tmp_map_group<<". slot: "<<tmp_slot<<". channel_low: "<<tmp_channel_low<<". channel_high: "<<tmp_channel_high<<endl;
	}

	delete stmt;
	delete connUniDb;

	return 0;
}


// Setters functions
int MpdDbMap1dim::SetMapId(int map_id)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update map_1dim "
		"set map_id = $1 "
		"where map_id = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, map_id);
	stmt->SetInt(1, i_map_id);

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

int MpdDbMap1dim::SetSerialHex(TString serial_hex)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update map_1dim "
		"set serial_hex = $1 "
		"where map_id = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetString(0, serial_hex);
	stmt->SetInt(1, i_map_id);

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

int MpdDbMap1dim::SetPlane(int plane)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update map_1dim "
		"set plane = $1 "
		"where map_id = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, plane);
	stmt->SetInt(1, i_map_id);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	i_plane = plane;

	delete stmt;
	return 0;
}

int MpdDbMap1dim::SetMapGroup(int map_group)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update map_1dim "
		"set map_group = $1 "
		"where map_id = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, map_group);
	stmt->SetInt(1, i_map_id);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	i_map_group = map_group;

	delete stmt;
	return 0;
}

int MpdDbMap1dim::SetSlot(int slot)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update map_1dim "
		"set slot = $1 "
		"where map_id = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, slot);
	stmt->SetInt(1, i_map_id);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	i_slot = slot;

	delete stmt;
	return 0;
}

int MpdDbMap1dim::SetChannelLow(int channel_low)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update map_1dim "
		"set channel_low = $1 "
		"where map_id = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, channel_low);
	stmt->SetInt(1, i_map_id);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	i_channel_low = channel_low;

	delete stmt;
	return 0;
}

int MpdDbMap1dim::SetChannelHigh(int channel_high)
{
	if (!connectionUniDb)
	{
		cout<<"Connection object is null"<<endl;
		return -1;
	}

	TSQLServer* uni_db = connectionUniDb->GetSQLServer();

	TString sql = TString::Format(
		"update map_1dim "
		"set channel_high = $1 "
		"where map_id = $2");
	TSQLStatement* stmt = uni_db->Statement(sql);

	stmt->NextIteration();
	stmt->SetInt(0, channel_high);
	stmt->SetInt(1, i_map_id);

	// write new value to database
	if (!stmt->Process())
	{
		cout<<"Error: updating the record has been failed"<<endl;

		delete stmt;
		return -2;
	}

	i_channel_high = channel_high;

	delete stmt;
	return 0;
}

// -----   Print current record ---------------------------------------
void MpdDbMap1dim::Print()
{
	cout<<"Table 'map_1dim'";
	cout<<". map_id: "<<i_map_id<<". serial_hex: "<<str_serial_hex<<". plane: "<<i_plane<<". map_group: "<<i_map_group<<". slot: "<<i_slot<<". channel_low: "<<i_channel_low<<". channel_high: "<<i_channel_high<<endl;

	return;
}

// -------------------------------------------------------------------
ClassImp(MpdDbMap1dim);
