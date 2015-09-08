// ----------------------------------------------------------------------
//                    MpdDbIntValue header file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

/** db_classes/MpdDbIntValue.h 
 ** Class for the table: int_value 
 **/ 

#ifndef MPDDBINTVALUE_H 
#define MPDDBINTVALUE_H 1 

#include "TString.h" 
#include "TDatime.h" 

#include "MpdDbConnection.h" 

class MpdDbIntValue
{
 private:
	MpdDbConnection* connectionUniDb;

	int i_value_id;
	int i_par_value;

	//Constructor
	MpdDbIntValue(MpdDbConnection* connUniDb, int value_id, int par_value);

 public:
	virtual ~MpdDbIntValue(); // Destructor

	// static class functions
	static MpdDbIntValue* CreateIntValue(int value_id, int par_value);
	static MpdDbIntValue* GetIntValue(int value_id);
	static int DeleteIntValue(int value_id);
	static int PrintAll();

	// Getters
	int GetValueId(){return i_value_id;}
	int GetParValue(){return i_par_value;}

	// Setters
	int SetValueId(int value_id);
	int SetParValue(int par_value);
	void Print();

 ClassDef(MpdDbIntValue,1);
};

#endif
