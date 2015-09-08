// ----------------------------------------------------------------------
//                    MpdDbStringValue header file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

/** db_classes/MpdDbStringValue.h 
 ** Class for the table: string_value 
 **/ 

#ifndef MPDDBSTRINGVALUE_H 
#define MPDDBSTRINGVALUE_H 1 

#include "TString.h" 
#include "TDatime.h" 

#include "MpdDbConnection.h" 

class MpdDbStringValue
{
 private:
	MpdDbConnection* connectionUniDb;

	int i_value_id;
	TString str_par_value;

	//Constructor
	MpdDbStringValue(MpdDbConnection* connUniDb, int value_id, TString par_value);

 public:
	virtual ~MpdDbStringValue(); // Destructor

	// static class functions
	static MpdDbStringValue* CreateStringValue(int value_id, TString par_value);
	static MpdDbStringValue* GetStringValue(int value_id);
	static int DeleteStringValue(int value_id);
	static int PrintAll();

	// Getters
	int GetValueId(){return i_value_id;}
	TString GetParValue(){return str_par_value;}

	// Setters
	int SetValueId(int value_id);
	int SetParValue(TString par_value);
	void Print();

 ClassDef(MpdDbStringValue,1);
};

#endif
