// ----------------------------------------------------------------------
//                    MpdDbParameterValue header file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

/** db_classes/MpdDbParameterValue.h 
 ** Class for the table: parameter_value 
 **/ 

#ifndef MPDDBPARAMETERVALUE_H 
#define MPDDBPARAMETERVALUE_H 1 

#include "TString.h" 
#include "TDatime.h" 

#include "MpdDbConnection.h" 

class MpdDbParameterValue
{
 private:
	MpdDbConnection* connectionUniDb;

	int i_value_id;

	//Constructor
	MpdDbParameterValue(MpdDbConnection* connUniDb, int value_id);

 public:
	virtual ~MpdDbParameterValue(); // Destructor

	// static class functions
	static MpdDbParameterValue* CreateParameterValue();
	static MpdDbParameterValue* GetParameterValue(int value_id);
	static int DeleteParameterValue(int value_id);
	static int PrintAll();

	// Getters
	int GetValueId(){return i_value_id;}

	// Setters
	int SetValueId(int value_id);
	void Print();

 ClassDef(MpdDbParameterValue,1);
};

#endif
