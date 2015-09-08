// ----------------------------------------------------------------------
//                    MpdDbRunParameter header file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

/** db_classes/MpdDbRunParameter.h 
 ** Class for the table: run_parameter 
 **/ 

#ifndef MPDDBRUNPARAMETER_H 
#define MPDDBRUNPARAMETER_H 1 

#include "TString.h" 
#include "TDatime.h" 

#include "MpdDbConnection.h" 

class MpdDbRunParameter
{
 private:
	MpdDbConnection* connectionUniDb;

	int i_run_number;
	TString str_component_name;
	int i_parameter_id;
	int i_value_id;

	//Constructor
	MpdDbRunParameter(MpdDbConnection* connUniDb, int run_number, TString component_name, int parameter_id, int value_id);

 public:
	virtual ~MpdDbRunParameter(); // Destructor

	// static class functions
	static MpdDbRunParameter* CreateRunParameter(int run_number, TString component_name, int parameter_id, int value_id);
	static MpdDbRunParameter* GetRunParameter(int run_number);
	static int DeleteRunParameter(int run_number);
	static int PrintAll();

	// Getters
	int GetRunNumber(){return i_run_number;}
	TString GetComponentName(){return str_component_name;}
	int GetParameterId(){return i_parameter_id;}
	int GetValueId(){return i_value_id;}

	// Setters
	int SetRunNumber(int run_number);
	int SetComponentName(TString component_name);
	int SetParameterId(int parameter_id);
	int SetValueId(int value_id);
	void Print();

 ClassDef(MpdDbRunParameter,1);
};

#endif
