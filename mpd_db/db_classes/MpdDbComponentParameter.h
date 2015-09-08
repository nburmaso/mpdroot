// ----------------------------------------------------------------------
//                    MpdDbComponentParameter header file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

/** db_classes/MpdDbComponentParameter.h 
 ** Class for the table: component_parameter 
 **/ 

#ifndef MPDDBCOMPONENTPARAMETER_H 
#define MPDDBCOMPONENTPARAMETER_H 1 

#include "TString.h" 
#include "TDatime.h" 

#include "MpdDbConnection.h" 

class MpdDbComponentParameter
{
 private:
	MpdDbConnection* connectionUniDb;

	int i_parameter_id;
	TString str_parameter_name;
	int i_parameter_type;

	//Constructor
	MpdDbComponentParameter(MpdDbConnection* connUniDb, int parameter_id, TString parameter_name, int parameter_type);

 public:
	virtual ~MpdDbComponentParameter(); // Destructor

	// static class functions
	static MpdDbComponentParameter* CreateComponentParameter(TString parameter_name, int parameter_type);
	static MpdDbComponentParameter* GetComponentParameter(int parameter_id);
	static MpdDbComponentParameter* GetComponentParameter(TString parameter_name);
	static int DeleteComponentParameter(int parameter_id);
	static int DeleteComponentParameter(TString parameter_name);
	static int PrintAll();

	// Getters
	int GetParameterId(){return i_parameter_id;}
	TString GetParameterName(){return str_parameter_name;}
	int GetParameterType(){return i_parameter_type;}

	// Setters
	int SetParameterId(int parameter_id);
	int SetParameterName(TString parameter_name);
	int SetParameterType(int parameter_type);
	void Print();

 ClassDef(MpdDbComponentParameter,1);
};

#endif
