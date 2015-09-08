// ----------------------------------------------------------------------
//                    MpdDbDoubleValue header file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

/** db_classes/MpdDbDoubleValue.h 
 ** Class for the table: double_value 
 **/ 

#ifndef MPDDBDOUBLEVALUE_H 
#define MPDDBDOUBLEVALUE_H 1 

#include "TString.h" 
#include "TDatime.h" 

#include "MpdDbConnection.h" 

class MpdDbDoubleValue
{
 private:
	MpdDbConnection* connectionUniDb;

	int i_value_id;
	double d_par_value;

	//Constructor
	MpdDbDoubleValue(MpdDbConnection* connUniDb, int value_id, double par_value);

 public:
	virtual ~MpdDbDoubleValue(); // Destructor

	// static class functions
	static MpdDbDoubleValue* CreateDoubleValue(int value_id, double par_value);
	static MpdDbDoubleValue* GetDoubleValue(int value_id);
	static int DeleteDoubleValue(int value_id);
	static int PrintAll();

	// Getters
	int GetValueId(){return i_value_id;}
	double GetParValue(){return d_par_value;}

	// Setters
	int SetValueId(int value_id);
	int SetParValue(double par_value);
	void Print();

 ClassDef(MpdDbDoubleValue,1);
};

#endif
