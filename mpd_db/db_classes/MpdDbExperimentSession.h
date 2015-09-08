// ----------------------------------------------------------------------
//                    MpdDbExperimentSession header file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

/** db_classes/MpdDbExperimentSession.h 
 ** Class for the table: experiment_session 
 **/ 

#ifndef MPDDBEXPERIMENTSESSION_H 
#define MPDDBEXPERIMENTSESSION_H 1 

#include "TString.h" 
#include "TDatime.h" 

#include "MpdDbConnection.h" 

class MpdDbExperimentSession
{
 private:
	MpdDbConnection* connectionUniDb;

	int i_session_number;
	TDatime dt_start_date;
	TDatime* dt_end_date;

	//Constructor
	MpdDbExperimentSession(MpdDbConnection* connUniDb, int session_number, TDatime start_date, TDatime* end_date);

 public:
	virtual ~MpdDbExperimentSession(); // Destructor

	// static class functions
	static MpdDbExperimentSession* CreateExperimentSession(int session_number, TDatime start_date, TDatime* end_date);
	static MpdDbExperimentSession* GetExperimentSession(int session_number);
	static int DeleteExperimentSession(int session_number);
	static int PrintAll();

	// Getters
	int GetSessionNumber(){return i_session_number;}
	TDatime GetStartDate(){return dt_start_date;}
	TDatime* GetEndDate(){return dt_end_date;}

	// Setters
	int SetSessionNumber(int session_number);
	int SetStartDate(TDatime start_date);
	int SetEndDate(TDatime* end_date);
	void Print();

 ClassDef(MpdDbExperimentSession,1);
};

#endif
