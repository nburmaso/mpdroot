// ----------------------------------------------------------------------
//                    MpdDbShift header file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

/** db_classes/MpdDbShift.h 
 ** Class for the table: shift 
 **/ 

#ifndef MPDDBSHIFT_H 
#define MPDDBSHIFT_H 1 

#include "TString.h" 
#include "TDatime.h" 

#include "MpdDbConnection.h" 

class MpdDbShift
{
 private:
	MpdDbConnection* connectionUniDb;

	int i_shift_id;
	int i_session_number;
	TString str_fio;
	TDatime dt_start_date;
	TDatime dt_end_date;
	TString* str_responsibility;

	//Constructor
	MpdDbShift(MpdDbConnection* connUniDb, int shift_id, int session_number, TString fio, TDatime start_date, TDatime end_date, TString* responsibility);

 public:
	virtual ~MpdDbShift(); // Destructor

	// static class functions
	static MpdDbShift* CreateShift(int session_number, TString fio, TDatime start_date, TDatime end_date, TString* responsibility);
	static MpdDbShift* GetShift(int shift_id);
	static int DeleteShift(int shift_id);
	static int PrintAll();

	// Getters
	int GetShiftId(){return i_shift_id;}
	int GetSessionNumber(){return i_session_number;}
	TString GetFio(){return str_fio;}
	TDatime GetStartDate(){return dt_start_date;}
	TDatime GetEndDate(){return dt_end_date;}
	TString* GetResponsibility(){return str_responsibility;}

	// Setters
	int SetShiftId(int shift_id);
	int SetSessionNumber(int session_number);
	int SetFio(TString fio);
	int SetStartDate(TDatime start_date);
	int SetEndDate(TDatime end_date);
	int SetResponsibility(TString* responsibility);
	void Print();

 ClassDef(MpdDbShift,1);
};

#endif
