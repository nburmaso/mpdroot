// ----------------------------------------------------------------------
//                    MpdDbRun header file 
//                      Generated 15-09-2015 
// ----------------------------------------------------------------------

/** db_classes/MpdDbRun.h 
 ** Class for the table: run_ 
 **/ 

#ifndef MPDDBRUN_H 
#define MPDDBRUN_H 1 

#include "TString.h"
#include "TDatime.h"

#include "MpdDbConnection.h"

class MpdDbRun
{
 private:
	/* GENERATED PRIVATE MEMBERS (SHOULDN'T BE CHANGED MANUALLY) */
	MpdDbConnection* connectionUniDb;

	int i_run_number;
	int* i_session_number;
	TString str_file_path;
	TString str_beam_particle;
	TString* str_target_particle;
	double* d_energy;
	TDatime dt_start_datetime;
	TDatime* dt_end_datetime;
	int* i_event_count;
	int* i_field_current_a;
	double* d_file_size_kb;
	int* i_geometry_id;

	//Constructor
	MpdDbRun(MpdDbConnection* connUniDb, int run_number, int* session_number, TString file_path, TString beam_particle, TString* target_particle, double* energy, TDatime start_datetime, TDatime* end_datetime, int* event_count, int* field_current_a, double* file_size_kb, int* geometry_id);
	/* END OF PRIVATE GENERATED PART (SHOULDN'T BE CHANGED MANUALLY) */

 public:
	/* GENERATED PUBLIC MEMBERS (SHOULDN'T BE CHANGED MANUALLY) */
	virtual ~MpdDbRun(); // Destructor

	// static class functions
	static MpdDbRun* CreateRun(int run_number, int* session_number, TString file_path, TString beam_particle, TString* target_particle, double* energy, TDatime start_datetime, TDatime* end_datetime, int* event_count, int* field_current_a, double* file_size_kb, int* geometry_id);
	static MpdDbRun* GetRun(int run_number);
	static MpdDbRun* GetRun(TString file_path);
	static int DeleteRun(int run_number);
	static int DeleteRun(TString file_path);
	static int PrintAll();

	// Getters
	int GetRunNumber(){return i_run_number;}
	int* GetSessionNumber(){return i_session_number;}
	TString GetFilePath(){return str_file_path;}
	TString GetBeamParticle(){return str_beam_particle;}
	TString* GetTargetParticle(){return str_target_particle;}
	double* GetEnergy(){return d_energy;}
	TDatime GetStartDatetime(){return dt_start_datetime;}
	TDatime* GetEndDatetime(){return dt_end_datetime;}
	int* GetEventCount(){return i_event_count;}
	int* GetFieldCurrentA(){return i_field_current_a;}
	double* GetFileSizeKb(){return d_file_size_kb;}
	int* GetGeometryId(){return i_geometry_id;}

	// Setters
	int SetRunNumber(int run_number);
	int SetSessionNumber(int* session_number);
	int SetFilePath(TString file_path);
	int SetBeamParticle(TString beam_particle);
	int SetTargetParticle(TString* target_particle);
	int SetEnergy(double* energy);
	int SetStartDatetime(TDatime start_datetime);
	int SetEndDatetime(TDatime* end_datetime);
	int SetEventCount(int* event_count);
	int SetFieldCurrentA(int* field_current_a);
	int SetFileSizeKb(double* file_size_kb);
	int SetGeometryId(int* geometry_id);
	void Print();
	/* END OF PUBLIC GENERATED PART (SHOULDN'T BE CHANGED MANUALLY) */

 ClassDef(MpdDbRun,1);
};

#endif
