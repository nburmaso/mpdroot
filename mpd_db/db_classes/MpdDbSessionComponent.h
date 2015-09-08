// ----------------------------------------------------------------------
//                    MpdDbSessionComponent header file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

/** db_classes/MpdDbSessionComponent.h 
 ** Class for the table: session_component 
 **/ 

#ifndef MPDDBSESSIONCOMPONENT_H 
#define MPDDBSESSIONCOMPONENT_H 1 

#include "TString.h" 
#include "TDatime.h" 

#include "MpdDbConnection.h" 

class MpdDbSessionComponent
{
 private:
	MpdDbConnection* connectionUniDb;

	int i_session_number;
	TString str_component_name;
	int* i_map_id;

	//Constructor
	MpdDbSessionComponent(MpdDbConnection* connUniDb, int session_number, TString component_name, int* map_id);

 public:
	virtual ~MpdDbSessionComponent(); // Destructor

	// static class functions
	static MpdDbSessionComponent* CreateSessionComponent(int session_number, TString component_name, int* map_id);
	static MpdDbSessionComponent* GetSessionComponent(int session_number);
	static int DeleteSessionComponent(int session_number);
	static int PrintAll();

	// Getters
	int GetSessionNumber(){return i_session_number;}
	TString GetComponentName(){return str_component_name;}
	int* GetMapId(){return i_map_id;}

	// Setters
	int SetSessionNumber(int session_number);
	int SetComponentName(TString component_name);
	int SetMapId(int* map_id);
	void Print();

 ClassDef(MpdDbSessionComponent,1);
};

#endif
