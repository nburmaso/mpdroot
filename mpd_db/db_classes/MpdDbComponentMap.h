// ----------------------------------------------------------------------
//                    MpdDbComponentMap header file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

/** db_classes/MpdDbComponentMap.h 
 ** Class for the table: component_map 
 **/ 

#ifndef MPDDBCOMPONENTMAP_H 
#define MPDDBCOMPONENTMAP_H 1 

#include "TString.h" 
#include "TDatime.h" 

#include "MpdDbConnection.h" 

class MpdDbComponentMap
{
 private:
	MpdDbConnection* connectionUniDb;

	int i_map_id;
	int i_map_type;

	//Constructor
	MpdDbComponentMap(MpdDbConnection* connUniDb, int map_id, int map_type);

 public:
	virtual ~MpdDbComponentMap(); // Destructor

	// static class functions
	static MpdDbComponentMap* CreateComponentMap(int map_type);
	static MpdDbComponentMap* GetComponentMap(int map_id);
	static int DeleteComponentMap(int map_id);
	static int PrintAll();

	// Getters
	int GetMapId(){return i_map_id;}
	int GetMapType(){return i_map_type;}

	// Setters
	int SetMapId(int map_id);
	int SetMapType(int map_type);
	void Print();

 ClassDef(MpdDbComponentMap,1);
};

#endif
