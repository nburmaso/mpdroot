// ----------------------------------------------------------------------
//                    MpdDbRunGeometry header file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

/** db_classes/MpdDbRunGeometry.h 
 ** Class for the table: run_geometry 
 **/ 

#ifndef MPDDBRUNGEOMETRY_H 
#define MPDDBRUNGEOMETRY_H 1 

#include "TString.h" 
#include "TDatime.h" 

#include "MpdDbConnection.h" 

class MpdDbRunGeometry
{
 private:
	MpdDbConnection* connectionUniDb;

	int i_geometry_id;
	void* blob_root_geometry;
	Long_t sz_root_geometry;

	//Constructor
	MpdDbRunGeometry(MpdDbConnection* connUniDb, int geometry_id, void* root_geometry, Long_t size_root_geometry);

 public:
	virtual ~MpdDbRunGeometry(); // Destructor

	// static class functions
	static MpdDbRunGeometry* CreateRunGeometry(void* root_geometry, Long_t size_root_geometry);
	static MpdDbRunGeometry* GetRunGeometry(int geometry_id);
	static int DeleteRunGeometry(int geometry_id);
	static int PrintAll();

	// Getters
	int GetGeometryId(){return i_geometry_id;}
	void* GetRootGeometry(){return blob_root_geometry;}
	Long_t GetRootGeometrySize(){return sz_root_geometry;}

	// Setters
	int SetGeometryId(int geometry_id);
	int SetRootGeometry(void* root_geometry, Long_t size_root_geometry);
	void Print();

 ClassDef(MpdDbRunGeometry,1);
};

#endif
