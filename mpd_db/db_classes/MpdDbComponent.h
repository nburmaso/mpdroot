// ----------------------------------------------------------------------
//                    MpdDbComponent header file 
//                      Generated 07-09-2015 
// ----------------------------------------------------------------------

/** db_classes/MpdDbComponent.h 
 ** Class for the table: component 
 **/ 

#ifndef MPDDBCOMPONENT_H 
#define MPDDBCOMPONENT_H 1 

#include "TString.h" 
#include "TDatime.h" 

#include "MpdDbConnection.h" 

class MpdDbComponent
{
 private:
	MpdDbConnection* connectionUniDb;

	TString str_component_name;
	TString* str_manufacturer_name;
	TString* str_responsible_person;
	TString* str_description;

	//Constructor
	MpdDbComponent(MpdDbConnection* connUniDb, TString component_name, TString* manufacturer_name, TString* responsible_person, TString* description);

 public:
	virtual ~MpdDbComponent(); // Destructor

	// static class functions
	static MpdDbComponent* CreateComponent(TString component_name, TString* manufacturer_name, TString* responsible_person, TString* description);
	static MpdDbComponent* GetComponent(TString component_name);
	static int DeleteComponent(TString component_name);
	static int PrintAll();

	// Getters
	TString GetComponentName(){return str_component_name;}
	TString* GetManufacturerName(){return str_manufacturer_name;}
	TString* GetResponsiblePerson(){return str_responsible_person;}
	TString* GetDescription(){return str_description;}

	// Setters
	int SetComponentName(TString component_name);
	int SetManufacturerName(TString* manufacturer_name);
	int SetResponsiblePerson(TString* responsible_person);
	int SetDescription(TString* description);
	void Print();

 ClassDef(MpdDbComponent,1);
};

#endif
