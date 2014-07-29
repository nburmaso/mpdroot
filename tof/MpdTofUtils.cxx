//------------------------------------------------------------------------------------------------------------------------
#include <assert.h>

#include "TFile.h"
#include "TObjArray.h"
#include "TObjString.h"
#include "TSystem.h"
#include "TXMLEngine.h"
#include "TXMLDocument.h"
#include "TXMLAttr.h"
#include "TGeoManager.h"
#include "TGeoMatrix.h"            
#include "TGeoPhysicalNode.h"
#include "TGeoBBox.h" 
  
#include "FairRunAna.h"
#include "FairBaseParSet.h"
#include "FairRuntimeDb.h"

#include "MpdTofPoint.h"
#include "MpdEtofPoint.h"
#include "MpdTofUtils.h"

//------------------------------------------------------------------------------------------------------------------------
Bool_t		MpdTofUtils::Find(PadMap& map, padPar** ptr, Int_t UID)
{
  	PadIter iter = map.find(UID);
  	if(iter != map.end())
    	{
      		*ptr = &(iter->second);
      		return true;
    	}
	
return false;
}
//------------------------------------------------------------------------------------------------------------------------
Int_t		MpdTofUtils::GetNeighboringPadUID(PadMap& map, Int_t UID, k_side side)
{
	assert(UID != Absent);
	
	PadIter iter = map.find(UID);
	if(iter != map.end())
	{
      		return (iter->second).neighboring[(int)side];
	}
	
return Absent;		
}
//------------------------------------------------------------------------------------------------------------------------	
TVector3	MpdTofUtils::GetPadCenter(PadMap& map, Int_t UID)
{
  	assert(UID != Absent);
	
	PadIter iter = map.find(UID); 
	if(iter != map.end())
	{
		return (iter->second).center;
	}
	
return TVector3();
}
//------------------------------------------------------------------------------------------------------------------------
void		MpdTofUtils::FindCrossPads(PadMap& map, TH2D *h2)
{
	const static Double_t thresh = 1.2; // porog na soseda
	Double_t minUp, minDown, minLeft, minRight, val;
	Int_t	 uidUp, uidDown, uidLeft, uidRight, curUID;
	padPar *par, *curPar;
	
	for(PadIter iter = map.begin(); iter != map.end(); iter++) 	// cycle by pads
	{
		par = (&iter->second);
		
		minUp = minDown = minLeft = minRight = 1.e6;
		uidUp = uidDown = uidLeft = uidRight = Absent;

		for(Int_t side = Up; side <= Left; side++)			// cycle by sides
		{
			for(PadIter current = map.begin(); current != map.end(); current++)
			{				
				if( current->first == iter->first) continue; // don't check itself
				curPar = (&current->second);
				curUID = current->first;				
				CheckAllSide(Up,    par, curPar, minUp,    uidUp,    curUID);				
				CheckAllSide(Down,  par, curPar, minDown,  uidDown,  curUID);
				CheckAllSide(Left,  par, curPar, minLeft,  uidLeft,  curUID);
				CheckAllSide(Right, par, curPar, minRight, uidRight, curUID);	
			}	
		}

      		if(h2)
		{
	  		h2->Fill(minUp, Up); 	h2->Fill(minDown, Down);
	  		h2->Fill(minLeft, Left); h2->Fill(minRight, Right);
		}	
	
      		par->neighboring[Up] 	= (minUp < thresh) 	? uidUp 	: Absent;
      		par->neighboring[Down] 	= (minDown < thresh) 	? uidDown 	: Absent;
      		par->neighboring[Left] 	= (minLeft < thresh) 	? uidLeft 	: Absent;
      		par->neighboring[Right] = (minRight < thresh) 	? uidRight 	: Absent;

    	} // cycle by pads
}
//------------------------------------------------------------------------------------------------------------------------
TVector3	MpdTofUtils::_charToVector(const char* data)
{
	TString buffer = data; TVector3 retvalue;
	TObjArray *array = buffer.Tokenize(" "); assert(array->GetEntries() == 3);

	retvalue.SetX(((TObjString*) array->At(0))->GetString().Atof());
	retvalue.SetY(((TObjString*) array->At(1))->GetString().Atof());
	retvalue.SetZ(((TObjString*) array->At(2))->GetString().Atof());
	
	delete array;	
return retvalue;
}
//------------------------------------------------------------------------------------------------------------------------
void		MpdTofUtils::WriteParamToXML(const char* flnm, const char* rootNodeName, const char* comment, 
						RegVec& _parRegions, ModMMap& _parModules, PadMap& _parPads)
{
	TXMLEngine *engine = new TXMLEngine();
	TXMLDocument *doc = (TXMLDocument*)engine->NewDoc();
	engine->AddDocComment(doc, comment);
	TXMLNode*  root = (TXMLNode*) engine->NewChild(0, 0, rootNodeName);
	engine->DocSetRootElement(doc, root);

	TXMLNode *reg, *mod, *pad; padPar *par; ModIter itM;
	Int_t curRegion, curModule, count;

        for(RegIter itR = _parRegions.begin(); itR != _parRegions.end(); ++itR)	// REGIONs
	{
		curRegion = itR->ID;
		reg = (TXMLNode*) engine->NewChild(root, 0, "reg");
		engine->NewIntAttr(reg, "id", curRegion);				// region id
		engine->NewAttr(reg, 0, "POS0", _vectorToChar(itR->center).Data()); 	// center position
		engine->NewAttr(reg, 0, "pA", _vectorToChar(itR->point[0]).Data());	// point A
		engine->NewAttr(reg, 0, "pB", _vectorToChar(itR->point[1]).Data());	// point B
		engine->NewAttr(reg, 0, "pC", _vectorToChar(itR->point[2]).Data());	// point C				
		engine->NewAttr(reg, 0, "pD", _vectorToChar(itR->point[3]).Data());	// point D

		count = _parModules.count(curRegion);
		itM = _parModules.find(curRegion);		
		for(Int_t i = 0; i < count; i++, itM++)				// MODULEs
		{
			curModule = itM->second.module;
			mod = (TXMLNode*) engine->NewChild(reg, 0, "mod");
			engine->NewIntAttr(mod, "reg", curRegion);
			engine->NewIntAttr(mod, "id", curModule);
			engine->NewAttr(mod, 0, "POS0", _vectorToChar(itM->second.center).Data());
			engine->NewAttr(mod, 0, "Perp", _vectorToChar(itM->second.perp).Data());	// perp. to module vector
			engine->NewAttr(mod, 0, "pA", _vectorToChar(itM->second.point[0]).Data());	// point A
			engine->NewAttr(mod, 0, "pB", _vectorToChar(itM->second.point[1]).Data());	// point B
			engine->NewAttr(mod, 0, "pC", _vectorToChar(itM->second.point[2]).Data());	// point C				
			engine->NewAttr(mod, 0, "pD", _vectorToChar(itM->second.point[3]).Data());	// point D

                        for(PadIter iter = _parPads.begin(); iter != _parPads.end(); ++iter) 		//CELLs
			{
				par = &(iter->second);
				if(!(curRegion == par->region && curModule == par->module))continue;

				pad = (TXMLNode*) engine->NewChild(mod, 0, "pad");
				engine->NewIntAttr(pad, "uid", iter->first);
				engine->NewIntAttr(pad, "reg", curRegion);
				engine->NewIntAttr(pad, "mod", curModule);
				engine->NewIntAttr(pad, "id", par->pad);
				engine->NewAttr(pad, 0, "POS0", _vectorToChar(par->center).Data());
				engine->NewAttr(pad, 0, "pA", _vectorToChar(par->point[0]).Data()); // point A
				engine->NewAttr(pad, 0, "pB", _vectorToChar(par->point[1]).Data()); // point B
				engine->NewAttr(pad, 0, "pC", _vectorToChar(par->point[2]).Data()); // point C	
				engine->NewAttr(pad, 0, "pD", _vectorToChar(par->point[3]).Data()); // point D
				engine->NewIntAttr(pad, "Up", par->neighboring[Up]);
				engine->NewIntAttr(pad, "Right", par->neighboring[Right]);
				engine->NewIntAttr(pad, "Down", par->neighboring[Down]);
				engine->NewIntAttr(pad, "Left", par->neighboring[Left]);
			}
		} // modules
	} // regions

	engine->SaveDoc(doc, flnm);
	delete engine;
}
//------------------------------------------------------------------------------------------------------------------------
bool		MpdTofUtils::ReadParamFromXML(const char* flnm, RegVec& _parRegions, ModMMap& _parModules, PadMap& _parPads)
{
	cout<<"\n-I- [MpdTofUtils::ReadParamFromXML] Read TOF parameters from \""<<flnm<<"\" file."<<endl;
	_parPads.clear(); _parModules.clear(); _parRegions.clear(); // Cleanup all containers

	if (gSystem->AccessPathName(flnm) ) { cout<<"\n File \""<<flnm<<"\" does not exist."; return false; }  

	TXMLEngine *engine = new TXMLEngine;
	TXMLDocument *doc = (TXMLDocument*) engine->ParseFile(flnm);
        if(!doc){
            delete engine;
            return false;
        }

	TXMLNode*  root = (TXMLNode*) engine->DocGetRootElement(doc);
        if(!root){
            delete engine;
            return false;
        }

	TXMLNode *reg, *mod, *pad;
	reg = (TXMLNode*) engine->GetChild(root); 			// first region node
	while(reg)
	{
		_setRegionPar(reg, engine, _parRegions);		// add region parameters entry
		mod = (TXMLNode*) engine->GetChild(reg); 		// first module node
		while(mod)
		{
			_setModulePar(mod, engine, _parModules);	// add module parameters entry
			pad = (TXMLNode*) engine->GetChild(mod);	// first pad node
			while(pad)
			{
				_setPadPar(pad, engine, _parPads);	// add pad parameters entry
				pad = (TXMLNode*) engine->GetNext(pad); // next pad node
			}
			mod = (TXMLNode*) engine->GetNext(mod);		// next mod node
		}
		reg = (TXMLNode*) engine->GetNext(reg);			// next region node
	}

        engine->FreeDoc(doc);
        delete engine;
return true;
}
//------------------------------------------------------------------------------------------------------------------------
void		MpdTofUtils::_setRegionPar(TXMLNode* node, TXMLEngine *engine, RegVec& _parRegions)
{
	regionData param; TVector3 pos; TString key;
	TXMLAttr *attr = (TXMLAttr*)engine->GetFirstAttr(node); // first attribute
	while(attr) 
	{		
		key = engine->GetAttrName(attr);	

		if(key ==  "id")  			param.ID	= atoi(engine->GetAttrValue(attr));
		else 
		{
			pos = _charToVector(engine->GetAttrValue(attr));
				if(key == "POS0") 	param.center 	= pos;
			else 	if(key == "pA")  	param.point[0] 	= pos;
			else 	if(key == "pB")  	param.point[1] 	= pos;
			else 	if(key == "pC")  	param.point[2] 	= pos;
			else 	if(key == "pD")  	param.point[3] 	= pos;
		}
		
		attr = (TXMLAttr*)engine->GetNextAttr(attr); // next attribute
	}	

	_parRegions.push_back(param); 
}
//------------------------------------------------------------------------------------------------------------------------
void		MpdTofUtils::_setModulePar(TXMLNode* node, TXMLEngine *engine, ModMMap& _parModules)
{
	modPar param; TVector3 pos; TString key; 
	TXMLAttr *attr = (TXMLAttr*)engine->GetFirstAttr(node); // first attribute
	while(attr) 
	{		
		key = engine->GetAttrName(attr);	

		if	(key ==  "id")  		param.module 	= atoi(engine->GetAttrValue(attr));
		else if(key ==  "reg")  		param.region  	= atoi(engine->GetAttrValue(attr));
		else 
		{
			pos = _charToVector(engine->GetAttrValue(attr));
				if(key == "POS0") 	param.center 	= pos;
			else 	if(key == "Perp")  	param.perp 	= pos;
			else 	if(key == "pA")  	param.point[0] 	= pos;
			else 	if(key == "pB")  	param.point[1] 	= pos;
			else 	if(key == "pC")  	param.point[2] 	= pos;
			else 	if(key == "pD")  	param.point[3] 	= pos;
		}
		
		attr = (TXMLAttr*)engine->GetNextAttr(attr); // next attribute
	}	

	_parModules.insert(ModMMap::value_type(param.region, param));
}
//------------------------------------------------------------------------------------------------------------------------
void		MpdTofUtils::_setPadPar(TXMLNode* node, TXMLEngine *engine, PadMap& _parPads)
{
	padPar param; TVector3 pos; TString key; Int_t UID;
	TXMLAttr *attr = (TXMLAttr*)engine->GetFirstAttr(node); // first attribute
	while(attr) 
	{		
		key = engine->GetAttrName(attr);	

		if	(key ==  "uid")  	UID		= atoi(engine->GetAttrValue(attr));
		else if(key ==  "reg") 		param.region	= atoi(engine->GetAttrValue(attr));
		else if(key ==  "mod")  	param.module	= atoi(engine->GetAttrValue(attr));
		else if(key ==  "id")  		param.pad	= atoi(engine->GetAttrValue(attr));
		else if(key ==  "Up")  		param.neighboring[Up] = atoi(engine->GetAttrValue(attr));
		else if(key ==  "Right") 	param.neighboring[Right] = atoi(engine->GetAttrValue(attr));
		else if(key ==  "Down")  	param.neighboring[Down] = atoi(engine->GetAttrValue(attr));
		else if(key ==  "Left")  	param.neighboring[Left] = atoi(engine->GetAttrValue(attr));
		else 
		{
			pos = _charToVector(engine->GetAttrValue(attr));
				if(key == "POS0") 	param.center 	= pos;
			else 	if(key == "pA")  	param.point[0] 	= pos;
			else 	if(key == "pB")  	param.point[1] 	= pos;
			else 	if(key == "pC")  	param.point[2] 	= pos;
			else 	if(key == "pD")  	param.point[3] 	= pos;
		}
		
		attr = (TXMLAttr*)engine->GetNextAttr(attr); // next attribute
	}	

	_parPads.insert(PadMap::value_type(UID, param));
}
//------------------------------------------------------------------------------------------------------------------------
