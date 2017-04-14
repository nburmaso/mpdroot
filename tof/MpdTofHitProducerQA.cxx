//------------------------------------------------------------------------------------------------------------------------
#include <iostream>

#include <TFile.h>

#include "FairLogger.h" 

#include "MpdTofHitProducerQA.h"
using namespace std;
//------------------------------------------------------------------------------------------------------------------------
MpdTofHitProducerQA::MpdTofHitProducerQA(const char *flnm, bool isEndcap)
: fFlnm(flnm), fIsEndcap(isEndcap)
{
      	fList.SetOwner(); //  all objects will be deleted whenever the collection itself is delete.	
      	
	Add(hOccup = new TH1D(mangling("Occupancy"), "occupancy per strips;occupancy;Events", 100, -0.5, 99.5));
	Add(hMergedTimes = new TH2D(mangling("MergedTimes"), "Merged hits on strip times test;faster hit time, ns;slower hit time, ns", 1000, 5., 105., 1000, 5., 105.));

    	Add(effSingleHit = new TEfficiency(mangling("effSingleHit"), "Efficiency single hit;R, cm;Side", 1000, -0.1, 1.)); 
	Add(effDoubleHit = new TEfficiency(mangling("effDoubleHit"), "Efficiency double hit;R, cm;Side", 1000, -0.1, 1.));
		
	Add(hDistance = new TH1D(mangling("Distance"), "Distance between strips;M, cm;Side", 1000, 0., 100.));  
	Add(hStrips = new TH2D(mangling("Strips"), ";Z, cm;#phi, rads", 2000, -300., 300., 500, -3.5, 3.5));	    		

	Add(hNeighborPair = new TH2D(mangling("NeighborPair"), "Neighbor strip pairs test;stripID1;stripID2", 30, -0.5, 29.5, 30, -0.5, 29.5));    
	Add(hXYSmeared = new TH2D(mangling("XYSmeared"), "Smeared XY (single hit) test;#Delta, cm;#DeltaZ, cm", 1000, 0., 5., 1000, -1., 1.));
	Add(hXYSmeared2 = new TH2D(mangling("XYSmeared2"), "Smeared XY (single hit) test;X, cm;Y, cm", 1000, -180., 180., 1000, -180., 180.));		
	Add(hXYSmearedDouble = new TH2D(mangling("XYSmearedDouble"), "Smeared XY (double hit) test;#Delta, cm;#DeltaZ, cm", 1000, 0., 5., 1000, -3., 3.));
	Add(hXYSmearedDouble2 = new TH2D(mangling("XYSmearedDouble2"), "Smeared XY (double hit) test;X, cm;Y, cm", 1000, -180., 180., 1000, -180., 180.));		
	Add(hEtaPhi = new TH2D(mangling("EtaPhi"), ";#eta;#phi, degree", 1000, -6., 6., 1000, -181., 181.));	
	Add(hRZ = new TH2D(mangling("RZ"), ";Z, cm;R, cm", 1000, -300., 300., 1000, 15., 160.));
}
//------------------------------------------------------------------------------------------------------------------------			
void	MpdTofHitProducerQA::Finish()
{
	FairLogger::GetLogger()->Info(MESSAGE_ORIGIN, " [MpdTofHitProducerQA::Finish] Update  %s file. ", fFlnm.Data());
	TFile *ptr = gFile;
	TFile file(fFlnm.Data(), "RECREATE");
	fList.Write(); 
	file.Close();
	gFile = ptr;
}
//------------------------------------------------------------------------------------------------------------------------
	
