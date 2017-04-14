runReco:
	g++ -g -O0 -o $@ runRecoCpc30.cxx -I`root-config --incdir` -I/opt/fairsoft/install/include -I$(VMCWORKDIR)/base/event -I$(VMCWORKDIR)/base/field -I$(VMCWORKDIR)/base/sim -I$(VMCWORKDIR)/base/source -I$(VMCWORKDIR)/base/steer -I$(VMCWORKDIR)/geobase -I$(VMCWORKDIR)/parbase -I$(VMCWORKDIR)/mcstack -I$(VMCWORKDIR)/mpdfield -I$(VMCWORKDIR)/generators -I$(VMCWORKDIR)/passive -I$(VMCWORKDIR)/trackbase -I$(VMCWORKDIR)/clustering -I$(VMCWORKDIR)/mpdbase -I$(VMCWORKDIR)/mpddata -I$(VMCWORKDIR)/cpc -I$(VMCWORKDIR)/ffd -I$(VMCWORKDIR)/kalman -I$(VMCWORKDIR)/lhetrack -I$(VMCWORKDIR)/geane -I$(VMCWORKDIR)/strawendcap -I$(VMCWORKDIR)/sts -I$(VMCWORKDIR)/ndet -I$(VMCWORKDIR)/etof -I$(VMCWORKDIR)/emc -I$(VMCWORKDIR)/tpc -I$(VMCWORKDIR)/tof -I$(VMCWORKDIR)/zdc -I$(VMCWORKDIR)/fairtools -L`root-config --libdir` -L"/usr/lib64" -L/opt/fairsoft/install/lib -L$(VMCWORKDIR)/build/lib -lMpdData -lCpc -lEmc -lCluster -lEtof -lFfd -lLHETrack -lStrawendcap -lSts -ltpc -lZdc -lTof -lPassive -lKalman -lGen -lMpdBase -lBase -lMpdBase -lFairTools -lMpdField -lMCStack -lTrkBase -lParBase -lGeoBase -lGeane -lGen -lXMLIO -lMinuit -lGeom -lGeomPainter -lVMC -lEG `root-config --glibs` -lpthread


#librunMC.a: $(patsubst %.cxx,%.o,$(wildcard *.cxx))
#	ar rv `root-config --glibs` $@ $^ 

#%.o: %.cxx
#	gcc -Wall -pg -g -c -pthread -m64 -I$(VMCWORKDIR)/base -I$(VMCWORKDIR)/parbase -I$(VMCWORKDIR)/field -I$(VMCWORKDIR)/generators -I`root-config --incdir` -I. $<

#clean:
#	@rm -f *.o *.bck *.a
