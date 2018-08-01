runMC:
	g++ -g -O0 -o $@ runMC.cxx -I`root-config --incdir` -I/opt/fairsoft18/install/include -I/opt/fairroot18/install/include -I$(VMCWORKDIR)/base/event -I$(VMCWORKDIR)/base/field -I$(VMCWORKDIR)/base/sim -I$(VMCWORKDIR)/base/source -I$(VMCWORKDIR)/base/steer -I$(VMCWORKDIR)/geobase -I$(VMCWORKDIR)/parbase -I$(VMCWORKDIR)/mcstack -I$(VMCWORKDIR)/mpdfield -I$(VMCWORKDIR)/generators -I$(VMCWORKDIR)/passive -I$(VMCWORKDIR)/trackbase -I$(VMCWORKDIR)/clustering -I$(VMCWORKDIR)/mpdbase -I$(VMCWORKDIR)/mpddata -I$(VMCWORKDIR)/cpc -I$(VMCWORKDIR)/ffd -I$(VMCWORKDIR)/kalman -I$(VMCWORKDIR)/lhetrack -I$(VMCWORKDIR)/geane -I$(VMCWORKDIR)/strawendcap -I$(VMCWORKDIR)/sts -I$(VMCWORKDIR)/ndet -I$(VMCWORKDIR)/etof -I$(VMCWORKDIR)/emc -I$(VMCWORKDIR)/tpc -I$(VMCWORKDIR)/tof -I$(VMCWORKDIR)/zdc -I$(VMCWORKDIR)/fairtools -I$(VMCWORKDIR)/mpdpid -L`root-config --libdir` -L"/usr/lib64" -L/opt/fairsoft18/install/lib -L/opt/fairroot18/install/lib -L$(VMCWORKDIR)/build/lib -lMpdData -lCpc -lEmc -lCluster -lEtof -lFfd -lLHETrack -lStrawendcap -lSts -ltpc -lZdc -lTof -lPassive -lKalman -lMpdGen -lMpdGeneralGenerator -lMpdBase -lFairTools -lMpdField -lMCStack -lTrkBase -lParBase -lGeoBase -lGen -lBase -lGeane -lXMLIO -lMinuit -lGeom -lGeomPainter -lVMC -lEG `root-config --glibs` -lpthread


#librunMC.a: $(patsubst %.cxx,%.o,$(wildcard *.cxx))
#	ar rv `root-config --glibs` $@ $^ 

#%.o: %.cxx
#	gcc -Wall -pg -g -c -pthread -m64 -I$(VMCWORKDIR)/base -I$(VMCWORKDIR)/parbase -I$(VMCWORKDIR)/field -I$(VMCWORKDIR)/generators -I`root-config --incdir` -I. $<

#clean:
#	@rm -f *.o *.bck *.a
