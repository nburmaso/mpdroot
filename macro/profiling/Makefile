runMC:
	gcc -g -o $@ runMC.cxx -I`root-config --incdir` -I$(VMCWORKDIR)/base -I$(VMCWORKDIR)/geobase -I$(VMCWORKDIR)/parbase -I$(VMCWORKDIR)/mcstack -I$(VMCWORKDIR)/field -I$(VMCWORKDIR)/generators -I$(VMCWORKDIR)/passive -I$(VMCWORKDIR)/trackbase -I$(VMCWORKDIR)/mpdbase -I$(VMCWORKDIR)/mpddata -I$(VMCWORKDIR)/bbc -I$(VMCWORKDIR)/cpc -I$(VMCWORKDIR)/fsa -I$(VMCWORKDIR)/ffd -I$(VMCWORKDIR)/kalman -I$(VMCWORKDIR)/lhetrack -I$(VMCWORKDIR)/mpdgenerators -I$(VMCWORKDIR)/geane -I$(VMCWORKDIR)/strawendcap -I$(VMCWORKDIR)/sts -I$(VMCWORKDIR)/bbc -I$(VMCWORKDIR)/fsa -I$(VMCWORKDIR)/ffd -I$(VMCWORKDIR)/cpc -I$(VMCWORKDIR)/ndet -I$(VMCWORKDIR)/etof -I$(VMCWORKDIR)/emc -I$(VMCWORKDIR)/stt -I$(VMCWORKDIR)/tpc -I$(VMCWORKDIR)/tof -I$(VMCWORKDIR)/zdc -I$(VMCWORKDIR)/fairtools -L`root-config --libdir` -L"/usr/lib64" -L$(VMCWORKDIR)/build/lib -lMpdData -lBbc -lCpc -lEmc -lEtof -lFfd -lFsa -lLHETrack -lSft -lStrawendcap -lSts -lStt -ltpc -lZdc -lTof -lNDet -lPassive -lKalman -lMpdgenerators -lMpdBase -lBase -lMpdBase -lFairTools -lField -lMCStack -lTrkBase -lParBase -lGeoBase -lGeane -lGen -lXMLIO -lMinuit -lGeom -lGeomPainter -lVMC -lEG `root-config --glibs` -liomp5 -lpthread -lirc -limf -lsvml -lintlc -ltcmalloc


#librunMC.a: $(patsubst %.cxx,%.o,$(wildcard *.cxx))
#	ar rv `root-config --glibs` $@ $^ 

#%.o: %.cxx
#	gcc -Wall -pg -g -c -pthread -m64 -I$(VMCWORKDIR)/base -I$(VMCWORKDIR)/parbase -I$(VMCWORKDIR)/field -I$(VMCWORKDIR)/generators -I`root-config --incdir` -I. $<

#clean:
#	@rm -f *.o *.bck *.a
