MPDROOT=/home/rsalmin/MPD/mpdRoot

ROOTFLAGS = $(shell root-config --cflags)
ROOTLIBS  = $(shell root-config --libs )
ROOTGLIBS = $(shell root-config --glibs ) -lGed -lEve -lRGL -lTreePlayer -lTreeViewer -lGeom -lEG -lXMLIO -lVMC -lMinuit -lGeomPainter

MPDINCDIRS = base parbase field generators passive sts tpc etof fsa bbc cpc tof strawendcap zdc ffd emc kalman lhetrack mpddata
MPDINCLUDE = $(addprefix -I$(MPDROOT)/, $(MPDINCDIRS) )

#MPDLIBSNAMES = $(subst EventDisplay, ,$(subst lib, ,$(basename $(notdir $(shell ls $(MPDROOT)/build/lib/*.so)))))
MPDLIBSNAMES = Base Gen  MpdBase  Passive  Tof Bbc Ffd  GeoBase MpdData Sft  tpc Cpc \
        Field Kalman Mpdgenerators Strawendcap TrkBase Emc Fsa  LHETrack NDet Sts  Zdc Etof  Geane  MCStack  \
        ParBase 
# EventDisplay Stt

MPDLIBS = -L$(MPDROOT)/build/lib $(addprefix -l, $(MPDLIBSNAMES))


CXXFLAGS = $(ROOTFLAGS) $(MPDINCLUDE)
LDFLAGS = $(MPDLIBS) $(ROOTGLIBS)

 
All: main show done

main: geometry_v1.cpp runMCTrack.cpp main.cpp tpcSimulation.cpp 

show:  showFoundClustersQA3.cpp show.cpp

done:
	@echo **Done**

clean:
	rm -f *.o main show
