CC=g++
CFLAGS=-c -Wall -I/*add_your_path*/fairsoft/install/include/root6 -I/*add_your_path*/mpdroot/mpddst/MpdMiniEvent
LDFLAGS=-L/*add_your_path*/mpdroot/mpddst/MpdMiniEvent -lMpdMiniDst -L/*add_your_path*/fairsoft/install/lib/root -lCore -lRIO -lNet -lHist -lGraf -lGraf3d -lGpad -lTree -lRint -lPostscript -lMatrix -lPhysics -lMathCore -lThread -pthread -lm -ldl -rdynamic -lEG -lMinuit -lrt
SOURCES=mpd_create_rm.cpp
OBJECTS=$(SOURCES:.cpp=.o)
EXECUTABLE=mpd_create_rm

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

.cpp.o:
	$(CC) -std=gnu++11 $(CFLAGS) $< -o $@
