/*
 * NicaMpdInterfaceLinkDef.h
 *
 *  Created on: 28 mar 2017
 *      Author: Daniel Wielanek
 *		E-mail: daniel.wielanek@gmail.com
 *		Warsaw University of Technology, Faculty of Physics
 */

#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

#pragma link C++ class NicaMpdEvent+;
#pragma link C++ class NicaMpdEventInterface+;
#pragma link C++ class NicaMpdEventTpcPads+;
#pragma link C++ class NicaMpdTrack+;
#pragma link C++ class NicaMpdTrackInterface+;
#pragma link C++ class NicaMpdTrackTpcPads+;
#pragma link C++ class NicaTpcSectorGeo+;

#pragma link C++ class NicaMpdDstMCEvent+;
#pragma link C++ class NicaMpdDstMCEventTpcPads+;

#pragma link C++ class NicaFairEvent+;
#pragma link C++ class NicaFairEventInterface+;
#pragma link C++ class NicaFairExtendedEvent+;
#pragma link C++ class NicaFairExtendedEventInterface+;
#pragma link C++ class NicaFairTrack+;
#pragma link C++ class NicaFairExtendedTrack+;
#pragma link C++ class NicaFairExtendedTrackInterface+;
#pragma link C++ class NicaFairTrackInterface+;
#pragma link C++ class NicaGeneratorWriteFairMC+;
#pragma link C++ class NicaGeneratorWriteFairMCFreez+;

#pragma link C++ class NicaGeneratorReadUnigen+;
#pragma link C++ class NicaGeneratorWriteUnigen+;
#pragma link C++ class NicaUnigenEvent+;
#pragma link C++ class NicaUnigenEventInterface+;
#pragma link C++ class NicaUnigenSource+;
#pragma link C++ class NicaUnigenTrack+;
#pragma link C++ class NicaUnigenTrackInterface+;
#pragma link C++ class UEvent+;
#pragma link C++ class UParticle+;


#endif /* INTERFACES_MPDROOT_NICAMPDINTERFACELINKDEF_H_ */
