# Create a library called "" which includes the source files given in
# the array .
# The extension is already found.  Any number of sources could be listed here.
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
Set(INCLUDE_DIRECTORIES
${BASE_INCLUDE_DIRECTORIES}
${CMAKE_SOURCE_DIR}/tpc
${CMAKE_SOURCE_DIR}/mpddata
${CMAKE_SOURCE_DIR}/mpdbase
${CMAKE_SOURCE_DIR}/mpdpid
${CMAKE_SOURCE_DIR}/kalman
${CMAKE_SOURCE_DIR}/mcstack
${CMAKE_SOURCE_DIR}/lhetrack
${CMAKE_SOURCE_DIR}/base/event
${CMAKE_SOURCE_DIR}/base/field 
${CMAKE_SOURCE_DIR}/base/steer
${CMAKE_SOURCE_DIR}/base/sim
${CMAKE_SOURCE_DIR}/base/source
${CMAKE_SOURCE_DIR}/fairtools
${CMAKE_SOURCE_DIR}/mpdbase
${CMAKE_SOURCE_DIR}/nicafemto/features
${CMAKE_SOURCE_DIR}/nicafemto/features
${CMAKE_SOURCE_DIR}/nicafemto/features/io
${CMAKE_SOURCE_DIR}/nicafemto/features/hist
${CMAKE_SOURCE_DIR}/nicafemto/features/std
${CMAKE_SOURCE_DIR}/nicafemto/nicagenerators
${CMAKE_SOURCE_DIR}/nicafemto/nicagenerators/readers
${CMAKE_SOURCE_DIR}/nicafemto/nicagenerators/writers
${CMAKE_SOURCE_DIR}/nicafemto/dataformat
${CMAKE_SOURCE_DIR}/nicafemto/dataformat/compound
${CMAKE_SOURCE_DIR}/nicafemto/dataformat/formats
${CMAKE_SOURCE_DIR}/nicafemto/dataformat/formats/fair
${CMAKE_SOURCE_DIR}/nicafemto/dataformat/detector
${CMAKE_SOURCE_DIR}/nica_mpd/format
${CMAKE_SOURCE_DIR}/nica_mpd/format/fairmc
${CMAKE_SOURCE_DIR}/nica_mpd/format/unigen
${CMAKE_SOURCE_DIR}/nica_mpd/format/combined
${CMAKE_SOURCE_DIR}/nica_mpd/format/kalman
${CMAKE_SOURCE_DIR}/nica_mpd/format/kalmandst
${CMAKE_SOURCE_DIR}/nica_mpd/format/mpd
${CMAKE_SOURCE_DIR}/nica_mpd/format/mpdevent
)

Set(SYSTEM_INCLUDE_DIRECTORIES
 ${ROOT_INCLUDE_DIR} 
  ${CLHEP_INCLUDE_DIR}
 ${Boost_INCLUDE_DIRS}
)

Set(LINK_DIRECTORIES
  ${ROOT_LIBRARY_DIR}
  ${FAIRROOT_LIBRARY_DIR}
)

Include_Directories(${INCLUDE_DIRECTORIES})
Include_Directories(SYSTEM ${SYSTEM_INCLUDE_DIRECTORIES})
 
Link_Directories(${LINK_DIRECTORIES})

# List of source files
set(SRCS
combined/NicaMpdDstMCEvent.cxx
mpdevent/NicaMpdEventInterface.cxx
combined/NicaMpdDstMCEventTpcPads.cxx
mpdevent/NicaMpdEvent.cxx
mpdevent/NicaMpdEventTpcPads.cxx
mpdevent/NicaMpdTrack.cxx
mpdevent/NicaMpdTrackInterface.cxx
mpdevent/NicaMpdTrackTpcPads.cxx
mpdevent/NicaTpcSectorGeo.cxx
fairmc/NicaFairEvent.cxx
fairmc/NicaFairEventInterface.cxx
fairmc/NicaFairExtendedEvent.cxx
fairmc/NicaFairTrack.cxx
fairmc/NicaFairExtendedTrack.cxx
fairmc/NicaFairExtendedEventInterface.cxx
fairmc/NicaFairTrackInterface.cxx
fairmc/NicaFairExtendedTrackInterface.cxx
fairmc/NicaGeneratorWriteFairMC.cxx
fairmc/NicaGeneratorWriteFairMCFreez.cxx
unigen/NicaGeneratorReadUnigen.cxx
unigen/NicaGeneratorWriteUnigen.cxx
unigen/NicaUnigenEvent.cxx
unigen/NicaUnigenEventInterface.cxx
unigen/NicaUnigenSource.cxx
unigen/NicaUnigenTrack.cxx
unigen/NicaUnigenTrackInterface.cxx
unigen/UEvent.cxx
unigen/UParticle.cxx
)

Set(HEADERS )
Set(LINKDEF NicaMpdInterfaceLinkDef.h)
Set(LIBRARY_NAME NicaMpdFormat)
Set(DEPENDENCIES NicaFeatures NicaGen NicaDataFormat MpdDst MpdBase tpc MCStack)

GENERATE_LIBRARY()


