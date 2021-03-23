if("${PROJECT_NAME}" STREQUAL "MPDROOT")
	set(CMAKE_SOURCE_FAIRMCSTACK "${CMAKE_SOURCE_DIR}/mcstack")
else()
	set(CMAKE_SOURCE_FAIRMCSTACK "${CMAKE_SOURCE_DIR}/examples/common/mcstack")
endif()
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/nicafemto/cmake/modules"  ${CMAKE_MODULE_PATH})
include(NicaFemto)
SetHeaderVariables(${CMAKE_SOURCE_DIR}/nicafemto)
set(NICAFEMTO_MPDCUTS 
    ${CMAKE_SOURCE_DIR}/nica_mpd/cuts
    ${CMAKE_SOURCE_DIR}/nica_mpd/cuts/paircuts/common
    ${CMAKE_SOURCE_DIR}/nica_mpd/cuts/paircuts/common/tpc_cylinder
    ${CMAKE_SOURCE_DIR}/nica_mpd/cuts/paircuts/full
    ${CMAKE_SOURCE_DIR}/nica_mpd/cuts/paircuts/full/tpc_modular
    ${CMAKE_SOURCE_DIR}/nica_mpd/cuts/paircuts/minidst
    ${CMAKE_SOURCE_DIR}/nica_mpd/cuts/trackcuts
    ${CMAKE_SOURCE_DIR}/nica_mpd/cuts/cutmonitors
)

set(NICAFEMTO_MPDFORMAT
    ${CMAKE_SOURCE_DIR}/nica_mpd/format/combined
    ${CMAKE_SOURCE_DIR}/nica_mpd/format/minidst
    ${CMAKE_SOURCE_DIR}/nica_mpd/format/hbtformat
    ${CMAKE_SOURCE_DIR}/nica_mpd/format/mpdevent
    ${CMAKE_SOURCE_DIR}/nica_mpd/format/mpdmc
    ${CMAKE_SOURCE_DIR}/nica_mpd/format/unigen
    ${CMAKE_SOURCE_DIR}/nica_mpd/format/
)
set (NICAFEMTO_MPDHELPERS
    ${CMAKE_SOURCE_DIR}/nica_mpd/nica_helpers
)
add_subdirectory(cuts)
add_subdirectory(format)
add_subdirectory(nica_helpers)
add_subdirectory(tasks)