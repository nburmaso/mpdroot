add_subdirectory(mpd)
add_subdirectory(tof)
add_subdirectory(physical_analysis)
add_subdirectory(ndet)

INSTALL(DIRECTORY ../ DESTINATION ${CMAKE_INSTALL_PREFIX}/macro/
	PATTERN "../mpd_scheduler" EXCLUDE
	PATTERN "../profiling" EXCLUDE
)
