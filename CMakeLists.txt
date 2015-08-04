### Basic settings
cmake_minimum_required (VERSION 2.6)

### Define the project
project (boozer_magfie)
enable_language(Fortran)

### Define the name of the executable
set(PROJECT_EXE_NAME "magfie_test.x")                  # Define the name
set(EXECUTABLE_OUTPUT_PATH .)

### Set compiler flags
set(CMAKE_Fortran_MODULE_DIRECTORY ./OBJS)

set(CMAKE_Fortran_FLAGS "-g -Wall -Wno-unused-dummy-argument -O2")

### Define executable
add_executable(${PROJECT_EXE_NAME}
  magfie_test.f90
  )
  
### Define library
add_library(boozer_magfie
	inter_interfaces.f90
	magfie.f90
	neo_magfie.f90
	neo_mod.f90
	neo_modules.f90
	neo_sub.f90
	nrutil.f90
	pspline.f90
	solve_system.f90
	test_function.f90    # New (from /proj/plasma/Neo2/Code/2011-06)
	spline_int.f90       # New
	spline_cof.f90       # New
)

add_dependencies(${PROJECT_EXE_NAME} boozer_magfie)

### Linking
target_link_libraries(${PROJECT_EXE_NAME}
	boozer_magfie
	openblas
	pthread
)

### For make install
# install (TARGETS ${PROJECT_EXE_NAME} DESTINATION /temp/gernot_k/)
# set_property(TARGET ${PROJECT_EXE_NAME} PROPERTY INSTALL_RPATH_USE_LINK_PATH TRUE)

