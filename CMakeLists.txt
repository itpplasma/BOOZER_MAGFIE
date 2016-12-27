### Basic settings
cmake_minimum_required (VERSION 2.6)

### Define the project
project (boozer_magfie)
enable_language(Fortran)

### Define paths to external libraries (load external file)
include(${CMAKE_SOURCE_DIR}/ProjectConfig.cmake.in)

### Define the name of the executable
set(PROJECT_EXE_NAME "magfie_test.x")                  # Define the name
set(EXECUTABLE_OUTPUT_PATH .)

### Set compiler flags
set(CMAKE_Fortran_MODULE_DIRECTORY ./OBJS)


### Find libraries
set(CMAKE_FIND_LIBRARY_SUFFIXES ".a")
find_library(SuperLU_lib superlu_4.1 ${SUPERLU_DIR}lib NO_DEFAULT_PATH)
find_library(Umfpack_lib umfpack ${SUITESPARSE_DIR}lib NO_DEFAULT_PATH)
find_library(Amd_lib amd ${SUITESPARSE_DIR}lib NO_DEFAULT_PATH)
find_library(Cholmod_lib cholmod ${SUITESPARSE_DIR}lib NO_DEFAULT_PATH)
find_library(Colamd_lib colamd ${SUITESPARSE_DIR}lib NO_DEFAULT_PATH)
find_library(Camd_lib camd ${SUITESPARSE_DIR}lib NO_DEFAULT_PATH)
find_library(Metis_lib metis ${SUITESPARSE_DIR}lib ${METIS_PATH} NO_DEFAULT_PATH)
find_library(Ccolamd_lib ccolamd ${SUITESPARSE_DIR}lib NO_DEFAULT_PATH)

### Source files (load external file)
include (CMakeSources.txt)

set(CMAKE_Fortran_FLAGS "-mkl -Wno-unused-dummy-argument -O3")

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
	neo_magfie_perturbation.f90
	sparse_mod.f90
  	${SUITESPARSE_SRC_FILES}
	${SUPERLU_SRC_FILES}
)

add_dependencies(${PROJECT_EXE_NAME} boozer_magfie)

### Linking
target_link_libraries(${PROJECT_EXE_NAME}
	boozer_magfie
        ${SuperLU_lib}
        ${Umfpack_lib} ${Amd_lib} ${Cholmod_lib} ${Colamd_lib} ${Camd_lib} ${Metis_lib} ${Ccolamd_lib}
	pthread
#	blas
#	lapack
)

### For make install
# install (TARGETS ${PROJECT_EXE_NAME} DESTINATION /temp/gernot_k/)
# set_property(TARGET ${PROJECT_EXE_NAME} PROPERTY INSTALL_RPATH_USE_LINK_PATH TRUE)

