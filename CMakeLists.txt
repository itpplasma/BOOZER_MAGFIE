### Basic settings
cmake_minimum_required (VERSION 3.10)

### Define the project
project (boozer_magfie)
enable_language(Fortran)

### Find libraries
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/cmake")

find_package(SuiteSparse REQUIRED)

### Define the name of the executable
set(PROJECT_EXE_NAME "magfie_test.x")                  # Define the name
set(EXECUTABLE_OUTPUT_PATH .)

### Set compiler flags
set(CMAKE_Fortran_MODULE_DIRECTORY ./OBJS)

### Source files (load external file)
include (CMakeSources.txt)

### Define executable
add_executable(${PROJECT_EXE_NAME} magfie_test.f90)

### Define library
add_library(boozer_magfie STATIC
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
)

add_dependencies(${PROJECT_EXE_NAME} boozer_magfie)

find_package(BLAS REQUIRED)
find_package(LAPACK REQUIRED)
find_package(OpenMP REQUIRED COMPONENTS C Fortran)

### Linking
target_link_libraries(boozer_magfie
	OpenMP::OpenMP_C
	OpenMP::OpenMP_Fortran
	${SUITESPARSE_LIBRARIES}
	BLAS::BLAS
	LAPACK::LAPACK
)

# Link metis if not on Mac
if(NOT APPLE)
	target_link_libraries(boozer_magfie metis)
endif()

target_link_libraries(${PROJECT_EXE_NAME}
	boozer_magfie
	pthread
)
