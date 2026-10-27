### Basic settings
cmake_minimum_required (VERSION 3.10)

### Define the project
project (boozer_magfie)
enable_language(Fortran)

### Find libraries
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/cmake")

find_package(SuiteSparse REQUIRED)
find_package(NetCDF REQUIRED COMPONENTS Fortran)

### Define the name of the executable
set(PROJECT_EXE_NAME "magfie_test.x")                  # Define the name
set(EXECUTABLE_OUTPUT_PATH .)

### Set compiler flags
set(CMAKE_Fortran_MODULE_DIRECTORY ./OBJS)

### Source files (load external file)
include (CMakeSources.txt)

### Define executables
add_executable(${PROJECT_EXE_NAME} magfie_test.f90)
add_executable(test_booz_xform_integration.x test_booz_xform_integration.f90)
add_executable(test_boozmn_field.x test_boozmn_field.f90)

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
add_dependencies(test_booz_xform_integration.x boozer_magfie)
add_dependencies(test_boozmn_field.x boozer_magfie)

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
	NetCDF::NetCDF_Fortran
)

# Link metis if not on Mac
if(NOT APPLE)
	target_link_libraries(boozer_magfie metis)
endif()

target_link_libraries(${PROJECT_EXE_NAME}
	boozer_magfie
	pthread
)

target_link_libraries(test_booz_xform_integration.x
	boozer_magfie
	pthread
)

target_link_libraries(test_boozmn_field.x
	boozer_magfie
	pthread
)

### Tests
enable_testing()
set(BOOZMN_FIXTURE "${PROJECT_SOURCE_DIR}/tokamak_circ/boozmn_circ.nc")
add_test(NAME test_boozmn_field
  COMMAND test_boozmn_field.x
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)
set_tests_properties(test_boozmn_field PROPERTIES
  ENVIRONMENT "BOOZMN_FIXTURE=${BOOZMN_FIXTURE}"
)

# Keep generated output separate for each field reader.
file(READ "${PROJECT_SOURCE_DIR}/tokamak_circ/neo.in" FIELD_CONTROL)
string(REGEX REPLACE "200 +theta_n" "64                  theta_n" FIELD_CONTROL "${FIELD_CONTROL}")
string(REGEX REPLACE "200 +phi_n" "8                   phi_n" FIELD_CONTROL "${FIELD_CONTROL}")
foreach(FORMAT bc nc)
  set(FIELD_DIR "${CMAKE_BINARY_DIR}/field_${FORMAT}")
  file(MAKE_DIRECTORY "${FIELD_DIR}")
  if(FORMAT STREQUAL "bc")
    set(FIELD_NAME "tok-synch2-n0.bc")
  else()
    set(FIELD_NAME "boozmn_circ.nc")
  endif()
  string(REPLACE "tok-synch2-n0.bc" "${FIELD_NAME}" CONTROL "${FIELD_CONTROL}")
  file(WRITE "${FIELD_DIR}/neo.in" "${CONTROL}")
  configure_file("tokamak_circ/${FIELD_NAME}" "${FIELD_DIR}/${FIELD_NAME}" COPYONLY)
endforeach()
set(FIELD_REFERENCE "${CMAKE_BINARY_DIR}/field_bc/reference.dat")
add_test(NAME test_field_bc
  COMMAND test_booz_xform_integration.x write "${FIELD_REFERENCE}")
set_tests_properties(test_field_bc PROPERTIES
  WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/field_bc" FIXTURES_SETUP field_reference)
add_test(NAME test_field_nc
  COMMAND test_booz_xform_integration.x compare "${FIELD_REFERENCE}")
set_tests_properties(test_field_nc PROPERTIES
  WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/field_nc" FIXTURES_REQUIRED field_reference)

add_executable(test_boozmn_formats.x test_boozmn_formats.f90)
target_link_libraries(test_boozmn_formats.x boozer_magfie)
foreach(VARIANT standard missing-bmnc bad-jlist asymmetric nonaxisymmetric)
  set(FORMAT_FIXTURE "${CMAKE_BINARY_DIR}/${VARIANT}.nc")
  if(VARIANT STREQUAL "standard")
    set(FORMAT_FIXTURE "${CMAKE_BINARY_DIR}/field_standard/standard.nc")
  endif()
  add_test(NAME fixture_${VARIANT}
    COMMAND test_boozmn_formats.x "${BOOZMN_FIXTURE}" "${FORMAT_FIXTURE}" "${VARIANT}")
  set_tests_properties(fixture_${VARIANT} PROPERTIES FIXTURES_SETUP ${VARIANT})
  if(VARIANT STREQUAL "standard")
    add_test(NAME test_boozmn_standard COMMAND test_boozmn_field.x)
    set_tests_properties(test_boozmn_standard PROPERTIES
      ENVIRONMENT "BOOZMN_FIXTURE=${FORMAT_FIXTURE}" FIXTURES_REQUIRED standard)
  else()
    if(VARIANT STREQUAL "missing-bmnc")
      set(EXPECTED "bmnc_b")
    elseif(VARIANT STREQUAL "bad-jlist")
      set(EXPECTED "invalid jlist")
    elseif(VARIANT STREQUAL "asymmetric")
      set(EXPECTED "asymmetric input is not supported")
    else()
      set(EXPECTED "non-axisymmetric input is not supported")
    endif()
    add_test(NAME test_reject_${VARIANT} COMMAND "${CMAKE_COMMAND}"
      "-DREADER=$<TARGET_FILE:test_boozmn_field.x>" "-DFIXTURE=${FORMAT_FIXTURE}"
      "-DEXPECTED=${EXPECTED}" -P "${PROJECT_SOURCE_DIR}/cmake/ExpectBoozmnFailure.cmake")
    set_tests_properties(test_reject_${VARIANT} PROPERTIES FIXTURES_REQUIRED ${VARIANT})
  endif()
endforeach()
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/field_standard")
string(REPLACE "boozmn_circ.nc" "standard.nc" CONTROL "${CONTROL}")
file(WRITE "${CMAKE_BINARY_DIR}/field_standard/neo.in" "${CONTROL}")
add_test(NAME test_field_standard
  COMMAND test_booz_xform_integration.x compare "${FIELD_REFERENCE}")
set_tests_properties(test_field_standard PROPERTIES
  WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/field_standard"
  FIXTURES_REQUIRED "field_reference;standard")
