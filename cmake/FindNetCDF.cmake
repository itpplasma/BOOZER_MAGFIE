# Find the Fortran module and its C runtime across distribution and custom prefixes.
find_package(PkgConfig QUIET)
if(PkgConfig_FOUND)
  pkg_check_modules(PC_NETCDFF QUIET netcdf-fortran)
  pkg_check_modules(PC_NETCDF QUIET netcdf)
endif()
find_program(NF_CONFIG nf-config HINTS "${NetCDF_ROOT}/bin" "$ENV{NetCDF_ROOT}/bin")
if(NF_CONFIG)
  execute_process(COMMAND "${NF_CONFIG}" --prefix
    OUTPUT_VARIABLE NetCDF_PREFIX OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND "${NF_CONFIG}" --fflags
    OUTPUT_VARIABLE NetCDF_FFLAGS OUTPUT_STRIP_TRAILING_WHITESPACE)
  separate_arguments(NetCDF_FFLAGS UNIX_COMMAND "${NetCDF_FFLAGS}")
  foreach(FLAG IN LISTS NetCDF_FFLAGS)
    if(FLAG MATCHES "^-I(.+)$")
      list(APPEND NetCDF_INCLUDE_HINTS "${CMAKE_MATCH_1}")
    endif()
  endforeach()
endif()
find_path(NetCDF_Fortran_INCLUDE_DIR NAMES netcdf.mod
  HINTS ${NetCDF_INCLUDE_HINTS} ${PC_NETCDFF_INCLUDE_DIRS}
  "${NetCDF_PREFIX}/include" "${NetCDF_ROOT}/include" "$ENV{NetCDF_ROOT}/include")
find_library(NetCDF_Fortran_LIBRARY NAMES netcdff
  HINTS ${PC_NETCDFF_LIBRARY_DIRS} "${NetCDF_PREFIX}/lib" "${NetCDF_ROOT}/lib")
find_library(NetCDF_C_LIBRARY NAMES netcdf
  HINTS ${PC_NETCDF_LIBRARY_DIRS} "${NetCDF_PREFIX}/lib" "${NetCDF_ROOT}/lib")
if(NetCDF_Fortran_INCLUDE_DIR AND NetCDF_Fortran_LIBRARY AND NetCDF_C_LIBRARY)
  set(NetCDF_Fortran_FOUND TRUE)
endif()
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(NetCDF
  REQUIRED_VARS NetCDF_Fortran_INCLUDE_DIR NetCDF_Fortran_LIBRARY NetCDF_C_LIBRARY
  HANDLE_COMPONENTS)
if(NetCDF_FOUND AND NOT TARGET NetCDF::NetCDF_Fortran)
  add_library(NetCDF::NetCDF_Fortran UNKNOWN IMPORTED)
  set_target_properties(NetCDF::NetCDF_Fortran PROPERTIES
    IMPORTED_LOCATION "${NetCDF_Fortran_LIBRARY}"
    INTERFACE_LINK_LIBRARIES "${NetCDF_C_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${NetCDF_Fortran_INCLUDE_DIR}")
endif()
mark_as_advanced(NF_CONFIG NetCDF_Fortran_INCLUDE_DIR NetCDF_Fortran_LIBRARY NetCDF_C_LIBRARY)
