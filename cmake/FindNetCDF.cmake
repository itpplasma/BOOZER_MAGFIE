# Find NetCDF Fortran library

find_program(NF_CONFIG nf-config)

if(NF_CONFIG)
  execute_process(
    COMMAND ${NF_CONFIG} --prefix
    OUTPUT_VARIABLE NetCDF_PREFIX
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )

  execute_process(
    COMMAND ${NF_CONFIG} --fflags
    OUTPUT_VARIABLE NetCDF_Fortran_INCLUDE_FLAGS
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )

  execute_process(
    COMMAND ${NF_CONFIG} --flibs
    OUTPUT_VARIABLE NetCDF_Fortran_LIBS_FLAGS
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )

  find_path(NetCDF_Fortran_INCLUDE_DIR
    NAMES netcdf.mod netcdf.f90
    HINTS ${NetCDF_PREFIX}/include /usr/include
  )

  find_library(NetCDF_Fortran_LIBRARY
    NAMES netcdff
    HINTS ${NetCDF_PREFIX}/lib /usr/lib
  )

  find_library(NetCDF_C_LIBRARY
    NAMES netcdf
    HINTS ${NetCDF_PREFIX}/lib /usr/lib
  )
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(NetCDF
  REQUIRED_VARS NetCDF_Fortran_LIBRARY NetCDF_C_LIBRARY
)

if(NetCDF_FOUND)
  if(NOT TARGET NetCDF::NetCDF_Fortran)
    add_library(NetCDF::NetCDF_Fortran UNKNOWN IMPORTED)
    set_target_properties(NetCDF::NetCDF_Fortran PROPERTIES
      IMPORTED_LOCATION "${NetCDF_Fortran_LIBRARY}"
      INTERFACE_LINK_LIBRARIES "${NetCDF_C_LIBRARY}"
      INTERFACE_INCLUDE_DIRECTORIES "${NetCDF_Fortran_INCLUDE_DIR}"
    )
  endif()
endif()

mark_as_advanced(NetCDF_Fortran_LIBRARY NetCDF_C_LIBRARY NetCDF_Fortran_INCLUDE_DIR)
