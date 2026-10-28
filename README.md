# BOOZER_MAGFIE

Fortran library for evaluating the magnetic field of a toroidal equilibrium
given in Boozer coordinates, based on the field routines of NEO and NEO-2. Its
`magfie` routine takes a point `x = (s, phi, theta)` and returns the field magnitude, the Jacobian
`sqrtg`, the derivatives of the field magnitude and the covariant,
contravariant and curl components of the unit field vector.

## Input

The field is read as Fourier coefficients in Boozer coordinates, selected in
the NEO control file `neo.in`:

- `.bc` text files (Boozer coefficient format)
- `boozmn` NetCDF files from booz_xform or libneo, for stellarator-symmetric,
  axisymmetric fields only

See [tokamak_circ/boozmn_fixture.md](tokamak_circ/boozmn_fixture.md) for the
conventions of the NetCDF reader and how the test fixture was generated.

## Building

Requirements: CMake 3.10 or newer, a Fortran and C compiler with OpenMP,
SuiteSparse (UMFPACK), NetCDF-Fortran, BLAS and LAPACK, and METIS on Linux.

    mkdir build && cd build
    cmake .. -DCMAKE_BUILD_TYPE=Release
    make
    ctest --output-on-failure

This builds the static library `libboozer_magfie.a`, the example program
`magfie_test.x` and the test programs.

## Example

`magfie_test.x` reads `neo.in` and `magfie_test.in` (flux surface label and
numbers of angle points) from the working directory and writes the field on
three flux surfaces to `magfie_spl.dat`. The circular tokamak case in
`tokamak_circ/` provides both input files:

    cd tokamak_circ
    ../build/magfie_test.x

## Tests

The CTest suite checks that the `.bc` and NetCDF readers give the same field
for the circular tokamak equilibrium in `tokamak_circ/`, and that unsupported
NetCDF inputs (asymmetric, non-axisymmetric, missing coefficients, invalid
surface list) are rejected.

## License

MIT, see [LICENSE](LICENSE). The UMFPACK and SuperLU wrappers in `contrib/`
carry their own licenses.
