# boozmn input and fixture

The `.nc` reader accepts symmetric, axisymmetric booz_xform files and libneo
conversions on a uniform booz_xform half grid. It normalizes them to the
symmetric legacy .bc convention (`inp_swi=8`, `lab_swi=8`). Asymmetric fields,
non-axisymmetric fields and libneo's explicit radial-grid extension are rejected.
Boozer chartmap input is not implemented. Pressure-gradient and mean-Jacobian
profiles are initialized to zero because they are not imported by this reader.

`boozmn_circ.nc` contains the same field coefficients as the shipped public
`tok-synch2-n0.bc` equilibrium. To regenerate its arrays, use Python with numpy,
scipy and netCDF4, and libneo at b40485684a9763d523cd7d5fb5c97e2b2c598705:

```sh
git clone https://github.com/itpplasma/libneo.git /tmp/libneo-boozmn
git -C /tmp/libneo-boozmn checkout b40485684a9763d523cd7d5fb5c97e2b2c598705
cd tokamak_circ
cp tok-synch2-n0.bc circ.bc
PYTHONPATH=/tmp/libneo-boozmn/python python3 -m libneo.bc_to_booz_xform circ.bc boozmn_circ.nc
rm circ.bc
```

The converter records `bc2boozmn_source`. For these files, `pmns_b` is Boozer
minus geometric toroidal angle in radians. The local `lmnc` multiplies this by
`nfp/(2*pi)`, where `nfp` is the number of field periods. Files without the
libneo attribute use the opposite sign, as written by the
[booz_xform NetCDF writer](https://github.com/hiddenSymmetries/booz_xform/blob/21aff118735de4255641aa1b64fc7ab35e8c9767/src/_booz_xform/write_boozmn.cpp#L118).
Its C++ in-memory angle convention differs from its file convention.

The [libneo converter](https://github.com/itpplasma/libneo/blob/b40485684a9763d523cd7d5fb5c97e2b2c598705/python/libneo/bc_to_booz_xform.py)
defines the radial staggering and current units. The reader selects profiles at
`jlist`, reconstructs `s=(jlist-1.5)/(ns_b-1)`, and uses the edge toroidal flux
only once when computing `psi_pr=abs(phi_b(edge))/(2*pi)`.

CTest compares all 14 outputs of `magfie` at three surfaces and five angle pairs
against the `.bc` reader. The tolerance is 5e-4 relative to each scalar or each
vector's maximum absolute component, plus 1e-8 absolute in the returned units.
This allows radial interpolation differences caused by the five-significant-digit
surface labels in the text `.bc`; the NetCDF half-grid labels are exact.
Coefficient tests check the currents, flux, phase normalization and grid
separately. A generated fixture with the standard booz_xform layout and sign
exercises the same coefficient and field checks; it is a format conversion of
the reference fixture, not an independent booz_xform equilibrium calculation.
