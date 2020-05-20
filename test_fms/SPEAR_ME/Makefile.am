# This is an automake file for the test_fms/fms2_io directory of the FMS
# package.

# J.Liptak, R.Menzel

# Find the fms_mod.mod file.
AM_CPPFLAGS = -I${top_builddir}/mpp -I${top_builddir}/fms -I${top_builddir}/diag_manager -I${top_builddir}/time_manager
# Link to the FMS library.
LDADD = ${top_builddir}/libFMS/libFMS.la

# Build this test program.
check_PROGRAMS = test_diag_manager

# This is the source code for the test.
test_diag_manager_SOURCES = test_diag_manager.F90

EXTRA_DIST = test_fms2_io.sh

# Run the test program.
TESTS = test_fms2_io.sh

# Set srcdir as evironment variable to be reference in the job script
TESTS_ENVIRONMENT = srcdir="$(top_srcdir)"

CLEANFILES = *.mod *.nc *.nc.* input.nml logfile.000000.out
