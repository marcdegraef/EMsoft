

module pytest

use local
use constants
use crystal
use typedefs

IMPLICIT NONE


contains

recursive subroutine py_CalcMatrices(latparam, sgnum, xtal_system, dmt, rmt, dsm, rsm, trigmat, vol)
!DEC$ ATTRIBUTES DLLEXPORT :: CalcMatrices

real(kind=dbl), INTENT(IN)     :: latparam(6)
integer(kind=irg), INTENT(IN)  :: sgnum
integer(kind=irg), INTENT(OUT) :: xtal_system
real(kind=dbl), INTENT(OUT)    :: dmt(3,3), rmt(3,3), dsm(3,3), rsm(3,3), trigmat(3,3), vol

type(unitcell),pointer  :: cell

nullify(cell)
allocate(cell)

cell%a = latparm(1)
cell%b = latparm(2)
cell%c = latparm(3)
cell%alpha = latparm(4)
cell%beta = latparm(5)
cell%gamma = latparm(6)
cell%sgnum = sgnum
cell%xtal_system = GetEMsoftXtalSystem(sgnum)

call CalcMatrices(cell)

dmt = cell%dmt
rmt = cell%rmt
dsm = cell%dsm
rsm = cell&rsm
trigmat = cell%trigmat
vol = cell%voll

deallocate(cell)

end subroutine py_CalcMatrices

end module pytest