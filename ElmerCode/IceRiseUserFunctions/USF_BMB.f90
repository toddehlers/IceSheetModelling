! Get BMB as Function of Ice Thickness, GL Position and Distance.
FUNCTION GetBMB ( Model, nodenumber, BMBMultiplier) RESULT(BMBOut)
   USE types
   USE CoordinateSystems
   USE SolverUtils
   USE ElementDescription
   USE DefUtils
   IMPLICIT NONE
   TYPE(Variable_t), POINTER :: DepthSol,DistanceSol
   INTEGER, POINTER :: DepthPerm(:),DistancePerm(:),BMBPerm(:)
   REAL(kind=dp), POINTER :: DepthVal(:),DistanceVal(:)
   TYPE(Model_t) :: Model
   TYPE(Solver_t), TARGET :: Solver
   INTEGER :: nodenumber,  NMAX, i, dim
   REAL(KIND=dp) :: BMBMultiplier,   BMBOut, alpha, G,A,rho
   REAL(KIND=dp), ALLOCATABLE :: BMB0(:)
   LOGICAL :: FirstTime=.True., UnFoundFatal

   SAVE FirstTime
   SAVE BMB0
   !Get Depthwithout catching any error messages if fields don't exist
   DepthSol => VariableGet( Model % Variables, 'Depth',UnFoundFatal=UnFoundFatal)
   DepthPerm => DepthSol % Perm
   DepthVal => DepthSol % Values

   !Get Distance without catching any error messages if fields don't exist
   DistanceSol => VariableGet( Model % Variables, 'Distance',UnFoundFatal=UnFoundFatal)
   DistancePerm => DistanceSol % Perm
   DistanceVal => DistanceSol % Values
!

!   dim = CoordinateSystemDimension()
!   NMAX = COUNT( DepthPerm > 0 )
!   ALLOCATE(BMB0(NMAX))
!   DO i = 1, Model % NumberOfNodes
!     IF (DepthPerm(i)==0) CYCLE
!     IF (dim==2) THEN
!          write(*,*) 'Only do this for 3D for now.'
!     ELSE
!
!         BMB0(BMBPerm(i)) = 50.0
!     END IF
!   END DO

  !alpha = 0.5
  !rho = 1-np.exp(-0.0001*distance) #transition GL to Ambient
  !G = 0.001 ## melting away from GL relative to H^alpha
  !A = 0.1   ##melting near GL relative to H^alpha
  !bmb = thickness**(alpha)*(rho*G+(1-rho)*A)


   alpha = 0.4
   G = 0.001
   A = 0.1
   rho = 1 - exp(-0.0001* DistanceVal(DistancePerm(nodenumber)))

   BMBOut = BMBMultiplier*DepthVal(DepthPerm(nodenumber))**alpha*(rho*G+(1-rho)*A)
   !print *, BMBOut
   !write(*,*) 'BMBOut:',Model % Nodes % x(nodenumber),Model % Nodes % y(nodenumber),BMBOut

END FUNCTION GetBMB



