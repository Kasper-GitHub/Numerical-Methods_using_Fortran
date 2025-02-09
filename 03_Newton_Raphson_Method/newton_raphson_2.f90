! Name : Ankush Bhowmik
! Program Name : Root Finding within [-5,5] for f(x) = x*sin(x) using Newton Raphson Method

PROGRAM newton_raphson_2

	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(4), max_iter = 100, a = 5
	INTEGER :: i, root_count = 0
	REAL(pr) :: guess, root
	REAL(pr), PARAMETER :: tolerance = 1.0E-06
	REAL(pr), DIMENSION(5) :: root_store, guess_store
	LOGICAL :: root_found, flag
	
	DO i = 1, size(guess_store)
		root_store(i) = 0.0
		guess_store(i) = 0.0
	END DO
	
	! Iterate over multiple initial guesses
  	DO i = -a, a, 1
  		guess = i
  		flag = .FALSE.
  		
  		CALL newton_raphson(guess,root,flag)
  		
  		! Check if this root is new (not close to previously found roots)
  		IF (.NOT. root_found(root, root_store, root_count, flag)) THEN
  			root_count = root_count + 1
        		root_store(root_count) = root
        		guess_store(root_count) = i
  		END IF
  	END DO
  	
  	PRINT*,"The roots of f(x) = x*sin(x) are as follows : "
  	PRINT*,(root_store(i), i = 1, root_count)
  	PRINT*,"for the following respective guess values"
  	PRINT*,(guess_store(i), i = 1, root_count)
  	PRINT'("Root searching complete within [", I2,",",I2,"] range.")',-a,a

END PROGRAM

! Newton-Raphson Method Implementation
SUBROUTINE newton_raphson(guess,root,flag)
  	IMPLICIT NONE
  	INTEGER :: j
  	INTEGER, PARAMETER :: pr = selected_real_kind(4), max_iter = 100
  	REAL(pr) :: x, f, df, x_0
  	REAL(pr), INTENT(IN) :: guess
	REAL(pr), INTENT(OUT) :: root
  	REAL(pr), PARAMETER :: tolerance = 1.0E-06
  	LOGICAL, INTENT(INOUT) :: flag
	
  	x = guess
  	j = 0
  	
	DO
		j = j + 1
		IF ((ABS(df(x)) < ABS(tolerance)) .OR. (j > max_iter)) THEN
			! Solution is not converging to a root / Stationary Point.
			flag = .TRUE.
			EXIT
		END IF
		
		x_0 = x
		x = x - f(x)/df(x)
		IF (ABS(x-x_0) < tolerance)  EXIT
	END DO
	
	root = x
	IF (ABS(root) < tolerance)	THEN
		root = 0.0
	END IF
END SUBROUTINE newton_raphson

! Function f(x) = x*sin(x)
FUNCTION f(x)
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(4)
	REAL(pr), INTENT(IN) :: x
	REAL(pr) :: f
	
	f = x*sin(x)
END FUNCTION f

! Derivative f'(x) = sin(x) + x*cos(x)
FUNCTION df(x)
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(4)
	REAL(pr), INTENT(IN) :: x
	REAL(pr) :: df
	
	df = x*cos(x)+sin(x)
END FUNCTION df

! Checking if the root is already found within tolerance
LOGICAL FUNCTION root_found(root, root_store, root_count, flag)
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(4), a = 5
	INTEGER :: k = 0
	INTEGER, INTENT(IN) :: root_count
	REAL(pr), INTENT(IN) :: root
	REAL(pr), PARAMETER :: tolerance = 1.0E-06
	REAL(pr), DIMENSION(5), INTENT(IN) :: root_store
	LOGICAL, INTENT(IN) :: flag
	
	root_found = .FALSE.
	
	IF ((.NOT. flag) .AND. (ABS(root) < a)) THEN
		DO k = 1, root_count
			IF (ABS(root - root_store(k)) < tolerance) THEN
				root_found = .TRUE.
				EXIT
			END IF
			
		END DO
	ELSE
		root_found = .TRUE.
	END IF
END FUNCTION root_found
