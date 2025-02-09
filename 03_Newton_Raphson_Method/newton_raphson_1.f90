! Name : Ankush Bhowmik
! Program Name : Evaluating x = 7^(1/5) using Newton Raphson Method

PROGRAM newton_raphson_1
	
	IMPLICIT NONE
	INTEGER :: i, guess_index
	INTEGER, PARAMETER :: pr = selected_real_kind(4), max_iter = 100
	COMPLEX(pr) :: guess, f, root, guess_store(5)
	COMPLEX(pr), PARAMETER :: pi = 2.0 * ACOS(0.0), tolerance = (1.0E-8, 1.0E-8)
	LOGICAL :: flag
	
	! Guess Values
	guess_store(1) = (1.5,0)
	guess_store(2) = (0.5,1.5)
	guess_store(3) = (0.5,-1.5)
	guess_store(4) = (-1.2,1)
	guess_store(5) = (-1.2,-1)
	
	! Iterate over multiple initial guesses
	DO guess_index = 1,5
		guess = guess_store(guess_index)
		flag = .FALSE.
		
		PRINT *,'================================================'
		PRINT'("> Case -",X,I2,X,"for x = 7^(1/5) i.e. f(x) = x^5 - 7")',guess_index
		PRINT *,'------------------------------------------------'
		
		CALL root_finding(guess,root,flag)

		IF (flag) THEN
			PRINT*, "Current Initial Guess skipped ..."
		ELSE IF (ABS(AIMAG(root)) < REAL(tolerance)) THEN
			PRINT'("Root is real for Initial Guess = (", F10.7,",",F10.7,")")',REAL(guess),AIMAG(guess)
			print'("The approximate solution is ",F10.7)',REAL(root)
		ELSE
			PRINT'("Root is imaginary for Initial Guess = (", F10.7,",",F10.7,")")',REAL(guess),AIMAG(guess)
			PRINT'("The approximate solution is (", F10.7,",",F10.7,")")',REAL(root),AIMAG(root)
		END IF
	END DO
	
END PROGRAM

! Function f(x) = x^5 - 7
FUNCTION f(x) RESULT(func)
	INTEGER, PARAMETER :: pr = selected_real_kind(4)
	COMPLEX(pr), INTENT(IN) :: x
	COMPLEX(pr) :: func
	
	func = x**5 - 7
END FUNCTION f

! Newton-Raphson Method Implementation
SUBROUTINE root_finding(guess,root,flag)
	INTEGER :: i
	INTEGER, PARAMETER :: pr = selected_real_kind(4), max_iter = 100
	COMPLEX(pr) :: x, f, df, x_0
	COMPLEX(pr), INTENT(IN) :: guess
	COMPLEX(pr), INTENT(OUT) :: root
	COMPLEX(pr), PARAMETER :: pi = 2.0 * ACOS(0.0), tolerance = (1.0E-8, 1.0E-8), h = (0.01,0.01)
	LOGICAL, INTENT(INOUT) :: flag
	
	x = guess
	i = 0
	
	DO
		i = i + 1
		df = (f(x+h) - f(x-h)) / (2.0 * h)	! Finding Derivative, f'(x)
		IF ((ABS(df) < ABS(tolerance)) .OR. (i > max_iter)) THEN
			PRINT*,"Solution is not converging to a root / Method not applicable."
			PRINT'("Current Initial Guess = (", F10.7,",",F10.7,")")',REAL(guess),AIMAG(guess)
			flag = .TRUE.
			EXIT
		END IF
		
		x_0 = x
		x = x - f(x)/df
		IF (ABS(x-x_0) < ABS(tolerance)) EXIT
	END DO
	
	! x is the root for the chosen initial guess value
	root = x
END SUBROUTINE root_finding
