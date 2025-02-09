! Name : Ankush Bhowmik
! Program Name : Root Finding for f(x) = x**3 - 5*x + 3 using Bisection Method

PROGRAM root_finding_bisection_1

	IMPLICIT NONE
	
	INTEGER :: i
	REAL, PARAMETER :: tolerance = 0.000001 
	REAL :: x_m, x_a, x_b, y_m, y_a, y_b, f
	
	! Initial Values
	i = 0		!! Iteration Counter
	x_a = 1.0	!! Lower Bound of Interval
	x_b = 2.0	!! Upper Bound of Interval
	y_a = f(x_a)
	y_b = f(x_b)
	
	PRINT*,"Problem Statement : f(x) = x**3 - 5*x + 3"
	PRINT*,"Interval for root finding : [", x_a, ",", x_b, "]"
	
	! Implementing Bisection Method
	IF (ABS(y_a) < tolerance) THEN		!! Case-1 (x_a is the root)
		PRINT*,"Root within the interval is ", x_a
	ELSE IF (ABS(y_b) < tolerance) THEN	!! Case-2 (x_b is the root)
		PRINT*,"Root within the interval is ", x_b
	ELSE IF (y_a*y_b > 0.0) THEN		!! Case-3 (Unsuitable Range)
		PRINT*,"Root cannot be found as unsuitable interval provided."
	ELSE					!! Case-4 (Successive Bisections)
		DO
			i = i + 1
			x_m = (x_a + x_b) / 2
			y_m = f(x_m)
			y_a = f(x_a)
			
			!! Case-1 (x_m is the root)
			IF (ABS(y_m) < tolerance) THEN
				PRINT*,"Root within the interval is ", x_m
				PRINT*,"Number of iterations used = ", i
				EXIT
			!! Case-2 (Root between x_a & x_m )
			ELSE IF (y_a*y_m < 0.0) THEN
				x_b = x_m
			!! Case-3 (Root between x_m & x_b )
			ELSE	
				x_a = x_m
			END IF
			
			!! Checking Precision of the Root
			IF (ABS(x_b - x_a) < tolerance) THEN 
				PRINT*,"Root within the interval is ", x_m
				PRINT*,"Number of iterations used = ", i
				EXIT
			END IF
		END DO
	END IF
	
END PROGRAM

REAL FUNCTION f(x)

	IMPLICIT NONE
	REAL, INTENT(IN) :: x
	f = x**3 - 5*x + 3
	
END FUNCTION f
