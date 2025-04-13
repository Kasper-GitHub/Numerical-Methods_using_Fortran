PROGRAM MC_Integration
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	INTEGER, PARAMETER :: n = 100000000
	REAL(pr), DIMENSION(n) :: x_unscaled, x_scaled
	REAL(pr) :: f_sum = 0, answer, f, a, b
	INTEGER :: i, counter = 0
	
	! Range
	a = 0.00000001
	b = 30
	
	! Random Points Generation
	CALL RANDOM_NUMBER(x_unscaled)
	x_scaled = a + (b-a) * x_unscaled
	
	! Monte-Carlo Simulation
	DO i = 1,n
		!print*,i, x_unscaled(i), x_scaled(i),f_sum
		f_sum = f_sum + f(x_scaled(i))
	END DO
	
	! Integral Value = Range * Function_Sum / No. of Points
	answer = (b-a) * (f_sum/REAL(n))
	
	! Printing Result
	PRINT *,'=========================================='
	WRITE(*,'(2X,A32)')"Method : Monte Carlo Integration"
	WRITE(*,'(2X,A31,F9.6)')"The value of the integral, I = ", answer
	PRINT *,'=========================================='
END PROGRAM

FUNCTION f(x)
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	REAL(pr), INTENT(IN) :: x
	REAL(pr) :: f
	
	f = EXP(-x)*LOG(x)
END FUNCTION
