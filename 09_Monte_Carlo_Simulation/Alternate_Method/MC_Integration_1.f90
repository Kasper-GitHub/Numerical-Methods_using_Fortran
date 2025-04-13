PROGRAM MC_Integration
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	REAL(pr) :: random_generator, area_total, answer
	REAL(pr) :: x, y, f, a, b, f_min, f_max
	INTEGER :: i, counter = 0, n
	
	! Range and Number of points
	a = -2
	b = 2
	n = 10000000
	f_min = 0
	f_max = 5
	
	! Monte-Carlo Simulation
	DO i = 1,n
		! Random Points Generation
		x = random_generator(a,b)
		y = random_generator(f_min,f_max)
		
		! Acceptinting Points under the curve
		IF (y .LE. f(x)) THEN
			counter = counter + 1
		END IF
	END DO
	
	! Area_Under_Curve = (Points under Curve / Total Points) * Total_Area
	area_total = (b-a) * (f_max-f_min) 
	answer = (REAL(counter)/REAL(n)) * area_total
	
	! Printing Result
	PRINT *,'========================================='
	WRITE(*,'(2X,A32)')"Method : Monte Carlo Integration"
	WRITE(*,'(2X,A31,F8.6)')"The value of the integral, I = ", answer
	WRITE(*,'(2X,A24,F8.6)')"The value of Pi = 2*I = ", 2*answer
	PRINT *,'========================================='
END PROGRAM

FUNCTION random_generator(a,b)
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	REAL(pr), INTENT(IN) :: a, b
	REAL(pr) :: random_generator, uniform
	
	CALL random_number(uniform)
	random_generator = a + (b-a) * uniform
END FUNCTION

FUNCTION f(x)
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	REAL(pr), INTENT(IN) :: x
	REAL(pr) :: f
	
	f = SQRT(1-x**2)
END FUNCTION
