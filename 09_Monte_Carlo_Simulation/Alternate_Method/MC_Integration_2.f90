PROGRAM MC_Integration
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	REAL(pr) :: random_generator, area_total, answer
	REAL(pr) :: x, y, f, a, b, f_min = 5, f_max = -5
	INTEGER :: i, counter_pos = 0, counter_neg = 0, n
	
	! Range and Number of points
	a = 0.00000001
	b = 30
	n = 100000000
	
	! Finding Minima and Maxima of the Function
	x = a
	DO WHILE (x .LE. b)
		f_min  = MIN(f_min,f(x))
		f_max  = MAX(f_max,f(x))
		x = x + (b-a)/n
	END DO
	
	! Monte-Carlo Simulation
	DO i = 1,n
		! Random Points Generation
		x = random_generator(a,b)
		y = random_generator(f_min,f_max)
		
		! Acceptinting Points under the curve
		IF (y>=0.0 .AND. y<=f(x)) THEN
			counter_pos = counter_pos + 1
		ELSE IF (y<0.0 .AND. y>=f(x)) THEN
			counter_neg = counter_neg + 1
		END IF
	END DO
	
	! Area_Under_Curve = (Points under Curve / Total Points) * Total_Area
	area_total = (b-a) * (f_max-f_min)
	answer = (REAL(counter_pos-counter_neg)/REAL(n)) * area_total
	
	! Printing Result
	PRINT *,'========================================='
	WRITE(*,'(2X,A32)')"Method : Monte Carlo Integration"
	WRITE(*,'(2X,A31,F8.6)')"The value of the integral, I = ", answer
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
	
	f = EXP(-x)*LOG(x)
END FUNCTION
