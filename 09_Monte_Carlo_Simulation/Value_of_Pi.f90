PROGRAM Value_of_Pi_MC_Method
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	REAL(pr) :: pi_find_inv_trig
	
	! Computing Pi
	CALL mc_circle(1000000)
	CALL mc_circle(10000000)
	CALL mc_circle(100000000)
	CALL mc_sphere(1000000)
	CALL mc_sphere(10000000)
	CALL mc_sphere(100000000)
	
	! Printing Result
	PRINT *,'==================================='
	WRITE(*,'(2X,A29)')"Method : Inverse Triginometry"
	WRITE(*,'(2X,A23)')"Case : arctan(1) = Pi/4"
	WRITE(*,'(2X,A18,F8.6)')"The value of Pi = ", pi_find_inv_trig()
	PRINT *,'==================================='
END PROGRAM

SUBROUTINE mc_circle(n)
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	INTEGER, INTENT(IN) :: n
	REAL(pr) :: x, y, circle_func, pi
	INTEGER :: i, counter
	
	counter = 0
	DO i = 1,n
		! Random Points Generation in range (0,1)
		CALL random_number(x)
		CALL random_number(y)
		
		! Acceptinting Points within Unit Circle
		IF (circle_func(x,y) .LE. 1.0) THEN
			counter = counter + 1
		END IF
	END DO
	
	! Points within circle / Total Points = 4 * Pi
	pi = (REAL(counter)/REAL(n)) * 4.0
	
	! Printing Result
	PRINT *,'==================================='
	WRITE(*,'(2X,A31)')"Method : Monte Carlo Simulation"
	WRITE(*,'(2X,A23,I9)')"Case : Circle with n = ", n
	WRITE(*,'(2X,A18,F8.6)')"The value of Pi = ", pi
	PRINT *,'==================================='
END SUBROUTINE

SUBROUTINE mc_sphere(n)
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	INTEGER, INTENT(IN) :: n
	REAL(pr) :: x, y, z, sphere_func, pi
	INTEGER :: i, counter
	
	counter = 0
	DO i = 1,n
		! Random Points Generation in range (0,1)
		CALL random_number(x)
		CALL random_number(y)
		CALL random_number(z)
		
		! Acceptinting Points within Unit Circle
		IF (sphere_func(x,y,z) .LE. 1.0) THEN
			counter = counter + 1
		END IF
	END DO
	
	! Points within circle / Total Points = 6 * Pi
	pi = (REAL(counter)/REAL(n)) * 6.0
	
	! Printing Result
	PRINT *,'==================================='
	WRITE(*,'(2X,A31)')"Method : Monte Carlo Simulation"
	WRITE(*,'(2X,A23,I9)')"Case : Sphere with n = ", n
	WRITE(*,'(2X,A18,F8.6)')"The value of Pi = ", pi
	PRINT *,'==================================='
END SUBROUTINE

FUNCTION circle_func(x,y)
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	REAL(pr), INTENT(IN) :: x, y
	REAL(pr) :: circle_func
	
	! Defining Differential Equation
	circle_func = x**2 + y**2
END FUNCTION

FUNCTION sphere_func(x,y,z)
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	REAL(pr), INTENT(IN) :: x, y, z
	REAL(pr) :: sphere_func
	
	! Defining Differential Equation
	sphere_func = x**2 + y**2 + z**2
END FUNCTION

FUNCTION pi_find_inv_trig()
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	REAL(pr) :: pi_find_inv_trig
	
	! Computing Pi
	pi_find_inv_trig = 4.0 * ATAN(1.0) 
END FUNCTION
