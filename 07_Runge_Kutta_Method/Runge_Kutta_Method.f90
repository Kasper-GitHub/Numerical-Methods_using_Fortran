PROGRAM RK4_Method
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(4)
	REAL(pr) :: x_0, y_0, h, val = 3.0
	
	! Initial Values
	x_0 = 0
	y_0 = 0
	
	! Performing RK-4 Operations
	CALL rk4(x_0,y_0,0.15,val)
	CALL rk4(x_0,y_0,0.5,val)
END PROGRAM

FUNCTION f(x,y)
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(4)
	REAL(pr), INTENT(IN) :: x, y
	REAL(pr) :: f
	
	! Defining Differential Equation
	f = 9.80 - 0.5*y**2
END FUNCTION

SUBROUTINE rk4(x,y,h,val)
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(4)
	REAL(pr), INTENT(IN) :: h, val
	REAL(pr), INTENT(INOUT) :: x, y
	REAL(pr) :: k, k1, k2, k3, k4, f
	INTEGER :: i, n
	
	! Determining Number of Intervals
	n = (val - x)/h
	
	! Calculating Next Term using RK4
	DO i = 1,n
		k1 = h * f(x,y)
		k2 = h * f(x + 0.5*h, y + 0.5*k1)
		k3 = h * f(x + 0.5*h, y + 0.5*k2)
		k4 = h * f(x + h, y + k3)
		
		k = ( k1 + 2*k2 + 2*k3 + k4 )/6
		
		x = x + h
		y = y + k
	END DO
	
	! Printing Result
	10 FORMAT(2X,A22,F3.1,A30,F8.6)
	PRINT *,'================================================================='
	WRITE(*,'(2X,A19,F4.2)')"Case : Step-size = ", h
	WRITE(*,10)"The value of y at x = ",x," (found using RK-4 Method) is ", y
	PRINT *,'================================================================='
END SUBROUTINE
