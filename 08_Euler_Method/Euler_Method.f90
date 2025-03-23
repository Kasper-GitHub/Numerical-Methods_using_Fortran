PROGRAM Euler_Method

	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	REAL(pr) :: x_0, y_0, h = -0.01, val = 0.95
		
	! Initial Values
	x_0 = 1.05
	y_0 = 0.70
	
	! Performing RK-4 Operations
	CALL euler(x_0,y_0,h,val)
	CALL modified_euler(x_0,y_0,h,val)
	
END PROGRAM

FUNCTION f(x,y)

	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	REAL(pr), INTENT(IN) :: x, y
	REAL(pr) :: f
	
	! Defining Differential Equation
	f = -9.80*(SIN(x))/y
	
	END FUNCTION

SUBROUTINE euler(x,y,h,val)

	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	REAL(pr), INTENT(IN) :: h, val, x, y
	REAL(pr) :: k, f, x_val, y_val
	INTEGER :: i, n
	
	! Assigning Values
	x_val = x
	y_val = y
	
	! Determining Number of Intervals
	n = (val-x_val)/h + 1
	
	! Calculating Iteration Terms using Euler Method
	DO i = 1,n
		k = h * f(x_val,y_val)
		
		x_val = x_val + h
		y_val = y_val + k
	END DO
	
	! Printing Result
	PRINT *,'================================================================='
	WRITE(*,'(2X,A41,F5.2)')"Method : Euler's Method with step size = ", h
	WRITE(*,'(2X,A22,F4.2,A4,F8.6)')"The value of y at x = ", x_val," is ", y_val
	PRINT *,'================================================================='
END SUBROUTINE


















SUBROUTINE modified_euler(x,y,h,val)

	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	REAL(pr), INTENT(IN) :: h, val, x, y
	REAL(pr), PARAMETER :: tolerance = 10E-08
	REAL(pr) :: f, x_mod, y_mod, temp, x_val, y_val
	INTEGER :: i, n
	
	! Assigning Values
	x_val = x
	y_val = y
	
	! Determining Number of Intervals
	n = (val-x_val)/h + 1
	
	! Calculating Iteration Terms using Modified Euler Method
	DO i = 1,n
		x_mod = x_val + h
		y_mod = y_val + h * f(x_val,y_val)
		
		!! Consideration so that while loop becomes True 
		temp = y_mod + 2*tolerance 
		
		DO WHILE (ABS(y_mod-temp) .GE. tolerance)
			temp = y_mod
			y_mod = y_val + 0.5 * h * (f(x_val,y_val)+f(x_mod,y_mod))
		END DO
		
		x_val = x_mod
		y_val = y_mod
	END DO
	
	! Printing Result
	10 FORMAT (2X,A50,F5.2)
	20 FORMAT (2X,A22,F4.2,A4,F8.6)
	PRINT *,'================================================================='
	WRITE(*,10)"Method : Modified Euler's Method with step size = ",h
	WRITE(*,20)"The value of y at x = ", x_val," is ", y_val
	PRINT *,'================================================================='	
	
END SUBROUTINE





