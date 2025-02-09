! Name : Ankush Bhowmik
! Program Name : Fitting of y = a + bx to given data-set using Least Square Method

PROGRAM least_square_fitting_1

	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(4)
	INTEGER:: n,i
	REAL(pr), DIMENSION(6) :: x,y
	REAL(pr):: s_x=0.0,s_x2=0.0,s_y=0.0,s_y2=0.0,s_xy=0.0,a,b,r
	
	! Data Points
	x = (/0.5,1.0,1.5,2.0,2.5,3.0/)
	y = (/0.31,0.82,1.29,1.85,2.51,3.02/)
	n = size(x)
	
	! Performing Different Summation Operations
	DO i=1,n
	  s_x = s_x + x(i)
	  s_y = s_y + y(i)
	  s_xy = s_xy +( x(i) * y(i) )
	  s_x2 = s_x2 + ( x(i)**2 )
	  s_y2 = s_y2 + ( y(i)**2 )
	END DO
	
	! Determining Least Square Estimates of Parameters : a, b
	a = ((s_y * s_x2)-(s_x * s_xy)) / ((n * s_x2)-(s_x**2))
	b = ((n * s_xy)-(s_x * s_y)) / ((n * s_x2)-(s_x**2))
	
	! Determining Correlation Coefficient
	r = ((n * s_xy)-(s_x * s_y)) / SQRT(((n * s_x2)-(s_x)**2)*((n * s_y2)-(s_y)**2))
	
	! Output
	PRINT *,'Equation of Fitted Line : y = a + bx '
	PRINT *,'Where,'
	PRINT *,'a = y-intercept = ', a
	PRINT *,'b = slope = ', b
	PRINT *,'With Correlation Coefficient (r) = ', r
	
END PROGRAM
