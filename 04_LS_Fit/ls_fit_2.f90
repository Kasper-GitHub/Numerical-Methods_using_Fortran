! Name : Ankush Bhowmik
! Program Name : Fitting of y = ax^n to given data-set using Least Square Method

PROGRAM least_square_fitting_2

	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(4)
	INTEGER:: n,i
	REAL(pr), DIMENSION(8) :: x,y
	REAL(pr):: s_x=0.0,s_x2=0.0,s_y=0.0,s_y2=0.0,s_xy=0.0,a,b
	
	! Data Points
	x = (/10,20,30,40,50,60,70,80/)
	y = (/1.06,1.33,1.52,1.68,1.81,1.91,2.01,2.11/)
	n = size(x)
	
	! Performing Different Summation Operations
	DO i=1,n
	  s_x = s_x + LOG(x(i))
	  s_y = s_y + LOG(y(i))
	  s_xy = s_xy +( LOG(x(i)) * LOG(y(i)) )
	  s_x2 = s_x2 + ( (LOG(x(i)))**2 )
	  s_y2 = s_y2 + ( (LOG(y(i)))**2 )
	END DO
	
	! Determining Least Square Estimates of Parameters : a, b
	a = ((s_y * s_x2)-(s_x * s_xy)) / ((n * s_x2)-(s_x**2))
	b = ((n * s_xy)-(s_x * s_y)) / ((n * s_x2)-(s_x**2))
	
	! Output
	PRINT *,'Equation of Fitted Line : y = a * x^n '
	PRINT *,'Where,'
	PRINT *,'a = ', EXP(a)
	PRINT *,'n = ', b
	
END PROGRAM
