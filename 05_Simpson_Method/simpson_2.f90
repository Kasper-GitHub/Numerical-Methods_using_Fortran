! Name : Ankush Bhowmik
! Program Name : Numerical Integration using Simpson's 1/3-rd rule on f(x) = (sin x)^8 within [0.0, 0.3]

PROGRAM simpson_2

	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(4)
	INTEGER :: i, n
	REAL(pr), PARAMETER :: tolerance = 1.0E-5
	REAL(pr) :: x, a, b, h, f, f_odd = 0.0, f_even = 0.0, integration
	
	! Limits & Parameters
	a = 0.0
	b = 0.3
	h = tolerance
	n = (b-a)/h
	
	! Simpson's 1/3rd Rule Implementation
	DO i = 1, n-1, 2
		f_odd = f_odd + f(a + i * h)
	END DO
	
	DO i = 2, n-2, 2
		f_even = f_even + f(a + i * h)
	END DO
	
	integration = (h/3)*( f(a) + f(b) + 4 * f_odd + 2 * f_even )
	
	! Output
	print * , "Numerical Integration using Simpson's 1/3-rd rule :" 
	print * , "---------------------------------------------------" 
	print 10, a, b, integration
	
10	FORMAT('>',2X,'f(x) = (sin x)^8 within limits a = ',F6.3,' & b = ',F6.3,' is',2X,':',2X, E9.3)

END PROGRAM

FUNCTION f(x)

	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(4)
	REAL(pr), INTENT(IN) :: x
	REAL(pr), PARAMETER :: tolerance = 1.0E-5
	REAL(pr) :: f

	f = (SIN(x))**8
	
END FUNCTION
