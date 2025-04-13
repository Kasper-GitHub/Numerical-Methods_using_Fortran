PROGRAM Gaussian_Quadrature_Method
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	REAL(pr) :: a, b
	
	! Range of Integration
	a = 0.0_pr
	b = 13.0_pr
	
	! Performing Gaussian Quadrature
	CALL Gauss_Quad(2,a,b)
	CALL Gauss_Quad(4,a,b)
	CALL Gauss_Quad(5,a,b)
END PROGRAM

FUNCTION f(x)
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	REAL(pr), INTENT(IN) :: x
	REAL(pr) :: f
	
	! Function Definition
	f = (((x**2)*EXP(x))/(1 + EXP(x))**2)
END FUNCTION

SUBROUTINE Gauss_Quad(n,a,b)
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	INTEGER, INTENT(IN) :: n
	REAL(pr), INTENT(IN) :: a, b
	REAL(pr) :: x(n), w(n), t, sum_val, integral, f
	INTEGER :: i
	
	! Selecting Gaussian Quadrature Mode
	SELECT CASE(n)
	  CASE(2)
	    x = (/ 0.577350_pr, -0.577350_pr /)
	    w = (/ 1.0_pr, 1.0_pr /)
	  CASE(4)
	    x = (/ 0.339981_pr, -0.339981_pr, 0.861136_pr, -0.861136_pr /)
	    w = (/ 0.652145_pr, 0.652145_pr, 0.347855_pr, 0.347855_pr /)
	  CASE(5)
	    x = (/ -0.906180_pr, -0.538469_pr, 0.0_pr, 0.538469_pr, 0.906180_pr /)
	    w = (/ 0.236927_pr, 0.478629_pr, 0.568889_pr, 0.478629_pr, 0.236927_pr /)
	END SELECT
	
	! Calculating Integral
	sum_val = 0.0_pr
	
	DO i = 1, n
	  t = ((b - a) / 2.0_pr) * x(i) + ((b + a) / 2.0_pr)
	  sum_val = sum_val + w(i) * f(t)
	END DO
	
	integral = (b - a) / 2.0_pr * sum_val
	
	! Printing Result
	PRINT *,'============================================'
	WRITE(*,'(2X,A34, I1, A7)')"Method : Gaussian Quadrature with ", n, " points"
	WRITE(*,'(2X,A31,F8.6)')"The value of the integral, I = ", integral
	PRINT *,'============================================'
END SUBROUTINE
