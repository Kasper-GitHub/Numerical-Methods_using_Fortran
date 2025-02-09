! Name : Ankush Bhowmik
! Date : 04-10-2024
! Program Name : Matrix Operations - Sum, Difference, Product

program matrix_addition_subtraction

	IMPLICIT NONE
	
	INTEGER :: i, j, k
	REAL :: A(3,3), B(3,3), S(3,3), D(3,3), P1(3,3), P2(3,3)
	
	! Reading & Printing Matrix A
	OPEN(unit = 1, file = "matrix_A.txt", action="read")
	READ(1,*)((A(i,j), j=1,3), i=1,3)
	WRITE(*,*)"Matrix A :"

	DO i=1,3
		write(*,*)(A(i,j), j=1,3)
	END DO
	
	! Reading & Printing Matrix B
	OPEN(unit = 2, file = "matrix_B.txt", action="read")
	READ(2,*)((B(i,j), j=1,3), i=1,3)
	WRITE(*,*)"Matrix B :"

	DO i=1,3
		WRITE(*,*)(B(i,j), j=1,3)
	END DO
	
	! Performing Matrix Sum, S = A + B
	DO i = 1,3
		DO j = 1,3
			S(i,j) = A(i,j) + B(i,j)
		END DO
	END DO
	
	! Performing Matrix Difference, D = A - B
	DO i = 1,3
		DO j = 1,3
			D(i,j) = A(i,j) - B(i,j)
		END DO
	END DO
	
	! Performing Matrix Multiplication, P1 = AB
	DO i = 1,3
		DO j = 1,3
			P1(i,j) = 0.0		!! Initialization
			DO k = 1,3
				P1(i,j) = P1(i,j) + A(i,k) * B(k,j)
			END DO
		END DO
	END DO
	
	! Performing Matrix Multiplication, P2 = BA
	DO i = 1,3
		DO j = 1,3
			P2(i,j) = 0.0		!! Initialization
			DO k = 1,3
				P2(i,j) = P2(i,j) + B(i,k) * A(k,j)
			END DO
		END DO
	END DO
	
	! Output (Matrix Sum)
	WRITE(*,*)"Result of the Matrix Sum (A+B) is given by Matrix S :"
	DO i = 1,3
		WRITE(*,*)(S(i,j), j=1,3)
	END DO
	
	! Output (Matrix Difference)
	WRITE(*,*)"Result of the Matrix Difference (A-B) is given by Matrix D :"
	DO i = 1,3
		WRITE(*,*)(D(i,j), j=1,3)
	END DO
	
	! Output (Matrix Product AB)
	WRITE(*,*)"Result of the Matrix Product (AB) is given by Matrix P1 :"
	DO i = 1,3
		WRITE(*,*)(P1(i,j), j=1,3)
	END DO
	
	! Output (Matrix Product BA)
	WRITE(*,*)"Result of the Matrix Product (BA) is given by Matrix P2 :"
	DO i = 1,3
		WRITE(*,*)(P2(i,j), j=1,3)
	END DO
	
end program
