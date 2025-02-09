! Name : Ankush Bhowmik
! Date : 04-10-2024
! Program Name : Matrix Multiplication with File Inputs

program matrix_multiplication

	IMPLICIT NONE
	
	INTEGER :: i, j, k
	REAL :: A(4,3), B(3,2), C(4,2)
	
	! Reading & Printing Matrix A
	OPEN(unit = 1, file = "matrix_A.txt", action="read")
	READ(1,*)((A(i,j), j=1,3), i=1,4)
	WRITE(*,*)"Matrix A :"

	DO i=1,4
		write(*,*)(A(i,j), j=1,3)
	END DO
	
	! Reading & Printing Matrix B
	OPEN(unit = 2, file = "matrix_B.txt", action="read")
	READ(2,*)((B(i,j), j=1,2), i=1,3)
	WRITE(*,*)"Matrix B :"

	DO i=1,3
		write(*,*)(B(i,j), j=1,2)
	END DO
	
	! Initialising Result Matrix C
	DO i = 1,4
		DO j = 1,2
			C(i,j) = 0.0
		END DO
	END DO
	
	! Performing Matrix Multiplication, C = AB
	DO i = 1,4
		DO j = 1,2
			DO k = 1,3
				C(i,j) = C(i,j) + A(i,k) * B(k,j)
			END DO
		END DO
	END DO
	
	!Output
	write(*,*)"Result of the matrix multiplication (AB) is given by Matrix C :"
	DO i = 1,4
		write(*,*)(C(i,j), j=1,2)
	END DO
	
end program

