! Name : Ankush Bhowmik
! Date : 04-10-2024
! Program Name : Matrix Algebra - Products AB & (B_T)(A_T)

program matrix_multiplication

	IMPLICIT NONE
	
	INTEGER :: i, j, k
	REAL :: A(3,3), B(3,1), A_T(3,3), B_T(1,3), P1(3,1), P2(3,3)
	
	! Reading & Printing Matrix A
	OPEN(unit = 1, file = "matrix_A.txt", action="read")
	READ(1,*)((A(i,j), j=1,3), i=1,3)
	WRITE(*,*)"Matrix A :"

	DO i=1,3
		write(*,*)(A(i,j), j=1,3)
	END DO
	
	! Reading & Printing Matrix B
	OPEN(unit = 2, file = "matrix_B.txt", action="read")
	READ(2,*)((B(i,j), j=1,1), i=1,3)
	WRITE(*,*)"Matrix B :"

	DO i=1,3
		write(*,*)(B(i,j), j=1,1)
	END DO

	! Performing Matrix Multiplication, P1 = AB
	DO i = 1,3
		DO j = 1,1
			P1(i,j) = 0.0		!! Initialization
			DO k = 1,3
				P1(i,j) = P1(i,j) + A(i,k) * B(k,j)
			END DO
		END DO
	END DO
	
	!Output
	WRITE(*,*)"Result of the matrix multiplication (AB) is given by Matrix P1 :"
	DO i = 1,3
		WRITE(*,*)(P1(i,j), j=1,1)
	END DO
	
	! Finding A_T (Transpose of Matrix A)
	DO i = 1,3
		DO j = 1,3
			A_T(i,j) = A(j,i)
		END DO
	END DO
	
	! Finding B_T (Transpose of Matrix B)
	DO i = 1,1
		DO j = 1,3
			B_T(i,j) = B(j,i)
		END DO
	END DO
	
	! Printing Matrix A_T & B_T
	WRITE(*,*)"Matrix A_T :"
	DO i=1,3
		write(*,*)(A_T(i,j), j=1,3)
	END DO
	WRITE(*,*)"Matrix B_T :"
	DO i=1,1
		write(*,*)(B_T(i,j), j=1,3)
	END DO
	
	! Performing Matrix Multiplication, P2 = (B_T)(A_T)
	DO i = 1,1
		DO j = 1,3
			P2(i,j) = 0.0		!! Initialization
			DO k = 1,3
				P2(i,j) = P2(i,j) + B_T(i,k) * A_T(k,j)
			END DO
		END DO
	END DO
	
	!Output
	WRITE(*,*)"Result of the matrix multiplication (B_T)(A_T) is given by Matrix P2 :"
	DO i = 1,1
		WRITE(*,*)(P2(i,j), j=1,3)
	END DO
	
end program

