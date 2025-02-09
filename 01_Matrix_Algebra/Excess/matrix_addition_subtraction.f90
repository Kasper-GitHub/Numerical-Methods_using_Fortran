! Name : Ankush Bhowmik
! Date : 04-10-2024
! Program Name : Matrix Addition and Subtraction with File Inputs

program matrix_addition_subtraction

	IMPLICIT NONE
	
	INTEGER :: i, j, k
	REAL :: M(4,3), N(4,3), S(4,3), D(4,3)
	
	! Reading & Printing Matrix M
	OPEN(unit = 1, file = "matrix_M.txt", action="read")
	READ(1,*)((M(i,j), j=1,3), i=1,4)
	WRITE(*,*)"Matrix M :"

	DO i=1,4
		write(*,*)(M(i,j), j=1,3)
	END DO
	
	! Reading & Printing Matrix N
	OPEN(unit = 2, file = "matrix_N.txt", action="read")
	READ(2,*)((N(i,j), j=1,3), i=1,4)
	WRITE(*,*)"Matrix N:"

	DO i=1,4
		WRITE(*,*)(N(i,j), j=1,3)
	END DO
	
	! Performing Matrix Addition, S = M + N
	DO i = 1,4
		DO j = 1,3
			S(i,j) = M(i,j) + N(i,j)
		END DO
	END DO
	
	! Performing Matrix Subtraction, D = M - N
	DO i = 1,4
		DO j = 1,3
			D(i,j) = M(i,j) - N(i,j)
		END DO
	END DO
	
	! Output (Matrix Addition)
	WRITE(*,*)"Result of the matrix addition (M+N) is given by Matrix S :"
	DO i = 1,4
		WRITE(*,*)(S(i,j), j=1,3)
	END DO
	
	! Output (Matrix Subtraction)
	WRITE(*,*)"Result of the matrix subtraction (M-N) is given by Matrix D :"
	DO i = 1,4
		WRITE(*,*)(D(i,j), j=1,3)
	END DO
	
end program
