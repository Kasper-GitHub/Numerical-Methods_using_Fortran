PROGRAM jacobi_method
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	REAL(pr), PARAMETER :: tolerance = 1.0E-4
	REAL(pr), DIMENSION(3,3) :: matrix, U_matrix, U_inv_matrix
	REAL(pr) :: theta, max_off_term
	INTEGER :: i, j, k, p, q

	! Reading and Printing Original Matrix
	OPEN(unit = 1, file = "matrix.txt", action = "read")
	READ(1,*)((matrix(i,j), j=1,3), i=1,3)
	WRITE(*,'(1X,A17)')"Original Matrix :"
	DO j = 1,3
		WRITE(*,'(2X,3(F7.4,4X))')(matrix(i,j), i=1,3)
	END DO
	
	! Jacobi Method Implementation
	DO k = 1,3
		! Determining largest off-diagonal term
		max_off_term = 0.0_pr
		DO i = 1,3
			DO j = 1,3
				IF (i .NE. j) THEN
					IF (ABS(matrix(i,j)) .GT. max_off_term) THEN
						max_off_term = ABS(matrix(i,j))
						p = i
						q = j
					END IF
				END IF
			END DO
		END DO
	
		! Computing rotation angle
		theta = (ATAN(2.0_pr * matrix(p,q) / (matrix(p,p) - matrix(q,q))))/2
		
		! Construction of the Rotation Matrix
		DO i = 1,3
			DO j = 1,3
				IF (i .NE. j) THEN
					U_matrix(i,j) = 0.0_pr
				ELSE
					U_matrix(i,j) = 1.0_pr
				END IF
			END DO
		END DO
		
		U_matrix(p,p) = COS(theta)
		U_matrix(p,q) = -SIN(theta)
		U_matrix(q,p) = SIN(theta)
		U_matrix(q,q) = COS(theta)
		
		! Similarity Transformation
		U_inv_matrix = TRANSPOSE(U_matrix)
		matrix = MATMUL(U_inv_matrix, MATMUL(matrix,U_matrix))
		
		! Rounding off within Tolerance
		DO i = 1,3
			DO j = 1,3
				IF (ABS(matrix(i,j)-NINT(matrix(i,j))) .LT. tolerance) THEN
					matrix(i,j) = NINT(matrix(i,j))
				END IF
			END DO
		END DO		
	END DO
	
	! Printing Result Matrix
	PRINT *,'=========================================='
	WRITE(*,'(3X,A38)')"Diagonalization Method : Jacobi Method"
	PRINT *,'=========================================='
	WRITE(*,'(1X,A21)')"Diagonalized Matrix :"
	DO j = 1,3
		WRITE(*,'(2X,3(F7.4,4X))')(matrix(i,j), i=1,3)
	END DO
END PROGRAM
