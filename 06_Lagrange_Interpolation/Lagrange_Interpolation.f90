PROGRAM lagrange_interpolation
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(4)
	REAL(pr) :: data_points(2,5), data_points_rev(2,5), x_interpolation, answer
	INTEGER :: i, j
	
	! Reading and Printing given Data-points
	OPEN(unit = 1, file = "Dataset.txt", action = "read")
	READ(1,*)((data_points(i,j), j=1,5), i=1,2)
	WRITE(*,'(1X,A9/1X,A7)')"DATASET :","======="
	WRITE(*,'(3X,A1,6X,A4)')"x","f(x)"
	DO j = 1,5
		WRITE(*,'(2X,F3.1,4X,F6.4)')(data_points(i,j), i=1,2)
	END DO
	
	! data_points_rev : x-f(x) swapped version of data_points array
	DO i = 1,2
		DO j = 1,5
			data_points_rev(i,j) = data_points(3-i,j)
		END DO
	END DO
	
	! Printing Results
	WRITE(*,*)"Lagrange Interpolation Results :"
	WRITE(*,*)"=============================="
	10 FORMAT(2X,A25,F3.1,A4,F6.4)
	
	! Performing Lagranrange Interpolation : f(x) for x
	x_interpolation = 2.0
	CALL interpolation(data_points,x_interpolation,answer)
	WRITE(*,10)"The value of f(x) at x = ",x_interpolation," is ", answer
	
	! Performing Lagranrange Interpolation : x for f(x)
	x_interpolation = 1.5
	CALL interpolation(data_points_rev,x_interpolation,answer)
	WRITE(*,10)"The value of x at f(x) = ",x_interpolation," is ", answer
END PROGRAM

SUBROUTINE interpolation(data_points,x_interpolation,answer)
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(4)
	REAL(pr), INTENT(IN) :: data_points(2,5)
	REAL(pr), INTENT(INOUT) :: x_interpolation 
	REAL(pr), INTENT(OUT) :: answer 
	REAL(pr) :: prod, temp
	INTEGER :: i, j
	
	! Computing Lagranrange Interpolation
	answer = 0.0
	DO i = 1,5
		Prod = 1.0
		DO j = 1,5
			IF (i .NE. j) THEN
				temp = data_points(1,i)-data_points(1,j)
				prod = prod * (x_interpolation-data_points(1,j))/temp
			END IF
		END DO
		answer = answer + data_points(2,i) * prod
	END DO	
END SUBROUTINE