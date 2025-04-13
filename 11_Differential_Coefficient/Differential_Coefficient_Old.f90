PROGRAM differential_coefficient
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	REAL(pr) :: difference_table(7,6), coeff_1, coeff_2, h
	INTEGER :: i, j, n = 6

	! Reading given Data-points
	OPEN(unit = 1, file = "Dataset.txt", action = "read")
	READ(1,*)((difference_table(i,j), j=1,6), i=1,2)
	
	! Generating the Forward Difference Table
	CALL difference_table_generator(difference_table,n)
	h = difference_table(1,2) - difference_table(1,1)
	
	WRITE(*,'(2X,A49)')"-------------------- Results --------------------"
	! Computing Differential Coefficient at First Point
	coeff_1 = 0.0_pr
	DO i = 3,7
		coeff_1 = coeff_1 + ((-1)**(i-1))*difference_table(i,1)/(i-2)
	END DO
	coeff_1 = coeff_1/h
	
	WRITE(*,'(2X,A42,F7.5)')"Differential Coefficient at First Point : ", coeff_1
	
	! Computing Differential Coefficient at Second Point
	coeff_2 = 0.0_pr
	DO i = 3,7
		coeff_2 = coeff_2 + ((-1)**(i-1))*difference_table(i,2)/(i-2)
	END DO
	coeff_2 = coeff_2/h
	
	WRITE(*,'(2X,A42,F7.5)')"Differential Coefficient at Second Point : ", coeff_2
	
END PROGRAM

SUBROUTINE difference_table_generator(difference_table,n)
	IMPLICIT NONE
	INTEGER, PARAMETER :: pr = selected_real_kind(8)
	REAL(pr), INTENT(INOUT) :: difference_table(7,6)
	INTEGER, INTENT(IN) :: n
	INTEGER :: i, j
	
	! Computing Forward Difference Table
	DO i = 3,7
		DO j = 1,n+2-i
			difference_table(i,j) = difference_table(i-1,j+1) - difference_table(i-1,j)
		END DO
	END DO
	
	WRITE(*,'(1X,A26/1X,A24)')"Forward Difference Table :","========================"
	WRITE(*,'(3X,A1,6X,A4,7X,5(A4,8X))')"x","f(x)","Δ","Δ^2","Δ^3","Δ^4","Δ^5"
	DO j = 1,6
		WRITE(*,'(2X,F3.1,7(4X,F7.5))')(difference_table(i,j), i=1,n+2-j)
	END DO
END SUBROUTINE
