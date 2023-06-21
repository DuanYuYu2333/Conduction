subroutine check_convergence
    
    use variable
    
    implicit none 
    
    real:: temp
    
    error=0
    
    do j=2,y_node_number-1
        do i=2,x_node_number-1
            temp=abs((T2(i,j)-T1(i,j))/(T1(i,j)+1E-30))
            if (temp.gt.error) then
                error=temp
            else
                error=error
            end if
        end do 
    end do
    
!   write(*,*)error
    
    if (error.gt.error_requirement) then
        convergence_index=.false.
    else
        convergence_index=.true.
    end if
    
    return
    end subroutine check_convergence