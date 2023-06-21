subroutine influence_coefficient
    
    use variable
    
    implicit none
    
    real:: x_area                             ! heat condution area in x directon
    real:: y_area_S                           ! heat condution area in y direction(south face)
    real:: y_area_N                           ! heat condution area in y direction(north face)
    
    real,dimension(1:x_node_number-1,2:y_node_number-1):: k_we                               ! the first dimension represents the control face number in x direction, the second dimension represents the node number in y direction
    real,dimension(2:x_node_number-1,1:y_node_number-1):: k_sn                               ! the first dimension represents the node number in x direction, the second dimension represents the control face number in y direction
    
    ! after calculating the boundary additional source term, let the boundary themal conductivity be 0 
    do j=2,y_node_number-1
        k_we(1,j)=0
        k_we(x_node_number-1,j)=0
    end do 
    
    do i=2,x_node_number-1
        k_sn(i,1)=0
        k_sn(i,y_node_number-1)=0
    end do
    
    ! calculating the control face thermal conductivity using harmonic average
    do j=2,y_node_number-1                                                                                 ! calculating thermal conducitvity in control face in x direction
        do i=2,x_node_number-2
            k_we(i,j)=dx_node(i)/(dx_face_node_E(i-1)/k(i,j)+dx_face_node_W(i)/k(i+1,j))
        end do
    end do
    
    do j=2,y_node_number-2                                                                                 ! calculating thermal conducitvity in control face in y direction
        do i=2,x_node_number-1
            k_sn(i,j)=dy_node(j)/(dy_face_node_N(j-1)/k(i,j)+dy_face_node_S(j)/k(i,j+1))
        end do 
    end do
    
    do j=2,(y_node_number-1)                  ! excludes the boudary and sub-boudndary nodes
        x_area=dy_face(j-1)*y_R_node(j)
        
        do i=2,(x_node_number-1)
            y_area_S=dx_face(i-1)*y_R_face(j-1)
            y_area_N=dx_face(i-1)*y_R_face(j)
            
            a_E(i,j)=k_we(i,j)*x_area/dx_node(i)
            a_W(i,j)=k_we(i-1,j)*x_area/dx_node(i-1)
            a_N(i,j)=k_sn(i,j)*y_area_N/dy_node(j)
            a_S(i,j)=k_sn(i,j-1)*y_area_S/dy_node(j-1)
            
!            a_P0(i,j)=density*cp*y_R_node(j)*dx_face(i-1)*dy_face(j-1)/dt            ! non_steady heat conduction
           a_P0(i,j)=0                                                              ! steady heat conduction
            
            a_P(i,j)=a_W(i,j)+a_E(i,j)+a_S(i,j)+a_N(i,j)+a_P0(i,j)-S_p(i,j)*y_R_node(j)*dx_face(i-1)*dy_face(j-1)
            b(i,j)=S_c(i,j)*y_R_node(j)*dx_face(i-1)*dy_face(j-1)+a_P0(i,j)*T1(i,j)                       ! T1(i,j)represents temperature at the last time step 
            
        end do 
    end do
    
    return 
    end subroutine influence_coefficient
    
        
        