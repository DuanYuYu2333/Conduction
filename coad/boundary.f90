subroutine boundary
    
    use variable 
    
    implicit none
    
    real:: S_c_W_coeff,S_c_N_coeff           ! define the additional source term coefficient for the first kind boundary condition
    
    k_boundary=1                        ! define the boundary conditions
    
    ! the first kind boundary condition
    T_f=10
    h=10
    q_boundary=20
    
    T_boundary_N=200
    T_boundary_W=100
    
    ! calculate the additional source term coefficient for the first kind boundary condition (compared to boundary1.f90)
    S_c_W_coeff=k_boundary/(dx_face_node_W(1)*dx_face(1))
    S_c_N_coeff=k_boundary/(dy_face_node_N(y_node_number-2)*dy_face(y_node_number-2))
    
    ! West face
    do j=2,(y_node_number-1)                 
        S_c(2,j)=S_c_W_coeff*T_boundary_W+S_c(2,j)
        S_p(2,j)=-S_c_W_coeff+S_p(2,j)
    end do

    ! East face
    do j=2,(y_node_number-1)
        S_c(x_node_number-1,j)=q_boundary/dx_face(x_node_number-2)+S_c(x_node_number-1,j)
    end do
    
    ! South face
    do i=2,(x_node_number-1)
        S_c(i,2)=T_f/((1/h+dy_node(1)/k_boundary)*dy_face(1))+S_c(i,2)
        S_p(i,2)=-1/((1/h+dy_node(1)/k_boundary)*dy_face(1))+S_p(i,2)
    end do
    
    ! North face
    do i=2,(x_node_number-1)
        S_c(i,y_node_number-1)=S_c_N_coeff*T_boundary_N+S_c(i,y_node_number-1)
        S_p(i,y_node_number-1)=-S_c_N_coeff+S_p(i,y_node_number-1)
    end do
    
    return 
    
    end subroutine boundary