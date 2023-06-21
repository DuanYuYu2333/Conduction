subroutine mesh
    
    use variable
    
    implicit none
    
    real:: x_grid,y_grid                                    ! grid length in x and y direction
    
    x_grid=x_length/(x_node_number-2)                       ! uniform grid in x direction
    y_grid=y_length/(y_node_number-2)                       ! uniform grid in y direction
    
    ! control face positon: x_face, y_face
    x_face(1)=0                                             ! control faces include the boundary faces
    y_face(1)=0
    
    do i=2,(x_node_number-1)                                ! control face positon: x_face
        x_face(i)=x_face(i-1)+x_grid
    end do 
    
    do j=2,(y_node_number-1)                                ! control face positon: y_face
        y_face(j)=y_face(j-1)+y_grid
    end do 
    
    ! control face distance: dx_face,dy_face
    do i=1,(x_node_number-2)
        dx_face(i)=x_face(i+1)-x_face(i)
    end do
    
    do j=1,(y_node_number-2)
        dy_face(j)=y_face(j+1)-y_face(j)
    end do
    
    ! node position: x_node, y_node
    x_node(1)=0                                             ! nodes include the boundary nodes
    y_node(1)=0
    
    do i=2,(x_node_number-1)                                ! node position: x_node
        x_node(i)=(x_face(i-1)+x_face(i))/2
    end do 
    
    x_node(x_node_number)=x_face(x_node_number-1)
    
     do j=2,(y_node_number-1)                               ! node position: y_node
        y_node(j)=(y_face(j-1)+y_face(j))/2
    end do 
    
    y_node(y_node_number)=y_face(y_node_number-1)
    
    ! node distance: dx_node,dy_node
    do i=1,(x_node_number-1)
        dx_node(i)=x_node(i+1)-x_node(i)
    end do
    
    do j=1,(y_node_number-1)
        dy_node(j)=y_node(j+1)-y_node(j)
    end do
    
    ! face node distance: dx_face_node_E, dx_face_node_W, dy_face_node_N, dy_face_node_S,
    
    do i=1,(x_node_number-2)                                  ! face node distance in x direction
        dx_face_node_W(i)=x_node(i+1)-x_face(i)
        dx_face_node_E(i)=x_face(i+1)-x_node(i+1)
    end do
    
    do j=1,(y_node_number-2)                                  ! face node distance in y direction
        dy_face_node_S(j)=y_node(j+1)-y_face(j)
        dy_face_node_N(j)=y_face(j+1)-y_node(j+1)
    end do
    
    ! conversion between the Cartesian coordinate and the Cylindrical coordinate
    y_R0=0
    if (coordinate_mode==1)then 
        y_R_face=1
        y_R_node=1
    else
        y_R_face(1)=y_R0                                      ! radius at control face position in y direction
        do j=2,(y_node_number-1)
            y_R_face(j)=y_R_face(j-1)+dy_face(j-1)
        end do
        
        y_R_node(1)=y_R0                                      ! radius at node position in y direction
        do j=2,(y_node_number-1)
            y_R_node(j)=(y_R_face(j-1)+y_R_face(j))/2
        end do
        y_R_node(y_node_number)=y_R_face(y_node_number-1)
    end if 
    
    return 
    
    end subroutine mesh