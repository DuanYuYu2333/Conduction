module variable
    
    implicit none
    
    integer:: coordinate_mode                  ! 1 for the Cartesian coordinate, 2 for the cylindrical coordinate
    real:: y_R0                           ! initstial radius in y  direction
    real:: x_length,y_length              ! initial length in the x and y direction
    
    real:: density,cp                     ! mass density and heat capacity
    real:: k_boundary                     ! thermal conductivity in boundary
    
    real:: T_f,h                          ! the third kind boundary condition (north wall)              
    real:: q_boundary                     ! heat fluxs in boundary for the second boundary condition (south wall)
    real:: T_boundary_N,T_boundary_W      ! wall temperature for the first boudary condition (the west and east wall)
    
    integer:: i,j,step
    integer,parameter:: x_node_number=202           ! node number in x direction including the boundary nodes, which can be changed for specific condition
    integer,parameter:: y_node_number=202           ! node number in y direction including the boundary nodes, which can be changed for specific condition
    
    real,dimension(2:(x_node_number-1),2:(y_node_number-1)):: k       ! thermal conductivity at specific place (do not include the boundary)
    
    real,dimension(1:x_node_number):: x_node                ! node position in x direction including the boundary nodes
    real,dimension(1:(x_node_number-1)):: x_face            ! face of controlled volume in x direction including the boundary faces
    real,dimension(1:(x_node_number-1)):: dx_node           ! distance between two adjacent nodes in x direciton
    real,dimension(1:(x_node_number-2)):: dx_face           ! distance between two faces of controlled volume in x direction
    real,dimension(1:(x_node_number-2)):: dx_face_node_E   ! distance between the control face and its east node
    real,dimension(1:(x_node_number-2)):: dx_face_node_W   ! distance between the control face and its west node
    
    real,dimension(1:y_node_number):: y_node                ! node position in y direction including the boundary nodes
    real,dimension(1:(y_node_number-1)):: y_face            ! face of controlled volume in y direction including the boundary faces
    real,dimension(1:(y_node_number-1)):: dy_node           ! distance between two adjacent nodes in y direciton
    real,dimension(1:(y_node_number-2)):: dy_face           ! distance between two faces of controlled volume in y direction
    real,dimension(1:(y_node_number-2)):: dy_face_node_N   ! distance between the control face and its north node
    real,dimension(1:(y_node_number-2)):: dy_face_node_S   ! distance between the control face and its south node
    
    real,dimension(1:y_node_number):: y_R_node              ! radius at the node in the y direction
    real,dimension(1:(y_node_number-1)):: y_R_face              ! radius at the control face in the y direction
    
    real,dimension(1:x_node_number,1:y_node_number):: T1,T2                              ! T1:former,T2:current
    real,dimension(2:(x_node_number-1),2:(y_node_number-1)):: a_E,a_W,a_S,a_N,a_P,a_P0,b ! influcence coefficient
    real,dimension(2:(x_node_number-1),2:(y_node_number-1)):: S_c,S_p                    ! coefficient for source term
    
    real:: time_iteration                               ! time_iteration=1: steady problem, time_iteration=specicified time iteration step, unsteady problem
    real:: dt                                           ! time step
    real:: time                                         ! time
    
    real:: error                                        ! inertia error
    real:: error_requirement                            ! requirement of inertia error for convergence
    logical:: convergence_index                         ! true-convergence,false-not convergence
    
end module
    
