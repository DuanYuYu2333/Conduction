subroutine initial
    
    use variable
    
    implicit none
    
    real:: x_temp,y_temp
    
    x_length=1                            ! difine the geometry of the model
    y_length=1       
    
    density=1                         ! define the material properties
    cp=1
    
    k=1                                 ! the value of all node thermal conductivity is the same (do not include the boundary nodes)
    
    S_c=0                                ! constant heat source
    S_p=0                                  
    
    dt=1E-3
    time=0
    error_requirement=1E-5
    
!    open(1,file='T_initial.dat')            ! read the initial temperature field
    
!    do i=1,x_node_number                              ! read the temperature field, which is defined in file 'T_initial.dat'
!        do j=1,y_node_number
!            read(1,*) x_temp,y_temp,T1(i,j)
!       end do
!    end do
    
    T1=100
    T2=T1
    
    time_iteration=1                      ! time_iteration !=1, unsteady problem 
    
    return
    
end subroutine initial
    
    
    
    