program main
    
    use variable
    
    implicit none
    
    ! define the elements in three_diagonal matrix (from node 2 to node y_node_number-1 in y direction)
    real,dimension(1:y_node_number-2)::bb       ! the upper sub_diagonal elements     
    real,dimension(1:y_node_number-2)::a       ! the diagonal elements
    real,dimension(1:y_node_number-2)::c       ! the low elements
    real,dimension(1:y_node_number-2)::d       ! the sum elements of matrix equation
    
    ! define temp_iteration variables in TDMA (Ti=Pi*Ti+1+Qi)
    real,dimension(1:y_node_number-2):: P
    real,dimension(1:y_node_number-2):: Q
    integer:: TDMA_temp
    
    coordinate_mode=1                         ! specify the coordinate mode
    
    call initial
    call mesh
    
    convergence_index=.false.
    
    do step=1,time_iteration
        
        T1=T2
        
        time=time+dt
        
        call boundary
        
        call influence_coefficient
        
        ! alternating direction calculation
100     do i=2,x_node_number-1
            do j=2,y_node_number-1
                
 !              a_P(i,j)*T2(i,j)=(a_E(i,j)*T1(i+1,j)+a_W(i,j)*T2(i-1,j)+a_S(i,j)*T2(i,j-1)+a_N(i,j)*T2(i,j+1)+b(i,j))               ! from west to east
                
                a(j-1)=a_P(i,j)
                bb(j-1)=a_N(i,j)
                c(j-1)=a_S(i,j)
                d(j-1)=b(i,j)+a_E(i,j)*T1(i+1,j)+a_W(i,j)*T2(i-1,j)
            end do
            
        ! TDMA calcualtes the T(i,2:y_node_number-1)
            P(1)=bb(1)/a(1)
            Q(1)=d(1)/a(1)
                
            do TDMA_temp=2,y_node_number-2
                P(TDMA_temp)=bb(TDMA_temp)/(a(TDMA_temp)-c(TDMA_temp)*P(TDMA_temp-1))
                Q(TDMA_temp)=(d(TDMA_temp)+c(TDMA_temp)*Q(TDMA_temp-1))/(a(TDMA_temp)-c(TDMA_temp)*P(TDMA_temp-1))
            end do 
                
            T2(i,y_node_number-1)=Q(y_node_number-2)                                  ! back iteration
            do TDMA_temp=y_node_number-2,2,-1
                T2(i,TDMA_temp)=P(TDMA_temp-1)*T2(i,TDMA_temp+1)+Q(TDMA_temp-1)
!               write(*,*)TDMA_temp,T2(i,TDMA_temp) 
            end do
                
        end do 
        
        ! the upper iteration has updaded the temperature field which is shown as T2, then check the convergence
        
        call check_convergence
        
        if(.not.convergence_index) then
            T1=T2
            goto 100
        end if 
        
!        write(*,*)time
    end do
    
    do i=1,x_node_number
        T2(i,1)=(h*dy_node(1)*T_f+k_boundary*T2(i,2))/(h*dy_node(1)+k_boundary)
    end do
    
    do j=1,y_node_number
        T2(x_node_number,j)=T2(x_node_number-1,j)+q_boundary*dx_node(x_node_number-1)/k_boundary
    end do
    
    
    call output
    
    stop
    
    end program main
        
        
                
                

                
                
                
                
        
        
        
        