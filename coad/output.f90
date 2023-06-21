subroutine output
    
    use variable
    
    implicit none
    
    open(2,file='T_result.dat')                      !  the output file
    
10  format(2xa)
20  format(1xa22)
30  format(1xa8,i3,6xa3,i3,3xa10)
40  format(2xf14.5,6xf14.5,8xf14.5)    
    
    write(2,10) 'Title=2D heat conduction problem'
    write(2,20) 'Variables="x","y","T"'
    write(2,30) 'Zone I=',x_node_number,'J=',y_node_number,'F=Point'
    
    do i=1,x_node_number
        do j=1,y_node_number
            write(2,40) x_node(i),y_node(j),T2(i,j)
        end do   
    end do
    
return

end subroutine output