# Loading packages ---------------------------------------------------------------------------------
library( mau )

r <- c( 9, 10, 7 , 4 )
R <- matrix( c( 7, 5, 3, 1, 
                7, 6, 2, 1,
                1, 7, 5, 3 ), 4, 3 )

s <- borda_count( R )

test_that( "Check the right result", {
  expect_equal( norm( s[[2]] - r, type = '2' ), 0 )
})
