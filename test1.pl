
:- dynamic( ensVers/2 ).
:- dynamic( compose/2 ).
:- dynamic( parole/1 ).

% Charles Baudelaire (1821-1867)

ensVers( a, [ vers( [ syllabe( "", "Ô", "" ),
                      syllabe( "f", "i", "ns" ),
					  syllabe( "d'", "au", "" ),
					  syllabe( "t", "o", "m" ),
					  syllabe( "n", "e", "" ),
					  syllabe( "h", "i", "" ),
					  syllabe( "v", "e", "rs" ),
					  syllabe( "pr", "i", "n" ),
					  syllabe( "t", "e", "mps" ),
					  syllabe( "tr", "e", "m" ),
					  syllabe( "p", "é", "s" ), 
					  syllabe( "d", "e", "" ), 
					  syllabe( "b", "oue", "" ) ] ), 
              vers( [ syllabe( "", "E", "n" ),
			          syllabe( "d", "o", "r" ),
					  syllabe( "m", "eu", "" ),
					  syllabe( "s", "e", "s" ),
					  syllabe( "s", "ai", "" ),
					  syllabe( "s", "o", "ns" ),
					  syllabe( "j", "e", "" ),
					  syllabe( "v", "ou", "s" ), 
					  syllabe( "", "ai", "" ),
					  syllabe( "m", "e", "" ),
					  syllabe( "", "e", "t" ),
					  syllabe( "v", "ou", "s" ),
					  syllabe( "l", "oue", "" ) ] ),
			  vers( [ syllabe( "D", "a", "ns" ),
 			          syllabe( "c", "e", "t" ),
					  syllabe( "t", "e", "" ), 
					  syllabe( "gr", "a", "n" ),
					  syllabe( "d", "e", "" ), 
					  syllabe( "pl", "ai", "" ), 
					  syllabe( "n", "e", "" ), 
					  syllabe( "", "o", "ù" ), 
					  syllabe( "l'", "au", "" ),
					  syllabe( "t", "a", "n" ), 
					  syllabe( "fr", "oi", "d" ), 
					  syllabe( "s", "e", "" ),
					  syllabe( "j", "oue", "" ) ] ),
			  vers( [ syllabe( "", "O", "ù" ), 
			          syllabe( "p", "a", "r" ), 
					  syllabe( "l", "e", "s" ), 
					  syllabe( "l", "o", "n" ),
					  syllabe( "g", "ue", "s" ), 
					  syllabe( "n", "ui", "ts" ), 
					  syllabe( "l", "a", "" ), 
					  syllabe( "g", "i", "" ),
					  syllabe( "r", "oue", "t" ),
					  syllabe( "t", "e", "" ), 
					  syllabe( "s'", "e", "n" ),
					  syllabe( "r", "oue", "" ) ] )
			] ).
ensVers( b, [ vers( [ syllabe( "D'", "e", "n" ), 
                      syllabe( "v", "e", "" ),
					  syllabe( "l", "o", "p" ),
					  syllabe( "p", "e", "r" ), 
					  syllabe( "", "ai", "n" ),
					  syllabe( "s", "i", "" ),
					  syllabe( "m", "o", "n" ), 
					  syllabe( "c", "oeu", "r" ), 
					  syllabe( "", "e", "t" ), 
					  syllabe( "m", "o", "n" ),  
					  syllabe( "c", "e", "r" ),
					  syllabe( "v", "eau", "" ) ] ),
			  vers( [ syllabe( "D'", "u", "n" ), 
			          syllabe( "l", "i", "n" ),
					  syllabe( "c", "eu", "l" ), 
					  syllabe( "v", "a", "" ),
					  syllabe( "p", "o", "" ),
					  syllabe( "r", "eu", "x" ),
					  syllabe( "", "e", "t" ), 
					  syllabe( "d'", "u", "n" ), 
					  syllabe( "v", "a", "" ),
					  syllabe( "g", "ue", "" ), 
					  syllabe( "t", "o", "m" ),
					  syllabe( "b", "eau", "" ) ] ),
			  vers( [ syllabe( "M", "o", "n" ), 
			          syllabe( "", "â", "" ), 
					  syllabe( "m", "e", ""),  
					  syllabe( "m", "ieu", "x" ),
					  syllabe( "qu'", "au", "" ),
					  syllabe( "t", "e", "mps" ), 
					  syllabe( "d", "u", "" ), 
					  syllabe( "t", "i", "" ),
					  syllabe( "", "è", "" ),
					  syllabe( "d", "e", "" ), 
					  syllabe( "r", "e", "" ),
					  syllabe( "n", "ou", "" ),
					  syllabe( "v", "eau", "" ) ] ),
			  vers( [ syllabe( "", "Ou", "" ), 
			          syllabe( "vr", "i", "" ), 
					  syllabe( "r", "a", "" ), 
					  syllabe( "l", "a", "r" ),
					  syllabe( "g", "e", "" ), 
					  syllabe( "m", "e", "nt" ), 
					  syllabe( "s", "e", "s" ), 
					  syllabe( "", "ai", "" ),
					  syllabe( "l", "e", "s" ), 
					  syllabe( "d", "e", "" ), 
					  syllabe( "c", "o", "r" ),
					  syllabe( "b", "eau", "" ) ] )
            ] ).
ensVers( c, [ vers( [ syllabe( "R", "ie", "n" ), 
                      syllabe( "n'", "e", "st" ), 
					  syllabe( "pl", "u", "s" ), 
					  syllabe( "d", "ou", "x" ), 
					  syllabe( "", "au", "" ), 
					  syllabe( "c", "oeu", "r" ), 
					  syllabe( "pl", "ei", "n" ), 
					  syllabe( "d", "e", "" ), 
					  syllabe( "ch", "o", "" ), 
					  syllabe( "s", "e", "s" ), 
					  syllabe( "f", "u", "" ),
					  syllabe( "n", "è", "" ),
					  syllabe( "br", "e", "s" ) ] ),
              vers( [ syllabe( "Qu", "e", "" ), 
			          syllabe( "l'", "a", "s" ),
					  syllabe( "p", "e", "ct" ), 
					  syllabe( "p", "e", "r" ),
					  syllabe( "m", "a", "" ),
					  syllabe( "n", "e", "nt" ), 
					  syllabe( "d", "e", "" ),
					  syllabe( "v", "o", "s" ), 
					  syllabe( "p", "â", "" ),
					  syllabe( "l", "e", "s" ), 
					  syllabe( "t", "é", "" ),
					  syllabe( "n", "è", "" ),
					  syllabe( "br", "e", "s" ) ] )
            ] ).

ensVers( d, [ vers( [ syllabe( "", "E", "t" ), 
                      syllabe( "s", "u", "r" ), 
					  syllabe( "qu", "i", "" ), 
					  syllabe( "d", "è", "s" ), 
					  syllabe( "l", "o", "ng" ),
					  syllabe( "t", "e", "mps" ), 
					  syllabe( "d", "e", "s" ),
					  syllabe( "c", "e", "n" ),
					  syllabe( "d", "e", "nt" ), 
					  syllabe( "l", "e", "s" ), 
					  syllabe( "fr", "i", "" ),
					  syllabe( "m", "a", "s" ) ] ),
              vers( [ syllabe( "", "Ô", "" ), 
			          syllabe( "bl", "a", "" ),
					  syllabe( "f", "a", "r" ),
					  syllabe( "d", "e", "s" ), 
					  syllabe( "s", "ai", "" ),
					  syllabe( "s", "o", "ns"), 
					  syllabe( "r", "ei", "" ),
					  syllabe( "n", "e", "s" ), 
					  syllabe( "d", "e", "" ), 
					  syllabe( "n", "o", "s" ), 
					  syllabe( "cl", "i", "" ),
					  syllabe( "m", "a", "ts" ) ] ) 
            ] ).

ensVers( e, [ vers( [ syllabe( "S", "i", "" ), 
                      syllabe( "c", "e", "" ), 
					  syllabe( "n'", "e", "st" ), 
					  syllabe( "p", "a", "r" ),  
					  syllabe( "", "u", "n" ), 
					  syllabe( "s", "oi", "r" ), 
					  syllabe( "s", "a", "ns" ), 
					  syllabe( "l", "u", "" ),
					  syllabe( "n", "e", "" ), 
					  syllabe( "d", "eu", "x" ), 
					  syllabe( "", "à", "" ),
					  syllabe( "d", "eu", "x" ) ] ),
              vers( [ syllabe( "D'", "e", "n" ),
			          syllabe( "d", "o", "r" ),
					  syllabe( "m", "i", "r" ), 
					  syllabe( "l", "a", "" ), 
					  syllabe( "d", "ou", "" ),
					  syllabe( "l", "eu", "r" ), 
					  syllabe( "s", "u", "r" ),
					  syllabe( "", "u", "n" ), 
					  syllabe( "l", "i", "t" ), 
					  syllabe( "h", "a", "" ),
					  syllabe( "s", "a", "r" ),
					  syllabe( "d", "eu", "x" ) ] ) 
            ] ).

compose( q, binaire( 2, ens( a ), 2, ens( b ) ) ).
compose( t, binaire( 1, ens( c ), 2, ens( d ) ) ).
compose( u, binaire( 1, ens( c ), 2, ens( e ) ) ).
compose( m, strophique( 2, seq( q ) ) ).
compose( n, prosique( [ seq( t ), seq( u ) ] ) ).
compose( p, prosique( [ seq( m ), seq( n ) ] ) ).

parole( seq( p ) ).

%style( heureux, 57, 16, 0.25 ).