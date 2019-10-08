% Wai Chun Leung, wleung11@ucsc.edu
% Helper functions, from Examples/graphpaths.pl
not( X ) :- X, !, fail.
not( _ ).

% Code modified from Examples/functions.pl
% Outputs distance in minutes
haversine_distance( A, B, Distance) :-
    airport( A, _, degmin( ALatD, ALatM ), degmin( ALonD, ALonM )),
    airport( B, _, degmin( BLatD, BLatM ), degmin( BLonD, BLonM )),
    % Convert degree minutes to radians
    Lat1 is (pi * (ALatD + (ALatM/60)))/180,
    Lon1 is (pi * (ALonD + (ALonM/60)))/180,
    Lat2 is (pi * (BLatD + (BLatM/60)))/180,
    Lon2 is (pi * (BLonD + (BLonM/60)))/180,
    % Convert radians to distance, then to minutes
    Dlon is Lon2 - Lon1,
    Dlat is Lat2 - Lat1,
    Temp is sin( Dlat / 2 ) ** 2
        + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
    Dist is 2 * atan2( sqrt( Temp ), sqrt( 1 - Temp )),
    Distance is ((Dist * 3961)/500) * 60.

% Code modified from misc/remainder.pl
remainder( Float, Modulus, Quotient, Remainder) :-
   Quotient is truncate( Float / Modulus),
   Remainder is round(Float - Quotient * Modulus).

% Code from misc/format.pl
to_upper( Lower, Upper) :-
   atom_chars( Lower, Lowerlist),
   maplist( lower_upper, Lowerlist, Upperlist),
   atom_chars( Upper, Upperlist).

% Code from misc/format.pl
print_trip( Action, Code, Name, time( Hour, Minute)) :-
   to_upper( Code, Upper_code),
   format( "%-6s  %3s  %-16s  %02d:%02d",
           [Action, Upper_code, Name, Hour, Minute]),
   nl.

% Code modified from Examples/grapthpaths.pl
writepath( [] ) :-
   nl.
writepath( [flight(Start,End,time(DH,DM))|Tail] ) :-
   % Compute arrival time in minutes
   haversine_distance(Start, End, Duration),
   ArrivalMins is Duration + (DH * 60) + DM,
   % Get the 24hr clock format given minutes
   remainder(ArrivalMins, 60, AH, AM),
   % Get names of airports
   airport(Start, StartName, _, _),
   airport(End, EndName, _, _),
   % Final print trip
   print_trip('depart', Start, StartName, time(DH, DM)),
   print_trip('arrive', End, EndName, time(AH, AM)),
   % Tail recursion call
   writepath( Tail ).
writepath(_).

% Base case for listpath()
listpath( Node, End, Outlist ) :-
   listpath( Node, End, [Node], 0, Outlist ).
% The case that it has finished finding the right path.
listpath( Node, Node, _, _, [Node] ).
% Finding the path recursively
listpath( Node, End, Tried, PrevArrivalTime,
                     [flight(Node, Next, time(Hrs,Min))|List] ) :-
   % Get current flight details
   flight(Node, Next, time(Hrs,Min)),
   % Compute this flight's arrival time for the next flight, if any
   haversine_distance(Node, Next, Duration),
   ArrivalTime is ((Hrs * 60) + Min) + Duration,
   % Compute the arrival time of the previous flight with transfer
   TimeAvailable is PrevArrivalTime + 30,
   % Compute the departure time of the current flight
   CurrDepartTime is (Hrs * 60) + Min,
   % Make sure the destination is not already visited
   not( member( Next, Tried )),
   % Check arrival time of the prev flight to this flight's departure
   (TimeAvailable < CurrDepartTime),
   % Tail recursion call
   listpath(Next, End, [Next|Tried], ArrivalTime, List ).

% Same Aiport Case
fly(Airport, Airport) :-
   write(' Error: Same Airport '), nl, !, fail.

% Main Function
fly(Start, End) :-
   % Make sure airports exist
   airport(Start, _, _, _),
   airport(End, _, _, _),
   % Call path finder
   listpath( Start, End, List ), !, nl,
   writepath( List ), true.

% No Path case
fly(Start, End) :-
   % Make sure airports exist
   airport(Start, _, _, _),
   airport(End, _, _, _),
   % Only reachable if no path and airports exist
   write(' Error: No Path '), nl, !, fail.

% Non-existent Airports
fly(_, _) :-
   write(' Error: Non-existent Airports '), nl, !, fail.
