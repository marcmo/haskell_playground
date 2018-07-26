-module(santa).
-author('ok@cs.otago.ac.nz'). % Richard A. O'Keefe
-export([start/0]).

%   This is an Erlang solution to "The Santa Claus problem",
%   as discussed by Simon Peyton Jones (with a Haskell solution using
%   Software Transactional Memory) in "Beautiful code".
%   He quotes J.A.Trono "A new exercise in concurrency", SIGCSE 26:8-10, 1994.
%
%	Santa repeatedly sleeps until wakened by either all of his
%	nine reindeer, back from their holidays, or by a group of three
%	of his ten elves.  If awakened by the reindeer, he harnesses
%	each of them to his sleight, delivers toys with them, and finally
%	unharnesses them (allowing them to go off on holiday).  If
%	awakened by a group of elves, he shows each of the group into
%	his study, consults with them on toy R&D, and finally shows them
%	each out (allowing them to go back to work).  Santa should give
%	priority to the reindeer in the case that there is both a group
%	of elves and a group of reindeer waiting.
%
%   Inspired by an old example of Dijkstra's, I solve this problem by
%   introducing two secretaries: Robin and Edna.  The reindeer ask Robin
%   for appointments.  As soon as she has nine waiting reindeer she sends
%   them as a group to Santa.  The elves as Edna for appointments.  As
%   soon as she has three waiting elves she sends them as a group to Santa.
%
%   The Haskell version is 77 SLOC of complex code.
%   The Erlang  version is 43 SLOC of straightforward code.

worker(Secretary, Message) ->
    receive after random:uniform(1000) -> ok end,  % random delay
    Secretary ! self(),			% send my PID to the secretary
    Gate_Keeper = receive X -> X end,	% await permission to enter
    io:put_chars(Message),		% do my action
    Gate_Keeper ! {leave,self()},	% tell the gate-keeper I'm done
    worker(Secretary, Message).		% do it all again

secretary(Santa, Species, Count) ->
    secretary_loop(Count, [], {Santa,Species,Count}).

secretary_loop(0, Group, {Santa,Species,Count}) ->
    Santa ! {Species,Group},
    secretary(Santa, Species, Count);
secretary_loop(N, Group, State) ->
    receive PID ->
        secretary_loop(N-1, [PID|Group], State)
    end.

santa() ->
    {Species,Group} =
	receive				% first pick up a reindeer group
	    {reindeer,G} -> {reindeer,G}% if there is one, otherwise
	after 0 ->
	        receive			% wait for reindeer or elves,
	            {reindeer,G} -> {reindeer,G}
	          ; {elves,G}    -> {elves,G}
	        end			% whichever turns up first.
	end,
    case Species
      of reindeer -> io:put_chars("Ho, ho, ho!  Let's deliver toys!\n")
       ; elves    -> io:put_chars("Ho, ho, ho!  Let's meet in the study!\n")
    end,
    [PID ! self() || PID <- Group],	% tell them all to enter
    [receive {leave,PID} -> ok end	% wait for each of them to leave
	|| PID <- Group],
    santa().

spawn_worker(Secretary, Before, I, After) ->
    Message = Before ++ integer_to_list(I) ++ After,
    spawn(fun () -> worker(Secretary, Message) end).

start() ->
    io:put_chars("starting..."),
    Santa = spawn(fun () -> santa() end),
    Robin = spawn(fun () -> secretary(Santa, reindeer, 9) end),
    Edna  = spawn(fun () -> secretary(Santa, elves,    3) end),
    [spawn_worker(Robin, "Reindeer ", I, " delivering toys.\n")
     || I <- lists:seq(1, 9)],
    [spawn_worker(Edna,  "Elf ",      I, " meeting in the study.\n")
     || I <- lists:seq(1, 10)].

