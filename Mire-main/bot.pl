:- encoding(utf8).
:- debug(mirebot).
:- use_module(library(socket)).
:- dynamic exit/1.
:- dynamic(room_count/1).
room_count(0).


e(north) --> [north].
e(south) --> [south].
e(west) --> [west].
e(east) --> [east].
exits([Exit]) --> e(Exit).
exits([Exit|Exits]) --> e(Exit), exits(Exits).
parse_exits(Exits) --> [exits], exits(Exits).

parse(Tokens) :- phrase(parse_exits(Exits), Tokens, Rest), retractall(exit(_)), assert(exit(Exits)).

parse(_).


filter_codes([], []).
filter_codes([H|T1], T2) :-
  char_code(C, H),
  member(C, ['(', ')', ':']),
  filter_codes(T1, T2).
filter_codes([H|T1], [F|T2]) :-
  code_type(F, to_lower(H)),
  filter_codes(T1, T2).


process(Stream) :-
  exit([Direction|_]),
  format(atom(Command), 'move ~w~n', [Direction]),
  write(Command),
  write(Stream, Command),
  flush_output(Stream),
  retractall(exit(_)),
  retract(room_count(Count)),
  NewCount is Count + 1,
  assert(room_count(NewCount)).


process(_).

show_room_count :-
  room_count(Count),
  format('You have visited ~w rooms.~n', [Count]).


hello(Stream) :-
  writeln(Stream, 'bot123'),
  flush_output(Stream).

run(Stream) :-
  hello(Stream),
  loop(Stream).

loop(Stream) :-
  read_line_to_codes(Stream, Codes),
  filter_codes(Codes, Filtered),
  atom_codes(Atom, Filtered),
  tokenize_atom(Atom, Tokens),
  write(Tokens),
  parse(Tokens),
  nl,
  flush(),
  process(Stream),
  (Tokens = [quit] -> show_room_count ; true),
  loop(Stream).


 main :-
   setup_call_cleanup(
     tcp_connect(localhost:3333, Stream, []),
     run(Stream),
     close(Stream)).