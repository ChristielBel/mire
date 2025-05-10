#!/usr/bin/env swipl

:- initialization(main, main).
:- use_module(library(socket)).
:- use_module(library(random)).
:- use_module(library(thread)).
:- use_module(library(pcre)).
:- dynamic server_messages/1, in_challenge/1.

main :-
    sleep(2),
    client(localhost, 3333).

client(Host, Port) :-
    setup_call_cleanup(
        tcp_connect(Host:Port, Stream, []),
        (
            thread_create(reader_thread(Stream), _, [detached(true)]),
            format("Bot connected to server~n", []),
            bot(Stream)
        ),
        close(Stream)
    ).

%–– Accumulate server lines
:- assertz(server_messages([])).
reader_thread(Stream) :-
    repeat,
      read_line_to_string(Stream, Line),
      ( Line == end_of_file -> !
      ; retract(server_messages(C)),
        append(C, [Line], New),
        assertz(server_messages(New)),
        fail
      ).

get_all_messages(Msgs) :- server_messages(Msgs).
clear_server_messages :-
    retractall(server_messages(_)),
    assertz(server_messages([])).

%–– Entry point
bot(Stream) :-
    setup_bot(Stream),
    listen_for_challenges(Stream).

%–– Introduce yourself
setup_bot(Stream) :-
    format(Stream,'bot~n',[]), flush_output(Stream), sleep(1),
    format(Stream,'4~n',[]),   flush_output(Stream), sleep(1),
    format(Stream,'3~n',[]),   flush_output(Stream), sleep(1),
    format(Stream,'3~n',[]),   flush_output(Stream), sleep(1).

%–– Wait for new-challenge announcements
listen_for_challenges(Stream) :-
    repeat,
      sleep(1),
      get_all_messages(Msgs),
      member(Line, Msgs),
      re_match("A new challenge '([^']+)'", Line),
      re_matchsub("A new challenge '([^']+)'", Line, Sub, []),
      Challenge = Sub.1,
      format("Detected new challenge: ~w~n", [Challenge]),
      clear_server_messages,
      handle_challenge(Stream, Challenge),
    fail.

%–– Join → wait → forge → loop back
handle_challenge(Stream, Challenge) :-
    retractall(in_challenge(_)),
    join_challenge(Stream, Challenge),
    wait_for_start,
    forge_loop(Stream),
    !,
    retractall(in_challenge(_)).

%–– Only join once per challenge
join_challenge(Stream, Challenge) :-
    ( in_challenge(Challenge)
    -> format("Already in challenge: ~w~n", [Challenge])
    ; format("Attempting to join challenge: ~w~n", [Challenge]),
      clear_server_messages,
      repeat,
        format(Stream, "join-challenge ~w~n", [Challenge]),
        flush_output(Stream),
        format('Sent command: join-challenge ~w~n', [Challenge]),
        sleep(2),
        get_all_messages(Msgs),
        (   member(L, Msgs),
            re_match("Joined challenge \\w+", L)
        ->  format("Joined challenge: ~w~n", [Challenge]),
            assertz(in_challenge(Challenge)),
            clear_server_messages,
            !
        ;   format("Join failed, retrying…~n", []),
            sleep(1),
            fail
        )
    ).

%–– Wait until any line starts with "Challenge started"
wait_for_start :-
    format("Waiting for challenge to start…~n", []),
    clear_server_messages,
    repeat,
      sleep(1),
      get_all_messages(Msgs),
      member(L, Msgs),
      re_match("Challenge started! Type 'forge' to earn points in the next 30 seconds!", L),
    !,
    format("Challenge started — beginning to forge…~n", []),
    clear_server_messages.

%–– Forge until someone wins, then return
forge_loop(Stream) :-
    repeat,
      random_between(1, 5, D), sleep(D),
      format(Stream, "forge~n", []), flush_output(Stream), sleep(1),
      get_all_messages(Msgs),
      (   member(W, Msgs),
          re_match("^Winner: .+ with \\d+ points!$", W)
      ->  format("~w~n", [W]),
          clear_server_messages
      ;   ( member(R, Msgs), sub_string(R,0,_,_,"Forged")
          -> format("Successfully forged!~n", [])
          ;  format("Forge reply missing or failed: ~w~n", [Msgs])
          ),
          clear_server_messages,
          fail
      ).
