#!/usr/bin/env swipl

:- initialization(main, main).
:- use_module(library(socket)).
:- use_module(library(random)).
:- use_module(library(thread)).
:- dynamic server_messages/1.

main :-
    sleep(2),
    client(localhost, 3333).

client(Host, Port) :-
    setup_call_cleanup(
        tcp_connect(Host:Port, Stream, []),
        (   thread_create(reader_thread(Stream), _, [detached(true)]),
            bot(Stream)
        ),
        close(Stream)
    ).

:- assertz(server_messages([])).

reader_thread(Stream) :-
    repeat,
    (   read_line_to_string(Stream, Line),
        (   Line == end_of_file
        ->  true, !
        ;   retract(server_messages(Current)),
            append(Current, [Line], New),
            assertz(server_messages(New)),
            fail
        )
    ).

get_last_message(Last) :-
    server_messages(Messages),
    (   Messages = [] -> Last = "";
        last(Messages, Last)
    ).

clear_server_messages :-
    retractall(server_messages(_)),
    assertz(server_messages([])).

bot(Stream) :-
    % Bot registration and setup
    setup_bot(Stream),

    % Join a challenge
    join_challenge(Stream, "BattleArena"),

    % Main forge loop
    forge_loop(Stream).

setup_bot(Stream) :-
    % Send bot name
    format(Stream, 'bot~n', []),
    flush_output(Stream),
    sleep(1),

    % Distribute points (power, agility, luck)
    format(Stream, '4~n', []), flush_output(Stream), sleep(1),  % Power
    format(Stream, '3~n', []), flush_output(Stream), sleep(1),  % Agility
    format(Stream, '3~n', []), flush_output(Stream), sleep(1).  % Luck

join_challenge(Stream, ChallengeName) :-
    % Join specified challenge
    format(Stream, 'join-challenge ~w~n', [ChallengeName]),
    flush_output(Stream),
    sleep(2),

    % Check if joined successfully
    get_last_message(Last),
    (   string_concat("Joined challenge '", ChallengeName, Prefix),
        sub_string(Last, 0, _, _, Prefix)
    ->  true
    ;   format(Stream, 'say Failed to join challenge!~n', []),
        flush_output(Stream),
        sleep(1)
    ).

forge_loop(Stream) :-
    random_between(1, 5, Delay),
    sleep(Delay),

    % Send forge command
    format(Stream, 'forge~n', []),
    flush_output(Stream),

    % Check response
    sleep(1),
    get_last_message(Last),
    (   sub_string(Last, 0, 6, _, "Forged")
    ->  format('Bot successfully forged!~n', [])
    ;   format('Bot failed to forge: ~w~n', [Last])
    ),

    clear_server_messages,
    forge_loop(Stream).