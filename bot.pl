#!/usr/bin/env swipl

:- initialization(main, main).
:- use_module(library(socket)).
:- use_module(library(random)).
:- use_module(library(thread)).
:- dynamic server_messages/1.

main :-
    sleep(5),
    client(localhost, 3333).

client(Host, Port) :-
    setup_call_cleanup(
        tcp_connect(Host:Port, Stream, []),
        (   thread_create(reader_thread(Stream), _, [detached(true)]),
            bot(Stream)
        ),
        close(Stream)
    ).

% Инициализация хранилища сообщений
:- assertz(server_messages([])).

% Поток для чтения сообщений от сервера (без вывода в консоль)
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

% Получить текущие сообщения от сервера
get_server_messages(Messages) :-
    server_messages(Messages).

% Получить последнее сообщение от сервера
get_last_message(Last) :-
    server_messages(Messages),
    (   Messages = [] -> Last = "";
        last(Messages, Last)
    ).

% Очистить хранилище сообщений
clear_server_messages :-
    retractall(server_messages(_)),
    assertz(server_messages([])).

bot(Stream) :-
    % Регистрация бота
    format(Stream,'~s~n',["bot"]),
    flush_output(Stream),
    sleep(1),
    format(Stream,'~s~n',["1"]),
    flush_output(Stream),
    sleep(1),
    format(Stream,'~s~n',["1"]),
    flush_output(Stream),
    sleep(1),    
    format(Stream,'~s~n',["1"]),
    flush_output(Stream),
    sleep(1),    

    % Основной цикл отправки команд
    repeat,
    (   
        random_between(1, 4, Direction),
        (   Direction = 1 -> Command = "move north";
            Direction = 2 -> Command = "move south";
            Direction = 3 -> Command = "move east";
            Command = "move west"
        ),
        format(Stream, '~s~n', [Command]),
        flush_output(Stream),
        format('Sent command: ~s~n', [Command]),

        sleep(1),  % Даем время серверу ответить
        
        get_last_message(Last),
        (   Last = "> You can't go that way."
            ->  format('Bot hit his head! Ouch!~n', []),
                sleep(4),
                format(Stream, '~s~n', ["say AAAAAAAAAAAAAAAAAAAA"]),
                flush_output(Stream),
                sleep(1)
            ;   true
        ),
        
        % Очищаем сообщения после обработки
        clear_server_messages,
        
        sleep(5),
        fail  % Продолжаем цикл
    ).

