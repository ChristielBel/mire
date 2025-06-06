# Сотав команды
- Беликова Кристина 34
- Щербина Даниил 34
- Ширяев Максим 35/1

# Новый функционал
- Сообщения от игроков подписываются их именем (модификация say)
- Очки характеристик для игроков (модификация player)
- Распредление свободных очков характеристик (модификация player)
- Просмотр всех имеющихся игроков в комнате (модификация look)
- Система вызовов (create-challenge, join-challenge, start-challenge, forge)

# Бот bot
Бот хаотично перемещается по локации, в случае если ударится головой о стену кричит в чат "АААААААААААААА"

# Бот challenge-bot
Бот автоматически участвует в игровых вызовах (challenges), имитируя поведение реального игрока

# Mire

It's a nonviolent MUD. (Multi-User Dungeon)

## Usage

First make sure that you have `java` installed on your
machine. [OpenJDK](https://adoptopenjdk.net) is recommended. It should
be at least version 8, but newer versions (tested up to 17) should work too.

Do `./lein run` inside the Mire directory to launch the Mire
server. Then players can connect by telnetting to port 3333.

## Motivation

The primary purpose of this codebase is as a demonstration of how to
build a simple multithreaded server in Clojure.

Mire is built up step-by-step, where each step introduces one or two
small yet key Clojure principles and builds on the last step. The
steps each exist in separate git branches. To get the most out of
reading Mire, you should start reading in the branch called
[step-01-echo-server](http://github.com/technomancy/mire/tree/01-echo-server)
and continue from there.

While you can learn from Mire on its own, it has been written
specifically for the [PluralSight screencast on
Clojure](https://www.pluralsight.com/courses/functional-programming-clojure).
A [blog post](https://technomancy.us/136) steps through the codebase
and shows how to make minor updates for a more recent version of Clojure.

Copyright © 2009-2021 Phil Hagelberg
Licensed under the same terms as Clojure.
