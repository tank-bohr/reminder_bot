reminder_bot [NOT READY][UNDER ACTIVE DEVELOPMENT]
=====

[![Build Status](https://travis-ci.org/tank-bohr/reminder_bot.svg?branch=master)](https://travis-ci.org/tank-bohr/reminder_bot)

A telegram [bot](https://t.me/tb_reminder_bot) helps you not to forget important stuff

Inspired by [slack reminders](https://get.slack.help/hc/en-us/articles/208423427-Set-a-reminder)

Usage
-----

tell the bot smth like

`remind me at 19:45 to take my pills`

and at 19:45 bot will modestly remind you

Supports only `remind me ...` commands for the time being


Build
-----

    $ make


Run
-----

    $ make run


Time format
-----------

  Bot relies on [legendary_goggles library](https://github.com/tank-bohr/legendary_goggles) for parsing time
