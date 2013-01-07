# POP3

Pop3 - is erlang library which provides work with pop3 protocol.

[![Build Status](https://travis-ci.org/0xAX/pop3.png)](https://travis-ci.org/0xAX/pop3)

## Features

  * USER/PASS - authorization
  * STAT command
  * LIST with args command
  * Delete message
  * RSET
  * NOOP
  * Retrive message with headers
  * Ssl support
  * quit :)

## Using

First of of all you need to got source code of `pop3` from github. Then go to the root dir of `pop3` build it:

```erlang
./rebar compile
```

Now you can use it:

```erlang
% start pop3 application
application:start(pop3).
% Some auth data
Data = {"user@mail.com", "password", "pop.mail.com", 110},
% Get stat
{ok, MessageCount, Size} = pop3:get_stat(Data),
% Data with message id
Data = {"user@mail.com", "password", "pop.mail.com", 110, 5},
% Delete message
ok = pop3:delete(Data2),
% RSET it
pop3:rset(Data),
% end of session
pop3:quit(Data).
```

## Contribute

  * Fork `pop3` repo
  * Make changes
  * Pull request
  * Thank you.

## TODO

  * Add APOP
  * ADD TOP

## AUTHORS

[@0xAX](https://twitter.com/anotherworldofw)

## Support

<a href='http://www.pledgie.com/campaigns/19013'><img alt='Click here to lend your support to: pop3 erlang library and make a donation at www.pledgie.com !' src='http://www.pledgie.com/campaigns/19013.png?skin_name=chrome' border='0' /></a>
