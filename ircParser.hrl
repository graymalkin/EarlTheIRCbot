% Records for the various types of message that can be sent
-record(privmsg, {target, from, message}).
-record(ping, {nonce}).
-record(user, {nick, username, host}).