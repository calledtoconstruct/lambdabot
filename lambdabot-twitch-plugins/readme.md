# Overview

The following example scripts can be used on start up and shutdown for integrating with Twitch.tv

These scripts should be created in the ./lambdabot/scripts folder

# onstartup.rc Script

twitch-auth oauth:??????????????????????????????
twitch-persist-connect twitch irc.chat.twitch.tv 6667 [bot-account-name]
twitch-join twitch:#[streamer-account-name]
admin + twitch:[streamer-account-name]
msg twitch:#[streamer-account-name] [message from the bot: Hey!  I'm here.]
offline

# shutdown.rc Script

msg twitch:#[streamer-account-name] [message from the bot: Goodbye!]
disconnect twitch

# onshutdown.rc

(empty)
