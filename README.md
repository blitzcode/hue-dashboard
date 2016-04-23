
# Hue Dashboard

### What's this?

An application providing a web based dashboard for monitoring and controlling [Philips Hue lights](http://www2.meethue.com/en-XX).

Implemented in Haskell, we're talking to the [Hue bridge](http://www2.meethue.com/en-us/productdetail/philips-hue-bridge) through its [REST API](http://www.developers.meethue.com/) using [http-conduit](https://www.stackage.org/package/http-conduit). The web interface is done using [threepenny-gui](https://wiki.haskell.org/Threepenny-gui). [Bootstrap](http://getbootstrap.com/) and [jQuery](https://jquery.com/) are used client-side.

The project in its current form is rather basic, but all the essential code is there. The program discovers the Hue bridge and whitelists a user, stores that configuration, can query all light information and display a dashboard to users connecting to its web server. The idea is that lights can be inspected and controlled on any device without installing an app. It's also simple to add new features like monitoring all bulbs for power consumption etc. Could be used to provide a custom dashboard on a cheap wall-mounted tabled. Work-in-progress.

![Web Interface](https://raw.github.com/blitzcode/hue-dashboard/master/web-interface.png)

Also see [this project on Blitzcode.net](http://www.blitzcode.net/haskell.shtml#hue-dashboard) for more information.

### What is Hue?

[Hue](http://www2.meethue.com/en-XX) is Philip's product range of "smart" light bulbs and switches. Hue devices use [ZigBee Light Link](http://www.zigbee.org/zigbee-for-developers/applicationstandards/zigbee-light-link/) mesh networking to communicate. Part of the Hue system is a bridge which connects the Hue devices to the network. The bridge offers both an integration into Apple's [HomeKit](http://www.apple.com/ios/homekit/) framework and its own [REST / HTTP / JSON API](http://www.developers.meethue.com/). There's a growing number of [apps](http://www.developers.meethue.com/otherapps/otherAppsIOS.html), home automation systems and 3rd party ZigBee devices working with Hue.

# Build & Setup

This project uses `stack`, so build with

    stack build

and run with

    stack exec haskell-hue

Like any Hue API application you'll have to authorize access to your bridge by pushlinking. The program will discover your bridge and prompt you to press the button on your bridge when running the first time. Subsequent runs will restore this configuration from the created `config.yaml`.

# Compatibility

This program was developed on OS X 10.10 with a v2 Hue bridge (the square HomeKit one) and a current (early 2016) firmware with a selection of original Hue lights and switches. It's of course possible that an older / newer bridge and 3rd party lights will not work correctly as no testing with them has been performed.

The program traces a lot of information and all errors / warnings to the console, please look for any error messages if things don't work as expected.

# Legal

This program is published under the [MIT License](http://en.wikipedia.org/wiki/MIT_License).

# Author

Developed by Tim C. Schroeder, visit my [website](http://www.blitzcode.net) to learn more.

