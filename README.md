
# Hue Dashboard

### What's this?

Hue Dashboard is a web application for comfortably controlling [Philips Hue lights](http://www2.meethue.com/en-XX) from any device with a browser.

Accessing the web interface from an iPhone:

![Web Interface](https://raw.github.com/blitzcode/hue-dashboard/master/doc-img/web-interface-iphone-1.jpg)
![Web Interface](https://raw.github.com/blitzcode/hue-dashboard/master/doc-img/web-interface-iphone-2.jpg)

And the same on the iPad (somewhat older screenshot):

![Web Interface](https://raw.github.com/blitzcode/hue-dashboard/master/doc-img/web-interface-ipad.jpg)

Another view on a Mac desktop browser:

![Web Interface](https://raw.github.com/blitzcode/hue-dashboard/master/doc-img/web-interface-mac.jpg)

### Features

- Aims to be an ideal control panel for daily operation of your Hue lights
- No hierarchy, menu diving or anything like that; switching on the lights should be fast & simple!
- Works with any modern browser
- Adjusting individual lights or the automatically created groups is comfortable both with a mouse and touch based input

![Web Interface](https://raw.github.com/blitzcode/hue-dashboard/master/doc-img/web-interface-group-and-light.png)

- Light groups can be shown / hidden, preferences are stored on a per-browser basis
- Scene creation interface allows easy building of scenes from the current light state, also editing and updating of scenes is supported

![Web Interface](https://raw.github.com/blitzcode/hue-dashboard/master/doc-img/web-interface-scene-creator.png)

- Scenes are displayed with a preview of the lights they are going to change, and which groups they touch

![Web Interface](https://raw.github.com/blitzcode/hue-dashboard/master/doc-img/web-interface-scenes.png)

- Existing scenes created in other Hue applications can be activated as well

![Web Interface](https://raw.github.com/blitzcode/hue-dashboard/master/doc-img/web-interface-scenes-imported.png)

- Lights can be put in a 'color loop' mode where they cycle between all available colors
- Clicking a light / group caption makes the lamps blink (can be used as a crude form of communication!)
- All official Philips Hue lights are recognized and displayed with the appropriate graphics
- Color temperature lights get their own simpler color picker

![Web Interface](https://raw.github.com/blitzcode/hue-dashboard/master/doc-img/web-interface-ct-picker.png)

- The UI is done with vector graphics and looks crisp on retina displays
- Schedule system allows automatic triggering of scenes at specified times, supports special actions like turning a scene off or making the lights blink, can be scheduled only on certain weekdays, checks for differences between client & server time to avoid surprises, schedules can be deactivated as well

![Web Interface](https://raw.github.com/blitzcode/hue-dashboard/master/doc-img/web-interface-schedules-1.png)
![Web Interface](https://raw.github.com/blitzcode/hue-dashboard/master/doc-img/web-interface-schedules-2.png)

- On-screen light status responds in real-time to changes with smooth animations and transitions
- Reliable enough to run 24/7 and be used by many people from many devices simultaneously
- A special tile to turn all lights off

![Web Interface](https://raw.github.com/blitzcode/hue-dashboard/master/doc-img/web-interface-all-lights.png)

- Server has been tested on OS X and Ubuntu, needs very little system resources to run
- Can be deployed on a Raspberry Pi, even features a server control panel for shutdown / reboot and monitoring CPU / RAM usage

![Web Interface](https://raw.github.com/blitzcode/hue-dashboard/master/doc-img/web-interface-admin.png)

### Implementation

Hue Dashboard is implemented in [Haskell](http://www.haskell.org), talking to the [Hue bridge](http://www2.meethue.com/en-us/productdetail/philips-hue-bridge) through its [REST API](http://www.developers.meethue.com/) using [http-conduit](https://www.stackage.org/package/http-conduit). The web interface is done using [threepenny-gui](https://wiki.haskell.org/Threepenny-gui) and [blaze-html](https://hackage.haskell.org/package/blaze-html). [Bootstrap](http://getbootstrap.com/) and [jQuery](https://jquery.com/) are used client-side.

Also see [the project page on Blitzcode.net](http://www.blitzcode.net/haskell.shtml#hue-dashboard).

### What is Hue?

[Hue](http://www2.meethue.com/en-XX) is Philips' product range of "smart" light bulbs and switches. Hue devices use [ZigBee Light Link](http://www.zigbee.org/zigbee-for-developers/applicationstandards/zigbee-light-link/) mesh networking to communicate. Part of the Hue system is a bridge which connects the Hue devices to the network. The bridge offers both an integration into Apple's [HomeKit](http://www.apple.com/ios/homekit/) framework and its own [REST / HTTP / JSON API](http://www.developers.meethue.com/). There's a growing number of [apps](http://www.developers.meethue.com/otherapps/otherAppsIOS.html), home automation systems and 3rd party ZigBee devices working with Hue.

# Build & Setup

This project uses [stack](http://docs.haskellstack.org/en/stable/README/), so build with

    stack build

and run with

    stack exec hue-dashboard

If you've never build a Haskell application before and have nothing prepared on the system, the complete list of steps on OS X would be something like this, assuming you have at least [Homebrew](http://brew.sh/) setup:

    brew install haskell-stack
    git clone https://github.com/blitzcode/hue-dashboard.git
    cd hue-dashboard
    stack setup
    stack build
    stack exec hue-dashboard

Like any Hue API application you'll have to authorize access to your bridge by pushlinking. The program will discover your bridge and prompt you to press the button on your bridge when running the first time. Subsequent runs will restore this configuration from the created `config.yaml`.

After startup connect to `localhost:8001` to view the dashboard. Be advised that the dashboard might also be visible to others on the network.

There's also a range of command line options to customize behavior:

```
Usage: hue-dashboard [OPTION...]
  -p PORT     --port=PORT              network port (default: 8001)
              --localhost              only bind to localhost
  -i SECONDS  --poll-interval=SECONDS  bridge poll interval (default: 1)
  -t LEVEL    --trace-level=LEVEL      execution trace level (default: i)
                                         n = none
                                         e = errors only
                                         w = warnings and errors
                                         i = infos, warnings and errors
              --trace-file=FILE        output file for execution trace (default: none)
              --trace-no-color         no ANSI colors for trace output
  -e          --trace-no-echo          disable echo execution trace to stdout
              --trace-append           append execution trace file instead of overwriting
              --trace-http             trace web server events
  -h          --help                   print usage information
```

The groups displayed are based on a prefix word. So 'Kitchen Ceiling' and 'Kitchen Table' will be put into the 'Kitchen' group. You might need to rename your lights to take full advantage of this feature.

# Compatibility

This program was developed on OS X 10.10 (also tested on Ubuntu 16.04 LTS and Raspbian) with a v2 Hue bridge (the square HomeKit one) and a current (early 2016) firmware with a selection of original Hue lights and switches. It's of course possible that an older / newer bridge and 3rd party lights will not work correctly as no testing with them has been performed.

The program traces a lot of information and all errors / warnings to the console, please look for any error messages if things don't work as expected.

# Raspberry Pi

![Raspberry Pi](https://raw.github.com/blitzcode/hue-dashboard/master/doc-img/raspberry-pi.jpg)

The server for Hue Dashboard needs to live somewhere, and a small ARM machine is an obvious choice if you don't already have a PC running 24/7 in your home. The state of Haskell on ARM is rather unsatisfactory at the moment, so here's a list of steps that worked for me. Hue Dashboard itself has very modest needs, the only real challenge is getting it build. It might be easier to build on an emulator like QEMU or perhaps by renting a cheap ARM VPS with more RAM, but this guide will focus on building on the actual Raspberry Pi.

* This worked for an Raspbery Pi 3 Model B, other models may or may not give the same results
* Prepare an SD card with `2016-03-18-raspbian-jessie-lite.img` as the OS image, download from [here](http://downloads.raspberrypi.org/raspbian_lite/images/raspbian_lite-2016-03-18/) or current version [here](https://www.raspberrypi.org/downloads/raspbian/). See [this guide](https://www.raspberrypi.org/documentation/installation/installing-images/) for how to write the SD card. You want at least a 16GB card, Haskell builds are large
* Insert SD card, power up your RPi, ssh into it. Default hostname is `raspberrypi`, user `pi` password `raspberry`. So `ssh pi@raspberrypi` should work. If the host can't be found, check the DNS settings or just use the IP your router assigned
* You might want to setup [key based login](https://debian-administration.org/article/530/SSH_with_authentication_key_instead_of_password) now for convenience
* Use [raspi-config](https://www.raspberrypi.org/documentation/configuration/raspi-config.md) to enlarge the partition to use the entire SD card and set the video/cpu memory split (Advanced Options) to the lowest amount of video memory possible. We need as much RAM and storage as we can get. Reboot
* Update with `sudo apt-get update`
* Now we need GHC. Don't bother with a GHC from the repository, it's ancient. Download the 7.10.3 bindist for ARMv7 from the [official site](http://downloads.haskell.org/%7Eghc/7.10.3/ghc-7.10.3-armv7-deb8-linux.tar.xz). Unpack (`tar xf archive.tar`) and do the whole `./configure` / `make install` dance
* Verify that you have a working GHC 7.10.3, i.e. `ghc --version`
* To do anything with it, you need the GMP library, install with `sudo apt install libgmp-dev`
* Now `ghci` should work, test that
* To actually compile something, we need LLVM. The [download page for our GHC](https://www.haskell.org/ghc/download_ghc_7_10_3) recommends LLVM 3.5. It's in the repository, but don't bother installing it with `apt`. It doesn't actually work, just produces executables that fail with a cryptic error. We need a version with a few bugfixes included, `3.5.2`. Download the bindist from the [official LLVM download page](http://llvm.org/releases/download.html) ([direct download link](http://llvm.org/releases/3.5.1/clang+llvm-3.5.1-armv7a-linux-gnueabihf.tar.xz)).
* Extract the LLVM tar file and `rsync` it over `/usr/local` so it's installed in the path. Verify with `opt --version`
* Now we should be able to build a GHC program, try to compile & run a Hello World to make sure everything is working
* We need Stack to build Hue Dashboard, but there's no bindist for ARM. And we can't compile it from source without Cabal. The Cabal in the repository is too old to work with our GHC, so don't bother. We need to build a more modern Cabal ourselves first.
* Get the source code for Cabal. Cloning the git repository is an option, so first `sudo apt install git` and then `git clone https://github.com/haskell/cabal.git`
* The `cabal-install` directory contains `bootstrap.sh` for building Cabal without Cabal. It only works for releases though, so check out `v1.22.8.0` / `1352025e823536ac8c29a823a1ad33adccc86a04` first
* Build with `EXTRA_CONFIGURE_OPTS="" ./bootstrap.sh` so it doesn't do a profile build as well
* There might be a step or two extra to install it into the actual `~/.cabal/bin` directory, the script should tell you. Might also want to do a `cabal update` and add the bin directory to your path, i.e. `PATH="$HOME/.cabal/bin/:$PATH"`
* Now we should have a working Cabal, give it a try
* We're now ready to install Stack, but `cabal install stack` is unlikely to work. We especially don't want to do it this way as any error discards the progress made and we have to start over. Start with `cabal unpack stack` (v1.1.0 was used here)
* First step, get all of Stack's numerous dependencies installed. Before we do that, we must anticipate a bug that would otherwise hours down the line cause linking of Stack to fail with thousands of errors like these:
```
/usr/bin/ld.gold: warning: cannot scan executable section 1 of /home/pi/.cabal/lib/arm-linux-ghc-7.10.3/filelock-0.1.0.1-0GuYZFry5kL0ASYgccAvYE/libHSfilelock-0.1.0.1-0GuYZFry5kL0ASYgccAvYE.a(Flock.o) for Cortex-A8 erratum because it has no mapping symbols.
```
Looks like a GHC bug, see [here](https://phabricator.haskell.org/D1599) and [here](https://ghc.haskell.org/trac/ghc/ticket/11205). To work around this we have to build everything with `--disable-library-stripping --disable-executable-stripping` and / or set `executable-stripping: False` + `library-stripping: False` in Cabal's `~/.cabal/config`.
* Now with that in mind, we can install the dependencies with `cabal install --dependencies-only`
* This will take a very long time and may or may not work immediately. The reason is that 1GB of memory is very little for building complex Haskell libraries and SD cards have very slow I/O. To control memory usage it can sometimes be enough to simply restart the build (all already build dependencies will not be rebuild, thankfully). There's also the option of controlling memory usage by compiling single threaded (`-j1`) and passing heap options to GHC (`--ghc-option=+RTS --ghc-option=-M500m --ghc-option=-RTS`). It's best to babysit the build a bit, monitor with a tool like `htop` and restart / tweak options once it hits the 100MB swap file and everything grinds to a halt. But it should work, with a bit of patience.
* Now we have the dependencies, we can build Stack. There's another bug to work around. The build will eventually fail with errors like these:
```
Building stack-1.1.0...
Preprocessing library stack-1.1.0...
[53 of 86] Compiling Stack.Constants  ( src/Stack/Constants.hs, dist/build/Stack/Constants.o )
src/Stack/Constants.hs:291:7:
    cannot find normal object file `dist/build/Stack/Types/PackageName.dyn_o'
    while linking an interpreted expression
```
Also see this [bug report](https://github.com/commercialhaskell/stack/issues/2096). The workaround is to invoke `cabal build` with `--ghc-options=-dynamic-too`.
* Building stack will take a long time, similar suggestions for managing memory as for building the dependencies apply
* A final `cabal install` to put Stack in the Cabal bin directory / path
* Now we have a working Stack, time to build Hue Dashboard. Get the source with `git clone https://github.com/blitzcode/hue-dashboard.git`. This guide was written when `164729ef0d28e749ccbfd135f0e3ea5ced8ca8f1` was the current revision, but it's probably best to try `HEAD`
* This time there's nothing really special to do, basically `stack build` should work. No special options required. Hue Dashboard itself builds relatively quickly, but the dependencies will require the same careful tweaking and babysitting as with building Stack
* Once building is done, verify with `stack exec hue-dashboard` that everything works
* It's recommended to setup Hue Dashboard as a daemon so it automatically starts when the RPi is powered up. I used [daemontools](https://cr.yp.to/daemontools.html), also see [this](https://info-beamer.com/blog/running-info-beamer-in-production) great tutorial
* Since moving the Hue Dashboard executable to a different location or invoking Stack from another user etc. can be a bit tricky at this point, using the following as the `run` script for the daemon might be easiest:
```
#!/bin/sh
cd /home/pi/hue-dashboard/
exec .stack-work/dist/arm-linux/Cabal-1.22.5.0/build/hue-dashboard/hue-dashboard 2>&1
```
* On ARM machines Hue Dashboard displays a 'Server' tile, allowing for shutdown and reboot of the Raspberry Pi without using SSH
* This hopefully worked out all fine, be sure to file bug reports with the respective parts of the Haskell ecosystem to make sure it keeps getting easier

# TODO

There's a large amount of `TODO` comments around the code, pointing out potential bugs and limitations, recording my thoughts for future improvements.

# Legal

This program is published under the [MIT License](http://en.wikipedia.org/wiki/MIT_License).

# Author

Developed by Tim C. Schroeder, visit my [website](http://www.blitzcode.net) to learn more.

