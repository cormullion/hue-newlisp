hue-newlisp
===========

The Philips Hue lights can be easily programmed using newLISP. The Hue context provides the following functions:

First, find out the bridge's IP address and insert it into this function call:

    (define bridge-IP "192.168.1.1") 

Then choose a user-name (it must be at least 10 characters long):

    (define user-name "yourusername")

Call

    (initialize) 
    
to introduce yourself to the Philips bridge - you have 10 seconds to hit the button after calling this function...

Later you can check if you've already done this step:

    (initialized?)

To switch light number `n` on or off:

    (set-light n "on")

To switch all lights:

    (set-all-lights "on")
    
To set the colour of light `n`:

    (set-colour n saturation brightness hue)

To set the brightness supply a percentage:

    (set-brightness n p)

A flashing function: `times` is how many repetitions, `gap` is mark/space in milliseconds:

    (flash times gap)

Cycle through the colours in a given number of seconds, optionally setting a starting value (between 0 and 65535) and saturation (0 to 255):

    (colour-cycle n duration start-value saturation)
    (colour-cycle-selected light-ids duration saturation)

Fade a light to black (by reducing brightness) and then switch it off, taking `duration` seconds:

    (fade-to-black n duration)

Fade selected lights to black and then off taking `duration` seconds:

    (fade-selected-to-black '(1 3) 10)
    (fade-all-to-black duration)

Quickly switch all lights on and brightly:

    (quick-on)

Other functions:

    (get-all-lights)     - get numbers and names of all lights
    (get-all-light-ids)  - get the numbers of all lights 
    (get-light n)        - get full details of light `n`

I haven't yet bothered with groups and configurations...

Some of these (`*-selected`) are not available on Windows, which doesn't offer `sync/spawn` processes (the subprocesses do the jobs in parallel).

The whole timing idea based on `sleep` doesn't work, because the lights have delays when being commanded. Another approach using references to absolute time is required.

# Using Keyboard Maestro

You can easily add commands to Keyboard Maestro. For example:

    Macro Action:
    Execute Shell Script:
        #!/usr/bin/env newlisp
		(load (string (env "HOME") "/projects/hue-newlisp/hue-lisp.lsp"))
		(Hue:fade-all-to-black 60)
		(exit)
