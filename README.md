hue-newlisp
===========

The Philips Hue lights can be easily programmed using newLISP. The Hue context provides the following functions:

First, define the bridge's IP address:

    (define bridge-IP "192.168.1.1") 

and the user-name (at least 10 characters long):

    (define user-name "yourusername")

Call

    (initialize) 
    
to introduce yourself to the Philips bridge - you have 10 seconds to hit the button...

Check if you've already done this step:

    (initialized?)

To switch light number `n` on or off:

    (set-light n "on")

To switch all lights:

    (set-all-lights "on")
    
To set the colour of light `n`:

    (set-colour n saturation brightness hue)

To set the brightness supply a percentage:

    (set-brightness n p)

A flashing function: `times` is how many repetitions, `gap` is milliseconds:

    (flash times gap)

A test function to step through colours in `n` steps:

    (test-colours steps)

Fade a light to black (by reducing brightness) and then switch it off, taking `duration` seconds:

    (fade-to-black n duration)

The next two functions are not available on Windows, which doesn't offer `sync/spawn` processes:

Fade selected lights to black and then off taking `duration` seconds:

    (fade-selected-to-black '(1 3) 10)
 
Fade all lights (won't work on Windows as it needs sync/spawn):

    (fade-all-to-black duration)

Quickly switch all lights on and brightly:

    (quick-on)

Other functions:

    (get-all-lights)     - get numbers and names of all lights
    (get-all-light-ids)  - get the numbers of all lights 
    (test) 
    (get-light n)        - get full details of light `n`

I haven't yet bothered with groups and configurations...

