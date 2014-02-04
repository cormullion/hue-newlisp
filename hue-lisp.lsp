#!/usr/bin/env newlisp

;; @module Hue
;; @author cormullion
;; @description some functions to control the Philips Hue lights
;; @location http://github.com/cormullion
;; @version of date 2014-02-04 15:29:41
;; 

(context 'Hue)

(define bridge-IP "192.168.1.114") ; you need to find this out somehow
(define user-name "newlispuser")   ; must be at least 10 characters!

(define (initialize)
    ; first ever communication with the bridge
    ; if successful, the bridge will recognise commands from user-name
    ; first, ask the bridge for a new username
    (set 'j-response 
        (post-url 
            (format "http://%s/api" bridge-IP) 
            (format [text]{"devicetype":"newlispscript","username":"%s"}[/text] user-name)))
    ; You have to press the button now!
    (println "go and press the button  on the bridge! You have 10 seconds...")
    (sleep 10000)
    (set 'j-response 
        (post-url 
            (format "http://%s/api" bridge-IP) 
            (format [text]{"devicetype":"newlispscript","username":"%s"}[/text] user-name)))
    (json-parse j-response))

(define (initialized?)
    ; check that user-name already exists in configuration; if not, need to initialize 
    (set 'j-response 
        (get-url (format "http://%s/api/%s/config/" bridge-IP user-name)))
    (set 'config (json-parse j-response))
    ; the whitelist is available, so user-name must be accepted
    ; I was going to check if the user-name was there, but...
    (ref "whitelist" config))

(define (get-all-lights)
    (set 'j-response (json-parse 
        (get-url (format "http://%s/api/%s/lights" bridge-IP user-name)))))

(define (get-light n)
    (set 'j-response (json-parse
        (get-url (format "http://%s/api/%s/lights/%d" bridge-IP user-name n)))))

(define (set-light n state)
    (let (switch)
        (if (find state '("on" 1 true 'true 'on "yes"))
            (set 'switch "true")
            (set 'switch "false"))
        (set 'j-response (json-parse
            (put-url (format "http://%s/api/%s/lights/%d/state"  bridge-IP user-name n)
                     (format [text]{"on":%s}[/text] switch))))))

(define (get-all-light-ids)
    (let ((lights (Hue:get-all-lights))
          (light-ids '())
          (light-refs '()))
    (set 'light-refs (ref-all '(? (*)) lights match))
    (dolist (i light-refs)
        (push (int (first (lights i))) light-ids -1))
    light-ids))
        
(define (set-all-lights state)
    (let ((light-ids (get-all-light-ids)))
       (dolist (light light-ids)
           (set-light light state))))

(define (set-colour n (saturation 128) (brightness 255) (hue 0))
    ; saturation is 0 to 255, brightness is 0 to 255, hue is 0 to 65535
    ; hue is 0 red, 12750 yellow, 36210 green, 46920 blue, 56100 magenta, 65280 red
    (set 'j-response (json-parse
       (put-url (format "http://%s/api/%s/lights/%d/state" bridge-IP user-name n)
                (format [text]{"on":true, "sat":%d, "bri":%d,"hue":%d}[/text] saturation brightness hue)))))

(define (set-brightness n (percentage 100))
    (let ((value (max (min (* percentage 2.55) 255) 0))) 
    (set 'j-response (json-parse
       (put-url (format "http://%s/api/%s/lights/%d/state" bridge-IP user-name n)
                (format [text]{"on":true, "bri":%d}[/text] value))))))

(define (flash times gap)
    ; how many cycles, gap in milliseconds
     (let ((light-ids (get-all-light-ids)))
        (dotimes (n times)
            (dolist (light light-ids)
                (set-light light "on"))
            (sleep gap)
            (dolist (light light-ids)
                (set-light light "off"))        
            (sleep gap))))

(define (test-colours steps)
    ; step through the colours, rest for a second between each 
    (let ((light-ids (get-all-light-ids)))
        (for (i 0 steps)
            (dolist (light light-ids)
                (set-colour light 255 255 (* i (/ 65535 steps))))
            (sleep 1000))))

(define (fade-to-black n (duration 10))
  ; duration in seconds
  ; fade a light to black, then switch off
  ; we'll just change the brightness, leaving saturation and hue as they are
  (for (level 100 0 10)
       (set-brightness n level)
       (sleep (/ (* duration 1000) 10)))
  (set-light n "off"))

(define (fade-selected-to-black light-ids (duration 10))
  ; fades all lights 
  ; not Windows (uses spawn/sync)
  ; duration in seconds
    (dolist (light light-ids)
        (spawn (sym (string "light" light)) (fade-to-black light duration))))

(define (fade-all-to-black (duration 10))
  ; fades all lights to black
  ; not Windows (uses spawn/sync)
  ; duration in seconds
   (let ((light-ids (Hue:get-all-light-ids)))
    (dolist (light light-ids)
        (spawn (sym (string "light" light)) (fade-to-black light duration)))
    ; no need to wait :)
    ;(until (sync 2000) (println " waiting... "))
    ;(println "completed")
    ))

(define (quick-on)
    (map (fn (light) (Hue:set-colour light 0 255 15000)) 
        (Hue:get-all-light-ids)))

(define (test)
    (get-all-lights)
    (flash 10 1000) ; they're off when this is finished
    (set-all-lights true)
    (test-colours 10)
    (set-brightness 1 100)
    (set-brightness 2 50)
    (set-brightness 3 20)    
    (fade-to-black 1 10)
    (fade-to-black 2 5)
    (fade-all-to-black 5)
    (set-light 1 true)
    (quick-on))

; see if initialize is needed?

(if (initialized?) "Hue lights are ready" "Not ready: needs setting up")

;eof