## Example Using SuperCollider

This code uses the osc framework to send messages to SuperCollider's 
synthesizer, "scsynth", to start playing a sine wave at 440 Hz.

Before trying it, you'll need to have supercollider's "scsynth"
running, and listening to UDP messages on port 13698.

On my machine, I run it like this:

    /Applications/SuperCollider/SuperCollider.app/Contents/Resources/scsynth -u 13698
 

After the server is running, use

    racket try-sending.rkt
    
It will try to send a status message
to the server. When you hit "return" the first time, it will try to
start a tone.  When you hit "return" a second time, it will try to
stop.


