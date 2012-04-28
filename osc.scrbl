#lang scribble/doc

@(require scribble/manual
          planet/scribble
          scribble/bnf)

@title{@bold{OSC}: Open Sound Control Byte String Conversion}

@author[(author+email "John Clements" "clements@racket-lang.org")]

@(require (for-label racket
                     (this-package-in main)))

@defmodule/this-package[main]{This collection provides the means to translate
 to and from byte strings representing OSC (Open Sound Control) bundles and 
 messages.
 
 An OSC Element is either a bundle or a message.
 
 An OSC Bundle can contain other elements:
 
 @defproc[(osc-bundle (timestamp timestamp?) (elements (listof osc-element?)))
           osc-bundle?]{Produce an OSC bundle.}
 
                              
                              
                              
                              This collection provides a means to represent, read,
write, play, and manipulate sounds. It depends on the @racket[clements/portaudio] 
package to provide bindings to the cross-platform `PortAudio' library which appears
to run on Linux, Mac, and Windows.

It represents all sounds internally as stereo 16-bit PCM, with all the attendant
advantages (speed, mostly) and disadvantages (clipping).

Does it work on your machine? Try this example (and accept my 
apologies if I forget to update the version number):
@racketblock[
 (require (planet "main.rkt" ("clements" "rsound.plt" 2 9)))
  
 (play ding)
 ]

}

A note about volume: be careful not to damage your hearing, please. To take a simple example,
the @racket[sine-wave] function generates a sine wave with amplitude 1.0.  That translates into
the @emph{loudest possible sine wave} that can be represented. So please set your volume low, 
and be careful with the headphones. Maybe there should be a parameter that controls the clipping 
volume. Hmm.

@section{Sound Control}

These procedures start and stop playing sounds and loops.

@defproc[(play (rsound rsound?)) void?]{
 Plays an rsound. Plays concurrently with an already-playing sound, if there is one.}

@;{@defproc[(rsound-loop (rsound rsound?)) void?]{
 Plays an rsound repeatedly.  Continues looping until interrupted by 
 another sound command.}

@defproc[(change-loop (rsound rsound?)) void?]{
 When the current sound or loop finishes, starts looping this one instead.}
}
@defproc[(stop) void]{
 Stop all of the the currently playing sounds.}

@section{Sound I/O}

These procedures read and write rsounds from/to disk.

The RSound library reads and writes WAV files only; this means fewer FFI dependencies
(the reading & writing is done in Racket), and works on all platforms. 

@defproc[(rs-read (path path-string?)) rsound?]{
 Reads a WAV file from the given path, returns it as an rsound.
 
 It currently
has lots of restrictions (it insists on 16-bit PCM encoding, for instance), but deals 
with a number of common bizarre conventions that certain WAV files have (PAD chunks,
extra blank bytes at the end of the fmt chunk, etc.), and tries to fail
relatively gracefully on files it can't handle.

Reading in a large sound can result in a very large value (~10 Megabytes per minute);
for larger sounds, consider reading in only a part of the file, using @racket[rs-read/clip].}

@defproc[(rs-read/clip (path path-string?) (start nonnegative-integer?) (finish nonnegative-integer?)) rsound?]{
 Reads a portion of a WAV file from a given path, starting at frame @racket[start]  and ending at frame @racket[finish].
                                                                    
 It currently
has lots of restrictions (it insists on 16-bit PCM encoding, for instance), but deals 
with a number of common bizarre conventions that certain WAV files have (PAD chunks,
extra blank bytes at the end of the fmt chunk, etc.), and tries to fail
relatively gracefully on files it can't handle.}


@section{Reporting Bugs}

For Heaven's sake, report lots of bugs!