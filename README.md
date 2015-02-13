# podcastifier

Podcastifier is a program for processing and assembling sound files. As you
probably can guess from the name, it mostly intended to automate the assembly
of podcasts. With Podcastifier you can automate assembling a final audio file
from separate sources. You might, for example, assemble a podcast from two
mp3 files -- one each for intro and outro music -- along with a wav file
containing the actual podcast content. As it creates the final sound file
Podcastifier can mix sound files and do fades or ducking, all under control
of a configuration file that you supply.

## Installation

You can build the Poscastifier jar file with lein:

    $ lein uberjar

## Usage

To use Poscastifier, just run the jar file as a Java application,
passing in the config file as the only argument:

    $ java -jar podcastifier-0.1.0-standalone.jar config-file


### Configuration File

Podcastifier takes a (EDN)[] configuration file, containing a map.
This map needs to have at least the following four entries:
 * :version
 * :output-file
 * :sounds
 * :final

In addition to these four entries, you can have any other entries
that you find useful in the configuration map.


#### The Basics

The `:version` entry just identifies the Podcastifier version and
should be set to `7`:

````edn
{:version 7
  ;; More to come!}
````

The `:output-file` entry specifies where the final sound file will
be written. This can be a simple string:

````edn
{:version 7
 :output-file "output.wav"
  ;; More to come!}
````

Or it can be a vector containing a Clojure format string and associated
values. Any keywords specified in the vector get looked up in the
configuration map, which makes this sort of thing possible:

```edn
{:version     7
 :show-name   "mypodcast"
 :label       "mary-smith"
 :number      66
 :output-file ["%s-%03d-%s.wav" :show-name  :number  :label]
 ;; Rest ommitted}
```

The `:sounds` entry is itself a map of keywords to values and defines
the individual sounds that are processed by Podcastifier. If the
value of an `:sounds` entry is a string, Podcastifier will read the
file specified by the string. So if you have this:

````edn
{:version 7
 :output-file "output.wav"
 :sounds {:interview "skype.wav" :music "theme.mp3"}
 ;; More to come!}
````

Then podcastifier will know about two sound samples, one read from
a wave file with the identifier `:interview` and the other read
from an MP3 file with the identifier `:music`.

Aside from `:sounds`, your config file will also need a `:final`
entry. The value of `:final` should be the ID of a sound and indicates
what sound should be written out. Thus if you simply wanted
Podcastifier to read in `skype.wav` and write it back out
to `output.wav` you would have something like this:

````edn
{:version 7
 :output-file "output.wav"
 :sounds {:interview "skype.wav"}
 :final :interview}
````

#### Processing Sounds

Now, Podcastifier would be pretty limited if all you could specify in
the `:sounds` part of the file were audio files to read. In fact, you
can do a lot more than that: The way to look at the values in the `:sounds`
map is as expressions that *evaluate to sound samples*. Strings, as we have
seen, evaluate to the sound sample read from the file specified by the
string. If, on the other hand, you put a vector in as your value, perhaps like this:

````edn
{:version 7
 :output-file "output.wav"
 :sounds {:podcast ["music.wav" "interview.wav"]}
 :final :podcast}
````

Then the resulting sound will be all of the sound samples in the vector concatenated
together. Thus, in the example above,  the sound associated with `:podcast` will
be the theme music followed immediately by the interview.

A keyword, in turn, evaluates to the sound associated with that keyword, so we could
recast our last example as:

````edn
{:version 7
 :output-file "output.wav"
 :sounds {:music "music.wav" :podcast [:music "interview.wav"]}
 :final :podcast}
````

If instead of a vector you supply a set, your sounds will get mixed instead of appended:

````edn
{:version 7
 :output-file "output.wav"
 :sounds {:interview "skype.wav"
          :background-music "background.mp3"
          :podcast #{:interview :background-music}   ;; Mix in the background music.
 :final :podcast}
````

Things get really interesting if you use a map as your sound value.
With a map you can specify all sorts of interesting audio processing.  For example,
if you wanted have your music fade in and out instead abruptly starting and stopping,
you might say this:

````edn
{:version 7
 :output-file "output.wav"
 :sounds {
   :interview "skype.wav"
   :intro-music {:source "theme.mp3" :fade-out #duration 10.0}
   :outro-music {:source "theme.mp3" :fade-in #duration 10.0}
   :podcast [:intro-music :interview]}
 :final :podcast}
````

Alternatively you could just use the first 20 seconds of the music
for your intro and the last 30 seconds for the outro:

````edn
{:version 7
 :output-file "output.wav"
 :sounds {
   :interview "skype.wav"
   :intro-music {:source "theme.mp3" :end #duration 20.0 :fade-out #duration 10.0}
   :outro-music {:source "theme.mp3" :tail #duration 30.0 :fade-in #duration 10.0}
   :podcast [:intro-music :interview :outro-music]}
 :final :podcast}
````

Here are all of the things that you can specify in a sound map:
 * :source The sound to start with, can be any valid sound spefication.
 * :fade-in <duration> Fade in the sound over the given duration.
 * :fade-out <duration> Fade the sound out over the given duration
 * :start <duration> :end <duration> Use the bit of the sound between the given times.
 * :duration <duration> Use only the given duration of sound, starting from the beginning.
 * :tail <duration> Use only the given duration of sound, but from the end.
 * :gain <db> Apply the gain. A positive gain amplifies the sound, negative quiets it.
 * :end-pad <duration> Extend the end of the sound by <duration>.
 * :start-pad <duration> Extend the beginning of the sound by <duration>.
 * :pre-silence <duration> Add <duration> of silence to the beginning of the sound.
 * :post-silence <duration> Add <duration> of silence to the beginning of the sound.
 * :normalize true Normalize the sound sample.
 * :pan <pan-value> Mixes the first two tracks of the sound into a stereo track with the given stereo separation.

A key part of of specifying sounds as a map is that the `:source` can be any
sound specification. Put a string in the `:source` slot and
Podcastifier will read the sound from the given file.
Put a map inside of your map and Podcastifier will do two layers
of audio processing. Specify a keyword and
Podcastifier will recursively look up the key in the `:sounds` map.

Thus if you weren't happy with the gain in your podcast theme, you could do
something like this:

````edn
{:version 7
 :output-file "output.wav"
 :sounds {
  :interview "skype.wav"
  :music {:source "theme.mp3" :gain #db 2.0}
  :intro-music {:source :music :end #duration 20.0 :fade-out #duration 10.0}
  :outro-music {:source :music :tail #duration 30.0 :fade-in #duration 10.0}
  :podcast [:intro-music :interview :intro-music]}
 :final :podcast}
```

Finally, you can also specify some additional processing with a list. For example,
you can do a nice  overlapping fade out like this:

````edn
  :music+interview (fade-out-music :music
                                   :interview
                                   :overlap #duration 35.0
                                   :fade #db -9
                                   :updown #duration 4.0)
````

Or a similar fade in:


````edn
  :music+interview (fade-out-music :interview
                                   :music
                                   :overlap #duration 35.0
                                   :fade #db -9
                                   :updown #duration 4.0)
````

You can also "duck" one sound over the other like this:

````edn
  :voice-over (duck :music :voice :offset #duration 55.0 :fade #db -9 :updown #duration 4.0)
````

To can also add a muted background sound, like this:

````edn
 :voice+background (background voice bg :extra #duration 7.0)
````

The `background` option will mix the two tracks together but fade the 2nd
(background) track just after first track ends. You can specify the
length of time that the background extends beyond the foreground
with the `:extra` option.


### Bugs


## License

Copyright © 2013-2015 Craig Andera and Russ Olsen

Distributed under the Eclipse Public License, the same as Clojure.
