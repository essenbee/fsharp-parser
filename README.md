# fsharp-parser

*2019-05-20*

This is a project that was begun on my live coding stream [Codebase Alpha](https://twitch.tv/codebasealpha) episode 27. That episode is archived to my [Youtube channel](https://www.youtube.com/channel/UCFFtfkaWjMb9UMDpPVnC1Sg), along with all of my other videos. My special guest on episode 27 was Mårten Rånge, and he led me through the process of building a parser in F# from first principles.

We felt that building a parser from scratch using the F# programming language and using only a functional style would be interesting and informative, hopefully yielding a better understanding of the functional paradigm through the medium of creating a genuinely useful software artefact. It is my belief that the episode was successful in that regard! I certainly learned a lot and the content certainly stretched my mind and my imagination.

Currently, the code is in the same state as it was at the end of the stream. I do hope to have Mårten on again sometime soon to pick up where we left off, and develop the parser to such an extent, that we can then begin to look at parsing frameworks such as _FParsec_, and understand them at a deep level. I may make a pass through the code to make some of the code a little more verbose with respect to the naming of parameters. The functional style can be mathematically terse!

Bear in mind that the stated aim of the stream was to end up with software that could parse 1+2 and yield the answer 3. This was achieved (and we went on to parse key-value pairs as well), but time constraints meant that the parser, although it compiles and yields results, remains incomplete as of the time of committing it to this repo. What out for updates in the coming months!
