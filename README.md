Animate DOT graphs together into a movie
========================================

Nothing works yet
=================

Working towards

    dotimate <dot file> <dot file> [[dot file] ..]

which will create a `frames` directory containing all the frames

Example
-------

    dotimate a.dot a.dot b.dot c.dot
    ffmpeg -r 15 -i frames/frame%05d.png movie.webm
    
About
-----
dotimate was created with the help of various Haskell hackers during the [UHac 2012](http://www.haskell.org/haskellwiki/DHD_UHac#Utrecht_Hackathon).
