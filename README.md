# Functional Image - Haskell project

This project show the use of images as functions that return a color for a point.

The project is describe here : https://github.com/vialette/fimage-squeleton

Student : Adrien Turiot (Dec. 2017)

## Getting Started 

Compile and launch :
```sh
> cabal sandbox init
> cabal install
> ./.cabal-sandbox/bin/fimage
```
## Result 

![colored-cross](/images/colored-cross.bmp)
![colored-tunnel](/images/colored-tunnel.bmp)

![gradient](/images/gradient.bmp)
![oblivion](/images/oblivion.bmp)
![sun](/images/sun.bmp)

We can create video by creating all the frames.
For example with this code 

```haskell
let images = [Image.gradient x Color.green Color.black | x <- [-1,-0.9..10]]
let names = [show n ++ ".bmp" | n <- [1..(length images)]]
F.mapM_ (write v w) (zip names images)
```
We merge the image sequence with another tool, see [Convert an image sequence to a movie](http://www.andrewnoske.com/wiki/Convert_an_image_sequence_to_a_movie)

At the end, we have [this video](/videos/video.avi).


## Thing added from the squeleton 

- The implementation of the default functions
- Color and ColorImage
- 2 functions to play with ColorImage (toColor and gradient)

