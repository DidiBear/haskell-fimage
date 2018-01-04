# Functional Image - Haskell project

This project show the use of images as functions that return a color for a point.

The project is describe here : https://github.com/vialette/fimage-squeleton

(Dec. 2017)

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

## Thing added from the squeleton 

- The implementation of the default functions
- Color and ColorImage
- 2 functions to play with ColorImage (toColor and gradient)

