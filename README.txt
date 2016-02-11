So, this uses Haskell.

You will need:
 - The latest version of Stack (https://haskellstack.org)
 - GHC 7.10.3
 - cabal-install 1.22.6.0
 - Cabal library 1.22.4.0

Once you have these installed, issue the following commands:
 > stack build
 > stack exec MP_1_test

This should generate, after a moment, a directory "rayt-imgs" with 5 images.
 - "ortho.png": The orthographic image
 - "persp1.png": The same scene with perspective rendering
 - "persp2.png": The same scene with perspective rendering and a different camera location
 - "single.png": New scene, perspective rendering, one sample in each pixel center
 - "multi.png": New scene, perspective rendering, multijittered sampling (9 samples per pixel)
