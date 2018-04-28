# haskell-6
haskell-6 created by GitHub Classroom
In this haskel project, we are creating a game called othello.
It can be played either single player or multi player.

In order to run this game or haskel project, follow the 
following steps:

1) Open the terminal using Ctrl + Alt + T

2) Enter the directory where you would like to copy the repository. 
   This can be anywhere in your local file system, like your home 
   directory. For example:  cd ~/

3) Navigate to the repository’s Code tab.

4) Click Clone or download.

5) Copy the URL provided.

6) Clone the repository by replacing <URL> with clone URL you copied in
   the previous step. The repository will be cloned into a new directory in this location.
      
            git clone <URL>
 
7) 

  Our game is having a few dependencies. First and foremost, we use a library called gloss for drawing
  all of our graphics. We could use cabal to install gloss globally, so that all Haskell code on the 
  computer could use it. However, with this approach we may eventually run into problems: if I have two 
  packages, A (which depends on gloss-1.0) and B (which requires gloss-1.2), having a single global 
  version of gloss won’t work, because we cannot have both A’s and B’s requirements satisfied simultaneously.
  

  Instead of installing gloss globally, we’re going to create a sandbox. A sandbox is a directory in which 
  cabal will (for the most part) ignore the global packages, and will instead install packages directly 
  to that directory.

8) Navigate into the directory of the repository you just created and open the
   terminal in this directory.

9) Turn this directory into a sandbox using command:

            cabal sandbox init
            
10) Now that we have a sandbox to play in, let’s create our first package. This command will turn 
    the directory into a Haskell package directory (as well as a sandbox):
    
            cabal init
            
11) We have to install the gloss library into the sandbox. Use the following command to
    install it :
    
              cabal install gloss==1.12.*
              
     In this command, we install gloss version 1.12.*
     
12) Next, once gloss is installed, we have to tell cabal that our package is allowed to use it. Find
    the line in haskell.cabal that mentions build-depends and change it to the following:
    
          build-depends:       base >=4.8 && <4.9, array >=0.5 && <0.6, gloss==1.12.* 
          
13) Now type the command :

            cabal run
            
    You will see the game running on your screen.
    
    ![Game](https://github.com/IITH-SBJoshi/haskell-6/blob/master/img/oth1.png)
