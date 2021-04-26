# Haskell GHCi tool for creating simple PCM synths

## Requirments
Glasgow Haskell Compiler interactive 8.6.5+ 
ffplay  

## Qucik Start
To start  
```console
$ ghci Main.hs 
```
To play sample melody:
```console
λ> play sand 120  
```
This command plays list of notes, called sand, in 120 beats per minute  

To create custom melody, simply create list of Tuples  
(Note, Octave, Duration)  
Where Note is in letter notation, eq A, Cs or G  
Octave is number, eq 4 for middle  
Duration is time in relation to beats, eq 1 beat, or 0.5   

After that simply type:
```console
λ> play (yourNameOfList) (bpmOfYourChoice)
```

## Inspired By:
[![Watch the Video](https://i.ytimg.com/vi/FYTZkE5BZ-0/hqdefault.jpg)](https://www.youtube.com/watch?v=FYTZkE5BZ-0)

